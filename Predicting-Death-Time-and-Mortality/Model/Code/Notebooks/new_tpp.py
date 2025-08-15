#!/usr/bin/env python3
"""
pdtm_two_phase_lazy.py

Two-phase PDTM evaluation with:
 - optional gating on positive-probability (--pos-prob-gate)
 - default gating: symmetric confidence (max(p, 1-p))
 - lazy loading: when threshold == 1.0, Phase-1 is NOT loaded (baseline: Phase-2 only)

Usage examples:
  python new_tpp.py --threshold 0.9
  python new_tpp.py --threshold 0.3 --pos-prob-gate
  python new_tpp.py --threshold 1.0   # baseline (Phase-2 only; Phase-1 not loaded)
"""
import os
import argparse
import numpy as np
import pandas as pd
import joblib
import dotenv
import psycopg2
from typing import Tuple
from sklearn.metrics import accuracy_score, roc_auc_score
from sklearn.metrics import precision_score, recall_score, f1_score, classification_report, confusion_matrix
# at top: ensure your custom transformers are importable when loading pickles
from custom_transformers import ItemSelector, MultiItemSelector, CustomLabelBinarizer
from utils import connect_db


# ---------------------------
# Helpers
# ---------------------------
def descretize(x: float, cutoff1: float, cutoff2: float) -> int:
    """Convert continuous death-time to 3 classes: <cutoff1, <cutoff2, else."""
    if pd.isna(x):
        return -1
    if x < cutoff1:
        return 0
    elif x < cutoff2:
        return 1
    else:
        return 2

def evaluate(y_true, y_pred):
    """Print detailed metrics: accuracy, precision, recall, F1, report, confusion matrix."""
    from sklearn.metrics import precision_score, recall_score, f1_score, classification_report, confusion_matrix
    acc = accuracy_score(y_true, y_pred)
    pm = precision_score(y_true, y_pred, average='micro', zero_division=0)
    pM = precision_score(y_true, y_pred, average='macro', zero_division=0)
    rm = recall_score(y_true, y_pred, average='micro', zero_division=0)
    rM = recall_score(y_true, y_pred, average='macro', zero_division=0)
    f1m = f1_score(y_true, y_pred, average='micro', zero_division=0)
    f1M = f1_score(y_true, y_pred, average='macro', zero_division=0)
    # print(f"Accuracy: {acc:.4f}\n"
    #       f"Precision Micro: {pm:.4f}, Macro: {pM:.4f}\n"
    #       f"Recall Micro: {rm:.4f}, Macro: {rM:.4f}\n"
    #       f"F1 Micro: {f1m:.4f}, Macro: {f1M:.4f}\n")
    # print("Classification Report:\n", classification_report(y_true, y_pred, zero_division=0))
    # print("Confusion Matrix:\n", confusion_matrix(y_true, y_pred))

# ---------------------------
# DB connector
# ---------------------------
# def connect_db(query: str) -> pd.DataFrame:
#     dotenv.load_dotenv('.env')
#     os.environ.pop('USER', None)
#     conn = psycopg2.connect(
#         user=os.environ['DB_USER'],
#         password=os.environ['PASSWORD'],
#         host=os.environ['HOST'],
#         port=int(os.environ['PORT']),
#         database=os.environ['DATABASE']
#     )
#     df = pd.read_sql(query, conn)
#     conn.close()
#     return df

# ---------------------------
# Model loaders (lazy)
# ---------------------------
def load_phase2(window: int):
    p2 = f'phase2_gridsearch_{window}h.pkl'
    if not os.path.exists(p2):
        raise FileNotFoundError(f"Missing Phase2 model file: {p2}")
    return joblib.load(p2)

def load_phase1_and_2(window: int):
    p1 = f'phase1_gridsearch_{window}h.pkl'
    p2 = f'phase2_gridsearch_{window}h.pkl'
    if not os.path.exists(p1) or not os.path.exists(p2):
        raise FileNotFoundError(f"Missing model files: {p1} or {p2}")
    m1 = joblib.load(p1)
    m2 = joblib.load(p2)
    return m1, m2

# ---------------------------
# Core run function
# ---------------------------
def run(window: int, threshold: float, pos_prob_gate: bool = False):
    df = connect_db(f"SELECT * FROM mp_data_{window}hr")
    #print(f"\n=== Window: {window}h ({len(df)} samples) ===")

    # Basic sanity checks
    if 'hospital_expire_flag' not in df.columns:
        raise KeyError("Dataframe missing 'hospital_expire_flag' column required for Phase1.")
    # capture original df index for alignment
    idx_all = df.index

    # Ground-truth binary and (possibly) death-time
    y_bin = df['hospital_expire_flag'].astype(int).to_numpy()

    # Decide lazy load: if threshold >= 1.0, we consider the baseline and DO NOT load Phase-1
    load_phase1 = not (threshold >= 1.0)

    # Load models lazily
    if load_phase1:
        m1, m2 = load_phase1_and_2(window)
    else:
        m1 = None
        m2 = load_phase2(window)
        print("Baseline run (threshold >= 1.0): Phase-1 model NOT loaded. Phase-2 loaded only.")

    # Phase1: compute probs/preds if model available
    if m1 is not None:
        probs = m1.predict_proba(df)[:, 1]
        preds1 = (probs >= 0.5).astype(int)
        try:
            auc = roc_auc_score(y_bin, probs)
        except Exception:
            auc = float('nan')
        #print(f"Phase1 → AUC: {auc:.3f}, Acc: {accuracy_score(y_bin, preds1):.3f}")
    else:
        probs = None
        preds1 = None
        print("Phase1 skipped (not loaded).")

    # Gating: build mask of samples to invoke Phase2
    if threshold >= 1.0:
        mask = np.ones(len(df), dtype=bool)
        print("Threshold=1.0 → Phase2 on ALL samples.")
    else:
        if pos_prob_gate:
            if probs is None:
                raise RuntimeError("pos-prob gating requested but Phase-1 model not loaded.")
            mask = (probs >= threshold)
            #print(f"Using positive-prob gating: proba >= {threshold:.2f}")
        else:
            if probs is None:
                raise RuntimeError("confidence gating requested but Phase-1 model not loaded.")
            conf = np.maximum(probs, 1 - probs)
            mask = conf >= threshold
            print(f"Using symmetric confidence gating (max(p,1-p)) >= {threshold:.2f}")
        #print(f"Invoke Phase2 on {mask.sum()}/{len(df)} samples ({mask.mean():.2%})")

    # Short-circuit if nothing invoked
    df_phase2 = df.loc[mask]
    invocation_rate = mask.mean()
    if df_phase2.empty:
        print("No samples invoked for Phase2.")
        # Build a sensible end-to-end fallback prediction:
        if preds1 is not None:
            # map Phase1 binary -> coarse classes: 0 if negative, 2 if positive (same as earlier semantics)
            final_series = pd.Series(np.where(preds1 == 1, 2, 0), index=idx_all)
            y_true_series = pd.Series(_build_y_true_class(df), index=idx_all)
            print("End-to-end (fallback: Phase1 decisions only):")
            evaluate(y_true_series.values, final_series.values)
        else:
            print("No Phase1 and no Phase2 outputs — cannot compute end-to-end metrics.")
        print(f"Invocation rate: {invocation_rate:.4f}")
        return

    # Run Phase2 on invoked samples (Phase2 applied to df_phase2 rows)
    try:
        raw_p2_preds = m2.predict(df_phase2)
    except Exception as e:
        # If the pipeline expects specific columns / preprocessing, raise helpful message
        raise RuntimeError(f"Phase2 predict failed: {e}")

    raw_p2_preds = np.asarray(raw_p2_preds)

    # Convert regressor outputs to classes if needed
    if np.issubdtype(raw_p2_preds.dtype, np.floating):
        p2_classes = np.array([descretize(x, 24, 24 * 7) for x in raw_p2_preds], dtype=int)
    else:
        p2_classes = raw_p2_preds.astype(int)

    # Build a pandas Series of Phase2 predicted classes aligned to df_phase2.index
    p2_series = pd.Series(p2_classes, index=df_phase2.index)

    # Evaluate Phase2 on the invoked subset that has valid ground-truth death-time
    df_phase2_valid = df_phase2[
        df_phase2['hosp_deathtime_hours'].notna() & (df_phase2['hosp_deathtime_hours'] >= 0)
    ]
    if not df_phase2_valid.empty:
        y2_true = df_phase2_valid['hosp_deathtime_hours'].map(lambda x: descretize(x, 24, 24 * 7)).astype(int)
        preds2_aligned = p2_series.loc[df_phase2_valid.index].values
        #print("Phase2 (invoked) metrics on valid samples:")
        evaluate(y2_true.values, preds2_aligned)
    else:
        print("No valid Phase2 samples with ground-truth death-time to evaluate Phase2 classifier/regressor.")

    # --------------------------
    # Build end-to-end final classes for *all* rows:
    # - fallback: map Phase1 binary -> class: 0 if phase1==0 else 2 if phase1==1
    # - overlay Phase2 class predictions for invoked samples using p2_series (pandas .loc assignment)
    # - if Phase1 not loaded (baseline), final classes are Phase2 predictions for all rows (if available)
    # --------------------------
    if preds1 is not None:
        phase1_class_map = np.where(preds1 == 1, 2, 0)
        final_series = pd.Series(phase1_class_map, index=idx_all)
        # overlay Phase2 outputs using .loc (preserves index alignment)
        final_series.loc[p2_series.index] = p2_series.values
    else:
        # baseline: use Phase2 predictions wherever available; otherwise default to class 0
        final_series = pd.Series(0, index=idx_all, dtype=int)
        final_series.loc[p2_series.index] = p2_series.values
        if invocation_rate < 0.9999 and (p2_series.shape[0] != len(idx_all)):
            print("Warning: Phase-1 not loaded and Phase-2 produced predictions for only a subset of rows; "
                  "missing predictions defaulted to class 0.")

    # Build y_true_class using hosp_deathtime_hours where available otherwise fallback to hospital_expire_flag
    y_true_series = pd.Series(_build_y_true_class(df), index=idx_all)

    #print("End-to-end (combined) metrics across all samples (using fallback mapping):")
    #evaluate(y_true_series.values, final_series.values)

    acc = accuracy_score(y_true_series.values, final_series.values)
    pm = precision_score(y_true_series.values, final_series.values, average='micro', zero_division=0)
    pM = precision_score(y_true_series.values, final_series.values, average='macro', zero_division=0)
    rm = recall_score(y_true_series.values, final_series.values, average='micro', zero_division=0)
    rM = recall_score(y_true_series.values, final_series.values, average='macro', zero_division=0)
    f1m = f1_score(y_true_series.values, final_series.values, average='micro', zero_division=0)
    f1M = f1_score(y_true_series.values, final_series.values, average='macro', zero_division=0)

    # Invocation rate summary
    #print(f"Invocation rate (Phase2 invoked fraction): {invocation_rate:.4f}")
    print(f"{window}, {invocation_rate:.4f}, {acc}, {pm}, {pM}, {rm}, {rM}, {f1m}, {f1M}")


def _build_y_true_class(df: pd.DataFrame) -> np.ndarray:
    """Helper to build a 3-class ground-truth array for rows in df."""
    y_true_class = []
    for _, row in df.iterrows():
        if pd.notna(row.get('hosp_deathtime_hours', np.nan)) and row['hosp_deathtime_hours'] >= 0:
            y_true_class.append(descretize(row['hosp_deathtime_hours'], 24, 24 * 7))
        else:
            # fallback: use hospital_expire_flag -> map 0 -> class 0, 1 -> class 2
            y_true_class.append(2 if row['hospital_expire_flag'] == 1 else 0)
    return np.array(y_true_class, dtype=int)

# ---------------------------
# CLI
# ---------------------------
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--threshold', type=float, required=True,
                        help='Gating threshold [0.0-1.0]; use 1.0 to run Phase2 for all samples (baseline).')
    parser.add_argument('--pos-prob-gate', action='store_true',
                        help='Gate using positive-class probability (proba >= alpha) instead of symmetric confidence.')
    args = parser.parse_args()

    for w in [6, 12, 24]:
        run(w, args.threshold, pos_prob_gate=args.pos_prob_gate)

if __name__ == '__main__':
    main()
