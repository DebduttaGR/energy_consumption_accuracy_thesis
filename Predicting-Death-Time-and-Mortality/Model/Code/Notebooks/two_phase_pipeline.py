#!/usr/bin/env python3
import os
import argparse
import pandas as pd
import numpy as np
import joblib
import dotenv
import psycopg2
from custom_transformers import ItemSelector, MultiItemSelector, CustomLabelBinarizer
from sklearn.metrics import (
    accuracy_score, roc_auc_score, precision_score, recall_score,
    f1_score, classification_report, confusion_matrix
)


def descretize(x, cutoff1, cutoff2):
    """Convert continuous death-time to 3 classes: <cutoff1, <cutoff2, else."""
    if x < cutoff1:
        return 0
    elif x < cutoff2:
        return 1
    else:
        return 2


def evaluate(y_true, y_pred):
    """Print detailed metrics: accuracy, precision, recall, F1, report, confusion matrix."""
    acc = accuracy_score(y_true, y_pred)
    pm = precision_score(y_true, y_pred, average='micro')
    pM = precision_score(y_true, y_pred, average='macro')
    rm = recall_score(y_true, y_pred, average='micro')
    rM = recall_score(y_true, y_pred, average='macro')
    f1m = f1_score(y_true, y_pred, average='micro')
    f1M = f1_score(y_true, y_pred, average='macro')
    print(f"Accuracy: {acc:.4f}\n"
          f"Precision Micro: {pm:.4f}, Macro: {pM:.4f}\n"
          f"Recall Micro: {rm:.4f}, Macro: {rM:.4f}\n"
          f"F1 Micro: {f1m:.4f}, Macro: {f1M:.4f}\n")
    print("Classification Report:\n", classification_report(y_true, y_pred))
    print("Confusion Matrix:\n", confusion_matrix(y_true, y_pred))


def connect_db(query: str) -> pd.DataFrame:
    """Executes SQL query using DB credentials from .env and returns a DataFrame."""
    dotenv.load_dotenv('.env')
    os.environ.pop('USER', None)
    conn = psycopg2.connect(
        user=os.environ['DB_USER'],
        password=os.environ['PASSWORD'],
        host=os.environ['HOST'],
        port=int(os.environ['PORT']),
        database=os.environ['DATABASE']
    )
    df = pd.read_sql(query, conn)
    conn.close()
    return df


def load_models(window: int):
    """Load saved Phase1 & Phase2 pipeline objects for a given window."""
    p1 = f'phase1_gridsearch_{window}h.pkl'
    p2 = f'phase2_gridsearch_{window}h.pkl'
    if not os.path.exists(p1) or not os.path.exists(p2):
        raise FileNotFoundError(f"Missing model files: {p1}, {p2}")
    return joblib.load(p1), joblib.load(p2)


def run(window: int, threshold: float):
    """Run the two-phase inference and print metrics for a given window and threshold."""
    df = connect_db(f"SELECT * FROM mp_data_{window}hr")
    print(f"\n=== Window: {window}h ({len(df)} samples) ===")

    # Phase1 labels & inference
    y1 = df['hospital_expire_flag']
    m1, m2 = load_models(window)
    probs = m1.predict_proba(df)[:,1]
    preds1 = (probs >= 0.5).astype(int)
    print(f"Phase1 → AUC: {roc_auc_score(y1, probs):.3f}, Acc: {accuracy_score(y1, preds1):.3f}")

    # Gating
    if threshold >= 1.0:
        mask = np.ones(len(df), bool)
        print("Threshold=1.0 → Phase2 on ALL samples.")
    else:
        conf = np.maximum(probs, 1-probs)
        mask = conf >= threshold
        print(f"Threshold={threshold:.2f} → invoke Phase2 on {mask.sum()}/{len(df)} ({mask.mean():.2%})")

    # Phase2 inference & metrics
    df2 = df[mask]
    df2_valid = df2[(df2.hospital_expire_flag == 1)
                    & df2.hosp_deathtime_hours.notna()
                    & (df2.hosp_deathtime_hours >= 0)]
    if df2_valid.empty:
        print("No valid Phase2 samples (dead & non-negative death time).")
    else:
        y2 = df2_valid['hosp_deathtime_hours'].map(lambda x: descretize(x, 24, 24*7))
        preds2 = m2.predict(df2_valid)
        print("Phase2 Metrics:")
        evaluate(y2, preds2)

    return df2


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--threshold', type=float, required=True,
                        help='Gating threshold [0.0-1.0], use 1.0 to skip Phase1.')
    args = parser.parse_args()
    # Run for each window and print metrics only
    for w in [6, 12, 24]:
        _ = run(w, args.threshold)

if __name__ == '__main__':
    main()