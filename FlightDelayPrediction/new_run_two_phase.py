#!/usr/bin/env python3
"""
run_two_phase_lazy_load.py

- Loads Phase-1 classifier only when threshold < 1.0
- Always loads Phase-2 regressor (both runs need regressor)
- Safe feature alignment before calling predict/predict_proba
- Prints invocation rate, confusion matrix counts, accuracy, and (optional) regression metrics
"""

import argparse
from pathlib import Path
import numpy as np
import pandas as pd
import pickle
from sklearn.metrics import (confusion_matrix, accuracy_score, precision_score,
                             recall_score, f1_score, mean_absolute_error,
                             mean_squared_error, r2_score)

DATA_CSV = "data/Flight_Weather.csv"   # adjust if needed
PHASE1_PATH = "RFC_SMOTE.pkl"
PHASE2_PATH = "RFR.pkl"


def load_data(base_dir: Path):
    final_csv = base_dir / DATA_CSV
    df = pd.read_csv(final_csv)
    # ensure the encoded airport column exists (if training used it)
    if "EncodedAirport" not in df.columns and "Airport" in df.columns:
        from sklearn.preprocessing import LabelEncoder
        le = LabelEncoder()
        df["EncodedAirport"] = le.fit_transform(df["Airport"])
    y = df[["ArrDel15", "ArrDelayMinutes"]]
    X = df.drop(columns=["FlightDate", "Date", "Airport", "ArrDelayMinutes"], errors="ignore")
    return X, y


def load_phase1(path=PHASE1_PATH):
    with open(path, "rb") as f:
        clf = pickle.load(f)
    return clf


def load_phase2(path=PHASE2_PATH):
    with open(path, "rb") as f:
        rfr = pickle.load(f)
    return rfr


def align_df_to_estimator(df: pd.DataFrame, estimator):
    """
    If estimator has feature_names_in_, reindex df to those columns (fill missing with 0).
    Otherwise, return df unchanged.
    """
    if hasattr(estimator, "feature_names_in_"):
        feat = list(estimator.feature_names_in_)
        return df.reindex(columns=feat, fill_value=0)
    else:
        return df


def evaluate_at_threshold(clf, rfr, X_test, y_test, gating_threshold, require_positive=True):
    """
    clf: classifier or None (if None, we run baseline that uses regressor only)
    rfr: regressor (must be provided)
    X_test: DataFrame that MAY include ArrDel15 column (we'll drop it)
    y_test: DataFrame with ArrDel15 and ArrDelayMinutes
    """
    # prepare feature DataFrame used for model inference
    Xc_test = X_test.copy()
    Xc_test = Xc_test.drop(columns=["ArrDel15"], errors="ignore")
    yc = y_test["ArrDel15"].astype(int).to_numpy()

    # Baseline: run Phase2 on ALL samples, no classifier required
    if gating_threshold >= 1.0 or clf is None:
        invocation_rate = 1.0
        X_for_r = align_df_to_estimator(Xc_test, rfr)
        reg_minutes = rfr.predict(X_for_r)
        reg_class = (reg_minutes >= 15).astype(int)

        # final predictions are phase-2 decisions for baseline
        final_pred = pd.Series(reg_class, index=Xc_test.index)

    else:
        # we need the classifier
        # align classifier input
        X_for_clf = align_df_to_estimator(Xc_test, clf)
        proba = clf.predict_proba(X_for_clf)[:, 1]
        phase1_pred = (proba >= 0.5).astype(int)

        if require_positive:
            invoke_mask = (proba >= gating_threshold) & (phase1_pred == 1)
        else:
            invoke_mask = (proba >= gating_threshold)

        invocation_rate = float(invoke_mask.mean())

        # build final predictions starting from phase1's decision
        final_pred = pd.Series(phase1_pred, index=Xc_test.index)

        # if we invoke phase2 on some rows, call regressor only on those rows
        if invoke_mask.any():
            X_for_r = Xc_test.loc[invoke_mask].copy()
            X_for_r = align_df_to_estimator(X_for_r, rfr)
            reg_minutes = rfr.predict(X_for_r)
            reg_class = (reg_minutes >= 15).astype(int)
            # assign regressor-derived class back to the final_pred Series using indices
            final_pred.loc[X_for_r.index] = reg_class

    # Compute confusion matrix and metrics
    cm = confusion_matrix(yc, final_pred.to_numpy(), labels=[0, 1])
    if cm.size == 4:
        TN, FP, FN, TP = cm.ravel()
    else:
        TN = FP = FN = TP = 0

    accuracy_from_cm = (TP + TN) / (TP + TN + FP + FN) if (TP + TN + FP + FN) > 0 else np.nan
    acc_score = accuracy_score(yc, final_pred.to_numpy())
    prec = precision_score(yc, final_pred.to_numpy(), zero_division=0)
    rec = recall_score(yc, final_pred.to_numpy(), zero_division=0)
    f1 = f1_score(yc, final_pred.to_numpy(), zero_division=0)

    # Build continuous minutes predictions (for MAE/RMSE/R2) - zeros where no regressor invoked
    y_full_pred_minutes = np.zeros(len(y_test), dtype=float)
    if gating_threshold >= 1.0 or clf is None:
        # reg_minutes length should equal n_samples
        y_full_pred_minutes = reg_minutes
    else:
        if invoke_mask.any():
            # map regressor outputs to corresponding original indices
            for i, idx in enumerate(X_for_r.index):
                y_full_pred_minutes[Xc_test.index.get_loc(idx)] = reg_minutes[i]
        # other indices remain 0 (or could use np.nan / a default)
    
    from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
    #import numpy as np
    mae = mean_absolute_error(y_test["ArrDelayMinutes"], y_full_pred_minutes)
    mse = mean_squared_error(y_test["ArrDelayMinutes"], y_full_pred_minutes)
    rmse = float(np.sqrt(mse))
    r2 = r2_score(y_test["ArrDelayMinutes"], y_full_pred_minutes)

    # Print CSV-style line for easy parsing
    print("alpha,invocation_rate,TN,FP,FN,TP,accuracy_from_cm,accuracy_score,precision,recall,f1,mae,rmse,r2")
    print(f"{gating_threshold:.2f},{invocation_rate:.4f},{TN},{FP},{FN},{TP},{accuracy_from_cm:.4f},{acc_score:.4f},{prec:.4f},{rec:.4f},{f1:.4f},{mae:.4f},{rmse:.4f},{r2:.4f}")

    return {
        "alpha": gating_threshold,
        "invocation_rate": invocation_rate,
        "TN": TN, "FP": FP, "FN": FN, "TP": TP,
        "accuracy_from_cm": accuracy_from_cm,
        "accuracy_score": acc_score,
        "precision": prec, "recall": rec, "f1": f1,
        "mae": mae, "rmse": rmse, "r2": r2
    }


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--threshold", "-t", type=float, required=True, help="gating threshold α in [0,1]")
    p.add_argument("--require-positive", action="store_true",
                   help="Require phase1 predicted positive (phase1_pred==1) in addition to proba>=alpha to invoke phase2")
    args = p.parse_args()

    BASE = Path(__file__).parent
    X, y = load_data(BASE)

    # single deterministic split (same as training procedure)
    from sklearn.model_selection import train_test_split
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=0)

    # Decide which models to load
    clf = None
    rfr = None

    # Always need regressor (phase2) for baseline and for invoked predictions
    rfr = load_phase2(PHASE2_PATH)

    if args.threshold < 1.0:
        # only load classifier if needed (saves memory/time)
        clf = load_phase1(PHASE1_PATH)

    # Run evaluation
    evaluate_at_threshold(clf, rfr, X_test, y_test, args.threshold, require_positive=args.require_positive)


if __name__ == "__main__":
    main()
