#!/usr/bin/env python3
import argparse
import pandas as pd
import numpy as np
import pickle
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score, accuracy_score
from pathlib import Path

def load_data(base_dir):
    #print("[INFO] Loading data...")
    final_csv = base_dir / 'data' / 'Flight_Weather.csv'
    df = pd.read_csv(final_csv)
    #print("[INFO] Data loaded.")
    data_label_encoder = LabelEncoder()
    data_label_encoder.fit(df['Airport'])
    df['EncodedAirport'] = data_label_encoder.transform(df['Airport'])
    y = df[['ArrDel15', 'ArrDelayMinutes']]
    X = df.drop(['FlightDate', 'Date', 'Airport', 'ArrDelayMinutes'], axis=1)
    return X, y

def train_models(X_train, y_train):
    #print("[INFO] Loading models...")
    Xc_train = X_train.drop('ArrDel15', axis=1)
    yc_train = y_train['ArrDel15']
    Xr_train = X_train[X_train['ArrDel15'] == 1].drop('ArrDel15', axis=1)
    yr_train = y_train[y_train['ArrDel15'] == 1]['ArrDelayMinutes']

    with open("RFC_SMOTE.pkl", "rb") as f:
        clf = pickle.load(f)
    with open("RFR.pkl", "rb") as f:
        rfr = pickle.load(f)

    #print("[INFO] Models loaded.")
    return clf, rfr

def evaluate_at_threshold(clf, rfr, X_test, y_test, gating_threshold):
    #print(f"[INFO] Evaluating at threshold {gating_threshold}...")
    Xc_test = X_test.drop('ArrDel15', axis=1)
    yc_test = y_test['ArrDel15']
    Xr_test = X_test[X_test['ArrDel15'] == 1].drop('ArrDel15', axis=1)
    yr_test = y_test[y_test['ArrDel15'] == 1]['ArrDelayMinutes']

    # Phase 1 classifier predictions
    proba = clf.predict_proba(Xc_test)[:, 1]
    phase1_pred = (proba >= 0.5).astype(int)  # standard 0.5 decision boundary for Phase 1

    if gating_threshold >= 1.0:
        # Baseline: Phase 2 only
        y2_pred_delay = (rfr.predict(Xc_test) >= 15).astype(int)  # classify based on delay minutes >= 15
        acc = accuracy_score(yc_test, y2_pred_delay)
        invocation_rate = 1.0
    else:
        conf = np.maximum(proba, 1 - proba)
        mask = conf >= gating_threshold  # which samples go to Phase 2
        final_pred = phase1_pred.copy()

        # Phase 2 only for those classified as delayed by Phase 1 and passing the gate
        idx_phase2 = np.where((phase1_pred == 1) & mask)[0]
        if len(idx_phase2) > 0:
            X2 = Xc_test.iloc[idx_phase2][Xr_test.columns]
            phase2_delay_pred = (rfr.predict(X2) >= 15).astype(int)
            final_pred[idx_phase2] = phase2_delay_pred

            acc = accuracy_score(yc_test, final_pred)
    
            invocation_rate = mask.mean()
   # if gating_threshold >= 1.0:
        #invocation_rate = 1.0  # Phase 2 invoked on all samples

    # End-to-end prediction array
    y_full_pred = np.zeros(len(y_test))  

    # Flights predicted delayed by Phase 1
    predicted_delayed_idx = np.where((proba >= gating_threshold))[0]

    # Compute regressor predictions and valid indices
    y2_pred = rfr.predict(Xr_test)
    valid_idx = Xr_test.index

    # Use Phase 2 regression for those flights (if they exist in regressor index)
    for i in predicted_delayed_idx:
        if i in valid_idx:
            y_full_pred[i] = y2_pred[list(valid_idx).index(i)]  # predicted minutes
        else:
            y_full_pred[i] = 15  # or some default delay threshold

    # End-to-end metrics
    mae = mean_absolute_error(y_test['ArrDelayMinutes'], y_full_pred)
    rmse = np.sqrt(mean_squared_error(y_test['ArrDelayMinutes'], y_full_pred))
    r2 = r2_score(y_test['ArrDelayMinutes'], y_full_pred)


    print(f"{gating_threshold},{invocation_rate:.4f},{mae:.4f},{rmse:.4f},{r2:.4f},{acc:.4f}")
    return acc

if __name__ == "__main__":
    #print("[INFO] Starting Flight Delay Prediction...")
    p = argparse.ArgumentParser()
    p.add_argument("--threshold", "-t", type=float, required=True,
                   help="gating threshold α in [0,1]")
    args = p.parse_args()

    BASE = Path(__file__).parent
    X, y = load_data(BASE)

    from sklearn.model_selection import train_test_split
    X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                        test_size=0.25,
                                                        random_state=0)

    clf, rfr = train_models(X_train, y_train)
    evaluate_at_threshold(clf, rfr, X_test, y_test, args.threshold)
    #print("[INFO] Flight Delay Prediction completed.")
