#!/usr/bin/env python3
import argparse
import pandas as pd
import numpy as np
import pickle
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import (mean_absolute_error, mean_squared_error,
                             r2_score, accuracy_score, roc_curve, auc)
from pathlib import Path

def load_data(base_dir):
    print("[INFO] Loading data...")
    final_csv = base_dir / 'data' / 'Flight_Weather.csv'
    df = pd.read_csv(final_csv)
    print("[INFO] Data loaded.")
    data_label_encoder = LabelEncoder()
    data_label_encoder.fit(df['Airport'])  # Use the same categories as in training
    df['EncodedAirport'] = data_label_encoder.transform(df['Airport'])
    y = df[['ArrDel15','ArrDelayMinutes']]
    # Drop 'FlightDate','Date','Airport','ArrDelayMinutes' (note: include 'Airport' here)
    X = df.drop(['FlightDate','Date','Airport','ArrDelayMinutes'], axis=1)
    return X, y

def train_models(X_train, y_train):
    # split into classifier/regressor training sets
    print("[INFO] Loading models...")
    Xc_train = X_train.drop('ArrDel15', axis=1)
    yc_train = y_train['ArrDel15']
    Xr_train = X_train[ X_train['ArrDel15']==1 ].drop('ArrDel15', axis=1)
    yr_train = y_train[ y_train['ArrDel15']==1 ]['ArrDelayMinutes']

    #clf = RandomForestClassifier(n_estimators=100, random_state=0)
    #clf.fit(Xc_train, yc_train)
    with open("RFC_SMOTE.pkl", "rb") as f:
        clf = pickle.load(f)

    # rfr = RandomForestRegressor(n_estimators=100, random_state=0)
    # rfr.fit(Xr_train, yr_train)
    with open("RFR.pkl", "rb") as f:
        rfr = pickle.load(f)

    print("[INFO] Models loaded.")
    return clf, rfr

def evaluate_at_threshold(clf, rfr, X_test, y_test, alpha):
    # prepare test splits
    print(f"[INFO] Evaluating at threshold {alpha}...")
    Xc_test = X_test.drop('ArrDel15', axis=1)
    yc_test = y_test['ArrDel15']
    Xr_test = X_test[ X_test['ArrDel15']==1 ].drop('ArrDel15', axis=1)
    yr_test = y_test[ y_test['ArrDel15']==1 ]['ArrDelayMinutes']

    # 1) phase1 probabilities
    proba = clf.predict_proba(Xc_test)[:,1]
    # 2) select those with P≥α
    mask = proba >= alpha
    invocation_rate = mask.mean()
    idx = Xc_test.index[mask]
    # only evaluate regressor on truly delayed rows
    valid_idx = idx.intersection(yr_test.index)

    X2 = Xc_test.loc[valid_idx, Xr_test.columns]
    y2 = yr_test.loc[valid_idx]

    # 3) re‑train regressor on full train, so we need to fit earlier
    y2_pred = rfr.predict(X2)

    mae  = mean_absolute_error(y2, y2_pred)
    rmse = np.sqrt(mean_squared_error(y2, y2_pred))
    r2   = r2_score(y2, y2_pred)

    # you can also compute accuracy of the classifier at α
    acc = accuracy_score(yc_test, (proba>=alpha).astype(int))

    # print a single line of JSON or CSV so RunnerConfig can ignore it:
    print(f"{alpha},{invocation_rate:.4f},{mae:.4f},{rmse:.4f},{r2:.4f},{acc:.4f}")
    print("[INFO] Evaluation complete.")

if __name__ == "__main__":
    print("[INFO] Starting Flight Delay Prediction...")
    p = argparse.ArgumentParser()
    p.add_argument("--threshold", "-t", type=float, required=True,
                   help="gating threshold α in [0,1]")
    args = p.parse_args()

    BASE = Path(__file__).parent
    X, y = load_data(BASE)
    # one single train/test split
    from sklearn.model_selection import train_test_split
    X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                        test_size=0.25,
                                                        random_state=0)

    clf, rfr = train_models(X_train, y_train)
    evaluate_at_threshold(clf, rfr, X_test, y_test, args.threshold)
    print("[INFO] Flight Delay Prediction completed.")
