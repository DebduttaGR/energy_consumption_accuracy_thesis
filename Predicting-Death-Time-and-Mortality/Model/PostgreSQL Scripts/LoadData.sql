-- At the psql prompt:
\copy icustays FROM '/Users/debduttaguharoy/Downloads/mimic-iii-clinical-database-demo-1.4/ICUSTAYS.csv' WITH (FORMAT csv, HEADER);
\copy admissions FROM '/Users/debduttaguharoy/Downloads/mimic-iii-clinical-database-demo-1.4/ADMISSIONS.csv' WITH (FORMAT csv, HEADER);
\copy patients FROM '/Users/debduttaguharoy/Downloads/mimic-iii-clinical-database-demo-1.4/PATIENTS.csv' WITH (FORMAT csv, HEADER);
\copy chartevents FROM '/Users/debduttaguharoy/Downloads/mimic-iii-clinical-database-demo-1.4/CHARTEVENTS.csv' WITH (FORMAT csv, HEADER);
\copy chartevents_staging FROM '/Users/debduttaguharoy/Downloads/mimic-iii-clinical-database-demo-1.4/CHARTEVENTS.csv' WITH (FORMAT csv, HEADER);
\copy labevents FROM '/Users/debduttaguharoy/Downloads/mimic-iii-clinical-database-demo-1.4/LABEVENTS.csv' WITH (FORMAT csv, HEADER);
\copy outputevents FROM '/Users/debduttaguharoy/Downloads/mimic-iii-clinical-database-demo-1.4/OUTPUTEVENTS.csv' WITH (FORMAT csv, HEADER);
--\copy mp_gcs FROM '/Users/debduttaguharoy/Downloads/mimic-iii-clinical-database-demo-1.4/mp_lab.csv' WITH (FORMAT csv, HEADER);
--\copy mp_hourly_cohort FROM '/Users/debduttaguharoy/Downloads/mimic-iii-clinical-database-demo-1.4/mp_lab.csv' WITH (FORMAT csv, HEADER);
--\copy mp_lab FROM '/Users/debduttaguharoy/Downloads/mimic-iii-clinical-database-demo-1.4/mp_lab.csv' WITH (FORMAT csv, HEADER);
