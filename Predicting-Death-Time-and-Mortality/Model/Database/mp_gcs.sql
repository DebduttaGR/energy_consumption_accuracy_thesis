-- Source: https://github.com/alistairewj/mortality-prediction/tree/master/queries

DROP TABLE IF EXISTS mp_gcs CASCADE;
CREATE TABLE mp_gcs AS

WITH base AS (
  SELECT
    pvt.icustay_id,
    pvt.charttime,
    MAX(CASE WHEN pvt.itemid = 454 THEN pvt.valuenum ELSE NULL END)    AS GCSMotor,
    MAX(CASE WHEN pvt.itemid = 723 THEN pvt.valuenum ELSE NULL END)    AS GCSVerbal,
    MAX(CASE WHEN pvt.itemid = 184 THEN pvt.valuenum ELSE NULL END)    AS GCSEyes,
    CASE
      WHEN MAX(CASE WHEN pvt.itemid = 723 THEN pvt.valuenum ELSE NULL END) = 0
        THEN 1
      ELSE 0
    END AS EndoTrachFlag,
    ROW_NUMBER() OVER (PARTITION BY pvt.icustay_id ORDER BY pvt.charttime) AS rn
  FROM (
    SELECT
      l.icustay_id,
      l.charttime,
      -- Normalize ITEMID for different code sets
      CASE
        WHEN l.itemid IN (723, 223900) THEN 723
        WHEN l.itemid IN (454, 223901) THEN 454
        WHEN l.itemid IN (184, 220739) THEN 184
        ELSE l.itemid
      END AS itemid,
      -- Zero out GCS entries marked ET/Trach or No-Response; otherwise use valuenum
      CASE
        WHEN l.itemid IN (723, 223900) THEN 0
        ELSE l.valuenum
      END AS valuenum
    FROM chartevents l
    JOIN mp_cohort co
      ON l.icustay_id = co.icustay_id
     AND co.excluded = 0
    WHERE l.itemid IN (184, 454, 723, 223900, 223901, 220739)
      AND l.error IS DISTINCT FROM 1
  ) pvt
  GROUP BY pvt.icustay_id, pvt.charttime
),

gcs AS (
  SELECT
    b.*,
    b2.GCSVerbal  AS GCSVerbalPrev,
    b2.GCSMotor   AS GCSMotorPrev,
    b2.GCSEyes    AS GCSEyesPrev,
    CASE
      WHEN b.GCSVerbal = 0 THEN 15
      WHEN b.GCSVerbal IS NULL AND b2.GCSVerbal = 0 THEN 15
      WHEN b2.GCSVerbal = 0 THEN
        COALESCE(b.GCSMotor,6)
      + COALESCE(b.GCSVerbal,5)
      + COALESCE(b.GCSEyes,4)
      ELSE
        COALESCE(b.GCSMotor,   COALESCE(b2.GCSMotor,6))
      + COALESCE(b.GCSVerbal,  COALESCE(b2.GCSVerbal,5))
      + COALESCE(b.GCSEyes,    COALESCE(b2.GCSEyes,4))
    END AS GCS
  FROM base b
  LEFT JOIN base b2
    ON b.icustay_id = b2.icustay_id
   AND b.rn          = b2.rn + 1
   AND b2.charttime > b.charttime - INTERVAL '6 hours'
),

gcs_stg AS (
  SELECT
    gs.icustay_id,
    gs.charttime,
    CEIL(EXTRACT(EPOCH FROM gs.charttime - co.intime) / 3600.0)::SMALLINT AS hr,
    gs.GCS,
    COALESCE(gs.GCSMotor, gs.GCSMotorPrev) AS GCSMotor,
    COALESCE(gs.GCSVerbal, gs.GCSVerbalPrev) AS GCSVerbal,
    COALESCE(gs.GCSEyes,  gs.GCSEyesPrev)  AS GCSEyes,
    -- Count how many components measured
    (CASE WHEN COALESCE(gs.GCSMotor, gs.GCSMotorPrev) IS NULL THEN 0 ELSE 1 END
     + CASE WHEN COALESCE(gs.GCSVerbal, gs.GCSVerbalPrev) IS NULL THEN 0 ELSE 1 END
     + CASE WHEN COALESCE(gs.GCSEyes,  gs.GCSEyesPrev)  IS NULL THEN 0 ELSE 1 END
    ) AS components_measured,
    gs.EndoTrachFlag
  FROM gcs gs
  JOIN mp_cohort co
    ON gs.icustay_id = co.icustay_id
   AND co.excluded    = 0
),

gcs_priority AS (
  SELECT
    icustay_id,
    hr,
    GCS,
    GCSMotor,
    GCSVerbal,
    GCSEyes,
    EndoTrachFlag,
    ROW_NUMBER() OVER (
      PARTITION BY icustay_id, hr
      ORDER BY components_measured DESC,
               EndoTrachFlag,
               GCS,
               charttime DESC
    ) AS rn
  FROM gcs_stg
)

SELECT
  icustay_id,
  hr,
  GCS,
  GCSMotor,
  GCSVerbal,
  GCSEyes,
  EndoTrachFlag
FROM gcs_priority
WHERE rn = 1
ORDER BY icustay_id, hr;
