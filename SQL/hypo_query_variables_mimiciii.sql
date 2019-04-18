set search_path to mimiciii;

/*

This code extracts all variables used in this model.

This includes pre HE serum creatinine, post HE serum creatinine, mean MAP and total urine output for 3 hours previous to onset of HE,
obtain age, gender, vital signs and service type (ICU), elixhauser scores, avg. temperature in 3 hour window before HE, and SAPS I scores.

Use table "hypo_cohort_fluids_vp" as starting point. This table includes information about hypotensive episode, treatment, database source, and length of stay (los).

*/

select count(*) from hypo_cohort_fluids_vp_subjects;
--5,736 patients in data set.


-- Step 1. Get Serum Creatinine measures
with pre24creat AS (
  SELECT h.*, l.itemid, l.charttime, l.valuenum as pre_creat, (h.he_onset - l.charttime) as first_creat_length
    FROM hypo_cohort_fluids_vp_subjects h
    LEFT JOIN labevents l ON
    H.SUBJECT_ID = L.SUBJECT_ID 
    WHERE l.itemid = 50912 AND  ( h.he_onset - l.charttime ) between '00:00:00' and '24:00:00'
    order by SUBJECT_ID
)
--select * from pre24creat;
, firstcreat AS(
  SELECT h.*
    FROM pre24creat h 
    INNER JOIN
    (
        SELECT icustay_id, MIN(first_creat_length) Mincreat
        FROM pre24creat
        GROUP BY icustay_id
    ) t ON h.icustay_id = t.icustay_id AND h.first_creat_length = t.Mincreat
)
--select * from firstcreat;
, post72creat AS (
  SELECT h.*, l.itemid, l.charttime, l.valuenum as post_creat, (l.charttime - h.he_offset) as last_creat_length
    FROM firstcreat h
    LEFT JOIN labevents l ON
    H.SUBJECT_ID = L.SUBJECT_ID 
    WHERE l.itemid = 50912 AND  ( l.charttime - h.he_offset) between '00:00:00' and '72:00:00'
    order by SUBJECT_ID
)

--select * from pre24creat;
, lastcreat AS(
  SELECT h.*
    FROM post72creat h 
    INNER JOIN
    (
        SELECT subject_id, MAX(POST_CREAT) MAXcreat
        FROM post72creat
        GROUP BY subject_id
    ) t ON h.subject_id = t.subject_id AND h.post_creat = t.MAXcreat
)
--select * from lastcreat order by subject_id;

, post72creat_subj as(
    select distinct on (subject_id) * from lastcreat order by subject_id
)
,finalcreat as(
  SELECT h.*, (h.post_creat - h.pre_creat) as rise_creat
    FROM post72creat_subj h
)
select dbsource, hadm_id, subject_id, icustay_id, he_onset, he_offset, he_length, admittime,los, iv_fluid_ind, vp_ind, pre_creat, post_creat, rise_creat
into hypo_trt_creat
from finalcreat;
-- 5,139 patients.

-------------------------------------------------------------------------------------------------------------------------
-- Step 2. Obtain baseline information such as age, gender, inhospital mortality, ethnicity
with gender_info as (
    select h.*, p.gender, age(h.admittime, p.dob) as age
    from hypo_trt_creat h
    inner join patients p on h.subject_id = p.subject_id
  ),
  
inhm_info as (
	select h.*, a.ethnicity, a.hospital_expire_flag
    from gender_info h
    inner join admissions a
    on h.hadm_id = a.hadm_id
),

--select sum(hospital_expire_flag) from inhm_info where dbsource = 'carevue';
saps_info as(
	select h.*, saps
    from inhm_info h
    inner join saps_table s
    on h.icustay_id = s.icustay_id
)
select k, percentile_disc(k) within group (order by saps)
from saps_info, generate_series(0.25, 0.75, 0.25) as k 
--where dbsource='carevue'
group by k;

select k, percentile_disc(k) within group (order by saps) from saps_table, generate_series(0.25, 0.75, 0.25) as k group by k;

