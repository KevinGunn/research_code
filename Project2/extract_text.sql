-- This code finds all patients with multiple visits to the ICU and extracts their discharge summaries into individual text files.
set search_path to mimiciii;

select * from admissions limit 5;


select subject_id, count(hadm_id) as count_adm
from admissions
group by subject_id
having count(hadm_id) >= 2;

-- A total of 7,537 patients have multiple ICU visits within MIMIC-III.

select subject_id, count(hadm_id) as count_adm
from admissions
group by subject_id
having count(hadm_id) = 2;

-- 5,160 patients have 2 ICU visits.

-- Next chunk of sql code provides most common ICD9_codes to get an idea of common disease status'.
-- Frequency of ICD9 codes
with multiple_ICU_subjects as (
    select subject_id, count(hadm_id) as count_adm
    from admissions
    group by subject_id
    having count(hadm_id) = 2
)
, diagnosis_table as (
select subject_id, icd9_code
from diagnoses_icd
    )
, icd9_code_multi_subs as (
select multiple_ICU_subjects.subject_id, icd9_code, count_adm
from diagnosis_table
inner join multiple_ICU_subjects on  diagnosis_table.subject_id = multiple_ICU_subjects.subject_id
    )
, icd9_freq as (
select icd9_code, count(1) as hadm_freq
    from icd9_code_multi_subs
    group by 1
order by hadm_freq desc
    )
select d_icd_diagnoses.short_title, icd9_freq.*
from icd9_freq
Left Join d_icd_diagnoses
ON icd9_freq.icd9_code = d_icd_diagnoses.icd9_code;


-- Hypertension most prevalent according to ICD9 code.

----------------------------------------------------------------------------------------------
/* Noteevents text data for subjects with multiple ICU visits. */

select * from noteevents limit 5;

-- discharge summary for both hospital admissions is not available for all patients.
with multiple_ICU_subjects as (
    select subject_id, count(hadm_id) as count_adm
    from admissions
    group by subject_id
    having count(hadm_id) = 2
)
, discharge_note as (
select *
from noteevents
where category like '%Discharge summary%'
)
, multi_note as (
select multiple_ICU_subjects.subject_id, discharge_note.hadm_id, discharge_note.text as discharge_note
from discharge_note
inner join multiple_ICU_subjects on  discharge_note.subject_id = multiple_ICU_subjects.subject_id
)
, two_note_hadm as (
select subject_id, count(hadm_id)
from multi_note
group by subject_id
having count(hadm_id) >= 2
order by subject_id
)
select multi_note.* 
from multi_note
inner join two_note_hadm on multi_note.subject_id = two_note_hadm.subject_id
;

-- 4,677 subjects with two hosptial stays and notes available from both stays.
-- 10,729 discharge summary notes available.


select * from multi_hadm_text limit 5;

-- store in individual text files to use in cTakes.
do $$
  declare
    arow record;
    files varchar;
  begin
    for arow in
    select subject_id, hadm_id, discharge_note from multi_hadm_text
    loop
      --files := 'COPY (select discharge_note from multi_hadm_text arow where subject_id = arow.subject_id) TO ''C:/Users/kpgunn/Documents/tmp/' || arow.subject_id || '_' || arow.hadm_id || '.txt''';
      files := 'D:/Project2/Notes/' || arow.subject_id || '_' || arow.hadm_id || '.txt';
      EXECUTE format('COPY (select discharge_note from multi_hadm_text where subject_id = %L and hadm_id = %L) TO %L (FORMAT CSV)',  arow.subject_id,arow.hadm_id,files);
    end loop;
  end;
$$;

