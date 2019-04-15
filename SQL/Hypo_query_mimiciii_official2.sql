set search_path to mimiciii;

/*
 
 Blood pressure itemid's
 
*/

-- Important Items to examine.
select * from d_items
where itemid in (1125, 225312, 220052, 6926, 52, 6702, 456, 220181);


/* Remove patients younger than 15. */
WITH first_admission_time AS (
SELECT 
    p.subject_id, p.dob, p.gender, 
    MIN (a.admittime) AS first_admittime
FROM patients p
INNER JOIN admissions a
ON p.subject_id = a.subject_id
GROUP BY p.subject_id, p.dob, p.gender, a.hadm_id
ORDER BY a.hadm_id, p.subject_id
),
age AS (
SELECT 
    subject_id, dob, gender, first_admittime, 
    age(first_admittime, dob) 
        AS first_admit_age, 
    CASE
       WHEN age(first_admittime, dob) > '89 years'
            THEN '>89'
        WHEN age(first_admittime, dob) >= '15 years'
            THEN 'adult'
        WHEN age(first_admittime, dob) <= '1  year'
            THEN 'neonate'
        ELSE 'middle'
        END AS age_group
FROM first_admission_time
ORDER BY subject_id
)
	,sub_age AS (
    SELECT *
    FROM age
     )
select sub_age.*, icustays.icustay_id
into subject_age
from sub_age
inner join icustays on sub_age.subject_id = icustays.subject_id;

/*
select *
into chartevents_adult
from chartevents
where chartevents.subject_id in (
  select subject_age.subject_id
  from subject_age
  where age_group = 'adult'
	) 
  and chartevents.icustay_id in (
  select icustay_id
  from icustays
  where first_careunit in ('MICU','CCU','SICU','CSRU') or last_careunit in ('MICU','CCU','SICU','CSRU')  
);
*/


/* This query returns DBSOURCE, HADM_ID, SUBJECT_ID, ICUSTAY_ID, HE_ONSET, HE_OFFSET, HE_LENGTH, LOS for 
  subjects in MIMIC-III DB on criteria:
  
  Entry: Time when first of 2 measurements of less than 60 mmHg within 1 hour was made
  Exit: Time when first of 2 measurements of greater than 60 mmHg within 1 hour was made   
    
****
  In this query, an EVENT is a record in the table with the status ENTRY or EXIT 
  being assigned to the beginnings of windows that qualify as ENTRY or EXIT 
  criteria as explained above. This term will be used from here on out.
****
  
Hypoentries: gets charttimes and values of windows of 1 hour that qualify as entry criteria
  
Hypoentries2: returns charttime of last measurement within 1 hour window. This table
			  finds all patients that fit the definition of the start of a hypotensive episode.

Hypoexits: gets charttimes and values of windows of 1 hour that qualify as exit criteria

Allevents: Gathers all ENTRY and EXIT events into one table, as well as
  information about the previous record for filtering
   
HEtable1/final query: Assembles an HE event if the next record has the same
  ICUSTAY_ID, the current event is ENTRY and the next event is EXIT.
  
After this query, the SQL file "Hypo_fluid_pressor_mimiciii.sql" will use hypo_cohort_final to obtain patients who recieved treatment.

*/

CREATE TABLE dual        
	(
     enter_hypo_threshold int, 
     enter_windownum int,
     exit_hypo_threshold int, 
     exit_windownum int
);
INSERT INTO dual
Values (60, 1, 60, 1);

-- drop table dual;
-- drop table he_set;
--select * from dual;

/* between 40 and (select...  is implemented as a lower threshold on BP 
  measurements that are considered due to quality and reliability of entered
  measurements. They can be too low to be realistic sometimes, so this 
  threshold can be changed by changing '40' to desired limit.
*/
With hypoentries as (
select subject_id, icustay_id, itemid, valuenum, charttime as he_time,
    row_number()  over (partition by subject_id, icustay_id order by charttime) as rn,
    last_value(charttime) over (partition by subject_id, icustay_id order by charttime ROWS BETWEEN CURRENT ROW and 1 following ) as windowendct,
    last_value(valuenum) over (partition by subject_id, icustay_id order by charttime ROWS BETWEEN CURRENT ROW and 1 following ) as windowend,
    last_value(itemid) over (partition by subject_id, icustay_id order by charttime ROWS BETWEEN CURRENT ROW and 1 following ) as windowend_itemid
    FROM chartevents_adult WHERE itemid in (52,456)
),
--select * from hypoentries limit 50;

hypoentries2 as (
select h.*,
 cast('ENTRY' AS text) as status
 FROM hypoentries h WHERE valuenum is not null and valuenum between 40 and (select enter_hypo_threshold from dual) 
                                      and windowend between 40 and (select enter_hypo_threshold from dual)and icustay_id is not null
    								  --and itemid = windowend_itemid
),
--select * from hypoentries2 limit 50;

/* between (select... and 180    is implemented as an upper threshold on BP 
  measurements that are considered due to quality and reliability of entered
  measurements. They can be too high to be realistic sometimes, so this 
  threshold can be changed by changing '180' to desired limit.
*/

hypoexits as(
select subject_id, icustay_id, itemid, valuenum, charttime as he_time,
    row_number()  over (partition by subject_id, icustay_id order by charttime) as rn,
    last_value(charttime) over (partition by subject_id, icustay_id order by charttime ROWS BETWEEN CURRENT ROW and 1 following ) as windowendct,
    last_value(valuenum) over (partition by subject_id, icustay_id order by charttime ROWS BETWEEN CURRENT ROW and 1 following ) as windowend,
    last_value(itemid) over (partition by subject_id, icustay_id order by charttime ROWS BETWEEN CURRENT ROW and 1 following ) as windowend_itemid
    FROM chartevents_adult WHERE itemid in (52,456) 
),

hypoexits2 as(
    select h.*,
    cast('EXIT' AS text) as status
    FROM hypoexits h WHERE valuenum is not null and valuenum > (select exit_hypo_threshold from dual) and valuenum <= 180
                                       and windowend > (select exit_hypo_threshold from dual) and valuenum <= 180 and icustay_id is not null
    								   --and itemid = windowend_itemid
),
--select * from hypoexits2 limit 500;

allevents as(
  select * from (select * from hypoentries2 union select * from hypoexits2) aa order by subject_id, he_time
),
--select * from allevents limit 1000;

allevents2 as(
	select *,
    lag(icustay_id, 1) over (partition by subject_id, icustay_id order by he_time) as prev_icustay,
    lag(status, 1) over (partition by subject_id, icustay_id order by he_time) as prev_status
    from allevents
),
--select * from allevents2 limit 500;

allevents3 as(
        select distinct icustay_id from allevents2 where ( status != prev_status )
),
--select * from allevents3;

allevents31 as(
	select a.* from allevents2 a 
    inner join allevents3 b
    on a.icustay_id = b.icustay_id
),
--select * from allevents31 limit 500;

allevents35 as(
  select subject_id, icustay_id, valuenum, he_time, status,
    lag(icustay_id, 1) over (partition by subject_id, icustay_id order by he_time) as prev_icustay,
    lag(status, 1) over (partition by subject_id, icustay_id order by he_time) as prev_status,
    lead(icustay_id, 1) over (partition by subject_id, icustay_id order by he_time) as next_icustay,
    lead(status, 1) over (partition by subject_id, icustay_id order by he_time) as next_status,
    LEAD(he_time, 1) over (partition by SUBJECT_ID, ICUSTAY_ID order by he_time) NEXT_CHARTTIME
  from allevents31
),
--select count(*) from allevents35 limit 500; 

hetable1 as(
  select SUBJECT_ID, ICUSTAY_ID,
    case 
      when (status = 'ENTRY') 
        then he_time else null 
    end he_onset,
    case 
      when (status = 'ENTRY' and ICUSTAY_ID = NEXT_ICUstay and NEXT_STATUS = 'EXIT')
        then NEXT_CHARTTIME else null 
    end he_offset
  from allevents35
)
--select h.*, (h.he_offset - h.he_onset) as he_length from hetable1 h where he_onset is not null and he_offset is not null order by h.subject_id, h.he_onset;

/*
This statement finds all hypotensive episodes in all icustays.
*/

select h.*, (h.he_offset - h.he_onset) as he_length into HE_SET from hetable1 h where he_onset is not null and he_offset is not null order by h.subject_id, h.he_onset;

/*
This statement finds all icustays with one hypotensive episodes.
*/
select * into he_set1 from HE_SET 
where subject_id in (
                        select subject_id from HE_SET
                        group by subject_id
                        having count(icustay_id) = 1
);

select dbsource, hadm_id, HE_set1.*, intime as admittime, los
	into HE_Cohort
    from he_set1
	INNER JOIN icustays
	ON HE_set1.icustay_id = icustays.icustay_id 
	order by HE_set1.icustay_id ;
 
--drop table HE_SET;
--drop table HE_SET1;
--drop table dual;

-- Remove pateints with CMO's.
WITH icustay_CMO AS (
    select h.icustay_id, chartevents_adult.itemid, chartevents_adult.value, chartevents_adult.charttime as CMO_time, h.he_length, h.he_onset, h.he_offset 
         from chartevents_adult
         inner join he_cohort h on chartevents_adult.icustay_id = h.icustay_id
          where  value in ('Comfort measures only','CMO') and
                        (h.he_onset - chartevents_adult.charttime ) between '-24:00:00' and '24:00:00' or
                   value in ('Comfort measures only','CMO') and
                        (h.he_offset - chartevents_adult.charttime ) between '-24:00:00' and '24:00:00'
    		)
select * into hypo_cohort_final_cv from he_cohort
where icustay_id not in (select distinct icustay_id from icustay_CMO);
 
--select * from hypo_cohort_final limit 500;

--select avg(los) from hypo_cohort_final;

select * from hypo_cohort_final_cv where dbsource = 'carevue' order by subject_id;
--select count(*) from he_cohort where dbsource='carevue';

/*

Extra Tables that can be removed:

-- drop table he_set1_cmo;
-- drop table he_set;
-- drop table he_set1;
-- drop table he_cohort;

*/

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

drop table he_set;
drop table he_set1;
drop table he_cohort;

/*

Next, find patients suffering from hypotensive episodes in the metavision database. 

*/

With hypoentries as (
select subject_id, icustay_id, itemid, valuenum, charttime as he_time,
    row_number()  over (partition by subject_id, icustay_id order by charttime) as rn,
    last_value(charttime) over (partition by subject_id, icustay_id order by charttime ROWS BETWEEN CURRENT ROW and 1 following ) as windowendct,
    last_value(valuenum) over (partition by subject_id, icustay_id order by charttime ROWS BETWEEN CURRENT ROW and 1 following ) as windowend,
    last_value(itemid) over (partition by subject_id, icustay_id order by charttime ROWS BETWEEN CURRENT ROW and 1 following ) as windowend_itemid
    FROM chartevents_adult WHERE itemid in (220052, 220181)
),
--select * from hypoentries limit 50;

hypoentries2 as (
select h.*,
 cast('ENTRY' AS text) as status
 FROM hypoentries h WHERE valuenum is not null and valuenum between 40 and (select enter_hypo_threshold from dual) 
                                      and windowend between 40 and (select enter_hypo_threshold from dual)and icustay_id is not null
    								  --and itemid = windowend_itemid
),
--select * from hypoentries2 limit 50;

/* between (select... and 180    is implemented as an upper threshold on BP 
  measurements that are considered due to quality and reliability of entered
  measurements. They can be too high to be realistic sometimes, so this 
  threshold can be changed by changing '180' to desired limit.
*/

hypoexits as(
select subject_id, icustay_id, itemid, valuenum, charttime as he_time,
    row_number()  over (partition by subject_id, icustay_id order by charttime) as rn,
    last_value(charttime) over (partition by subject_id, icustay_id order by charttime ROWS BETWEEN CURRENT ROW and 1 following ) as windowendct,
    last_value(valuenum) over (partition by subject_id, icustay_id order by charttime ROWS BETWEEN CURRENT ROW and 1 following ) as windowend,
    last_value(itemid) over (partition by subject_id, icustay_id order by charttime ROWS BETWEEN CURRENT ROW and 1 following ) as windowend_itemid
    FROM chartevents_adult WHERE itemid in (220052, 220181)
),

hypoexits2 as(
    select h.*,
    cast('EXIT' AS text) as status
    FROM hypoexits h WHERE valuenum is not null and valuenum > (select exit_hypo_threshold from dual) and valuenum <= 180
                                       and windowend > (select exit_hypo_threshold from dual) and valuenum <= 180 and icustay_id is not null
    								   --and itemid = windowend_itemid
),
--select * from hypoexits2 limit 500;

allevents as(
  select * from (select * from hypoentries2 union select * from hypoexits2) aa order by subject_id, he_time
),
--select * from allevents limit 1000;

allevents2 as(
	select *,
    lag(icustay_id, 1) over (partition by subject_id, icustay_id order by he_time) as prev_icustay,
    lag(status, 1) over (partition by subject_id, icustay_id order by he_time) as prev_status
    from allevents
),
--select * from allevents2 limit 500;

allevents3 as(
        select distinct icustay_id from allevents2 where ( status != prev_status )
),
--select * from allevents3;

allevents31 as(
	select a.* from allevents2 a 
    inner join allevents3 b
    on a.icustay_id = b.icustay_id
),
--select * from allevents31 limit 500;

allevents35 as(
  select subject_id, icustay_id, valuenum, he_time, status,
    lag(icustay_id, 1) over (partition by subject_id, icustay_id order by he_time) as prev_icustay,
    lag(status, 1) over (partition by subject_id, icustay_id order by he_time) as prev_status,
    lead(icustay_id, 1) over (partition by subject_id, icustay_id order by he_time) as next_icustay,
    lead(status, 1) over (partition by subject_id, icustay_id order by he_time) as next_status,
    LEAD(he_time, 1) over (partition by SUBJECT_ID, ICUSTAY_ID order by he_time) NEXT_CHARTTIME
  from allevents31
),
--select count(*) from allevents35 limit 500; 

hetable1 as(
  select SUBJECT_ID, ICUSTAY_ID,
    case 
      when (status = 'ENTRY') 
        then he_time else null 
    end he_onset,
    case 
      when (status = 'ENTRY' and ICUSTAY_ID = NEXT_ICUstay and NEXT_STATUS = 'EXIT')
        then NEXT_CHARTTIME else null 
    end he_offset
  from allevents35
)
--select h.*, (h.he_offset - h.he_onset) as he_length from hetable1 h where he_onset is not null and he_offset is not null order by h.subject_id, h.he_onset;

/*
This statement finds all hypotensive episodes in all icustays.
*/

select h.*, (h.he_offset - h.he_onset) as he_length into HE_SET from hetable1 h where he_onset is not null and he_offset is not null order by h.subject_id, h.he_onset;

/*
This statement finds all icustays with one hypotensive episodes.
*/
select * into he_set1 from HE_SET 
where subject_id in (
                        select subject_id from HE_SET
                        group by subject_id
                        having count(icustay_id) = 1
);

select dbsource, hadm_id, HE_set1.*, intime as admittime, los
	into HE_Cohort
    from he_set1
	INNER JOIN icustays
	ON HE_set1.icustay_id = icustays.icustay_id 
	order by HE_set1.icustay_id ;
 
--drop table HE_SET;
--drop table HE_SET1;
--drop table dual;

-- Remove pateints with CMO's.
WITH icustay_CMO AS (
    select h.icustay_id, chartevents_adult.itemid, chartevents_adult.value, chartevents_adult.charttime as CMO_time, h.he_length, h.he_onset, h.he_offset 
         from chartevents_adult
         inner join he_cohort h on chartevents_adult.icustay_id = h.icustay_id
          where  value in ('Comfort measures only','CMO') and
                        (h.he_onset - chartevents_adult.charttime ) between '-24:00:00' and '24:00:00' or
                   value in ('Comfort measures only','CMO') and
                        (h.he_offset - chartevents_adult.charttime ) between '-24:00:00' and '24:00:00'
    		)
select * into hypo_cohort_final_mv from he_cohort
where icustay_id not in (select distinct icustay_id from icustay_CMO);
 
--select * from hypo_cohort_final limit 500;

--select avg(los) from hypo_cohort_final;

select * from hypo_cohort_final_mv where dbsource = 'metavision' order by subject_id;
--select count(*) from he_cohort where dbsource='carevue';


/* Final cohort combines both these tables */

with he_final as (
    select * from hypo_cohort_final_cv
    union
    select* from hypo_cohort_final_mv
)
select * into he_cohort_final from he_final;


