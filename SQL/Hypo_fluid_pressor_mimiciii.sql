set search_path to mimiciii;

/* 

First step is to find itemid for Fluid treatment in MIMIC-III.
We need to find itemid for carevue and metavision datbases seperately.

*/


/*

Obtain Treatment patient received between IV fluids and Vasopressors. Also find hypotensive episode duration post treatment.

*/


/* Find all patients that recieved IV fluid treatments. */

--patients in cv.
WITH fluid_patients_cv AS (
     select h.icustay_id , h.he_onset, h.he_offset, inputevents_cv.itemid as cvfluid_id, inputevents_cv.charttime as cvfluid_input_time, inputevents_cv.amount as cvfluid_vol
     from hypo_cohort_final h
     inner join inputevents_cv on h.icustay_id = inputevents_cv.icustay_id
     where h.dbsource != 'metavision' AND inputevents_cv.charttime BETWEEN h.he_onset AND (h.he_offset + INTERVAL '59 min')
            AND inputevents_cv.itemid in ( 
              
                30176 --0.25% NS
                ,30161 -- 0.3% NS
                , 30020 -- 0.45% NS
                , 30013 -- D5W
                , 30018 -- 0.9% NS
                , 30353 -- 0.45% NS
                , 30352 -- 0.9% NS
                , 30143 -- 3% NS
                --, 45232 -- bladder pres. saline
                , 30160 -- D5 Normal Saline 
                , 30014 -- D5/.2NS
                , 30015 -- D5/.45NS
                , 30060 -- D5NS
                , 45374 -- D5NS 40k
                , 45358 -- D%NS with 40 meqkcl
                , 30061 -- D5RL
                , 44399 -- D5RL 20KCL
                , 30021 -- Lactated Ringers
                , 44440 -- Normal Saline Bolus
                , 44053 -- Normal Saline bolus 
                , 43354 -- Normal Saline Flush
                , 41526 -- 1/2 ns bolus
                , 41913 -- NS
                , 40865 -- NS BOLUS
                , 41459 -- 1/4 NS
                , 43168 -- d10 bolus
                , 30190 -- NS 0.9%
                , 30159 -- D5 Ringers Lact.
                , 43939 -- NS 500+40k
                , 43997 -- D10W bolus
                , 45073 -- IV fluid bolus
                , 45079 -- 500cc ns bolus
                , 43229 -- D10 bolus
                , 44335 -- D5W bolus
                , 44503 -- 1/2NS CC:CC
                , 44537 -- D5 .2
                , 46357 -- NSS With multivitamin
                --, 46383 -- D5W/0.9NS with meds
                , 45359 -- d10,1/2NS
                , 46546 -- D10 1/2NS
                , 46630 -- D10NSS
                , 44893 -- D5W 500ml bolus
                , 44894 -- N/s 500ml bolus
                , 45532 -- IR Lactated Ringers
                , 46258 -- D15 bolus
                , 43423 -- D10W bolus
                , 43438 -- D12.5 bolus
                , 45159 -- ER IV NS
                , 44439 -- 0.45NS bolus
                , 44570 -- d10w bolus
               --Beginning of new list
               ,30168
               , 30018 --	.9% Normal Saline
              , 30021 --	Lactated Ringers
              --, 30058 --	Free Water Bolus
              , 40850 --	ns bolus
              , 41491 --	fluid bolus
              , 42639 --	bolus
              --, 30065 --	Sterile Water
              --, 42187 --	free h20
              , 43819 --	1:1 NS Repletion.
              , 30063 --	IV Piggyback
              --, 41430 --	free water boluses
              --, 40712 --	free H20
              , 44160 --	BOLUS
              --, 42383 --	cc for cc replace
              --, 30169 --	Sterile H20_GU
              , 42297 --	Fluid bolus
              , 42453 --	Fluid Bolus
              --, 40872 --	free water
              --, 41915 --	FREE WATER
              , 41490 --	NS bolus
              --, 46501 --	H2O Bolus
              --, 45045 --	WaterBolus
              --, 41984 --	FREE H20
              , 41371 --	ns fluid bolus
              --, 41582 --	free h20 bolus
              , 41322 --	rl bolus
              --, 40778 --	Free H2O
              , 41896 --	ivf boluses
              , 41428 --	ns .9% bolus
              --, 43936 --	FREE WATER BOLUSES
              , 44200 --	FLUID BOLUS
              --, 41619 --	frfee water boluses
              --, 40424 --	free H2O
              --, 41457 --	Free H20 intake
              --, 41581 --	Water bolus
              , 42844 --	NS fluid bolus
              --, 42429 --	Free water
              , 41356 --	IV Bolus
              --, 40532 --	FREE H2O
              , 42548 --	NS Bolus
              , 44184 --	LR Bolus
              , 44521 --	LR bolus
              , 44741 --	NS FLUID BOLUS
              , 44126 --	fl bolus
              , 44110 --	RL BOLUS
              , 44633 --	ns boluses
              , 44983 --	Bolus NS
              , 44815 --	LR BOLUS
              , 43986 --	iv bolus
              , 45079 --	500 cc ns bolus
              , 46781 --	lr bolus
              , 45155 --	ns cc/cc replacement
              --, 43909 --	H20 BOlus
              , 41467 --	NS IV bolus
              , 44367 --	LR
              --, 41743 --	water bolus
              --, 40423 --	Bolus
              , 44263 --	fluid bolus ns
              , 42749 --	fluid bolus NS
              , 45480 --	500cc ns bolus
              , 44491 --	.9NS bolus
              , 41695 --	NS fluid boluses
              --, 46169 --	free water bolus.
              --, 41580 --	free h2o bolus
              , 41392 --	ns b
              , 45989 --	NS Fluid Bolus
              , 45137 --	NS cc/cc
              --, 45154 --	Free H20 bolus
              , 44053 --	normal saline bolus
              --, 41416 --	free h2o boluses
              --, 44761 --	Free H20
              , 41237 --	ns fluid boluses
              , 44426 --	bolus ns
              --, 43975 --	FREE H20 BOLUSES
              , 44894 --	N/s 500 ml bolus
              , 41380 --	nsbolus
              --, 42671 --	free h2o
            )
    			AND amount is not NULL
        		AND coalesce(amount,0) > 250
                AND coalesce(amount,0) < 2000
    
    		OR h.dbsource != 'metavision' and inputevents_cv.charttime BETWEEN h.he_onset AND (h.he_offset + INTERVAL '59 min')
    			AND inputevents_cv.itemid in ( 
                   30008 --	Albumin 5%
                  ,30009 --	Albumin 25%
                  ,42832 --	albumin 12.5%
                  ,40548 --	ALBUMIN
                  ,45403 --	albumin
                  ,44203 --	Albumin 12.5%
                  ,30181 -- Serum Albumin 5%
                  ,46564 -- Albumin
                  ,43237 -- 25% Albumin
                  ,43353 -- Albumin (human) 25%

                  ,30012 --	Hespan
                  ,46313 --	6% Hespan

                  ,42975 --	DEXTRAN DRIP
                  ,42944 --	dextran
                  ,46336 --	10% Dextran 40/D5W
                  ,46729 --	Dextran
                  ,40033 --	DEXTRAN
                  ,45410 --	10% Dextran 40
                  ,30011 -- Dextran 40
                  ,30016 -- Dextrose 10%
                  ,42731 -- Dextran40 10%
                  ,
                  30008, -- Albumin 5%
                  30016, -- Dextrose 10%
                  30187, -- D10W
                  30317, -- D 12.5 W
                  30318 --D15W      
                    )
   ),
 --select distinct on (icustay_id) * from fluid_patients_cv;
CV_FLUID_ID AS(
	select distinct icustay_id from fluid_patients_cv
),
--select distinct icustay_id from CV_FLUID_ID;

fluid_pats_ce AS (
    select h.icustay_id , ce.itemid , ce.charttime as cvfluid_input_time, ce.valuenum as cvfluid_vol
     from hypo_cohort_final h
     inner join chartevents_adult ce on h.icustay_id = ce.icustay_id
     where h.dbsource != 'metavision' and ce.charttime BETWEEN h.he_onset AND (h.he_offset + INTERVAL '59 min') 
    AND ce.itemid in
  ( 
      2510 --	DEXTRAN LML 10%
    , 3087 --	DEXTRAN 40  10%
    , 6937 --	Dextran
    , 3087 -- | DEXTRAN 40  10%
    , 3088 --	DEXTRAN 40%
      -- NOTE - THESE ARE COLLOIDS IN CE TABLE.
  )
  --and ce.valuenum is not null
    OR h.dbsource != 'metavision' and ce.charttime BETWEEN h.he_onset AND (h.he_offset + INTERVAL '59 min') 
    AND ce.itemid in
    (
      
     1460 -- D5W 100meq NaHCO3
    ,5392 -- D10W bolus
    ,1848 -- D5W (Dextrose in water)
    ,2619 --3% NS
    ,5321 -- ns bolus
    ,1634 -- lactated ringers
    ,4647 -- NS bolus
    ,4970 -- NS bolus
    ,5199 --NS bolus
    ,5333 -- saline flush
    ,6190 --NS
    )
    AND valuenum > 250
),

CV_FLUID_ID_CE AS(
	select distinct icustay_id from fluid_pats_ce
),
--select * from CV_FLUID_ID_CE;

-- patients in mv.    
fluid_patients_mv AS (
       select h.icustay_id , inputevents_mv.itemid as mvfluid_id, inputevents_mv.starttime as mvfluid_input_time, inputevents_mv.amount as mvfluid_vol ,
    		   inputevents_mv.cancelreason
            from hypo_cohort_final h
        	inner join inputevents_mv on h.icustay_id = inputevents_mv.icustay_id
        		where h.dbsource = 'metavision' and inputevents_mv.starttime BETWEEN h.he_onset AND (h.he_offset + INTERVAL '59 min')
                	AND inputevents_mv.itemid in ( 
                    	225158, -- NaCl 0.9%
    					225828, -- LR
    					--225944, -- Sterile Water
    					--225797,  -- Free Water
                        225823, -- D5 1/2NS
                        225825, -- D5NS
                        225827, -- D5LR
                        225941 -- D5 1/4NS
                    )
						
        			AND
                      -- in MetaVision, these ITEMIDs appear with a null rate IFF endtime=starttime + 1 minute
                      -- so it is sufficient to:
                      --    (1) check the rate is > 240 if it exists or
                      --    (2) ensure the rate is null and amount > 240 ml
					    (
                          (rate is not null and rateuom = 'mL/hour' and rate > 250)
                          OR (rate is not null and rateuom = 'mL/min' and rate > (250/60.0))
                          OR (rate is null and amountuom = 'L' and amount > 0.250)
                          OR (rate is null and amountuom = 'ml' and amount > 250)
                        )
    				AND cancelreason = 0
                    AND statusdescription != 'Rewritten'
    			OR h.dbsource = 'metavision' AND inputevents_mv.starttime BETWEEN h.he_onset AND (h.he_offset + INTERVAL '59 min')
                	AND inputevents_mv.itemid in ( 
                    220864, --	Albumin 5%	7466 132 7466
                    220862, --	Albumin 25%	9851 174 9851
                    225174, --	Hetastarch (Hespan) 6%	82 1 82
                    225795,  --	Dextran 40	38 3 38
                    225796 --  Dextran 70
                    -- below ITEMIDs not in use
                   -- 220861 | Albumin (Human) 20%
                   -- 220863 | Albumin (Human) 4%
                   ,
                   220949, -- Dextrose 5%
                   220950, -- Dextrose 10%
                   220952 -- Dextrose 50%
                    )
                    
    			    AND cancelreason = 0
    				AND statusdescription != 'Rewritten'
    				AND rate is not null
    				AND rate > 0
        ),
icustays_mv_fluid as (
	select distinct icustay_id from fluid_patients_mv
),
fluid_icu_ids as(
    select icustay_id from CV_FLUID_ID
    union
    select * from icustays_mv_fluid
    union 
    select * from CV_FLUID_ID_CE
)
select distinct icustay_id into fluid_icu_ids from fluid_icu_ids;

-- now make indicator variable if patient recieved fluids treatment.
select *, ( CASE
		WHEN icustay_id in (select icustay_id from fluid_icu_ids ) Then 1
    	ELSE 0
    	END ) AS IV_fluid_ind
into hypo_cohort_fluids_trt
from hypo_cohort_final;

--drop table hypo_cohort_fluids_trt;
--drop table fluid_icu_ids;

/* Next, examine patients who recieved vassopressor treatment */ 

--patients in cv.
WITH vp_patients_cv AS (
     select h.icustay_id , h.he_onset, h.he_offset, ii.itemid , ii.charttime as cvvp_input_time, ii.rate as cvvp_rate
     from hypo_cohort_final h
     inner join inputevents_cv ii on h.icustay_id = ii.icustay_id
     where h.dbsource != 'metavision' and ii.charttime BETWEEN h.he_onset AND (h.he_offset + INTERVAL '59 min')
            AND  ii.itemid in
				(
                   30047,30120 -- norepinephrine
    			  ,30044,30119,30309 -- epinephrine 
    			  ,30127,30128 -- phenylephrine
    		      ,30051, 42273, 42802 -- vasopressin
    		      ,30306, 30042 -- dobutamine
                  ,30043,30307,30125 -- dopamine
                  ,42273, 46570 -- vasopressin
                 -- ,30125 -- milrinone
                 -- ,30046 -- isuprel
					)
    		 AND rate is not null
  			 AND rate > 0
        ),
        --select distinct on (icustay_id) * from vp_patients_cv; 
icustays_cv_vp_ie as (
	select distinct icustay_id from vp_patients_cv 
),
--select * from icustays_cv_vp_ie;

/* NO INFORMATION ABOUT VASOPRESSOR ADMINISTRATION GIVEN IN CHARTEVENTS

vp_patients_cv_ce AS (
     select h.icustay_id , ce.itemid , ce.charttime , ce.valuenum
     from hypo_cohort_final h
     inner join chartevents_adult ce on h.icustay_id = ce.icustay_id
	 where h.dbsource = 'carevue' AND ce.charttime BETWEEN h.he_onset AND h.he_offset + INTERVAL '15 min'
            AND ce.itemid in (
            			 1136 -- vasopressin
                		,5461 -- phenylephrine drops
                		,1327 -- vasopressin unit/min
                		,3112 -- epinephrine mcg/min
                		,5656 -- phenylephrine
                		,5747 -- dobutamine
                		,2334 -- vasopressin u/hr
                		,6690 -- phenylephrine ggts ("guttae" - latin for drops)
                		,5805 -- DOPAMINE MICS/KG/MIN
                		,5329 -- Dopamine
                		,2248 -- VASOPRESSIN UNIT/MIN
                		,2334 -- vasopressin u/hr
                		,2445 -- vasopressin
                		,2561, 2765 -- VASOPRESSIN
                		,42273 -- vasopressin unit/min
            )
    		AND valuenum > 0
  ),
icustays_cv_vp_ce as (
	select distinct icustay_id from vp_patients_cv_ce
)
select * from icustays_cv_vp_ce;
*/

-- patients in mv.    
vp_patients_mv AS (
        select h.icustay_id , inputevents_mv.itemid, inputevents_mv.starttime , inputevents_mv.amount as mvvp_amount ,
    		   inputevents_mv.cancelreason
            from hypo_cohort_final h
            inner join inputevents_mv on h.icustay_id = inputevents_mv.icustay_id
        	WHERE h.dbsource = 'metavision' and inputevents_mv.starttime BETWEEN h.he_onset AND (h.he_offset + INTERVAL '59 min') 
                AND inputevents_mv.itemid in
				(
                    221906 -- norepinephrine
  					,221289 -- epinephrine
  					,221749 -- phenylephrine
  					,222315 -- vasopressin
  					,221653 -- dobutamine
                    ,221662 -- dopamine
                    ,222315 -- vasopressin
                    --,221986 -- milrinone
                    --,227692 -- isuprel
					)
    			AND rate is not null
  				AND rate > 0
  				AND statusdescription != 'Rewritten' -- only valid orders
    			AND cancelreason = 0
        ), 
icustays_mv_vp as (
	select distinct icustay_id from vp_patients_mv
), 
vp_icu_ids as(
    select * from icustays_mv_vp
    union
    select * from icustays_cv_vp_ie
)
--select * into vp_icu_ids from vp_icu_ids;

-- now make indicator variable if patient recieved fluids treatment.
select *, ( CASE
		WHEN icustay_id in (select icustay_id from vp_icu_ids ) Then 1
    	ELSE 0
    	END ) AS vp_ind
into hypo_cohort_fluids_vp
from hypo_cohort_fluids_trt;

select count(*) from hypo_cohort_fluids_vp where dbsource != 'metavision' and vp_ind=1 and iv_fluid_ind=0;

-- Final Table.
SELECT distinct on(subject_id) * INTO hypo_cohort_fluids_vp_subjects FROM hypo_cohort_fluids_vp order by subject_id;
    
-- drop table hypo_cohort_fluids_trt;
-- drop table hypo_cohort_fluids_vp;
-- drop table fluid_icu_ids;
-- drop table vp_icu_ids;

/*
select * from d_items where lower(label) like 
    			 '%norepinephrine%'
  				 --OR lower(label) like '%epinephrine%'
  				 --OR lower(label) like '%phenylephrine%'
  				 --OR lower(label) like '%vasopressin%'
  				 --OR lower(label) like '%dobutamine%'
                 --OR lower(label) like '%dopamine%'
                 --OR lower(label) like '%NEOSYNEPHRINE%';

select * from d_items where label like 
    			 '%NS%'
  				 OR label like '%bolus%'
  				 OR label like '%rl%'
  				 OR label like '%D5%'
  				 OR label like '%saline%'
                 OR label like '%ringer%';

 select * from chartevents_adult where itemid in (
 						1136 -- vasopressin
                		--,5461 -- phenylephrine drops
                		--,1327 -- vasopressin unit/min
                		--,3112 -- epinephrine mcg/min
                		--,5656 -- phenylephrine
                		--,5747 -- dobutamine
                		--,2334 -- vasopressin u/hr
                		--,6690 -- phenylephrine ggts ("guttae" - latin for drops)
                		--,42273 -- vasopressin unit/min
 )
 
select * from inputevents_mv where itemid in (
               221906 -- norepinephrine
  					,221289 -- epinephrine
  					,221749 -- phenylephrine
  					,222315 -- vasopressin
  					,221653 -- dobutamine
                    ,221662 -- dopamine
                    ,222315 -- vasopressin,
 )
 
select * from d_items where itemid in (30018,30176, 30161, 30020, 30018, 30353, 30352, 30143, 45232, 30160, 30014, 30015, 30060, 45374, 
                   45358, 30061, 44399, 1634, 30021,5455, 6190,4647, 44440,44053, 43354,30168, 1494, 5333,221986);
                    

select * from d_items where itemid in (
										30047,30120, 30044,30119,30309,30127,30128,
                  30051, 30043,30307,30042,30306, 30125 
					);
*/                    
                    