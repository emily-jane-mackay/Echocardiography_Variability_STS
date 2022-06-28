// setup  
// Change working directory

// set markdoc to OFF
//OFF
cd "/path/"

set more off
set linesize 255

// set file name
// set file path
local file_name = "filename"
local path = "/path/"

// create markdoc log file
qui log using "`path'`file_name'", replace name("markdoc_log")

//OFF
// create typical file name and path
log using "`path'`file_name'.txt", text replace 


//ON
/***
Outcome Variables
===========================
STS variable name: <inoptee>
---------------------------
***/
//OFF

// load in the original data in .dta format
// note: converted from .csv to .dta in another .do file
use "/path/filename.dta", clear 


* STS v.2.81 
* PayorGov: 1 = yes vs 2 = no 
* PayorGovMcare: 1 = yes vs 2 = no 
* PayorGovMcareFFS: 1 = yes vs 2 = no 
* PayorGovMcaid: 1 = yes vs 2 = no 
* PayorGovMil: 1 = yes vs 2 = no 


* drop rows with surgyear <2011 
count /* 2,540,244 */
drop if surgyear <2011 /* exclude count == 19 */ 
count /* 2,540,225 */
* drop rows with surgyear == . 
count /* 2,540,225 */
drop if surgyear == . /* exclude count == 26 */ 
count /* 2,540,199 */ 

* outcome variable: inoptee: categorical: 1 = yes | 2 = no [note: inoptee == . assumed tee == 0]
tabulate inoptee, miss
generate tee = cond(inoptee == 1, 1, 0)

// STS Data dictionary "opvalve" -> indicates whether a surgical procedure was done on the aortic, mitral, tricuspid, or pulmonic valves
// opvalve: categorical: 1 = yes | 2 = no 
tabulate opvalve, miss
generate opvalve_str = " " 
replace opvalve_str = "yes" if opvalve == 1
replace opvalve_str = "no" if opvalve == 2
replace opvalve_str = "-" if opvalve != 1 & opvalve != 2
// sanity check
tabulate opvalve_str, miss
// check variable covers 2011 - 2019
tabulate opvalve_str surgyear, miss
// 
generate op_valve = cond((opvalve_str == "yes"), 1, 0) 

//////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Aortic Valve
// v2.9  2017-2019
// v2.81 2014-2017
// v2.73 2011-2014
// Will need data three distinct data dictionaries to join
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// aortic valve
// VSAV
tabulate vsav, miss
tabulate vsav surgyear, miss
// [note: vsav == 1 av surgery vs vsav == 2 no av surgery]
// [note: vsmv == 3 "planned"]
// [note: vsav == 4 & vsav == 5 code "unplanned"]
generate vsav_2011_2019 = cond((vsav == 1 | vsav == 3 | vsav == 4 | vsav == 5), 1, 0)

// av_replace | av_repair 
tabulate vsavpr, miss
tabulate vsavpr surgyear, miss
// [note: vsavpr == 1 av_replace vs vsavpr == 2 av_repair]
generate aortic_valve_replace = cond(vsavpr == 1, 1, 0)  
generate aortic_valve_repair = cond(vsavpr == 2, 1, 0)
generate aortic_proximal_complex = cond((vsavpr == 3 | vsavpr == 4 | vsavpr == 5 | vsavpr == 6 | vsavpr == 7 | vsavpr == 8 | vsavpr == 9 | vsavpr == 10 | vsavpr == 11 | vsavpr == 13 | vsavpr == 14 | vsavpr == 15), 1, 0) 

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Mitral Valve
// v2.9  2017-2019
// v2.81 2014-2017 
// v2.73 2011-2014 
// Will need data three distinct data dictionaries to join
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// vsmv 
tabulate vsmv, miss
tabulate vsmv surgyear, miss
// [note: vsmv == 1 +mv surgery vs vsmv == 2 no mv surgery]
// [note: vsmv == 3 "planned"]
// [note: vsmv == 4 & vsmv == 5 cod "unplanned"]
generate vsmv_2011_2019 = cond((vsmv == "1" | vsmv == "3" | vsmv == "4" | vsmv == "5"), 1, 0) 

// mv_replace | mv_repair 
tabulate vsmvpr, miss
tabulate vsmvpr surgyear, miss
// [note: vsmvpr == 1 mv_replace vs vsmvpr == 2 mv_repair]
generate mitral_valve_replace = cond(vsmvpr == "1", 1, 0)
generate mitral_valve_repair = cond(vsmvpr == "2", 1, 0)


///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Tricuspid Valve
// v2.9  2017-2019 <vstv>
// v2.81 2014-2017 
// v2.73 2011-2014 
// Will need data three distinct data dictionaries to join
// Note: 2018 & 2019 missing tricuspid granularity from "OpTricus"
// Note: Will only have vstv yes vs no for 2018 & 2019
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// optricus or vstv (encompassing tricuspid valve repair/replacement)
tabulate surgyear optricus, miss
tabulate surgyear vstv, miss
// [note: 1 indicates no tricuspid across both optricus & vstv]
generate vstv_2011_2019 = cond((optricus == 2 | optricus == 3 | optricus == 4 | optricus == 5 | optricus == 6 | vstv == 3 | vstv == 4 | vstv == 5), 1, 0)
// labeled
generate tv_repair_replace = vstv_2011_2019

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Pulmonic Valve
// v2.9  2017-2019
// v2.81 2014-2017
// v2.73 2011-2014 
// Will need data three distinct data dictionaries to join
// Note: pulmonic valve OpPulm covers all years (unlike tricuspid)
// Note: create variable to match tricuspid (e.g. repair & replacement variable)
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// OpPulm vs VSPV
tabulate surgyear oppulm, miss
tabulate surgyear vspv, miss 
// [note: 1 indicates no pulmonic across both oppulm and vspv]
generate vspv_2011_2019 = cond((oppulm == 2 | oppulm == 3 | oppulm == 4 | vspv == 3 | vspv == 4 | vspv == 5), 1, 0)
// labeled 
generate pv_repair_replace = vspv_2011_2019

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// +CABG Surgery
// v2.9  2017-2019
// v2.81 2014-2017 
// v2.73 2011-2014 
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// opcab
tabulate surgyear opcab, miss
// [note: 2 indicates no cabg surgery]
generate cabg = cond((opcab == 1 | opcab == 3 | opcab == 4 | opcab == 5), 1, 0)
// create cabg_isolated 
generate cabg_isolated = cabg == 1 & (opvalve != 1)
// sanity check 
tabulate cabg_isolated op_valve, miss

// cabg + valve
generate valve_plus_cabg = cond((cabg == 1 & op_valve == 1), 1, 0)

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// include / exclude conditions 
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

* generate include variable to find missing data
generate include = cond((cabg == 1 | aortic_valve_replace == 1 | aortic_valve_repair == 1 | aortic_proximal_complex == 1 | mitral_valve_replace == 1 | mitral_valve_repair == 1 | tv_repair_replace == 1 | pv_repair_replace == 1), 1, 0)

// lvadproc
generate vad = cond(vadproc == 5, 1, 0)

// opvalve_nos
generate opvalve_nos = cond((opvalve == 1 & include != 1), 1, 0)

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Exclusions
// (1) vadproc == 5
// (2) opvalve == . 
// (3) opcab == . 
// (4) opvalve == 1 & include != 1 
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

* cohort development 
generate ct_surgery = 1 

// exclude vads  
tabulate vad if ct_surgery == 1 
// exclusion count
count if vad == 1 /* 2,940 */ 
// inclusion count
count if ct_surgery == 1 & vad != 1 /* 2,537,295 */ 

// exclude missing opvalve  
tabulate opvalve if ct_surgery == 1 & vad != 1, miss
// exclusion count 
count if opvalve == . & vad != 1 /* 2,110 */ 
// inclusion count 
count if ct_surgery == 1 & opvalve != . & vad != 1 /* 2,535,185 */ 

// exclude missing opcab
tabulate opcab if ct_surgery == 1 & opvalve != . & vad != 1, miss 
// exclusion count 
count if opcab == . & opvalve != . & vad != 1 /* 2,092 */ 
// inclusion count 
count if ct_surgery == 1 & opcab != . & opvalve != . & vad != 1  /* 2,533,093 */ 

// exlcude opvalve_nos
tabulate opvalve_nos if ct_surgery == 1 & opcab != . & opvalve != . & vad != 1
// exclusion count 
count if opvalve_nos == 1 & opcab != . & opvalve != . & vad != 1 /* 2,444 */ 
// inclusion count
count if ct_surgery == 1 & opvalve_nos != 1 & opcab != . & opvalve != . & vad != 1 /* 2,530,649 */ 

// exclude age <=18 
summarize age if ct_surgery == 1 & opvalve_nos != 1 & opcab != . & opvalve != . & vad != 1, detail
// [note: observations with age <18 years already excluded after above exclusion criteria]

* generate exclude variable encompassing above conditions 
generate exclude = cond((vad == 1 | opvalve == . | opcab == . | opvalve_nos == 1), 1, 0)

// check against final cohort count based on exclusion criteria above 
tabulate ct_surgery if include == 1 & exclude != 1 

// final observation count == 2,530,649

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************

//Insurance categories 

*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

* STS v.2.81 
* PayorGov: 1 = yes vs 2 = no 
* PayorGovMcare: 1 = yes vs 2 = no 
* PayorGovMcareFFS: 1 = yes vs 2 = no 
* PayorGovMcaid: 1 = yes vs 2 = no 
* PayorGovMil: 1 = yes vs 2 = no 

* insuarnce categories - mutually exclusive 
* [note: observations with more than one type of insurance were placed in the category of highest ranking]
* (1) private (commercial)
* (2) Medicare (medicare or medicare ffs) 
* (3) Medicaid
* (4) Gov - other
* (5) HMO (HMO listed as primary) 
* (6) non_US_charitable 
* (7) no insurance / self pay 
* (8) missing insurance data 

// private (commercial / private) 
generate private_i = cond((payorcom == 1 | payorprim == 9), 1, 0)

// Medicare 
generate medicare_i = cond(((payorgovmcare == 1 | payorgovmcareffs == 1 | payorprim == 2) & private_i != 1), 1, 0) 

// Medicaid 
generate medicaid_i = cond(((payorgovmcaid == 1 | payorprim == 3) & private_i != 1 & medicare_i != 1), 1, 0) 

// Gov - other 
generate gov_other_i = cond(((payorgov == 1 | payorgovmil == 1 | payorgovstate == 1 | payorgovihs == 1 | payorgovcor == 1 | payorgovoth == 1 | payorprim == 4 | payorprim == 5 | payorprim == 6 | payorprim == 7 | payorprim == 8) & private_i != 1 & medicare_i != 1 & medicaid_i != 1), 1, 0)

// HMO - not categorized into any of the above
generate hmo_i = cond(((payorhmo == 1 | payorprim == 10) & private_i != 1 & medicare_i != 1 & medicaid_i != 1 & gov_other_i != 1), 1, 0)

// insurance - non-US / charitable 
generate non_us_charitable_i = cond(((payornonus == 1 | payorprim == 12) & private_i != 1 & medicare_i != 1 & medicaid_i != 1 & gov_other_i != 1 & hmo_i != 1), 1, 0)

// no insurance (self-pay) 
generate no_i = cond(((payorns == 1 | payorprim == 1) & private_i != 1 & medicare_i != 1 & medicaid_i != 1 & gov_other_i != 1 & hmo_i != 1 & non_us_charitable_i != 1), 1, 0)

// insurance missing data 
generate missing_i = cond((private_i != 1 & medicare_i != 1 & medicaid_i != 1 & gov_other_i != 1 & hmo_i != 1 & non_us_charitable_i != 1 & no_i != 1), 1, 0) 

// check to make sure all observations have an insurance classification 
count if private_i != 1 & medicare_i != 1 & medicaid_i != 1 & gov_other_i != 1 & hmo_i != 1 & non_us_charitable_i != 1 & no_i != 1 & missing_i != 1

// check for mutual exclusivity 
generate insurance_labeled = " " 
replace insurance_labeled = "private" if private_i == 1
replace insurance_labeled = "medicare" if medicare_i == 1
replace insurance_labeled = "medicaid" if medicaid_i == 1
replace insurance_labeled = "gov_other" if gov_other_i == 1
replace insurance_labeled = "hmo" if hmo_i == 1
replace insurance_labeled = "non-US_or_charitable" if non_us_charitable_i == 1
replace insurance_labeled = "no_insurance" if no_i == 1 
replace insurance_labeled = "missing_data" if missing_i == 1 
encode insurance_labeled, generate(insurance_labeled_e) 
// sanity check 
tabulate insurance_labeled if exclude != 1, miss 
tabulate private_i insurance_labeled if exclude != 1, miss
tabulate medicare_i insurance_labeled if exclude != 1, miss
tabulate medicaid_i insurance_labeled if exclude != 1, miss
tabulate gov_other_i insurance_labeled if exclude != 1, miss
tabulate hmo_i insurance_labeled if exclude != 1, miss
tabulate non_us_charitable_i insurance_labeled if exclude != 1, miss
tabulate no_i insurance_labeled if exclude != 1, miss
tabulate missing_i insurance_labeled if exclude != 1, miss

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Demographics
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// Age
summarize age, detail

// bsa 
summarize bsa, detail 

// HeightCm
summarize heightcm, detail

// WeightKg
summarize weightkg, detail

// calculate BMI
// (1) convert cm to m
generate heightm = heightcm/100 
// check variable
summarize heightm, detail
// (2) BMI formula
generate bmi = weightkg / (heightm)^2
// check
summarize bmi, detail

// sex 
tabulate gender, miss
generate female = cond((gender == 2), 1, 0)
generate male = cond((gender == 1), 1, 0)
tabulate female male, miss

// race 
generate race_white = cond((racecaucasian == 1), 1, 0)
generate race_black = cond((raceblack == 1), 1, 0)
generate race_asian = cond((raceasian == 1), 1, 0)
generate race_native_am = cond((racenativeam == 1), 1, 0)
generate race_native_pacific = cond((racnativepacific == 1), 1, 0)
generate race_other = cond((raceother == 1), 1, 0)

// ethnicity 
generate ethnic_hisp_latino = cond((ethnicity == 1), 1, 0)

// geographic region 
tabulate sts_region, miss
replace sts_region = "missing" if sts_region == "" 
tabulate sts_region, miss
encode sts_region, gen(sts_region_e)

// surgyear
summarize surgyear, detail 

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Hospital / Surgical / Transfer / Acuity factors 
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// other hospital performs cardiac surgery
generate osh_cardiac_srg = cond((othhoscs == 1), 1, 0)

// operative status
generate op_elective = cond((status == 1), 1, 0)
generate op_urgent = cond((status == 2), 1, 0)
generate op_emergent = cond((status == 3), 1, 0)
generate op_emergent_salvage = cond((status == 4), 1, 0)
generate op_missing = cond((status == .), 1, 0) 

// admit source
generate ad_elective = cond((admitsrc == 1), 1, 0)
generate ad_emergent = cond((admitsrc == 2), 1, 0)
generate ad_transfer = cond((admitsrc == 3), 1, 0) 
generate ad_other = cond((admitsrc == 4), 1, 0) 
generate ad_missing = cond((admitsrc == .), 1, 0) 

// previous cabg 
generate pr_cabg_prior = cond(prcab == 1, 1, 0)

// previous valve
generate pr_valve_prior = cond(prvalve == 1, 1, 0)

// iabp pre/intraop
generate iabp_pre_intra_op = cond((iabp == 1 & (iabpwhen == 1 | iabpwhen == 2)), 1, 0) 

// cardiogenic shock preoperative
generate car_shock_preop = cond((carshock == 1 | carshock == 3 | carshock == 4), 1, 0) 

// resuscitation within one hour of surgery
generate resusc_preop = cond((resusc == 1 | resusc == 3 | resusc == 4), 1, 0) 


///////////////////////////////////////////////////////////////////////////////
// Preexisting Disease & Preoperative Covariates
///////////////////////////////////////////////////////////////////////////////

// coded using [cond( )] command

// Ejection Fraction (preoperative)
summarize hdef, detail 

// diabetes 
generate diabetes_preexist = cond((diabetes == 1), 1, 0)

// dialysis
generate dialysis_preexist = cond((dialysis == 1), 1, 0)

// hypertension
generate hypertension_preexist = cond((hypertn == 1), 1, 0)

// infective endocarditis
generate endocarditis_preexist = cond((infendo == 1), 1, 0) 

// chronic lung disease
generate chronic_lung_preexist = cond((chrlungd == 2 | chrlungd == 3 | chrlungd == 4 | chrlungd == 5), 1, 0)

// liver disease
generate liver_preexist = cond((liverdis == 1), 1, 0)

// pvd 
generate pvd_preexist = cond((pvd == 1), 1, 0)

// cvd
generate cvd_preexist = cond((cvd == 1), 1, 0) 

// NYHA class 
generate class_nyha_1 = cond((classnyh == 1), 1, 0)
generate class_nyha_2 = cond((classnyh == 2), 1, 0)
generate class_nyha_3 = cond((classnyh == 3), 1, 0)
generate class_nyha_4 = cond((classnyh == 4), 1, 0)

// STS risk predictions 
summarize predmort, detail 
summarize preddeep, detail 
summarize predreop, detail 
summarize predstro, detail 
summarize predvent, detail 
summarize predrenf, detail 
summarize predmm, detail 
summarize pred6d, detail 
summarize pred14d, detail 

// 

///////////////////////////////////////////////////////////////////////////////

// surgical volume by siteid | surgid | hospid 

///////////////////////////////////////////////////////////////////////////////

list sts_siteid sts_hospid sts_surgid surgyear inoptee tee op_valve cabg_isolated ct_surgery if sts_siteid == 500121 & sts_surgid == 801269 & surgyear == 2014

// surgical volume by siteid 
gsort sts_siteid sts_hospid sts_surgid surgyear
egen surgery_vol_siteid = count(ct_surgery), by(sts_siteid)
summarize surgery_vol_siteid, detail
histogram surgery_vol_siteid
* sanity check 
tabulate ct_surgery if sts_siteid == 500121
list sts_siteid sts_hospid sts_surgid surgyear inoptee tee op_valve cabg_isolated ct_surgery  surgery_vol_siteid if sts_siteid == 500121 & sts_surgid == 801269 & surgyear == 2014
// surgical volume by siteid & yr 
egen surgery_vol_siteid_yr = count(ct_surgery), by(sts_siteid surgyear)

// annual surgical volume by surgid  
egen surgery_vol_surgid = count(ct_surgery), by(sts_surgid)
summarize surgery_vol_surgid, detail
histogram surgery_vol_surgid
* sanity check 
tabulate ct_surgery if sts_surgid == 801269
list sts_siteid sts_hospid sts_surgid surgyear inoptee tee op_valve cabg_isolated ct_surgery  surgery_vol_siteid surgery_vol_surgid if sts_siteid == 500121 & sts_surgid == 801269 & surgyear == 2014
// surgical volume by surgid & yr 
egen surgery_vol_surgid_yr = count(ct_surgery), by(sts_surgid surgyear)
* sanity check 
list sts_siteid sts_hospid sts_surgid surgyear inoptee tee op_valve cabg_isolated ct_surgery  surgery_vol_siteid surgery_vol_surgid surgery_vol_surgid_yr if sts_siteid == 500121 & sts_surgid == 801269 & surgyear == 2014

// annual surgical volume by hospid
gsort sts_siteid sts_hospid sts_surgid surgyear 
egen surgery_vol_hospid = count(ct_surgery), by(sts_hospid)
summarize surgery_vol_hospid, detail
histogram surgery_vol_hospid
* sanity check 
tabulate ct_surgery if sts_hospid == 970267
list sts_siteid sts_hospid sts_surgid surgyear inoptee tee op_valve cabg_isolated ct_surgery  surgery_vol_siteid surgery_vol_hospid if sts_siteid == 500121 & sts_surgid == 801269 & surgyear == 2014
// surgical volume by siteid & yr 
egen surgery_vol_hospid_yr = count(ct_surgery), by(sts_hospid surgyear)
list sts_siteid sts_hospid sts_surgid surgyear inoptee tee op_valve cabg_isolated ct_surgery  surgery_vol_siteid surgery_vol_hospid surgery_vol_hospid_yr if sts_siteid == 500121 & sts_surgid == 801269 & surgyear == 2014

///////////////////////////////////////////////////////////////////////////////

// tee rates by siteid | surgid | hospid {overall; valve; cabg}

///////////////////////////////////////////////////////////////////////////////

// tee rate by siteid 
egen tee_rate_siteid = mean(tee), by(sts_siteid) 
// look at the distribution
summarize tee_rate_siteid, detail
histogram tee_rate_siteid
* sanity check 
list sts_siteid sts_hospid sts_surgid surgyear inoptee tee op_valve cabg_isolated ct_surgery tee_rate_siteid surgery_vol_siteid if sts_siteid == 500121 & sts_surgid == 801269 & surgyear == 2014
// tee rate [valve]  
egen tee_rate_siteid_valve = mean(tee) if cabg_isolated != 1, by(sts_siteid) 
histogram tee_rate_siteid_valve
// tee rate [CABG]  
egen tee_rate_siteid_cabg = mean(tee) if cabg_isolated == 1, by(sts_siteid) 
histogram tee_rate_siteid_cabg 
// dichotomize overall tee rate 
summarize tee_rate_siteid, detail 
generate tee_siteid_high = cond((tee_rate_siteid >= .7089947), 1, 0) 

// tee rate by surgid 
egen tee_rate_surgid = mean(tee), by(sts_surgid) 
// look at the distribution
summarize tee_rate_surgid, detail
histogram tee_rate_surgid
* sanity check 
list sts_siteid sts_hospid sts_surgid surgyear inoptee tee op_valve cabg_isolated ct_surgery tee_rate_siteid tee_rate_surgid if sts_siteid == 500121 & sts_surgid == 801269 & surgyear == 2014
// tee rate [valve]  
egen tee_rate_surgid_valve = mean(tee) if cabg_isolated != 1, by(sts_surgid) 
histogram tee_rate_surgid_valve
// tee rate [CABG]  
egen tee_rate_surgid_cabg = mean(tee) if cabg_isolated == 1, by(sts_surgid) 
histogram tee_rate_surgid_cabg 
// dichotomize overall tee rate 
summarize tee_rate_surgid, detail 
generate tee_surgid_high = cond((tee_rate_surgid >= .7136258), 1, 0) 

// tee rate by hospid 
egen tee_rate_hospid = mean(tee), by(sts_hospid) 
// look at the distribution
summarize tee_rate_hospid, detail 
histogram tee_rate_hospid
* sanity check 
list sts_siteid sts_hospid sts_surgid surgyear inoptee tee op_valve cabg_isolated ct_surgery tee_rate_siteid tee_rate_surgid tee_rate_hospid if sts_siteid == 500121 & sts_surgid == 801269 & surgyear == 2014
// tee rate [valve]  
egen tee_rate_hospid_valve = mean(tee) if cabg_isolated != 1, by(sts_hospid) 
histogram tee_rate_hospid_valve
// tee rate [CABG]  
egen tee_rate_hospid_cabg = mean(tee) if cabg_isolated == 1, by(sts_hospid) 
histogram tee_rate_hospid_cabg 
// dichotomize overall tee rate 
summarize tee_rate_hospid, detail 
generate tee_hospid_high = cond((tee_rate_hospid >= .7093644), 1, 0) 

///////////////////////////////////////////////////////////////////////////////
// list of Surgical Categories  
///////////////////////////////////////////////////////////////////////////////

tabulate aortic_valve_replace if exclude != 1, miss
tabulate aortic_valve_repair if exclude != 1, miss
tabulate aortic_proximal_complex if exclude != 1, miss
tabulate mitral_valve_replace if exclude != 1, miss
tabulate mitral_valve_repair if exclude != 1, miss
tabulate tv_repair_replace if exclude != 1, miss
tabulate pv_repair_replace if exclude != 1, miss
tabulate valve_plus_cabg if exclude != 1, miss
tabulate cabg_isolated if exclude != 1, miss
* confirm all observations have a surgical classification 
count if aortic_valve_replace != 1 & aortic_valve_repair != 1 & aortic_proximal_complex != 1 & mitral_valve_replace != 1 & mitral_valve_repair != 1 & tv_repair_replace != 1 & pv_repair_replace != 1 & valve_plus_cabg != 1 & cabg_isolated != 1 & exclude != 1

///////////////////////////////////////////////////////////////////////////////
// Table 1: Characteristics 
///////////////////////////////////////////////////////////////////////////////

// mtable set command
mtable set characteristics
//

tabulate insurance_labeled_e tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" insurance_labeled_e "insurance" 

ttest age if exclude != 1, by(tee) 
mtable "ttest" age "age"

ttest bsa if exclude != 1, by(tee) 
mtable "ttest" bsa "bsa"

ttest bmi if exclude != 1, by(tee) 
mtable "ttest" bmi "bmi"

tabulate female tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" female "female" 

tabulate male tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" male "male" 

tabulate race_white tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" race_white "white" 

tabulate race_white tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" race_white "white" 

tabulate race_black tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" race_black "black" 

tabulate race_asian tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" race_asian "asian" 

tabulate race_native_am tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" race_native_am "native_am" 

tabulate race_native_pacific tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" race_native_pacific "native_pacific" 

tabulate race_other tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" race_other "race_other" 

tabulate ethnic_hisp_latino tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" ethnic_hisp_latino "hisp_latino" 

tabulate sts_region_e tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" sts_region_e "region" 

tabulate surgyear tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" surgyear "year" 

tabulate op_elective tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" op_elective "op_elective" 

tabulate op_urgent tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" op_urgent "op_urgent" 

tabulate op_emergent tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" op_emergent "op_emergent" 

tabulate op_emergent_salvage tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" op_emergent_salvage "op_salvage" 

tabulate op_missing tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" op_missing "op_missing" 

tabulate ad_elective tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" ad_elective "ad_elective" 

tabulate ad_emergent tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" ad_emergent "ad_emergent" 

tabulate ad_transfer tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" ad_transfer "ad_transfer" 

tabulate ad_other tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" ad_other "ad_other" 

tabulate ad_missing tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" ad_missing "ad_missing" 

tabulate osh_cardiac_srg tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" osh_cardiac_srg "osh_cardiac_srg" 

tabulate pr_cabg_prior tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" pr_cabg_prior "cabg_prior" 

tabulate pr_valve_prior tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" pr_valve_prior "valve_prior" 

tabulate iabp_pre_intra_op tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" iabp_pre_intra_op "iabp" 

tabulate car_shock_preop tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" car_shock_preop "shock" 

tabulate resusc_preop tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" resusc_preop "resuscitation" 

ttest surgery_vol_siteid_yr if exclude != 1, by(tee) 
mtable "ttest" surgery_vol_siteid_yr "srg_vol_site"

ttest surgery_vol_surgid_yr if exclude != 1, by(tee) 
mtable "ttest" surgery_vol_surgid_yr "srg_vol_surgeon"

ttest surgery_vol_hospid_yr if exclude != 1, by(tee) 
mtable "ttest" surgery_vol_hospid_yr "srg_vol_hosp"

ttest tee_rate_siteid if exclude != 1, by(tee) 
mtable "ttest" tee_rate_siteid "tee_rate_siteid"

ttest tee_rate_surgid if exclude != 1, by(tee) 
mtable "ttest" tee_rate_surgid "tee_rate_surgid"

ttest tee_rate_hospid if exclude != 1, by(tee) 
mtable "ttest" tee_rate_hospid "tee_rate_hospid"

tabulate aortic_valve_replace tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" aortic_valve_replace "aortic_valve_replace" 

tabulate aortic_valve_repair tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" aortic_valve_repair "aortic_valve_repair" 

tabulate aortic_proximal_complex tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" aortic_proximal_complex "aortic_proximal" 

tabulate mitral_valve_replace tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" mitral_valve_replace "mitral_valve_replace" 

tabulate mitral_valve_repair tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" mitral_valve_repair "mitral_valve_repair" 

tabulate tv_repair_replace tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" tv_repair_replace "tv_repair_replace" 

tabulate pv_repair_replace tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" pv_repair_replace "pv_repair_replace" 

tabulate pv_repair_replace tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" pv_repair_replace "pv_repair_replace" 

tabulate valve_plus_cabg tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" valve_plus_cabg "valve_plus_cabg" 

tabulate cabg_isolated tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" cabg_isolated "cabg_isolated" 

ttest hdef if exclude != 1, by(tee) 
mtable "ttest" hdef "EF"

tabulate diabetes_preexist tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" diabetes_preexist "diabetes" 

tabulate dialysis_preexist tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" dialysis_preexist "dialysis" 

tabulate hypertension_preexist tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" hypertension_preexist "hypertension" 

tabulate endocarditis_preexist tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" endocarditis_preexist "endocarditis" 

tabulate chronic_lung_preexist tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" chronic_lung_preexist "chronic_lung" 

tabulate liver_preexist tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" liver_preexist "liver" 

tabulate pvd_preexist tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" pvd_preexist "pvd" 

tabulate cvd_preexist tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" cvd_preexist "cvd" 

tabulate class_nyha_1 tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" class_nyha_1 "NYHA_1" 

tabulate class_nyha_2 tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" class_nyha_2 "NYHA_2"   

tabulate class_nyha_3 tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" class_nyha_3 "NYHA_3" 

tabulate class_nyha_4 tee if exclude != 1, chi2 expected miss column matcell(values) matrow(names)
mtable "tabulate" class_nyha_4 "NYHA_4" 

ttest predmort if exclude != 1, by(tee) 
mtable "ttest" predmort "sts_mort_pred"

ttest predreop if exclude != 1, by(tee) 
mtable "ttest" predreop "sts_reop_pred"

ttest predstro if exclude != 1, by(tee) 
mtable "ttest" predstro "sts_stroke_pred"

ttest predrenf if exclude != 1, by(tee) 
mtable "ttest" predrenf "sts_renal_pred"

ttest predmm if exclude != 1, by(tee) 
mtable "ttest" predmm "sts_mm_pred"


///////////////////////////////////////////////////////////////////////////////

log close _all

markdoc "`path'`file_name'", export(docx) replace install title("Echocardiography: Variabiliy") 

///////////////////////////////////////////////////////////////////////////////

