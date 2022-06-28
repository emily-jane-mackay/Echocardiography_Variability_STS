################################################################################
# This file prepares data for statistical analyses
################################################################################
# Load packages
library(data.table)
library(dplyr)
library(tableone)
library(gdata)
library(xtable)

# Read raw data with the following covariates
vbs = c('exclude', 'tee', 'private_i', 'medicare_i',
        'medicaid_i', 'gov_other_i', 'hmo_i', 'non_us_charitable_i',
        'no_i', 'missing_i', 'insurance_labeled', 
        'age', 'bsa', 'heightcm','weightkg', 'bmi', 'female', 
        'male', 'race_white', 'race_black', 'race_asian', 
        'race_native_am', 'race_native_pacific', 
        'race_other', 'ethnic_hisp_latino',
        'sts_region_e', 'surgyear', 'op_elective',
        'op_urgent', 'op_emergent', 'op_emergent_salvage',
        'op_missing', 'ad_elective', 'ad_emergent', 
        'ad_transfer', 'ad_other', 'ad_missing', 
        'osh_cardiac_srg', 'iabp_pre_intra_op',
        'car_shock_preop', 'resusc_preop',
        'pr_cabg_prior', 'pr_valve_prior',
        'class_nyha_1', 'class_nyha_2', 'class_nyha_3', 'class_nyha_4',
        'diabetes_preexist', 'hypertension_preexist',
        'endocarditis_preexist', 'chronic_lung_preexist',
        'liver_preexist', 'pvd_preexist', 'cvd_preexist', 'hdef',
        'predmort', 'predreop', 'predstro', 'predmm', 
        'surgery_vol_siteid_yr', 'surgery_vol_siteid',
        'surgery_vol_surgid_yr', 'surgery_vol_surgid',
        'surgery_vol_hospid_yr', 'surgery_vol_hospid',
        'tee_rate_siteid', 'tee_rate_surgid', 'tee_rate_hospid',
        'aortic_valve_replace', 'aortic_valve_repair', 
        'aortic_proximal_complex', 'mitral_valve_replace',
        'mitral_valve_repair', 'tv_repair_replace',
        'pv_repair_replace', 'valve_plus_cabg', 'cabg_isolated',
        'sts_siteid', 'sts_surgid', 'sts_hospid')

data = fread('Program_1_clean_data_2022_02_15.csv', select = vbs)


# Consolidate and define surgery categories
surgery_cat = cbind(AV_repair = data$aortic_valve_repair, 
                    AV_replacement = data$aortic_valve_replace,
                    MV_repair = data$mitral_valve_repair,
                    MV_replacement = data$mitral_valve_replace,
                    TV = data$tv_repair_replace,
                    PV = data$pv_repair_replace,
                    AP = data$aortic_proximal_complex,
                    plus_CABG = data$valve_plus_cabg,
                    CABG_isolated = data$cabg_isolated)

# Tabulate all possible surgery combinations
cat_collapsed = apply(surgery_cat,1,paste,collapse='')

# Put surgery into several categories
ind1 = which(cat_collapsed == '000000001') # isolated CABG
ind2 = which(cat_collapsed == '010000000') # AV replacement only
ind3 = which(cat_collapsed == '010000010') # AV replacement with CABG
ind4 = which(cat_collapsed == '000100000') # MV replacement
ind5 = which(cat_collapsed == '000100010') # MV replacement with CABG
ind6 = which(cat_collapsed == '001000000') # MV repair
ind7 = which(cat_collapsed == '001000010') # MV repair with CABG

data$surg_cat = rep('other', dim(data)[1])
data$surg_cat[ind1] = 'CABG isolated'
data$surg_cat[ind2] = 'AV only'
data$surg_cat[ind3] = 'AV w/ CABG'
data$surg_cat[ind4] = 'MV replace'
data$surg_cat[ind5] = 'MV replace w/ CABG'
data$surg_cat[ind6] = 'MV repair'
data$surg_cat[ind7] = 'MV repair w/ CABG'

# n = 2,540,199
data = data %>%
  filter(exclude == 0) # n = 2,530,649 after exclusion

# More preprocessing: coding race, sex, op
data = data %>%
  mutate(sex = ifelse(female == 1, 'female', 'Other')) %>%
  mutate(sex = ifelse(male == 1, 'male', sex)) %>%
  mutate(race_other = 1 - race_white - race_black) %>%
  mutate(race_white = ifelse(race_other == -1, 0, race_white)) %>%
  mutate(race_black = ifelse(race_other == -1, 0, race_black)) %>%
  mutate(race_other = ifelse(race_other == -1, 1, race_other)) %>%
  mutate(race = ifelse(race_white == 1, 'White', 'Other')) %>%
  mutate(race = ifelse(race_black == 1, 'Black', race)) %>%
  mutate(OP = ifelse(op_elective == 1, 'Elective', 'Other')) %>%
  mutate(OP = ifelse(op_urgent == 1, 'Urgent', OP)) %>%
  mutate(OP = ifelse(op_emergent == 1 | op_emergent_salvage == 1, 
                     'Emergent', OP)) %>%
  mutate(OP = ifelse(op_missing == 1, 'Missing', OP)) %>%
  mutate(EF_missing = is.na(hdef),
         predmort_missing = is.na(predmort),
         predmm_missing = is.na(predmm),
         sts_hospid_missing = is.na(sts_hospid),
         sts_surgid_missing = is.na(sts_surgid))

###############################################################################
# Isolated CABG cohort
###############################################################################
data_CABG = data %>% # n = 1,517,911
  filter(surg_cat == 'CABG isolated')

# Keep those without missing data on key covariates
data_CABG_final = data_CABG %>% # 1517911
  select(tee, age, sex, race, 
         predmort, predmm,
         insurance_labeled, surgyear,
         sts_region_e, OP, hdef,
         surgery_vol_surgid,
         surgery_vol_hospid,
         surg_cat, sts_hospid, sts_surgid) %>%
  filter(!insurance_labeled == 'missing_data') %>%
  filter(!OP == 'Missing') %>% # n = 1483813
  filter(!is.na(hdef)) %>%  # n = 1,443,378
  filter(!sex == 'Other') %>%
  filter(!sts_region_e == 'missing') %>%
  filter(complete.cases(.)) %>% # n = 1,379,668
  mutate(sts_hospid = factor(sts_hospid),
         sts_surgid = factor(sts_surgid),
         sts_region_e = factor(sts_region_e),
         surg_cat = factor(surg_cat),
         insurance_labeled = factor(insurance_labeled),
         insurance_labeled = relevel(insurance_labeled, 'no_insurance'),
         OP = factor(OP),
         OP = relevel(OP, 'Elective'),
         vol_hosp_std = scale(surgery_vol_hospid),
         vol_surg_std = scale(surgery_vol_surgid),
         EF_std = scale(hdef)) %>%
  mutate(age_std = scale(age)) %>%
  mutate(year_since_2011 = as.numeric(surgyear) - 2011) #1,365,708

################################################################################
# Valve cohort
###############################################################################
data_valve = data %>%
  filter(surg_cat %in% c('AV only', 'AV w/ CABG', 'MV repair', 
                         'MV repair w/ CABG', 'MV replace', 'MV replace w/ CABG'))

# Keep those without missing data on key covariates
data_valve_final = data_valve %>% # 
  select(tee, age, sex, race, 
         predmort, predmm,
         insurance_labeled, surgyear,
         sts_region_e, OP, hdef,
         surgery_vol_surgid,
         surgery_vol_hospid,
         surg_cat, sts_hospid, sts_surgid) %>%
  filter(!insurance_labeled == 'missing_data') %>%
  filter(!OP == 'Missing') %>% # n = 1483813
  filter(!is.na(hdef)) %>%  # n = 1,443,378
  filter(!sex == 'Other') %>%
  filter(!sts_region_e == 'missing') %>%
  filter(complete.cases(.)) %>% # n = 1,379,668
  mutate(sts_hospid = factor(sts_hospid),
         sts_surgid = factor(sts_surgid),
         sts_region_e = factor(sts_region_e),
         surg_cat = factor(surg_cat),
         sex = factor(sex),
         race = factor(race),
         insurance_labeled = factor(insurance_labeled),
         insurance_labeled = relevel(insurance_labeled, 'no_insurance'),
         OP = factor(OP),
         OP = relevel(OP, 'Elective'),
         vol_hosp_std = scale(surgery_vol_hospid),
         vol_surg_std = scale(surgery_vol_surgid),
         EF_std = scale(hdef)) %>%
  mutate(age_std = scale(age)) %>%
  mutate(year_since_2011 = as.numeric(surgyear) - 2011)

###########################################################################################
###########################################################################################
# Save final data to .csv files
write.csv(data_CABG_final, './data_CABG_analysis.csv')
write.csv(data_valve_final, './data_valve_analysis.csv')
