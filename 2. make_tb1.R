################################################################################
# This file makes Table 1 in the main article
################################################################################
# Load packages
library(data.table)
library(dplyr)
library(tableone)
library(gdata)
library(xtable)

###########################################################################################
###########################################################################################
# Make table one for ALL
tb1_vbs = c('age', 'sex', 'race', 
            'predmort', 'predmm',
            'insurance_labeled', 'surgyear',
            'sts_region_e', 'OP', 'hdef',
            'surgery_vol_surgid',
            'surgery_vol_hospid',
            'surg_cat', 'EF_missing',
            'predmort_missing', 'predmm_missing',
            'sts_hospid_missing', 'sts_surgid_missing')

tb1_fvbs = c('sex', 'race', 
             'insurance_labeled', 'surgyear',
             'sts_region_e', 'OP', 
             'surg_cat')


# Make table one for those without missing data
data_CABG = data.table::fread('./data_CABG_analysis.csv')
data_valve = data.table::fread('./data_valve_analysis.csv')

tb_CABG = CreateTableOne(vars = tb1_vbs, strata = 'tee', data = data_CABG, 
                          factorVars = tb1_fvbs, test = FALSE)
tb_valve = CreateTableOne(vars = tb1_vbs, strata = 'tee', data = data_valve, 
                           factorVars = tb1_fvbs, test = FALSE)

# Combine and output
tb_CABG = print(tb_CABG, contDigits = 4, smd = TRUE)
tb_valve = print(tb_valve, contDigits = 4, smd = TRUE)
tb = cbindX(tb_CABG, tb_valve)
xtable(tb) # This is the final TABLE 1