###############################################################################
# This file fits the GLMM and outputs Table 2 nd eTable 1
###############################################################################
# Load packages
library(dplyr)
library(data.table)
library(GLMMadaptive)

# Load data
data_CABG_final = data.table::fread('./data_CABG_analysis.csv')
data_valve_final = data.table::fread('./data_valve_analysis.csv')

################################################################################
# GLMM Modeling
################################################################################
# Isolated CABG: Hospital-Level Analysis
###############################################################################
# Fit a GLMM model
md_CABG <- mixed_model(fixed = tee ~ age_std + sex + race + EF_std + OP + 
                         insurance_labeled + predmort + year_since_2011 + 
                         vol_hosp_std + sts_region_e, 
                       random = ~ 1 | sts_hospid, 
                       data = data_CABG_final,
                       family = binomial())

# Extract fixed effects coefficients
summary_md_CABG = summary(md_CABG)
fixed_coef = summary_md_CABG$coef_table

# Construct 95% CI
Est_coef = data.frame(Est = paste0(round(fixed_coef[,1], 3), ' (', 
                      round(fixed_coef[,1] - 1.96*fixed_coef[,2], 3), ', ', 
                      round(fixed_coef[,1] + 1.96*fixed_coef[,2], 3), ')'))

# Odds ratio for fixed effect coefficients
OR = round(exp(fixed_coef[1:16,1]), 2)
upper_limit = round(exp(fixed_coef[1:16,1] + 1.96*fixed_coef[1:16,2]), 2)
lower_limit = round(exp(fixed_coef[1:16,1] - 1.96*fixed_coef[1:16,2]), 2)
Odds_ratio_fixed = paste0(OR, ' (', lower_limit, ', ', upper_limit, ')')

# Calculate median OR
rf = confint(md_CABG, parm = 'var-cov')
rf_se = (sqrt(rf[3]) - sqrt(rf[2]))/1.96
MOR = exp(qnorm(0.75)*sqrt(2)*sqrt(rf[2]))
upper_limit = exp(qnorm(0.75)*sqrt(2)*(sqrt(rf[2]) + 1.96*rf_se))
lower_limit = exp(qnorm(0.75)*sqrt(2)*(sqrt(rf[2]) - 1.96*rf_se))

# Calculate IOR for cluster-level covariates
IOR_lower = round(exp(fixed_coef[17:25,1] + qnorm(0.25)*sqrt(2)*sqrt(rf[2])), 2)
IOR_upper = round(exp(fixed_coef[17:25,1] + qnorm(0.75)*sqrt(2)*sqrt(rf[2])), 2)
IOR = paste0(' [', IOR_lower, '; ', IOR_upper, ']')

res_all = data.frame(Est = Est_coef, Interpretation = c(Odds_ratio_fixed, IOR))
row.names(res_all) = names(fixed_coef[,1])
xtable(res_all) # This is the first two columns of Table 2

################################################################################
# Isolated CABG: Surgeon-Level Analysis
###############################################################################
# Fit a GLMM model
md_CABG_surg <- mixed_model(fixed = tee ~ age_std + sex + race + EF_std + OP + 
                              insurance_labeled + predmort + year_since_2011 + 
                              vol_surg_std + sts_region_e, 
                            random = ~ 1 | sts_surgid, 
                            data = data_CABG_final,
                            family = binomial())

# Extract fixed effects coefficients
summary_md_CABG_surg = summary(md_CABG_surg)
fixed_coef = summary_md_CABG_surg$coef_table

# Construct 95% CI
Est_coef = data.frame(Est = paste0(round(fixed_coef[,1], 3), ' (', 
                      round(fixed_coef[,1] - 1.96*fixed_coef[,2], 3), ', ', 
                      round(fixed_coef[,1] + 1.96*fixed_coef[,2], 3), ')'))

# Odds ratio for fixed effect coefficients
OR = round(exp(fixed_coef[1:16,1]), 2)
upper_limit = round(exp(fixed_coef[1:16,1] + 1.96*fixed_coef[1:16,2]), 2)
lower_limit = round(exp(fixed_coef[1:16,1] - 1.96*fixed_coef[1:16,2]), 2)
Odds_ratio_fixed = paste0(OR, ' (', lower_limit, ', ', upper_limit, ')')

# Calculate median OR
rf = confint(md_CABG_surg, parm = 'var-cov')
rf_se = (sqrt(rf[3]) - sqrt(rf[2]))/1.96
MOR = exp(qnorm(0.75)*sqrt(2)*sqrt(rf[2]))
upper_limit = exp(qnorm(0.75)*sqrt(2)*(sqrt(rf[2]) + 1.96*rf_se))
lower_limit = exp(qnorm(0.75)*sqrt(2)*(sqrt(rf[2]) - 1.96*rf_se))

# Calculate IOR for cluster-level covariates
IOR_lower = round(exp(fixed_coef[17:25,1] + qnorm(0.25)*sqrt(2)*sqrt(rf[2])), 2)
IOR_upper = round(exp(fixed_coef[17:25,1] + qnorm(0.75)*sqrt(2)*sqrt(rf[2])), 2)
IOR = paste0(' [', IOR_lower, '; ', IOR_upper, ']')

res_all_surg = data.frame(Est = Est_coef, Interpretation = c(Odds_ratio_fixed, IOR))
row.names(res_all_surg) = names(fixed_coef[,1])
xtable(res_all_surg) # This is the first two columns of eTable 1



################################################################################
# Valve: Hospital-Level Analysis
###############################################################################
# Fit a GLMM model
md_valve <- mixed_model(fixed = tee ~ age_std + sex + race + EF_std + OP + 
                          insurance_labeled + 
                          predmort + year_since_2011 + surg_cat +
                          vol_hosp_std + sts_region_e, 
                        random = ~ 1 | sts_hospid, 
                        data = data_valve_final,
                        family = binomial())

# Extract fixed effects coefficients
summary_md_valve = summary(md_valve)
fixed_coef = summary_md_valve$coef_table

# Construct 95% CI
Est_coef = data.frame(Est = paste0(round(fixed_coef[,1], 3), ' (', 
                                   round(fixed_coef[,1] - 1.96*fixed_coef[,2], 3), ', ', 
                                   round(fixed_coef[,1] + 1.96*fixed_coef[,2], 3), ')'))

# Odds ratio for fixed effect coefficients
OR = round(exp(fixed_coef[1:21,1]), 2)
upper_limit = round(exp(fixed_coef[1:21,1] + 1.96*fixed_coef[1:21,2]), 2)
lower_limit = round(exp(fixed_coef[1:21,1] - 1.96*fixed_coef[1:21,2]), 2)
Odds_ratio_fixed = paste0(OR, ' (', lower_limit, ', ', upper_limit, ')')

# Calculate median OR
rf = confint(md_valve, parm = 'var-cov')
rf_se = (sqrt(rf[3]) - sqrt(rf[2]))/1.96
MOR = exp(qnorm(0.75)*sqrt(2)*sqrt(rf[2]))
upper_limit = exp(qnorm(0.75)*sqrt(2)*(sqrt(rf[2]) + 1.96*rf_se))
lower_limit = exp(qnorm(0.75)*sqrt(2)*(sqrt(rf[2]) - 1.96*rf_se))

# Calculate IOR for cluster-level covariates
IOR_lower = round(exp(fixed_coef[22:30,1] + qnorm(0.25)*sqrt(2)*sqrt(rf[2])), 2)
IOR_upper = round(exp(fixed_coef[22:30,1] + qnorm(0.75)*sqrt(2)*sqrt(rf[2])), 2)
IOR = paste0(' [', IOR_lower, '; ', IOR_upper, ']')

res_all_valve = data.frame(Est = Est_coef, Interpretation = c(Odds_ratio_fixed, IOR))
row.names(res_all_valve) = names(fixed_coef[,1])
xtable(res_all_valve) # This is the third and fourth columns of Table 2


################################################################################
# Valve: Surgeon-Level Analysis
###############################################################################
# Fit a GLMM model
md_valve_surg <- mixed_model(fixed = tee ~ age_std + sex + race + EF_std + OP + 
                               insurance_labeled + 
                               predmort + year_since_2011 + surg_cat +
                               vol_surg_std + sts_region_e, 
                             random = ~ 1 | sts_surgid, 
                             data = data_valve_final,
                             family = binomial())

# Extract fixed effects coefficients
summary_md_valve_surg = summary(md_valve_surg)
fixed_coef = summary_md_valve_surg$coef_table

# Construct 95% CI
Est_coef = data.frame(Est = paste0(round(fixed_coef[,1], 3), ' (', 
                                   round(fixed_coef[,1] - 1.96*fixed_coef[,2], 3), ', ', 
                                   round(fixed_coef[,1] + 1.96*fixed_coef[,2], 3), ')'))

# Odds ratio for fixed effect coefficients
OR = round(exp(fixed_coef[1:21,1]), 2)
upper_limit = round(exp(fixed_coef[1:21,1] + 1.96*fixed_coef[1:21,2]), 2)
lower_limit = round(exp(fixed_coef[1:21,1] - 1.96*fixed_coef[1:21,2]), 2)
Odds_ratio_fixed = paste0(OR, ' (', lower_limit, ', ', upper_limit, ')')

# Calculate median OR
rf = confint(md_valve_surg, parm = 'var-cov')
rf_se = (sqrt(rf[3]) - sqrt(rf[2]))/1.96
MOR = exp(qnorm(0.75)*sqrt(2)*sqrt(rf[2]))
upper_limit = exp(qnorm(0.75)*sqrt(2)*(sqrt(rf[2]) + 1.96*rf_se))
lower_limit = exp(qnorm(0.75)*sqrt(2)*(sqrt(rf[2]) - 1.96*rf_se))

# Calculate IOR for cluster-level covariates
IOR_lower = round(exp(fixed_coef[22:30,1] + qnorm(0.25)*sqrt(2)*sqrt(rf[2])), 2)
IOR_upper = round(exp(fixed_coef[22:30,1] + qnorm(0.75)*sqrt(2)*sqrt(rf[2])), 2)
IOR = paste0(' [', IOR_lower, '; ', IOR_upper, ']')

res_all_valve = data.frame(Est = Est_coef, Interpretation = c(Odds_ratio_fixed, IOR))
row.names(res_all_valve) = names(fixed_coef[,1])
xtable(res_all_valve) # This is the third and fourth columns of eTable 1