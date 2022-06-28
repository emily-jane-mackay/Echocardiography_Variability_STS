#############################################################################
# This file conducts all exploratory data analyses, including 
# 1. Plotting the trend
# 2. Testing the trend
# 3. Plotting the hospital-level and surgeon-level preference 
# 4. Testing unimodality of preference
############################################################################
# load packages
library(data.table)
library(ggplot2)
library(dplyr)

#############################################################################
# Trend: eFigure 1 in the supplement
#############################################################################
data_final_CABG = data.table::fread('./data_CABG_analysis.csv')
data_final_valve = data.table::fread('./data_valve_analysis.csv')
data_final_CABG_tee_count_year = data_final_CABG %>%
  group_by(surgyear) %>%
  summarize(y = sum(tee))

data_final_CABG_count_year = data_final_CABG %>%
  group_by(surgyear) %>%
  count()

data_final_valve_tee_count_year = data_final_valve %>%
  group_by(surgyear) %>%
  summarize(y = sum(tee))

data_final_valve_count_year = data_final_valve %>%
  group_by(surgyear) %>%
  count()

year_df_CABG = merge(data_final_CABG_count_year, 
                     data_final_CABG_tee_count_year, 
                     by = 'surgyear')
year_df_CABG = year_df_CABG %>%
  mutate(prop = y/n)

year_df_valve = merge(data_final_valve_count_year, 
                      data_final_valve_tee_count_year, 
                      by = 'surgyear')
year_df_valve = year_df_valve %>%
  mutate(prop = y/n)

year_df = rbind(year_df_CABG, year_df_valve)
year_df$surg_type = rep(c('Isolated CABG', 'Valve'), each = 9)


# Plot the trend together
ggplot(year_df, aes(x = surgyear, y = prop, linetype = surg_type)) +
        geom_line(size = 1.5, aes(linetype = surg_type)) + 
        geom_point(aes(shape = surg_type), size = 3, stroke = 3) +
        theme_bw(base_size = 24) +
        scale_linetype_manual(values=c("solid", "dashed"))+
        scale_shape_manual(values=c(21,17))+
        scale_x_continuous(name ="Year", breaks=seq(2011,2019,1)) + 
        ylab('Proportion of surgery with TEE') +
        theme(legend.position = 'top', legend.title = element_blank(),
              legend.key.width = unit(5, "line"),
              legend.spacing.x = unit(1.0, 'cm'))


################################################################################
# Test for trend
###############################################################################
library(Kendall)
MannKendall(x = year_df$prop[year_df$surg_type == 'Isolated CABG'])
MannKendall(x = year_df$prop[year_df$surg_type == 'Valve'])

###############################################################################
###############################################################################
# Preference distribution
###############################################################################
###############################################################################
# Isolated CABG all years by hospital: eFigure 2
###############################################################################
data_final_tee_count = data_final_CABG %>%
  group_by(sts_hospid) %>%
  summarize(y = sum(tee))

data_final_CABG_count = data_final_CABG %>%
  group_by(sts_hospid) %>%
  count()

icc_df_CABG_h = merge(data_final_CABG_count, data_final_tee_count, 
                      by = 'sts_hospid')
icc_df_CABG_h$prop = icc_df_CABG_h$y/icc_df_CABG_h$n

ggplot(icc_df_CABG_h, aes(x = prop)) + 
        geom_histogram(col = 'black', fill = 'white', size = 1.2) + 
        theme_bw(base_size = 24) + ylim(c(0, 250)) +
        xlab('Proportion of isolated CABG surgery with intraoperative TEE') +
        ylab('Number of hospitals')

###############################################################################
# Isolated CABG all years by surgeon: eFigure 3
###############################################################################
data_final_tee_count = data_final_CABG %>%
  group_by(sts_surgid) %>%
  summarize(y = sum(tee))

data_final_CABG_count = data_final_CABG %>%
  group_by(sts_surgid) %>%
  count()

icc_df_CABG_s = merge(data_final_CABG_count, data_final_tee_count, 
                      by = 'sts_surgid')
icc_df_CABG_s$prop = icc_df_CABG_s$y/icc_df_CABG_s$n

ggplot(icc_df_CABG_s, aes(x = prop)) + 
        geom_histogram(col = 'black', fill = 'white', size = 1.2) + 
        theme_bw(base_size = 24) + ylim(c(0, 600)) +
        xlab('Proportion of isolated CABG surgery with intraoperative TEE') +
        ylab('Number of surgeons')

###############################################################################
# Valve all years by hospital: eFigure 4
###############################################################################
data_final_tee_count = data_final_valve %>%
  group_by(sts_hospid) %>%
  summarize(y = sum(tee))

data_final_valve_count = data_final_valve %>%
  group_by(sts_hospid) %>%
  count()

icc_df_valve_h = merge(data_final_tee_count, data_final_valve_count, by = 'sts_hospid')
icc_df_valve_h$prop = icc_df_valve_h$y/icc_df_valve_h$n

ggplot(icc_df_valve_h, aes(x = prop)) + 
        geom_histogram(col = 'black', fill = 'white', size = 1.2) + 
        theme_bw(base_size = 24) + ylim(c(0, 250)) +
        xlab('Proportion of valve surgery with intraoperative TEE') +
        ylab('Number of hospitals')

###############################################################################
# Valve all years by surgeon: eFigure 5
###############################################################################
data_final_tee_count = data_final_valve %>%
  group_by(sts_surgid) %>%
  summarize(y = sum(tee))

data_final_valve_count = data_final_valve %>%
  group_by(sts_surgid) %>%
  count()

icc_df_valve_s = merge(data_final_tee_count, data_final_valve_count, by = 'sts_surgid')
icc_df_valve_s$prop = icc_df_valve_s$y/icc_df_valve_s$n

ggplot(icc_df_valve_s, aes(x = prop)) + 
        geom_histogram(col = 'black', fill = 'white', size = 1.2) + 
        theme_bw(base_size = 24) + ylim(c(0, 600)) +
        xlab('Proportion of valve surgery with intraoperative TEE') +
        ylab('Number of surgeons')

###############################################################################
# CABG and valve all year by hospital together: Figure 1
###############################################################################
icc_df_together_h = cbind(rbind(icc_df_CABG_h, icc_df_valve_h), 
                        Type = c(rep('Isolated CABG', dim(icc_df_CABG_h)[1]),
                                 rep('Valve', dim(icc_df_valve_h)[1])))

ggplot(icc_df_together_h, aes(x = prop, fill = Type)) + 
        geom_histogram(col = 'black', size = 1, alpha = 0.2, position = 'identity') + 
        theme_bw(base_size = 24) + ylim(c(0, 250)) +
        theme(legend.position = 'top') +
        scale_fill_manual('Surgery Type', values = c('blue', 'green')) +
        xlab('Proportion of cardiac surgery with intraoperative TEE') +
        ylab('Number of hospitals')


###############################################################################
# CABG and valve all year by surgeon together: Figure 2
###############################################################################
icc_df_together_s = cbind(rbind(icc_df_CABG_s, icc_df_valve_s), 
                          Type = c(rep('Isolated CABG', dim(icc_df_CABG_s)[1]),
                                   rep('Valve', dim(icc_df_valve_s)[1])))

ggplot(icc_df_together_s, aes(x = prop, fill = Type)) + 
        geom_histogram(col = 'black', size = 1, alpha = 0.2, position = 'identity') + 
        theme_bw(base_size = 24) + ylim(c(0, 600)) +
        theme(legend.position = 'top') +
        scale_fill_manual('Surgery Type', values = c('blue', 'green')) +
        xlab('Proportion of cardiac surgery with intraoperative TEE') +
        ylab('Number of surgeons')


################################################################################
# Test for multimodality
################################################################################
library(diptest)
dip.test(icc_df_CABG_h$prop)
dip.test(icc_df_valve_h$prop)
