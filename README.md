# This folder contains the Stata and R code necessary to reproduce results in the article
#### Please note: the data required for this analysis is owned by the Society of Thoracic Surgeons and is not publicly available

## File: "0.STS_data_cleaning_git.do" contains the STATA code the loads the original dataset (converted from a .csv to a .dta file), cleans the data, and defines covariates. 

## File: "1.process_data.R" prepares data for statistical analyses. This file outputs two .csv files corresponding to the isolated CABG and valve cohorts. These two .csv files are the inputs for all statistical analyses.

## File: "2. make_tb1.R" outputs the Table 1 in the main article.. 

## File: "3.plot_preference" conducts all exploratory data analyses including:
#### 1. Plotting the trend
#### 2. Testing the trend
#### 3. Plotting the hospital-level and surgeon-level preference 
#### 4. Testing unimodality of preference

## File: "4.model_fitting.R" fits the generalized mixed-effects model. This file outputs the Table 2 in the main article and eTable 1 in the eSupplement. 

##### Please email any questions to: mackay.ej@gmail.com;  emily.mackay@pennmedicine.upenn.edu; or Bzhang3@fredhutch.org. 
