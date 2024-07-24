# Time-trends in life expectancy of people with severe mental illness in Scotland, 2000-2019: a population-based study

This repository accompanies the paper:
> Fleetwood K, Alotaibi R, Scheuer SH, Smith D, Wild SH, Jackson CA. Time-trends in life expectancy of people with severe mental illness in Scotland, 2000-2019: a population-based study. (submitted for peer review)

It includes the following R scripts: 

**01_data.R**
   *	Reads in the SMR04 records
   *	Reads in the death records
   *	Identifies people with schizophrenia, bipolar disorder or depression, based on the SMR04 records
   *	Identifies the cohort 
   *	Creates the baseline characteristics tables

**02_calculate_lyl.R**
   *	Loads the data output from 01_data.R
   *	Loads the Scottish national lifetables
   *	Uses the R package lillies (https://cran.r-project.org/web/packages/lillies/index.html) to calculate life years lost, excess life years lost and survival curves

**03_lyl_plot.R**
   * Loads the data output from 02_calculate_lyl.R
   * Creates the life years lost plot (figure 2)

**04_excess_lyl_plot.R**
   * Loads the data output from 02_calculate_lyl.R
   * Creates the excess life years lost plot (figure 3)

**05_survival_plot.R**
   * Loads the data output from 02_calculate_lyl.R
   * Creates the survival curves plot (figure 4)
