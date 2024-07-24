#
# Calculate life years lost for the SMI cohort, and compare to the Scottish population
#

# This script was developed within the National Safe Haven. In accordance with 
# rules for output of code from the National Safe Haven all file paths have been
# redacted. 

#
# 1. Set-up -------------------------------------------------------------------
#

library(tidyverse)
library(janitor)
library(lillies)
library(readxl)
library(lubridate)

study_path <- "*****"
data_path  <- file.path(study_path, "*****")
le_path   <- file.path(study_path, "*****")

output_path <- file.path(le_path, "*****")
checkplot_path <- file.path(output_path, "*****")
bs_path <- file.path(output_path, "*****")
sp_path <- file.path(output_path, "*****")
sp_data_path <- file.path(output_path, "*****")

#
# 2. Load data ----------------------------------------------------------------
#

# 2.1 Load SMI cohort data ----------------------------------------------------

smi_cohort <- readRDS(file.path(le_path, "*****"))

# Define cause of death category
#   - Unnatural:
#     - Suicides: ICD-10: X60-X84, Y87.0
#     - Accidents, including homicides: ICD-10: V01-X59, X85-Y09, Y10-Y86, Y87.1, Y87.2, Y88, Y89 
#   - Natural
#     - Everything else:

smi_cohort <- 
  smi_cohort %>%
  mutate(
    death_cat = 
      case_when(
        underlying_cause_of_death %in% c(paste0("X",60:84), "Y870") ~ "Unnatural",
        substring(underlying_cause_of_death, 1, 1) %in% c("V","W") ~ "Unnatural",
        substring(underlying_cause_of_death, 1, 3) %in% paste0("X",str_pad(c(00:59, 85:99), 2, pad = "0")) ~ "Unnatural",
        substring(underlying_cause_of_death, 1, 3) %in% paste0("Y",str_pad(c(00:86, 88, 89), 2, pad = "0")) ~ "Unnatural",
        underlying_cause_of_death %in% c("Y871", "Y872") ~ "Unnatural",
        !is.na(underlying_cause_of_death) ~ "Natural",
        TRUE ~ "Alive"
      )
  )

# 2.2 Load national lifetables for Scotland -----------------------------------

# Study period is 2000 to 2019 so load life tables within this range

# Define names of of life tables to load
lt_years <- paste0(2000:2017, "-", str_pad(2:19, 2, pad = "0"))

# Define empty list to hold life tables
life_tables <- vector(mode = 'list', length = length(lt_years))
# Add names to the empty list
names(life_tables) <- lt_years

# Path to lifetables
lt_path <- file.path(le_path, "*****")

# Loop through each three year period and read in the appropriate lifetable
for (i in 1:length(lt_years)) {
  
  life_tables[[i]] <-
    read_excel(
      lt_path,
      sheet = lt_years[i],
      skip = 7,
      col_names =
        c(
          "age", "male_mx", "male_qx", "male_lx", "male_dx", "male_ex", 
          "blank", "female_mx", "female_qx", "female_lx", "female_dx", "female_ex"
        ) 
    ) %>%
    select(-"blank") %>%
    filter(!is.na(male_mx)) %>%
    mutate(age = as.numeric(age))
  
}


#
# 3. Calculate LYLs -----------------------------------------------------------
#

# 3.1 Prepare data ------------------------------------------------------------

# Age in years must be numeric, not an integer

smi_cohort <- 
  smi_cohort %>%
  mutate(
    age_in_years = as.numeric(age_in_years)
  )


# 3.2 Calculate LYLs for each SMI, sex and period -----------------------------

# Define SMIs
smis <- c("Schizophrenia", "Bipolar disorder" , "Depression")

# Define sex
sexes <- c("Male", "Female")

# Define periods
lt_years_start <- 2000:2017

# Set up empty data frame to hold results
rep_na <- rep(NA, 108)

lyl_tab <- 
  data.frame(
    smi = rep(c("Schizophrenia", "Bipolar disorder", "Depression"), each = 36),
    sex = rep(rep(c("Male", "Female"), each = 18), 3),
    period = rep(lt_years, 6),
    #
    smi_rle = rep_na,
    smi_rle_lower = rep_na,
    smi_rle_upper = rep_na,
    #
    smi_lyl_95 = rep_na,
    smi_lyl_95_lower = rep_na,
    smi_lyl_95_upper = rep_na,
    #
    smi_lyl_nat_95 = rep_na,
    smi_lyl_nat_95_lower = rep_na,
    smi_lyl_nat_95_upper = rep_na,
    #
    smi_lyl_unn_95 = rep_na,
    smi_lyl_unn_95_lower = rep_na,
    smi_lyl_unn_95_upper = rep_na,
    #
    smi_lyl_pop = rep_na,
    smi_lyl_pop_lower = rep_na,
    smi_lyl_pop_upper = rep_na
  )  

# For each combination of SMI, sex and period, calculate life years lost

for (smi_i in smis) {
  
  for(sex_i in sexes){
    
    for(lt_i in lt_years_start) {
      
      # Define key dates
      start_fu <- as.Date(paste(lt_i, "01", "01", sep = "-"))
      end_fu <- as.Date(paste(lt_i + 2, "12", "31", sep = "-"))
      
      # Define SMI cohort subset
      # - Filter by SMI and sex
      # - Exclude people who died before the start of the period
      # - Exclude people diagnosed after the start of the period
      # Define key variables
      # - age_start_fu: age at the start of the period
      # - age_start_i: age at the start of follow-up
      # - date_start_i: date at the start of follow-up
      # - date_end_i: date at the end of follow-up
      # - age_incr_i: age increment during follow-up
      # - age_end_i: age at the end of follow-up
      # - death_i: indicator variable for death by end of period
      # - death_cat_i: cause of death: natural or unnatural if death by end of
      #                of period, otherwise alive
      smi_cohort_i <- 
        smi_cohort %>%
        filter(
          cat %in% smi_i,
          sex %in% sex_i,
          is.na(dod) | dod >= start_fu,
          doa <= end_fu
        ) %>%
        mutate(
          age_start_fu = as.numeric(start_fu - dob_approx)/365.2425,
          age_start_i =
            case_when(
              doa < start_fu ~ age_start_fu,
              TRUE ~ age_in_years + 0.5
            ),
          date_start_i = 
            case_when(
              doa < start_fu ~ start_fu,
              TRUE ~ doa
            ),
          date_end_i = 
            case_when(
              dod < end_fu ~ dod,
              TRUE ~ end_fu
            ),
          age_incr_i = 
            case_when(
              date_end_i > date_start_i ~ as.numeric(date_end_i - date_start_i)/365.2425,
              date_end_i == date_start_i ~ 0.5/365.2425
            ),
          age_end_i = age_start_i + age_incr_i,
          death_i = !is.na(dod) & dod <= end_fu,
          death_cat_i = 
            case_when(
              death_i ~ death_cat,
              TRUE ~ "Alive"
            ),
          death_cat_i = as.factor(death_cat_i)
        )
      
      # Calculate life years lost for people with SMI
      range_i <- 
        lyl_range(
          data = as.data.frame(smi_cohort_i), 
          t0 = age_start_i,
          t = age_end_i,
          status = death_cat_i, 
          age_begin = 18,
          age_end = 94, 
          tau = 95
        )
      
      # Create numbers at risk plot and save to PDF
      pdf(
        file = 
          file.path(
            checkplot_path, 
            paste0(smi_i, "_", sex_i, "_", lt_i, "-", str_pad(lt_i + 2 - 2000, 2, pad = "0"), ".pdf")
          )
      )
      print(lyl_checkplot(range_i))
      dev.off()
      
      # Calculate confidence intervals using bootstrap
      # - Note this step takes a long time
      range_i_ci <- lyl_ci(range_i, niter = 500)
      
      # Calculate age at diagnosis weights based on the whole cohort
      weights_i <- 
        smi_cohort %>%
        filter(
          cat %in% smi_i,
          sex %in% sex_i,
          doa >= as.Date("2000-01-01"),
          doa <= as.Date("2019-12-31")
        ) %>%
        pull(age_in_years) + 0.5
      
      weights_i <- weights_i[weights_i <= 94]
      
      # Create plot to check convergence of bootstrap and save to PDF
      pdf(
        file.path(
          bs_path,
          paste0(smi_i, "_", sex_i, "_", lt_i, "-", str_pad(lt_i + 2 - 2000, 2, pad = "0"), ".pdf")
        ),
        width = 15
      )
      print(plot(range_i_ci, weights = weights_i))
      dev.off()
      
      # Summarise results of bootstrap      
      range_i_sum <- 
        summary(
          range_i_ci, 
          weights = weights_i
        )
      
      # Add results to output data frame
      # - Identify appropriate row of output data frame
      lyl_tab_ndx <- c(1:108)[lyl_tab$smi == smi_i & lyl_tab$sex == sex_i & lyl_tab$period == paste0(lt_i, "-", str_pad(lt_i + 2 - 2000, 2, pad = "0"))]
      
      # Output remaining life expectancy and 95% CI 
      lyl_tab$smi_rle[lyl_tab_ndx] <- range_i_sum$lyl_estimate$life_exp
      lyl_tab$smi_rle_lower[lyl_tab_ndx] <- range_i_sum$lyl_ci_left$life_exp
      lyl_tab$smi_rle_upper[lyl_tab_ndx] <- range_i_sum$lyl_ci_right$life_exp
      
      # Output total life years lost up to age 95 and 95% CI
      lyl_tab$smi_lyl_95[lyl_tab_ndx] <- range_i_sum$lyl_estimate$TotalLYL
      lyl_tab$smi_lyl_95_lower[lyl_tab_ndx] <- range_i_sum$lyl_ci_left$TotalLYL
      lyl_tab$smi_lyl_95_upper[lyl_tab_ndx] <- range_i_sum$lyl_ci_right$TotalLYL
      
      # Output life years lost due to natural causes up to age 95 and 95% CI
      lyl_tab$smi_lyl_nat_95[lyl_tab_ndx] <- range_i_sum$lyl_estimate$Natural
      lyl_tab$smi_lyl_nat_95_lower[lyl_tab_ndx] <- range_i_sum$lyl_ci_left$Natural
      lyl_tab$smi_lyl_nat_95_upper[lyl_tab_ndx] <- range_i_sum$lyl_ci_right$Natural
      
      # Output life years lost due to unnatural causes up to age 95 and 95% CI
      lyl_tab$smi_lyl_unn_95[lyl_tab_ndx] <- range_i_sum$lyl_estimate$Unnatural
      lyl_tab$smi_lyl_unn_95_lower[lyl_tab_ndx] <- range_i_sum$lyl_ci_left$Unnatural
      lyl_tab$smi_lyl_unn_95_upper[lyl_tab_ndx] <- range_i_sum$lyl_ci_right$Unnatural
      
      # Identify life table for period from list of life tables
      pop_i <- 
        life_tables[[paste0(lt_i, "-", str_pad(lt_i + 2 - 2000, 2, pad = "0"))]]
      
      # Get correct column of lifetable for males
      if (sex_i %in% "Male"){
        pop_i <- pop_i %>% select(age, male_qx) %>% rename(qx = male_qx)
      }
      
      # Get correct column of lifetable for females
      if (sex_i %in% "Female"){
        pop_i <- pop_i %>% select(age, female_qx) %>% rename(qx = female_qx)
      }
      
      # Compare people with SMI to Scottish population life table
      pop_range_i <- 
        lyl_diff_ref(
          range_i_ci, 
          data_ref = pop_i, 
          age = age, 
          rates = qx,
          weights = weights_i
        )
      
      # Output excess life years lost relative to the Scottish population    
      lyl_tab$smi_lyl_pop[lyl_tab_ndx] <- pop_range_i$lyl_estimate$TotalLYL
      lyl_tab$smi_lyl_pop_lower[lyl_tab_ndx] <- pop_range_i$lyl_ci_left$TotalLYL
      lyl_tab$smi_lyl_pop_upper[lyl_tab_ndx] <- pop_range_i$lyl_ci_right$TotalLYL
      
      # Calculate survival curves from age 18
      age_18 <- 
        lyl(
          data = as.data.frame(smi_cohort_i),
          t0 = age_start_i,
          t = age_end_i,
          status = death_cat_i, 
          age_specific = 18,
          tau = 95
        )
      
      # Extract survival curve from age 18 and label with SMI, sex, period and start age (18)
      tmp_18 <- 
        age_18$data_plot %>%
        mutate(
          smi = smi_i,
          sex = sex_i,
          period = paste0(lt_i, "-", str_pad(lt_i + 2 - 2000, 2, pad = "0")),
          age = 18
        )
      
      # Calculate survival curves from age 35
      age_35 <- 
        lyl(
          data = as.data.frame(smi_cohort_i),
          t0 = age_start_i,
          t = age_end_i,
          status = death_cat_i, 
          age_specific = 35,
          tau = 95
        )
      
      # Extract survival curve from age 35 and label with SMI, sex, period and start age (35)
      tmp_35 <- 
        age_35$data_plot %>%
        mutate(
          smi = smi_i,
          sex = sex_i,
          period = paste0(lt_i, "-", str_pad(lt_i + 2 - 2000, 2, pad = "0")),
          age = 35
        )
      
      # Save survival curves  
      saveRDS(
        bind_rows(tmp_18, tmp_35), 
        file = file.path(sp_data_path, paste0(smi_i, "_", sex_i, "_", lt_i, "-", str_pad(lt_i + 2 - 2000, 2, pad = "0"), ".rds"))
      )
      
    }
    
  }
  
}

# Output results
write.csv(lyl_tab, file = file.path(output_path, "*****"))
  