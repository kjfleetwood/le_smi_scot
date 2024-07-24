#
# Prepare data for life expectancy analysis
#

# This script was developed within the National Safe Haven. In accordance with 
# rules for output of code from the National Safe Haven all file paths have been
# redacted. 

#
# 1. Set-up -------------------------------------------------------------------
#

library(tidyverse)
library(janitor)
library(lubridate)
library(arsenal)

study_path <- "*****"
data_path  <- file.path(study_path, "*****")
le_path   <- file.path(study_path, "*****")
shared_path <- file.path("*****")

# Source functions file
source(file.path(le_path, "*****"))

#
# 2. Load data ----------------------------------------------------------------
#

# 2.1 SMR04 (psychiatric hospital data) ---------------------------------------

# Read in SMR04 data
smr04 <- 
  read.csv(
    file.path(data_path, "*****"),
    fileEncoding = "UTF-8-BOM"
  )

# Tidy SMR04 data
# - rename columns
# - select required columns
# - put doa (date of admission) in date format
# - reformat id column
smr04 <-
  smr04 %>% 
  rename_all(tolower) %>%
  rename(
    id = "patientid",
    doa = "admission_date"
  ) %>%
  select(
    id,
    sex,
    doa,
    age_in_years,
    carstairs1981_sc_quintile,
    carstairs1991_sc_quintile,
    carstairs2001_sc_quintile,
    carstairs2011_sc_quintile,
    main_condition,
    other_condition_1,
    other_condition_2,
    other_condition_3,
    other_condition_4,
    other_condition_5
  ) %>%
  mutate(
    doa = as.Date(as.character(doa), format="%Y%m%d"),
    id = word(id, 2, 2, sep = "/")
  )

# Recode sex
# - swap 1/2 for male/female
smr04 <- recode_sex(smr04)

# Recode deprivation
# - select appropriate version of the Carstairs Index based on date of admission 
smr04 <- recode_dep(smr04)

# Create long version of SMR04 records
# - 1 row per diagnosis, instead of 1 row per person
# - exclude columns no longer required (Carstairs Index columns)
# - exclude records prior to 1/1/1981
smr04_long <-
  smr04 %>%
  select(-starts_with("carstairs")) %>%
  filter(doa >= as.Date("1981-01-01")) %>%
  gather(main_condition,
         other_condition_1,
         other_condition_2,
         other_condition_3,
         other_condition_4,
         other_condition_5,
         key = "condition",
         value = "code") %>%
  filter(!code == " ") %>%
  mutate(source = "smr04")

# 2.2 Death records -----------------------------------------------------------

# Read in death records
death <- 
  read.csv(
    file.path(data_path, "*****"),
    fileEncoding = "UTF-8-BOM"
  )

# Tidy death data
# - rename columns
# - put dod (date of death) in date format
# - reformat id column
death <-
  death %>% 
  rename_all(tolower) %>%
  rename(
    id = "patientid",
    dod = "date_of_death"
  ) %>% 
  mutate(
    dod = as.Date(as.character(dod), format="%Y%m%d"),
    id = word(id, 2, 2, sep = "/")
  )

# Recode sex
# - swap 1/2 for male/female
death <- recode_sex(death)

# Check for duplicate deaths
table(table(death$id))

# Identify IDs of duplicate deaths
dup_id <- names(table(death$id)[table(death$id) == 2])
death %>% filter(id %in% dup_id) %>% arrange(id) %>% select(id, dod, age, sex)
# Reviewed duplicate deaths: date of death is always the same, or prior to 2000

# Remove duplicate deaths
death <- death[!duplicated(death$id),]

# Rename columns
death <- 
  death %>%
  rename(
    age_death = age,
    sex_death = sex
  )

#
# 3. Identify SMI -------------------------------------------------------------
#

# Identify people with schizophrenia, bipolar disorder or depression

# Exclude diagnoses after the study period
smr04_long <- 
  smr04_long %>% 
  filter(doa <= as.Date("2019-12-31"))

# Read in code list
code_list <- read.csv(file.path(shared_path, "*****"))

# Tidy code list
# - Rename columns
# - Keep rows for schizophrenia, bipolar disorder and depression
# - Format codes as character variable
# - Correct codes in codelist that have been rounded
code_list <- 
  code_list %>% 
  rename(
    code = Code, 
    cat = MHC, 
    description = Description
  ) %>% 
  filter(
    cat %in% c("Schizophrenia", "Bipolar disorder", "Depression")
  ) %>%
  mutate(
    code = as.character(code),
    code = 
      case_when(
        code %in% "295" ~ "295.0",
        code %in% "296" ~ "296.0",
        code %in% "298" ~ "298.0",
        TRUE ~ code
      )
  )

# SMR04 data: reformat codes
# - Get first 3 digits from ICD-10 codes
# - Get first 4 digits from ICD-9 codes
smr04_long <- 
  smr04_long %>% 
  rename(old_code = code) %>%
  mutate(
    code = 
      case_when(
        substring(old_code,1,1) == "F"      ~ substring(old_code,1,3),
        substring(old_code,1,1) %in% c(2,3) ~ paste0(substring(old_code,1,3),".",substring(old_code,4,4)) 
      )
  )

# ICD-9 code 311 has been recorded as 311.9 => update
smr04_long$code[smr04_long$code=="311.9"] <- "311"

# Merge SMR04 data with codelist
# Define numeric version of severe mental illness category (cat_num)
# - Schizophrenia = 1, Bipolar disorder = 2, Depression = 3
# Group data by person ID
smr04_long <- 
  smr04_long %>% 
  left_join(select(code_list, code, cat, description)) %>%
  filter(!is.na(cat)) %>%
  mutate(
    cat_num = 
      case_when(
        cat %in% "Schizophrenia" ~ 1,
        cat %in% "Bipolar disorder" ~ 2,
        cat %in% "Depression" ~ 3
      )
  ) %>%
  group_by(id)

# For each person, identify overall SMI category using hierarchy
smi_cat <- 
  smr04_long %>%
  summarise(
    cat_num = min(cat_num)
  ) %>%
  mutate(
    cat = 
      case_when(
        cat_num %in% 1 ~ "Schizophrenia",
        cat_num %in% 2 ~ "Bipolar disorder",
        cat_num %in% 3 ~ "Depression"
      )
  )

# For each person, identify first SMI record
smi_first <- 
  smr04_long %>%
  slice(which.min(doa))

# Create main SMI dataset by merging SMI category with first SMI record
smi <- 
  smi_cat %>%
  select(-cat_num) %>%
  left_join(
    select(
      smi_first,
      id, sex, doa, age_in_years, dep_cat, dep_cat_miss
    )
  )

# Merge in date of death and cause of death
dim(smi)
smi <- 
  smi %>%
  left_join(
    select(
      death,
      id, dod, age_death, sex_death, underlying_cause_of_death
    )
  )
dim(smi)

#
# 4. Create cohort -------------------------------------------------------------
#

# 4.1 Exclude people who died before start of study ----------------------------
smi <- 
  smi %>%
  filter(is.na(smi$dod) | smi$dod >= as.Date("2000-01-01"))

# 4.2 Checks on the cohort -----------------------------------------------------

# 4.2.1 Check for people with hospital record after their death record ---------

# Identify people with a hospital record after their death record
sum(!is.na(smi$dod) & smi$doa > smi$dod)
smi %>% filter(!is.na(dod) & doa > dod)
smi_ex1 <- smi %>% filter(!is.na(dod) & doa > dod) %>% pull(id)

# 4.2.2.Check whether age at death is consistent with age at diagnosis ---------

smi <- 
  smi %>%
  mutate(
    dob_approx = doa %m-% years(age_in_years),
    dob_approx = dob_approx %m-% months(6),
    age_death_check = as.numeric(dod - dob_approx)/365.2425,
    age_death_diff = abs(age_death_check - age_death)
  )

# Summarise inconsistencies between age at diagnosis and age at death
summary(smi$age_death_diff)
sum(!is.na(smi$age_death_diff) & smi$age_death_diff > 2) 
sum(!is.na(smi$age_death_diff) & smi$age_death_diff > 3) 
sum(!is.na(smi$age_death_diff) & smi$age_death_diff > 4) 
sum(!is.na(smi$age_death_diff) & smi$age_death_diff > 5) 

# Review inconsistencies between age at diagnosis and age at death
smi %>% 
  filter(!is.na(age_death_diff) & age_death_diff > 2) %>%
  select(-dep_cat, -dep_cat_miss)

# Identify people with inconsistent age at diagnosis and age at death
smi_ex2 <- 
  smi %>% 
  filter(!is.na(age_death_diff) & age_death_diff > 2) %>%
  pull(id)

# 4.2.3. Check whether sex at death is consistent with sex at diagnosis --------

smi %>% tabyl(sex, sex_death)
smi %>% filter(sex == "Male" & sex_death == "Female")

# Identify people with inconsistent sex at death and sex at diagnosis
smi_ex3 <- 
  smi %>% 
  filter(
    (sex %in% "Male" & sex_death %in% "Female")|
    (sex %in% "Female" & sex_death %in% "Male")
  ) %>%
  pull(id)

# 4.2.4 Update cohort ----------------------------------------------------------

# Review exclusions
length(smi_ex1)
length(smi_ex2)
length(smi_ex3)

# Collate exclusions
smi_ex <- unique(c(smi_ex1, smi_ex2, smi_ex3))

# Create updated cohort
# - remove exclusions
# - remove people with missing sex
smi2 <- 
  smi %>%
  filter(
    !id %in% smi_ex,
    !is.na(sex)
  )

#
# 5. Create baseline characteristics tables ------------------------------------
#

# 5.1 Tidy data ----------------------------------------------------------------

# Define year of diagnosis as a categorical variable
# Define SMI category as a factor variable
# For each person calculate 
# - study start date, 
# - study end date, and
# - years of follow-up
# Remove unnecessary columns
smi2 <- 
  smi2 %>%
  mutate(
    year_of_diag = year(doa),
    year_of_diag_cat = 
      case_when(
        year_of_diag <= 1989 ~ "1981-1989",
        year_of_diag >= 1990 & year_of_diag <= 1999 ~ "1990-1999",
        year_of_diag >= 2000 & year_of_diag <= 2009 ~ "2000-2009",
        year_of_diag >= 2010                        ~ "2010-2019"
      ),
    cat = 
      factor(
        cat, 
        levels = c("Schizophrenia", "Bipolar disorder", "Depression")
      ),
    start = 
      case_when(
        doa < as.Date("2000-01-01") ~ as.Date("2000-01-01"),
        TRUE ~ doa
      ),
    end = 
      case_when(
        is.na(dod)                  ~ as.Date("2019-12-31"), 
        dod > as.Date("2019-12-31") ~ as.Date("2019-12-31"),
        dod <= as.Date("2019-12-31") ~ dod
      ),
    follow_up = as.numeric(end - start)/365.2425
  ) %>%
  select(-age_death_check, -age_death_diff)

# 5.2 Create baseline characteristics table by SMI -----------------------------

# Define settings for baseline characteristics table
tab1_cntrls <- 
  tableby.control(
    test = FALSE, total = FALSE,
    numeric.stats = c("medianq1q3")
  )

# Define labels for each variable
labels(smi2) <- 
  c(
    sex = 'Sex',
    age_in_years = 'Age at first SMI diagnosis',
    year_of_diag_cat = 'Year of first SMI diagnosis',
    dep_cat_miss = 'Carstairs Index quintile',
    follow_up = "Follow-up (years)"
  )

# Create baseline characteristics table 
tab1 <- 
  tableby(
    cat ~ sex + age_in_years + year_of_diag_cat + dep_cat_miss + follow_up,
    data = smi2,
    control = tab1_cntrls
  )
summary(tab1, text = TRUE, digits = 1)

# 5.3 Create baseline characteristics table by SMI and sex ---------------------

# Create variable that defines SMI and sex
# Convert this variable to a factor variable
smi2 <- 
  smi2 %>%
  mutate(
    cat_sex = paste(cat, sex),
    cat_sex = 
      factor(
        cat_sex,
        levels = 
          c(
            "Schizophrenia Male",
            "Schizophrenia Female",
            "Bipolar disorder Male",
            "Bipolar disorder Female",
            "Depression Male",
            "Depression Female"
          )
      )
  )

# Create baseline characteristics table 
tab1a <- 
  tableby(
    cat_sex ~ age_in_years + year_of_diag_cat + dep_cat_miss + follow_up,
    data = smi2,
    control = tab1_cntrls
  )
summary(tab1a, text = TRUE, digits = 1)

# 5.4 Tidy deaths data ---------------------------------------------------------

# Exclude deaths outside of the study period
smi2_sub <- 
  smi2 %>%
  filter(
    !is.na(dod) & dod <= as.Date("2019-12-31")
  )

# Label natural and unnatural causes of death
smi2_sub <- 
  smi2_sub %>%
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

# Categorise causes of death:
# - Circulatory diseases / neoplasms/ respiratory diseases /
#   mental and behavioural disorders 
# - suicide self-harm and injuries of undetermined intent / other external causes
# - other natural causes
smi2_sub <- 
  smi2_sub %>%
  mutate(
    death_cat2 =
      case_when(
        substring(underlying_cause_of_death, 1, 1) %in% "I" ~ "Circulatory diseases",
        substring(underlying_cause_of_death, 1, 1) %in% "C" ~ "Neoplasms", 
        substring(underlying_cause_of_death, 1, 3) %in% paste0("D",str_pad(00:48, 2, pad = "0")) ~ "Neoplasms",
        substring(underlying_cause_of_death, 1, 1) %in% "J" ~ "Respiratory diseases", 
        substring(underlying_cause_of_death, 1, 1) %in% "F" ~ "Mental and behavioural disorders",
        substring(underlying_cause_of_death, 1, 1) %in% c("V", "W") ~ "Other external causes",
        substring(underlying_cause_of_death, 1, 3) %in% paste0("X",str_pad(00:59, 2, pad = "0")) ~ "Other external causes",
        substring(underlying_cause_of_death, 1, 3) %in% c("Y85", "Y86") ~ "Other external causes",
        substring(underlying_cause_of_death, 1, 3) %in% paste0("X",str_pad(60:84, 2, pad = "0")) ~ "Suicide, self-harm and injuries of undetermined intent",
        substring(underlying_cause_of_death, 1, 3) %in% paste0("Y",str_pad(10:34, 2, pad = "0")) ~ "Suicide, self-harm and injuries of undetermined intent",
        underlying_cause_of_death %in% c("Y870", "Y872") ~ "Suicide, self-harm and injuries of undetermined intent",
        substring(underlying_cause_of_death, 1, 3) %in% paste0("Y",str_pad(c(00:09, 40:84, 88:89), 2, pad = "0")) ~ "Other external causes",
        substring(underlying_cause_of_death, 1, 3) %in% paste0("X",85:99) ~ "Other external causes",
        underlying_cause_of_death %in% "Y871" ~ "Other external causes",
        TRUE ~ "Other natural causes"
      ),
    death_cat2 = 
      factor(
        death_cat2,
        levels = c("Circulatory diseases", "Neoplasms", "Respiratory diseases", "Mental and behavioural disorders", "Other natural causes",
                  "Suicide, self-harm and injuries of undetermined intent", "Other external causes")
      )
  )

# 5.5 Create characteristics table for deaths by SMI ---------------------------

# Define labels for each variable
labels(smi2_sub) <- 
  c(
    sex_death = 'Sex',
    age_death = 'Age at death',
    death_cat = 'Cause of death (natural/unnatural)',
    death_cat2 = 'Cause of death (main causes)'
  )


# Create baseline characteristics table
tab2 <- 
  tableby(
    cat ~ sex_death + age_death + death_cat + death_cat2,
    data = smi2_sub,
    control = tab1_cntrls
  )
summary(tab2, text = TRUE, digits = 1)

# 5.5 Create characteristics table for deaths by SMI and sex -------------------

tab2a <- 
  tableby(
    cat_sex ~ age_death + death_cat + death_cat2,
    data = smi2_sub,
    control = tab1_cntrls
  )
summary(tab2a, text = TRUE, digits = 1)

#
# 6. Output results ------------------------------------------------------------
#

# Output baseline characteristics tables

saveRDS(tab1, file = file.path(le_path, "*****"))
saveRDS(tab2, file = file.path(le_path, "*****"))

saveRDS(tab1a, file = file.path(le_path, "*****"))
saveRDS(tab2a, file = file.path(le_path, "*****"))

# Output cohort
saveRDS(smi2, file = file.path(le_path, "*****"))

