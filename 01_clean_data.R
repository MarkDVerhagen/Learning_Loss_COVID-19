# ---
# CLEANING DATA AND SPECIFYING SAMPLE
# ---

# LOAD LIBRARIES
library(tidyverse, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)

# LOAD THE DATA
tests_raw <- readRDS("data/raw/cito4.Rds") %>% 
  select(-X)

pupils_raw <- readRDS("data/raw/pupil4.Rds") %>% 
  select(-X)


## CLEAN PUPIL DATA ------------------------------------------------------------

# generate pupil-level independent variables
pupils <- pupils_raw %>%
  filter(!is.na(pupil_id)) %>%  # drop na pupils
  mutate(pupil_id = as.character(pupil_id),
         school_id = as.character(school_id)) %>%
  group_by(pupil_id) %>%
  summarise(pupil_month_birth = first(pupil_month_birth),
            pupil_gender = first(pupil_gender),
            pupil_inspection_weight = first(pupil_inspection_weight),
            cohort = paste0(unique(cohort), collapse = ","),
            sibling_id = list(unique(sibling_id)),
            school_id = paste0(unique(school_id), collapse = ",")) %>%
  mutate(female = ifelse(pupil_gender=="V", 1,
                         ifelse(pupil_gender=="M", 0, NA)),
         ses = ifelse(pupil_inspection_weight==0, "high",
                      ifelse(pupil_inspection_weight==0.3, "low",
                             ifelse(pupil_inspection_weight==1.2, "lowest", NA))),
         ses = factor(ses, levels = c("high" , "low", "lowest")),
         month_birth = as_date(pupil_month_birth)) %>%
  select(pupil_id, school_id, month_birth, female, ses, sibling_id, cohort)


## Subset data
# 1. ONLY SELECT THOSE STUDENTS WHO HAVENT CHANGED SCHOOLS AND REMAINED WITHIN
# THE SAME COHORT (OMIT PUPILS WHO CHANGE GRADES DURING THE YEAR)
# 2. OMIT PUPILS GRADE 3 (2024 COHORT)

pupils_final <- pupils %>%
  filter(!str_detect(school_id, ",")) %>% 
  filter(!str_detect(cohort, ",")) %>%
  filter(cohort != "2024")

## CLEAN TEST DATA -------------------------------------------------------------

# select variables of interest
tests <- tests_raw %>%
  mutate(pupil_id = as.character(pupil_id),
         class_id = as.character(class_id),
         month = substr(date_taken, 6, 7),  # extract month from testing date
         date_taken = as.Date(date_taken),
         other_test = ifelse(paste0(semester, class_year) !=
                               cito_taken_test, 1, 0)) %>%  # only use tests taken at appropriate time
  select(pupil_id, class_id, date_taken, class_year, semester, school_year,
         month, absolute_score, percentile_score, cito_taken_test, cito_subject,
         test_scale_code, test_series)

# subset data to include tests within the relevant timeframe
tests_final <- tests %>%
  filter(cito_taken_test != "",  # drop undefined tests
         semester != "B",  # only use mid- and end-of-year tests
         !(class_year %in% c(1, 2, 8)),  # omit grades with limited testing (Kindergarten, final grade)
         ((semester == "M") & (month %in% c("12", "01", "02"))) |  # only use tests taken at appropriate time
           ((semester == "E") & (month %in% c("05", "06", "07"))))

# SAVE DATA -----------------------------------------------------------------------------------
write_rds(pupils_final, "data/edit/pupils.rds")
write_rds(tests_final, "data/edit/tests.rds")
