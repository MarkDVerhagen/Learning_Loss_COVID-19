# ---
# GENERATE YEARLY SET OF TEST AND PUPIL INFORMATION
# ---
rm(list = ls())
library(tidyverse)
library(data.table)

# LOAD DATA
pupils <- read_rds("data/edit/pupils.rds")
tests <- read_rds("data/edit/tests.rds")
schools <- read_rds("data/raw/schools.rds")

## -- Subset pupil, test and school to relevant years

# First determine relevant pupil ids: pupils who took a test in relevant years
p_ids <- tests %>%
  filter((date_taken > as.Date("2016-08-15")) &
           (date_taken < as.Date("2020-08-15"))) %>%
  select(pupil_id)

# select all pupils who took a test within relevant years and merge school data
pupils_school <- pupils %>%
  filter(pupil_id %in% p_ids$pupil_id) %>%
  filter(!is.na(ses),
         !is.na(female)) %>%  # omit pupils with unit missingness
  left_join(schools)

# subset test data to include only tests by in-sample pupils
tests <- tests %>%
  filter(pupil_id %in% pupils_school$pupil_id)

## -- Generate set of test data including past-years performance
# (student's performance in previous school year)

# select test types (Mathematics (RW), Spelling (SP), Reading (TBL))
select_vars <- c("RW", "SP", "TBL")


relevant_tests <- c()
pupils_ability <- c()

for(year in 2016:2020) {
  print(paste0("Generating data for year: ", year))
  # only select tests taken within specified time period
  current_year_tests <- tests %>%
    filter((date_taken > as.Date(paste0(year, "-08-15"))) &
             (date_taken < as.Date(paste0(year + 1, "-08-15")))) %>%
    mutate(year = paste0(year, year+1))
  
  # unit test
  stopifnot(all(current_year_tests$pupil_id %in% p_ids$pupil_id))
  
  # calculate mean ability in previous year
  previous_year_tests <- tests %>%
    filter((date_taken > as.Date(paste0(year - 1, "-08-15"))) &
             (date_taken < as.Date(paste0(year, "-08-15")))) %>% 
    group_by(pupil_id, cito_subject) %>%
    summarise(mean_score = mean(percentile_score, na.rm = T)) %>%
    ungroup()
  
  previous_year_abilities <- previous_year_tests %>%
    pivot_wider(names_from = cito_subject, values_from = mean_score) %>%
    mutate(ability = rowMeans(dplyr::select(., all_of(select_vars)), na.rm = T)) %>%
    mutate(raw_ability = ability) %>%
    select(-RW, -DMT, -TBL, -SP) %>%
    mutate_at(vars(-pupil_id, -raw_ability),  # assign raw ability to tertiles
              function(x) ifelse(x <= quantile(x, 0.33, na.rm = T), "bottom", 
                                 ifelse(x > quantile(x, 0.33, na.rm = T) & 
                                          x <= quantile(x, 0.667, na.rm = T), "middle", 
                                        ifelse(x > quantile(x, 0.667, na.rm = T), "top", NA)))) %>%
    mutate_at(vars(-pupil_id, -raw_ability), function(x) factor(x, levels = c("top", "middle", "bottom"))) %>% 
    mutate(year = paste0(year, year+1))  
  
  pupils_ability <- rbind(pupils_ability, previous_year_abilities)
  relevant_tests <- rbind(relevant_tests, current_year_tests)
}

# Cut-off any tests taken outside of the school year (5 observations)
relevant_tests <- relevant_tests %>%
  filter(date_taken <= as.Date("2020-09-01"))

# save data
saveRDS(relevant_tests, "data/edit/tests_relevant.rds")

# calculate length between tests

DT <- relevant_tests %>%
  mutate(date_julian = julian(date_taken)) %>%
  data.table()

test_timing <- DT[ , list(date_julian = min(date_julian)),
                   by = list(pupil_id, year, cito_subject, semester, class_year)] %>%  # determine start date of test (some tests have multiple components)
  as.data.frame() %>%
  pivot_wider(names_from = semester,
              values_from = date_julian) %>%  # generate wide data with columns for the start of the Middle- and End-of-year tests
  mutate(days_between = E-M) %>%  # calculate difference between End-of-year and Middle-of-year tests
  dplyr::select(-E, -M) %>%
  pivot_wider(names_from = cito_subject, 
              values_from = days_between) %>%
  rename_at(vars(-pupil_id, -year, -class_year), function(x) tolower(paste0("days_between_", x))) %>%
  relocate(pupil_id, year) %>%
  ungroup() %>%
  mutate(days_between_all = rowMeans(dplyr::select(., c("days_between_rw", "days_between_sp", "days_between_tbl")), na.rm = T))

## -- Generate dependent variable (4x) and indicator of current grade

# uniforming of test results and swapping to data.table for efficient grouping
mean_tests_DT <- relevant_tests %>%
  group_by(cito_subject, class_year, semester) %>% 
  mutate(percentile_score = 100 * tiger::to.uniform(percentile_score)) %>%
  data.table()

# mean multiple tests into a single score per semester per pupil
mean_relevant_tests <- mean_tests_DT[ ,list(percentile_score = mean(percentile_score)),
                                      by = list(pupil_id, year, cito_subject, semester, class_year)] %>%
  as.data.frame()

saveRDS(mean_relevant_tests, "data/edit/analysis_raw_scores.rds")

# generate dependent variables by differencing End-of-year and Middle-of-year tests
dep_var <- mean_relevant_tests %>%
  pivot_wider(names_from = semester,
              values_from = percentile_score) %>%
  mutate(y = E - M) %>%
  pivot_wider(names_from = cito_subject, values_from = c(E, M, y)) %>%
  rename_at(vars(starts_with("y_")), funs(sub("y_", "", .))) %>%
  ungroup() %>%
  mutate(ALL = rowMeans(dplyr::select(., c("RW", "SP", "TBL")), na.rm = T), 
         subset_all = ifelse(!is.na(RW) & !is.na(SP) & !is.na(TBL), 1, 0))  # indicator for subset including only observations where all tests are nonmissing

## --- Generate total variable (mean of RW, SP, and TBL)
# Merge final set 
analysis_df <- left_join(dep_var, pupils_ability) %>%
  left_join(test_timing) %>%
  left_join(pupils_school) %>%
  filter(class_year > 3) %>%
  filter(!year=="20202021")

## --- Include family ids for each pupil

# generate a matrix of sibling ids per pupil
sib_df <- pupils %>%
  filter(!sibling_id=="0") %>%
  select(pupil_id, sibling_id) %>%
  unnest(sibling_id) %>% 
  group_by(pupil_id) %>% 
  mutate(col=seq_along(pupil_id)) %>%
  spread(key=col, value=sibling_id) 

# REPLACE ZEROES WITH NA
sib_df[sib_df == "0"] <- NA

# GET RID OF PUPILS WHOSE ONLY SIBLING IS THEMSELVES
sib_df <- sib_df %>%
  filter(!(pupil_id == `1` & is.na(`2`)))

# ARRANGE COLUMNS BY PUPIL_ID ROW
s <- sib_df %>%
  ungroup() %>%
  mutate_all(as.character) %>%
  as.data.frame(.)

# REARRANGE ALL SETS OF SIBLINGS IN ASCENDING ORDER SUCH THAT SIBLINGS HAVE THE SAME FAMILY ID
# E.G.: ID 1, 2, 3, 4 and 2, 3, 4, 1 are the same family
s[is.na(s)] <- 999999999

sorted_s <- t(apply(s, 1, sort)) %>%
  as.data.frame()

sorted_s[sorted_s == 999999999] <- NA

sorted_s <- sorted_s[!duplicated(sorted_s), ]

# generate a  list of all siblings per family_id
sib_df_max_size <- sorted_s %>%
  group_by(V1) %>%
  summarise(siblings = paste(unique(c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11,
                                      V12, V13, V14)),collapse=",")) %>%
  mutate(siblings = gsub(",NA", "", siblings)) %>%
  mutate(family_id = 1:dim(.)[1]) %>%
  select(-V1)

# separate and reshape data to get same family_id per related pupil
fam_df <- sib_df_max_size %>%
  separate(siblings, c(as.character(seq(1:14)))) %>%
  reshape2::melt(id.var = "family_id", value.name = "pupil_id") %>%
  select(-variable) %>%
  filter(!is.na(pupil_id))

# Assign pupils with >1 family to their first family
fam_df <- fam_df %>%
  group_by(pupil_id) %>%
  summarise(family_id = first(family_id)) %>%
  mutate(family_id = as.factor(family_id)) %>%
  ungroup()
  
analysis_df <- analysis_df %>%
  left_join(fam_df)

saveRDS(analysis_df, paste0("data/edit/analysis.rds"))
