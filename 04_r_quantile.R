# ----
# EFFECT BY QUANTILE OF INITIAL ABILITY
# ---
## (RAW_ABILITY NOW IN MAIN DF)
library(tidyverse)
library(texreg)
library(estimatr)

theme_set(theme_bw())

school_FE <- FALSE

# SPECIFY TREATMENT AND CONTROL PERIODS
control_period <- c("20162017", "20172018", "20182019")
treatment_period <- "20192020"

#SPECIFY ROBUST PATH FOR TABLES AND PLOTS
tables_path <- "tables/supp/r_quantile/"
plots_path <- "plots/supp/r_quantile/"
models_path <- "data/final/supp/r_quantile/"

# SPECIFY MODELS 
fun <- "ALL ~ 1 + treat + year_s + days_between_all_s"

# SPECIFY LABELS
var_names <- list("treat" = "Treatment", 
                  "treat:female" = "Treat x Female", 
                  "treat:seslow" = "Treat x Par. Educ. (low)", 
                  "treat:seslowest" = "Treat x Par. Educ. (lowest)",
                  "treat:seslow:abilitymiddle" = "Treat x Par. Educ. (low) x Prior Perf. (middle)",
                  "treat:seslowest:abilitymiddle" = "Treat x Par. Educ. (lowest) x Prior Perf. (middle)",
                  "treat:seslow:abilitybottom" = "Treat x Par. Educ. (low) x Prior Perf. (bottom)",
                  "treat:seslowest:abilitybottom" = "Treat x Par. Educ. (lowest) x Prior Perf. (bottom)",
                  "treat:seslow:female" = "Treat x Par. Educ. (low) x Female",
                  "treat:seslowest:female" = "Treat x Par. Educ. (lowest) x Female",
                  "treat:abilitymiddle" = "Treat x Prior Perf. (middle)",
                  "treat:abilitybottom" = "Treat x Prior Perf. (bottom)",
                  "female" = "Female", 
                  "seslow" = "Parental Educ. (low)", "seslowest" = "Parental Educ. (lowest)", 
                  "abilitymiddle" = "Prior Perf. (middle)", "abilitybottom" = "Prior Perf. (bottom)", 
                  "year_s" = "Year (std.)", 
                  "days_between_all_s" = "Days between tests (std.)", 
                  "as.factor(class_year)4" = "Age 8", 
                  "as.factor(class_year)5" = "Age 9", 
                  "as.factor(class_year)6" = "Age 10",
                  "as.factor(class_year)7" = "Age 11",
                  "(Intercept)" = "(Intercept)")


# LOAD DATA
total_df <- readRDS(paste0("data/edit/analysis.rds")) %>%
  mutate(treat = ifelse(year==treatment_period, 1, 
                        ifelse(year %in% control_period, 0, NA))) %>%
  mutate(year_s = scale(as.numeric(as.character(substr(year, 5, 8)))),
         days_between_all_s = scale(days_between_all)) %>%
  mutate(school_id = factor(school_id))


# LIMIT DATA TO NON-MISSING ON SES, ABILITY, AND GENDER
total_df <- total_df %>%
  filter(!is.na(female) & !is.na(ses) & !is.na(ability) & !is.na(days_between_all_s))


# GENERATE SUBSET DATASET BY PRIOR ABILITY QUINTILE

data_subset <- list()

cutoffs <- c(quantile(total_df$raw_ability, c(0, 0.2, 0.4, 0.6, 0.8)), 100)
num <- 1
for (i in 1 : 5) {
  data_subset[[num]] <- total_df %>%
    filter(raw_ability > cutoffs[i],
           raw_ability <= cutoffs[i + 1])
  num <- num + 1
}

# CLIP BOTTOM AND TOP PERFORMERS FROM DATASET

data_clipped <- list(total_df %>% filter(abs(100 - raw_ability) >= 5),  # CLIP BOTTOM AND TOP 5 PERFORMERS
                     total_df %>% filter(abs(100 - raw_ability) >= 10),  # CLIP BOTTOM AND TOP 10 PERFORMERS
                     total_df %>% filter(abs(100 - raw_ability) >= 15))  # CLIP BOTTOM AND TOP 15 PERFORMERS


fit_subset <-lapply(data_subset,
                    FUN = function(x) lm_robust(formula(fun), clusters = school_id,
                                                se_type = "stata", data = x))

fit_clipped <-lapply(data_clipped,
                     FUN = function(x) lm_robust(formula(fun), clusters = school_id,
                                                 se_type = "stata", data = x))

names(fit_subset) <- c("Bottom 20\\%", "20\\%-40\\%", "40\\%-60\\%", "60\\%-80\\%", "Top 20\\%")
names(fit_clipped) <- c("5\\%-95\\%", "10\\%-90\\%", "15\\%-85\\%")

# SAVE OUTPUT
screenreg(fit_subset, include.ci = F)
screenreg(fit_clipped, include.ci = F)

texreg(fit_subset,
       caption = "Overall learning loss, subsetting by prior ability quintiles", 
       center = TRUE,
       include.ci = FALSE,
       file = paste0(tables_path, "ll_ability_quintile_subsets", 
                     paste(control_period, collapse = ""), "_",
                     treatment_period, ".tex"))

texreg(fit_clipped,
       caption = "Overall learning loss, clipping top and bottom ability", 
       center = TRUE,
       include.ci = FALSE,
       file = paste0(tables_path, "ll_ability_quintile_clipped", 
                     paste(control_period, collapse = ""), "_",
                     treatment_period, ".tex"))

# SAVE MODELS
full_models_subset <- c()
full_models_clipped <- c()


for(i in names(fit_clipped)) {
  tidy_model <- tidy(fit_clipped[[i]]) %>% 
    mutate(model = i, 
           nobs = fit_clipped[[i]]$nobs,
           nclusters = fit_clipped[[i]]$nclusters)
  
  full_models_clipped <- rbind(full_models_clipped, tidy_model)
}

for(i in names(fit_subset)) {
  tidy_model <- tidy(fit_subset[[i]]) %>% 
    mutate(model = i, 
           nobs = fit_subset[[i]]$nobs,
           nclusters = fit_subset[[i]]$nclusters)
  
  full_models_subset <- rbind(full_models_subset, tidy_model)
}

# SAVE MODELS
saveRDS(full_models_clipped, 
        file= paste0(models_path, "models_overall_clipped_",
                     paste(control_period, collapse = ""), "_",
                     treatment_period, ".rds"))

saveRDS(full_models_subset, 
        file= paste0(models_path, "models_overall_subset_",
                     paste(control_period, collapse = ""), "_",
                     treatment_period, ".rds"))
