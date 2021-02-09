# ---
# ANALYSIS OF DATA AND RESULTS
# ---

library(tidyverse)
library(broom)
library(estimatr)
library(texreg)

theme_set(theme_bw())

# SPECIFY TREATMENT AND CONTROL PERIODS
control_period <- c("20162017", "20172018", "20182019")
treatment_period <- "20192020"

#SPECIFY PATHS
tables_path <- "tables/supp/r_three_way_int/"
plots_path <- "plots/supp/r_three_way_int/"
models_path <- "data/final/supp/r_three_way_int/"

#SPECIFY INTERACTION
int_var <- c("female", "ses", "ability", "class_year")

# SPECIFY MODELS 
fun <- " ~ 1 + treat + year_s + days_between_all_s"

# OVERALL DEPENDENT VARIABLES
dvs <- c("ALL", "RW", "TBL", "SP", "DMT")

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



# THREE-WAY INTERACTION -----------------------------------------------------------------------

# Interacting variable
for(var in int_var[!int_var %in% "ses"]) {
  
  # SPECIFY MODELS 
    fun <- paste0(" ~ 1 + treat * ses *", var, " + year_s + days_between_all_s")

  # RUN MODEL
    fit <- lapply(dvs,
                  FUN = function(x) lm_robust(formula(paste(x, fun)), 
                                              clusters = school_id, se_type = "stata", 
                                              data = total_df))
  
  names(fit) <- dvs
  
  # SAVE OUTPUT
  screenreg(fit, include.ci = FALSE, 
            custom.coef.map = var_names)
  
  
  texreg(list(fit$ALL, fit$RW, fit$TBL, fit$SP), 
         caption = paste0("Learning loss by SES and ", var), 
         custom.model.names = c("Composite", "Maths", "Reading", "Spelling"),
         custom.coef.map = var_names,
         center = TRUE,
         include.ci = FALSE,
         label = paste0("table_ses_", var),
         file = paste0(tables_path, "ll_ses_", var, "_", 
                       paste(control_period, collapse = ""), "_", 
                       treatment_period, ".tex"))
}
