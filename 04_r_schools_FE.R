# ---
# ROBUSTNESS: INCLUDING SCHOOL FIXED EFFECTS
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
tables_path <- "tables/supp/r_schools_FE/"
plots_path <- "plots/supp/r_schools_FE/"
models_path <- "data/final/supp/r_schools_FE/"

#SPECIFY INTERACTION
int_var <- c("female", "ses", "ability")

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



# MAIN ANALYSIS -------------------------------------------------------------------------------

fit <- lapply(dvs,
              FUN = function(x) lm_robust(formula(paste(x, fun)), 
                                          clusters = school_id, se_type = "stata", 
                                          fixed_effects = school_id,
                                          data = total_df))

names(fit) <- dvs

# SAVE OUTPUT
screenreg(fit, include.ci = F)

texreg(list(fit$ALL, fit$RW, fit$TBL, fit$SP), 
       caption = "Overall learning loss, by subject", 
       custom.model.names = c("Composite", "Maths", "Reading", "Spelling"),
       custom.coef.map = var_names,
       center = TRUE,
       include.ci = FALSE,
       label = "table:overall", 
       file = paste0(tables_path, "ll_overall_", 
                     paste(control_period, collapse = ""),
                     "_", treatment_period, ".tex"))

# TIDY MODELS
full_models <- c()

for(i in names(fit)) {
  tidy_model <- tidy(fit[[i]]) %>% 
    mutate(model = i, 
           nobs = fit[[i]]$nobs,
           nclusters = fit[[i]]$nclusters)
  
  full_models <- rbind(full_models, tidy_model)
}

# SAVE MODELS
saveRDS(full_models, 
        file= paste0(models_path, "models_overall_",
                     paste(control_period, collapse = ""), "_",
                     treatment_period, ".rds"))


# TOTAL LEARNING LOSS BY GRADE AND SUBJECT ----------------------------------------------------

# SPECIFY CLASS YEARS
class_years <- c(4:7)

# RUN MODELS BY GRADE

fit <- c()

for(i in class_years) {
  fit_temp <- lapply(dvs,
                     FUN = function(x) lm_robust(formula(paste(x, fun)), 
                                                 clusters = school_id, se_type = "stata", 
                                                 fixed_effects = school_id,
                                                 data = total_df[total_df$class_year==i, ]))
  
  names(fit_temp) <- paste0(dvs, i)
  fit <- c(fit_temp, fit)
}

# SAVE OUTPUT
screenreg(fit, include.ci = FALSE)

texreg(fit,
       caption = "Overall learning loss by grade", 
       custom.coef.map = var_names,
       center = TRUE,
       include.ci = FALSE,
       label = "table:grade",
       file = paste0(tables_path, "ll_grade_", 
                     paste(control_period, collapse = ""), "_",
                     treatment_period, ".tex"))


# TIDY MODELS
full_models <- c()

for(i in names(fit)) {
  tidy_model <- tidy(fit[[i]]) %>% 
    mutate(model = i, 
           nobs = fit[[i]]$nobs, 
           nclusters = fit[[i]]$nclusters)
  
  full_models <- rbind(full_models, tidy_model)
}

# save models
saveRDS(full_models, 
        file= paste0(models_path, "models_grade_", 
                     paste(control_period, collapse = ""), "_",
                     treatment_period, ".rds"))

# LEARNING LOSS BY INTERACTION AND SUBJECT ------------------------------------------------------------

# Interacting variable
for(var in int_var) {
  
  # SPECIFY MODELS 
  fun <- paste0(" ~ 1 + treat * ", var, " + year_s + days_between_all_s")
  
  # RUN MODEL
  fit <- lapply(dvs,
                FUN = function(x) lm_robust(formula(paste(x, fun)), 
                                            clusters = school_id, se_type = "stata", 
                                            fixed_effects = school_id,
                                            data = total_df))
  
  names(fit) <- dvs
  
  
  # SAVE OUTPUT
  print(screenreg(fit, include.ci = FALSE, custom.coef.map = var_names))
  
  texreg(list(fit$ALL, fit$RW, fit$TBL, fit$SP), 
         caption = paste0("Learning loss by ", var), 
         custom.model.names = c("Composite", "Maths", "Reading", "Spelling"),
         custom.coef.map = var_names,
         center = TRUE,
         include.ci = FALSE,
         label = paste0("table", var),
         file = paste0(tables_path, "ll_", var, "_", 
                       paste(control_period, collapse = ""), "_", 
                       treatment_period, ".tex"))
  
  
  # TIDY MODELS
  full_models <- c()
  
  for(i in dvs) {
    tidy_model <- tidy(fit[[i]]) %>% 
      mutate(model = i, 
             nobs = fit[[i]]$nobs, 
             nclusters = fit[[i]]$nclusters)
    var_cov <- as.data.frame(vcov(fit[[i]]))
    
    # add full terms
    for (j in tidy_model$term) {
      tidy_model <- tidy_model %>%
        mutate(std.error = as.numeric(std.error),
               estimate = as.numeric(estimate))
      
      if (grepl("treat:", j)) {
        coeff_name <- paste0("full_", j)
        estimate <- tidy_model$estimate[tidy_model$term == "treat"] +
          tidy_model$estimate[tidy_model$term == j]
        treat_loc <- which(names(var_cov) == "treat")
        j_loc <- which(names(var_cov) == j)
        se <- sqrt(sum(var_cov[c(treat_loc, j_loc), c(treat_loc, j_loc)]))
        new_row <- c(coeff_name, estimate, se, NA, NA, NA, NA, NA, i, i, fit[[i]]$nobs, fit[[i]]$nclusters)
        
        tidy_model <- rbind(tidy_model, new_row)
      }
    }
    
    full_models <- rbind(full_models, tidy_model) %>%
      mutate_at(vars(estimate, std.error, statistic, p.value, conf.low, conf.high, df, nobs, nclusters), 
                function(x) as.numeric(x))
  }
  
  sapply(full_models, class)
  
  # save models
  saveRDS(full_models, 
          file= paste0(models_path, 
                       "models_", var, "_", 
                       paste(control_period, collapse = ""), "_", treatment_period, ".rds"))
}

