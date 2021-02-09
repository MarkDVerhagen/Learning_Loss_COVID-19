# ---
# ROBUSTNESS CHECKS: MATCH TREATMENT AND CONTROL USING ENTROPY BALANCING AND PROP. SCORE WEIGHTING
# ---

library(tidyverse)
library(broom)
library(WeightIt)
library(cobalt)
library(ggsci)
library(ebal)

theme_set(theme_bw())

# SPECIFY TREATMENT AND CONTROL PERIODS
control_period <- c("20162017", "20172018", "20182019")
treatment_period <- "20192020"

#SPECIFY ROBUST PATH FOR TABLES AND PLOTS
tables_path <- "tables/supp/r_matching/"
plots_path <- "plots/supp/r_matching/"
models_path <- "data/final/supp/r_matching/"

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

# matching requires full non-missingness
final_df <- total_df %>%
  select(ALL, RW, SP, TBL, treat, year_s, female, ses, ability, DEN, SWEIGHT, PNW, 
         class_year, school_id, days_between_all_s) %>%
  na.omit()

final_df <- final_df %>%
  mutate(SWEIGHT = as.factor(SWEIGHT),
         PNW = as.factor(PNW),
         class_year = as.factor(class_year))

# -- Evaluate covariate imbalance for treated and non-treated

cat_set <- final_df %>%
  select(ALL, treat, female, ses, ability, DEN, SWEIGHT, PNW, 
         class_year) 

cat_set %>%
  fastDummies::dummy_cols() %>%
  dplyr::select(-one_of(names(cat_set)), treat) %>%
  group_by(treat) %>%
  summarise_all(list(mean)) %>%
  writexl::write_xlsx(paste0(tables_path, "covariate_balance_",
                             paste(control_period, collapse = ""), "_" , treatment_period,".xlsx"))


# MATCHING ------------------------------------------------------------------------------------

# INDIVIDUAL VARIABLES + CLASS YEARS ONLY --------
fun <- formula("treat ~ female * ses * ability + DEN + SWEIGHT + PNW + class_year")


# -- PROPENSITY SCORES
m_ps <- weightit(fun, data = final_df, int = TRUE,
                 method = "ps", replace = T, estimand = "ATE")

# ENTROPY BALANCING (matching on mean, variance, and skew)
m_ebal <- weightit(fun, data = final_df, int = TRUE,
                   method = "ebal", moments = 3, replace = T, estimand = "ATE")

# CHECK BALANCE
bal.tab(m_ps, un = TRUE, m.threshold = .1)
bal.tab(m_ebal, un = TRUE, m.threshold = .1)


# PLOT RESULTS
var_labels <- c(ability_top = "Prior performance (top)",
                ability_middle = "Prior performance (middle)",
                ability_bottom = "Prior performance (bottom)",
                ability_rw_top = "Maths ability (top)",
                ability_rw_middle = "Maths ability (middle)",
                ability_rw_bottom = "Maths ability (bottom)",
                ability_sp_top = "Spelling ability (top)",
                ability_sp_middle = "Spelling ability (middle)",
                ability_sp_bottom = "Spelling ability (bottom)",
                ability_tbl_top = "Writing ability (top)",
                ability_tbl_middle = "Writing ability (middle)",
                ability_tbl_bottom = "Writing ability (bottom)",                
                ses_high = "Parental Educ. (high)",
                ses_low = "Parental Educ. (low)",
                ses_lowest = "Parental Educ. (lowest)",
                female = "Female", 
                female_1 = "Female",
                female_0 = "Male",
                Female_0 = "Male",
                class_year_4 = "Age 8", 
                class_year_5 = "Age 9", 
                class_year_6 = "Age 10", 
                class_year_7 = "Age 11", 
                DEN_Christian = "Christian Den.", 
                DEN_Other = "Other Den.", 
                DEN_Public = "Public Den.", 
                SWEIGHT_1 = "School Disadvantage (1. Tertile)", 
                SWEIGHT_2 = "School Disadvantage (2. Tertile)", 
                SWEIGHT_3 = "School Disadvantage (3. Tertile)", 
                DEP = "School Deprivation", 
                PNW_1 = "% Non-Western (1. Tertile)",
                PNW_2 = "% Non-Western (2. Tertile)",
                PNW_3 = "% Non-Western (3. Tertile)")

set.cobalt.options(binary = "std")

g1 <- love.plot(fun,
                stars = "raw",
                data = final_df, 
                position = "top",
                title = "",
                estimand = "ATE",
                weights = list(w1 = get.w(m_ebal), 
                               w2 = get.w(m_ps)),
                var.order = "unadjusted",
                abs = TRUE,
                alpha = 0.8,
                size = 3.5,
                thresholds = c(m = .1),
                var.names = var_labels,
                shapes = c("triangle filled", "circle filled", "square filled"),
                sample.names = c("Unweighted", "Entropy Balance", "Propensity Score"))

g1 + theme(text = element_text(size=15), 
           panel.grid.major.x = element_line(color = "lightgrey", linetype = 3),
           panel.grid.minor.x = element_line(color = "lightgrey", linetype = 3)) +
  scale_color_aaas()

ggsave(paste0(plots_path, "bal_plot.pdf"), width = 10.5, height = 14)


# RUN MATCHED REGRESSIONS -----

# save weights 

final_df <- final_df %>%
  mutate(w_ps = get.w(m_ps), 
         w_ebal = get.w(m_ebal))


fun <- " ~ 1 + treat + year_s + days_between_all_s "

dvs <- c("ALL", "RW", "TBL", "SP")

# RUN MODELS

fit_ps <- lapply(dvs,
                 FUN = function(x) lm_robust(formula(paste(x, fun)), 
                                             clusters = school_id, se_type = "stata", 
                                             data = final_df,
                                             weights = w_ps))

fit_ebal <- lapply(dvs,
                   FUN = function(x) lm_robust(formula(paste(x, fun)), 
                                               clusters = school_id, se_type = "stata", 
                                               data = final_df,
                                               weights = w_ebal))
names(fit_ps) <- names(fit_ebal) <- dvs

fit <- c(fit_ps, fit_ebal)
names(fit) <- c(paste0(names(fit_ps), "_p"), 
                paste0(names(fit_ebal), "_eb"))


screenreg(fit, include.ci = F)

texreg(fit,
       caption = "Overall learning loss, by subject", 
       custom.model.names = c("Composite (p-score)", "Maths (p-score)", 
                              "Reading (p-score)", "Spelling (p-score)",
                              "Composite (e-balance)", "Maths (e-balance)", 
                              "Reading (e-balance)", "Spelling (e-balance)"),
       custom.coef.map = var_names,
       center = TRUE,
       include.ci = FALSE,
       label = "table:overall_match", 
       file = paste0(tables_path, "ll_overall_match_", 
                     paste(control_period, collapse = ""),
                     "_", treatment_period, ".tex"))


# TIDY MODELS
full_models <- c()

for(i in names(fit_ps)) {
  tidy_model_ps <- tidy(fit_ps[[i]]) %>% 
    mutate(model = i, 
           nobs = fit_ps[[i]]$nobs, 
           nclusters = fit_ps[[i]]$nclusters, 
           model_type = "P-Score")
  tidy_model_ebal <- tidy(fit_ebal[[i]]) %>% 
    mutate(model = i, 
           nobs = fit_ebal[[i]]$nobs, 
           nclusters = fit_ebal[[i]]$nclusters, 
           model_type = "E-Balance")
  
  full_models <- rbind(full_models, tidy_model_ps, tidy_model_ebal)
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
fit_ps <- fit_ebal <- fit_ps_temp <- fit_ebal_temp <- c()

for(i in class_years) {
  fit_ps_temp <- lapply(dvs,
                        FUN = function(x) lm_robust(formula(paste(x, fun)), 
                                                    clusters = school_id, se_type = "stata", 
                                                    data = final_df[final_df$class_year==i, ],
                                                    weights = w_ps))
  
  fit_ebal_temp <- lapply(dvs,
                          FUN = function(x) lm_robust(formula(paste(x, fun)), 
                                                      clusters = school_id, se_type = "stata", 
                                                      data = final_df[final_df$class_year==i, ],
                                                      weights = w_ebal))
  
  names(fit_ps_temp) <- paste0(dvs, i)
  names(fit_ebal_temp) <- paste0(dvs, i)
  
  fit_ps <- c(fit_ps, fit_ps_temp)
  fit_ebal <- c(fit_ebal, fit_ebal_temp)
}


full_models <- c()

for(i in names(fit_ps)) {
  tidy_model_ps <- tidy(fit_ps[[i]]) %>% 
    mutate(model = i, 
           nobs = fit_ps[[i]]$nobs, 
           nclusters = fit_ps[[i]]$nclusters, 
           model_type = "P-Score")
  tidy_model_ebal <- tidy(fit_ebal[[i]]) %>% 
    mutate(model = i, 
           nobs = fit_ebal[[i]]$nobs, 
           nclusters = fit_ebal[[i]]$nclusters, 
           model_type = "E-Balance")
  
  full_models <- rbind(full_models, tidy_model_ps, tidy_model_ebal)
}


# save models
saveRDS(full_models, 
        file= paste0(models_path, "models_grade_", 
                     paste(control_period, collapse = ""), "_",
                     treatment_period, ".rds"))


# INTERACTIONS --------------------------------------------------------------------------------

# function to obtain int effects
tidy_model_int_ft <- function(list_of_models, dvs, model_type = NULL) {
  full_models <- c()
  for(i in dvs) {
    tidy_model <- tidy(list_of_models[[i]]) %>% 
      mutate(model = i, 
             nobs = list_of_models[[i]]$nobs,
             nclusters = list_of_models[[i]]$nclusters)
    var_cov <- as.data.frame(vcov(list_of_models[[i]]))
    
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
        new_row <- c(coeff_name, estimate, se, NA, NA, NA, NA, NA, i, i, list_of_models[[i]]$nobs,
                     list_of_models[[i]]$nclusters)
        tidy_model <- rbind(tidy_model, new_row)
      }
    }
    full_models <- rbind(full_models, tidy_model) 
  } 
  full_models <- full_models %>%
    mutate(model_type = model_type, 
           estimate = as.numeric(estimate), 
           std.error = as.numeric(std.error))
  
  return(full_models)
}


# Interacting variable
for(var in int_var) {
  
  fun <- paste0(" ~ 1 + treat * ", var, " + year_s + days_between_all_s")
  
  # RUN MODEL
  fit_ps <- lapply(dvs,
                   FUN=function(x) lm_robust(formula(paste(x, fun)), 
                                             clusters = school_id, se_type = "stata", 
                                             data = final_df,
                                             weights = w_ebal))
  
  
  fit_ebal <- lapply(dvs,
                     FUN=function(x) lm_robust(formula(paste(x, fun)), 
                                               clusters = school_id, se_type = "stata", 
                                               data = final_df,
                                               weights = w_ebal))
  
  names(fit_ebal) <- names(fit_ps) <- dvs
  
  # SAVE OUTPUT
  fit <- c(fit_ps, fit_ebal)
  names(fit) <- c(paste(names(fit_ps), "(p-score)"), 
                  paste(names(fit_ebal), "(e-bal)"))
  
  
  print(screenreg(fit, include.ci = F))
  
  texreg(fit, 
         caption = paste0("Learning loss by ", var), 
         custom.model.names = c("Composite (p-score)", "Maths (p-score)", 
                                "Reading (p-score)", "Spelling (p-score)",
                                "Composite (e-balance)", "Maths (e-balance)", 
                                "Reading (e-balance)", "Spelling (e-balance)"),
         custom.coef.map = var_names,
         label = paste0("table:", var, "_match"),
         center = TRUE,
         include.ci = FALSE,
         file = paste0(tables_path, "ll_", var, "_", "match_",
                       paste(control_period, collapse = ""), "_", 
                       treatment_period, ".tex"))
  
  
  full_models <- rbind(
    tidy_model_int_ft(fit_ps, dvs, model_type = "P-Score"), 
    tidy_model_int_ft(fit_ebal, dvs, model_type = "E-Balance"))
  
  # save models
  saveRDS(full_models, 
          file= paste0(models_path, 
                       "models_", var, "_", 
                       paste(control_period, collapse = ""), "_", treatment_period, ".rds"))
}
