# ---
# COMPUTE DIFFERENCE IN PROGRESS BY TIME BETWEEN TESTS
# ---

library(tidyverse)
library(broom)
library(estimatr)
library(texreg)
library(kableExtra)


theme_set(theme_bw())

#SPECIFY PATHS
tables_path <- "tables/supp/r_only_days/"
plots_path <- "plots/supp/r_only_days/"
models_path <- "data/final/supp/r_only_days/"

#SPECIFY INTERACTION
int_var <- c("female", "ses", "ability", "class_year")

# SPECIFY MODELS 
fun <- " ~ 1 + weeks_between_all + year"

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
total_df <- readRDS(paste0("data/edit/analysis.rds")) 


# LIMIT DATA TO NON-MISSING ON SES, ABILITY, AND GENDER
total_df <- total_df %>%
  filter(!is.na(female) & !is.na(ses) & !is.na(ability) & !is.na(days_between_all)) %>%
  mutate(class_year = as.factor(class_year), 
         weeks_between_all = days_between_all/7) %>%
  filter(year != "20192020")



# MAIN ANALYSIS -------------------------------------------------------------------------------

fit <- lapply(dvs,
              FUN = function(x) lm_robust(formula(paste(x, fun)), 
                                          clusters = school_id, se_type = "stata", 
                                          data = total_df))

names(fit) <- dvs

# SAVE OUTPUT
print(screenreg(fit, include.ci = F))

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
        file= paste0(models_path, "weekly_progress_overall.rds"))

# LEARNING LOSS BY INTERACTION AND SUBJECT ------------------------------------------------------------

# Interacting variable
for(var in int_var) {
  
  # SPECIFY MODELS 
  fun <- paste0(" ~ 1 + as.factor(year) + weeks_between_all * ", var)
  
  # RUN MODEL
  fit <- lapply(dvs,
                FUN = function(x) lm_robust(formula(paste(x, fun)), 
                                            clusters = school_id, se_type = "stata", 
                                            data = total_df))
  
  names(fit) <- dvs
  
  
  # SAVE OUTPUT
  print(screenreg(fit, include.ci = FALSE))
  
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
      
      if (grepl("weeks_between_all:", j)) {
        coeff_name <- paste0("full_", j)
        estimate <- tidy_model$estimate[tidy_model$term == "weeks_between_all"] +
          tidy_model$estimate[tidy_model$term == j]
        cov_loc <- which(names(var_cov) == "weeks_between_all")
        j_loc <- which(names(var_cov) == j)
        se <- sqrt(sum(var_cov[c(cov_loc, j_loc), c(cov_loc, j_loc)]))
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
                       "weekly_progress_", var, ".rds"))
}



# Make table of daily progress by category ----------------------------------------------------

weekly_overall <- readRDS("data/final/supp/r_only_days/weekly_progress_overall.rds") %>%
  mutate(type = "overall")
weekly_ses <- readRDS("data/final/supp/r_only_days/weekly_progress_ses.rds") %>%
  mutate(type = "ses")
weekly_female <- readRDS("data/final/supp/r_only_days/weekly_progress_female.rds") %>% 
  mutate(type = "female")
weekly_ability <- readRDS("data/final/supp/r_only_days/weekly_progress_ability.rds") %>%
  mutate(type = "ability")
weekly_grade <- readRDS("data/final/supp/r_only_days/weekly_progress_class_year.rds") %>%
  mutate(type = "grade")

weekly_df <- bind_rows(weekly_overall, weekly_female, weekly_ses, weekly_ability, weekly_grade) %>%
  filter(term =="weeks_between_all" |str_detect(term, "full_weeks")) %>%
  filter(model!="DMT") %>%
  select(term, estimate, std.error, model, type, nobs) %>%
  mutate(label =ifelse(str_detect(term, "7"), "Age 11", 
                       ifelse(str_detect(term, "6"), "Age 10", 
                              ifelse(str_detect(term, "5"), "Age 9", 
                                     ifelse(type=="grade" & term=="weeks_between_all", "Age 8",
                                            ifelse(type=="ses" & term=="weeks_between_all", "High", 
                                                   ifelse(type=="ses" & term=="full_weeks_between_all:seslow", "Low", 
                                                          ifelse(type=="ses" & term=="full_weeks_between_all:seslowest", "Lowest",   
                                                                 ifelse(type=="female" & term=="weeks_between_all",  "Boys", 
                                                                        ifelse(type=="female" & term=="full_weeks_between_all:female", "Girls",
                                                                               ifelse(type=="ability" & term=="weeks_between_all", "Top", 
                                                                                      ifelse(type=="ability" & term=="full_weeks_between_all:abilitymiddle", 
                                                                                             "Middle", 
                                                                                             ifelse(type=="ability" & term=="full_weeks_between_all:abilitybottom", 
                                                                                                    "Bottom", "Total")))))))))))),
         label = factor(label, levels = c("", "Total","High", "Low", "Lowest", "Boys", "Girls", 
                                          "Maths", "Spelling", "Reading", "Top", "Middle", "Bottom", 
                                          "Age 8", "Age 9", "Age 10", "Age 11")), 
         type = ifelse(type == "overall", "Total",
                       ifelse(type %in% c('RW', 'TBL', 'SP'), "Subject",
                              ifelse(type == "ses", "Parental Educ.",
                                     ifelse(type == "female", "Sex",
                                            ifelse(type == "ability", "Prior Perf.",
                                                   ifelse(type == "grade", "School Grade", NA))))))) %>%
  select(estimate, std.error, model, label, nobs)

weekly_df <- weekly_df %>%
  mutate(estimate = round(estimate, digits = 2), 
         estimate = ifelse(estimate < std.error * 1.96, "n.s.", estimate)) %>%
  select(-std.error)

nobs <- weekly_df %>% group_by(model) %>%
  summarise(nobs = as.character(unique(nobs)), 
            label = "Num. obs.") %>%
  spread(model, nobs)

weekly_df <- weekly_df %>%
  select(-nobs) %>%
  spread(model, estimate) %>%
  bind_rows(nobs)

weekly_df %>%
  mutate(label = ifelse(label == "Total", "", label)) %>%
  rename(" " = label,
         "Composite" = "ALL", 
         "Maths" = "RW", 
         "Spelling" = "SP", 
         "Reading" = "TBL") %>%
  kable("latex", booktabs = T, 
        caption = "Estimated weekly learning gains",
        label = "weekly_progress") %>%
  pack_rows("All", 1, 1,
            bold = FALSE, italic = TRUE, indent = TRUE) %>%
  pack_rows("Parental Education", 2, 4, 
            bold = FALSE, italic = TRUE, indent = TRUE) %>%
  pack_rows("Sex", 5, 6, 
            bold = FALSE, italic = TRUE, indent = TRUE) %>%
  pack_rows("Prior Performance", 7, 9, 
            bold = FALSE, italic = TRUE, indent = TRUE) %>%
  pack_rows("School Grade", 10, 13, 
            bold = FALSE, italic = TRUE, indent = TRUE) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  cat(file = paste0(tables_path, "weekly_progress.tex"))


