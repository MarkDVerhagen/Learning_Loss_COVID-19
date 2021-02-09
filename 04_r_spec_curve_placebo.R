# ----
# SPECIFICATION CURVE
# ---

library(tidyverse)
library(broom)
library(estimatr)
library(MuMIn)
library(cowplot)
library(ggsci)
library(lfe)

args = commandArgs(trailingOnly=TRUE)
print(paste("Found:", args[1]))

theme_set(theme_bw(base_size = 15))

# SPECIFY TREATMENT AND CONTROL PERIODS
control_period <- c("20162017", "20172018")
treatment_period <- "20182019"


#SPECIFY PATH FOR TABLES AND PLOTS
tables_path <- "tables/supp/r_spec_curve/"
plots_path <- "plots/supp/r_spec_curve/"
models_path <- "data/final/supp/r_spec_curve/"

#READ DATA
total_df <- readRDS("data/edit/analysis.rds") %>%
  mutate(treat = ifelse(year==treatment_period, 1, 
                        ifelse(year %in% control_period, 0, NA))) %>%
  mutate(year_s = scale(as.numeric(as.character(substr(year, 5, 8)))),
         days_between_all_s = scale(days_between_all)) %>%
  mutate(school_id = factor(school_id))

total_df <- total_df %>% 
  filter(!is.na(treat), !is.na(ses), !is.na(ability), !is.na(female), !is.na(days_between_all_s))

# SIBLING FE
families_treat <- unique(total_df$family_id[total_df$treat == 1])  # select all families who have a student in treatment year
families_control <- unique(total_df$family_id[total_df$treat == 0])  # select all families who have a student in treatment year
relevant_families <- families_treat[families_treat %in% families_control] # select all families who had a child in both treatment and control years

# SUBSET DATA TO INCLUDE FAMILIES ONLY WITH AT LEAST 1 PUPIL IN TREATMENT YEAR
total_df_sib <- total_df %>%
  filter(!is.na(female) & !is.na(ses) & !is.na(ability)) %>%
  filter(family_id %in% relevant_families) %>%
  filter(!is.na(family_id))

# ENSURE FAMILIES HAVE MULTIPLE OBSERVATIONS
count_fams <- total_df_sib %>%
  filter(!duplicated(pupil_id)) %>%
  group_by(family_id) %>%
  count()

# Only select students with at least one sibling
total_df_sib <- total_df_sib %>%
  filter(family_id %in% count_fams$family_id[count_fams$n > 1]) 


# GENERATE SINGLE YEAR COMPARISONS, COMPARING TREATMENT YEAR TO MOST RECENT CONTROL YEAR
df_year <- total_df %>% 
  filter(year %in% c(control_period[length(control_period)],
                     treatment_period))

df_year_fe_sib <- total_df_sib %>% 
  filter(year %in% c(control_period[length(control_period)],
                     treatment_period))
count_df_year_sib <- count(df_year_fe_sib, family_id)
df_year_fe_sib <- df_year_fe_sib %>%
  filter(!(family_id %in% count_df_year_sib$family_id[count_df_year_sib$n == 1]))

# SPECIFY MODELS  -----------------------------------------------------------------------------

# variables to interact
int_vars <- c("class_year", "female", "ses", "ability")

# full model
full_model <- lm(formula(paste("ALL ~ treat + year_s + days_between_all_s + ", paste(int_vars, collapse = " * "))),
                 data = total_df, na.action = "na.fail")

# full model, single year
full_model_year <- lm(formula(paste("ALL ~ treat + days_between_all_s + ", paste(int_vars, collapse = " * "))),
                      data = df_year, na.action = "na.fail")

# get complete list of models as separate strings
all_models <- dredge(full_model, fixed = c("treat", "year_s"), eval = F) %>%
  lapply(toString) %>%
  str_remove_all("lm, ALL") %>%
  str_remove_all(", total_df, na.fail")

all_models_year <- dredge(full_model_year, fixed = c("treat"), eval = F) %>%
  lapply(toString) %>%
  str_remove_all("lm, ALL") %>%
  str_remove_all(", df_year, na.fail")


# RUN MODELS ----------------------------------------------------------------------------------

#dvs <- c("ALL", "RW", "SP", "TBL")
dvs <- args[1]

lm_ests <- lm19_ests <- fe_school_ests <- fe19_school_ests <- fe_sibling_ests <- fe19_sibling_ests <- c()

time <- Sys.time()
print(paste("Starting", Sys.time()))
for(i in dvs) {
  print(paste("Generating all possible models for:", i, "..."))

  # Linear Model
  lm_ests_temp <- lapply(all_models,
                         FUN = function(x) lm_robust(formula(paste(i, x)),
                                                     clusters = school_id, se_type = "stata",
                                                     data = total_df))
  names(lm_ests_temp) <- paste0(i, all_models)
  lm_ests <- c(lm_ests, lm_ests_temp)

  # Linear Model, year
  lm19_ests_temp <- lapply(all_models_year,
                           FUN = function(x) lm_robust(formula(paste(i, x)),
                                                       clusters = school_id, se_type = "stata",
                                                       data = df_year))
  names(lm19_ests_temp) <- paste0(i, all_models_year)
  lm19_ests <- c(lm19_ests, lm19_ests_temp)

  # save(lm_ests, lm19_ests,
  #      file = paste0(models_path, "lm_ests_", i, "_",
  #                    paste(control_period, collapse=""), "_", treatment_period, ".Rda"))
  # lm_ests <- NULL
  # lm_ests_temp <- NULL
  # lm19_ests <- NULL
  # lm19_ests_temp <- NULL

  print(paste("Finished LM Models "))
  
  # School Fixed Effects
  fe_school_ests_temp <- lapply(all_models,
                                FUN = function(x) lm_robust(formula(paste(i, x)),  se_type = "stata",
                                                            data = total_df, clusters = school_id,
                                                            fixed_effects =  school_id))
  
  names(fe_school_ests_temp) <- paste0(i, all_models)
  fe_school_ests <- c(fe_school_ests, fe_school_ests_temp)
  
  # School Fixed Effects, year
  fe19_school_ests_temp <- lapply(all_models_year,
                                  FUN = function(x) lm_robust(formula(paste(i, x)),
                                                              clusters = school_id, se_type = "stata",
                                                              data = df_year,
                                                              fixed_effects =  school_id))
  names(fe19_school_ests_temp) <- paste0(i, all_models_year)
  fe19_school_ests <- c(fe19_school_ests, fe19_school_ests_temp)
  
  # save(fe_school_ests, fe19_school_ests,
  #      file = paste0(models_path, "fe_school_ests_", i, "_",
  #                    paste(control_period, collapse=""), "_", treatment_period, ".Rda"))
  # fe_school_ests <- NULL
  # fe_school_ests_temp <- NULL
  # fe19_school_ests <- NULL
  # fe19_school_ests_temp <- NULL
  
  print(paste("Finished School FE Models "))
  
  # Sibling Fixed Effects
  fe_sibling_ests_temp <- lapply(all_models,
                                 FUN = function(x) felm(formula(paste(i, x, " | family_id")),
                                                        data = total_df_sib))

  names(fe_sibling_ests_temp) <- paste0(i, all_models)
  fe_sibling_ests <- c(fe_sibling_ests, fe_sibling_ests_temp)

  # Sibling Fixed Effects, year
  fe19_sibling_ests_temp <- lapply(all_models_year,
                                   FUN = function(x) felm(formula(paste(i, x, " | family_id")),
                                                          data = df_year_fe_sib))

  names(fe19_sibling_ests_temp) <- paste0(i, all_models_year)
  fe19_sibling_ests <- c(fe19_sibling_ests, fe19_sibling_ests_temp)
  # 
  # save(fe_sibling_ests, fe19_sibling_ests,
  #      file = paste0(models_path, "fe_sibling_ests_", i, "_",  
  #                    paste(control_period, collapse=""), "_", treatment_period, ".Rda"))
  # 
  print(paste("Finished Sibling FE Models, time: ", Sys.time() - time ))
}
Sys.time() - time 

# load(paste0(models_path, "fe_sibling_ests_", dvs, "_",
#             paste(control_period, collapse=""), "_", treatment_period, ".Rda"))
# 
# load(paste0(models_path, "lm_ests_", dvs, "_",
#             paste(control_period, collapse=""), "_", treatment_period, ".Rda"))

print(paste("Finished Loading Models "))

lm_models <- lm19_models <- fe_school_models <- fe19_school_models <-fe_sibling_models <- fe19_sibling_models <- c()

for(i in names(lm_ests)) {
  tidy_lm <- tidy(lm_ests[[i]]) %>%
    mutate(model = i,
           nobs = lm_ests[[i]]$nobs,
           nclusters = lm_ests[[i]]$nclusters)
  tidy_fe_school <- tidy(fe_school_ests[[i]]) %>%
    mutate(model = i,
           nobs = fe_school_ests[[i]]$nobs,
           nclusters = fe_school_ests[[i]]$nclusters)
  tidy_fe_sibling <- tidy(fe_sibling_ests[[i]]) %>%
    mutate(model = i,
           nobs = fe_sibling_ests[[i]]$nobs,
           nclusters = fe_sibling_ests[[i]]$nclusters)
  lm_models <- rbind(lm_models, tidy_lm)
  fe_school_models <- rbind(fe_school_models, tidy_fe_school)
  fe_sibling_models <- rbind(fe_sibling_models, tidy_fe_sibling)
}

for(i in names(lm19_ests)) {
  tidy_lm19 <- tidy(lm19_ests[[i]]) %>%
    mutate(model = i,
           nobs = lm19_ests[[i]]$nobs,
           nclusters = lm19_ests[[i]]$nclusters)
  tidy_fe19_school <- tidy(fe19_school_ests[[i]]) %>%
    mutate(model = i,
           nobs = fe19_school_ests[[i]]$nobs,
           nclusters = fe19_school_ests[[i]]$nclusters)
  tidy_fe19_sibling <- tidy(fe19_sibling_ests[[i]]) %>%
    mutate(model = i,
           nobs = fe19_sibling_ests[[i]]$nobs,
           nclusters = fe19_sibling_ests[[i]]$nclusters)
  
  lm19_models <- rbind(lm19_models, tidy_lm19)
  fe19_school_models <- rbind(fe19_school_models, tidy_fe19_school)
  fe19_sibling_models <- rbind(fe19_sibling_models, tidy_fe19_sibling)
}

if(treatment_period=="20192020") {
  lm_models <- lm_models %>% filter(term=="treat") %>% mutate(type = "None", period = "2017-2020")
  lm19_models <- lm19_models %>% filter(term=="treat") %>% mutate(type = "None", period = "2019-2020")
  fe_school_models <- fe_school_models %>% filter(term=="treat") %>% mutate(type = "School", period = "2017-2020")
  fe19_school_models <- fe19_school_models %>% filter(term=="treat") %>% mutate(type = "School", period = "2019-2020")
  fe_sibling_models <- fe_sibling_models %>% filter(term=="treat") %>% mutate(type = "Sibling", period = "2017-2020")
  fe19_sibling_models <- fe19_sibling_models %>% filter(term=="treat") %>% mutate(type = "Sibling", period = "2019-2020")
} else if(treatment_period=="20182019") {
  lm_models <- lm_models %>% filter(term=="treat") %>% mutate(type = "None", period = "2017-2019")
  lm19_models <- lm19_models %>% filter(term=="treat") %>% mutate(type = "None", period = "2018-2019")
  fe_school_models <- fe_school_models %>% filter(term=="treat") %>% mutate(type = "School", period = "2017-2019")
  fe19_school_models <- fe19_school_models %>% filter(term=="treat") %>%mutate(type = "School", period = "2018-2019")
  fe_sibling_models <- fe_sibling_models %>% filter(term=="treat") %>% mutate(type = "Sibling", period = "2017-2019")
  fe19_sibling_models <- fe19_sibling_models %>% filter(term=="treat") %>% mutate(type = "Sibling", period = "2018-2019")
}

complete_models <- bind_rows(lm_models, lm19_models,
                             fe_school_models, fe19_school_models,
                             fe_sibling_models, fe19_sibling_models)

complete_models <- complete_models %>%
  mutate(ability = ifelse(str_detect(model, " ability "), 1, 0),
         "School Grade" = ifelse(str_detect(model, "class_year"), 1, 0),
         "Sex" = ifelse(str_detect(model, "female"), 1, 0),
         "Parental Education" = ifelse(str_detect(model, "ses"), 1, 0),
         "Prior Performance" = ifelse(str_detect(model, "ability"), 1, 0),
         "Year" = ifelse(str_detect(model, "year_s"), 1, 0),
         "Days between tests" = ifelse(str_detect(model, "days_between_all_s"), 1, 0),
         "Prior Perf. x Grade" = ifelse(str_detect(model, "ability:class_year"), 1, 0),
         "Prior Perf. x Grade x Sex" = ifelse(str_detect(model, "ability:class_year:female"), 1, 0),
         "Prior Perf. x Grade x Par. Educ." = ifelse(str_detect(model, "ability:class_year:ses"), 1, 0),
         "Prior Perf. x Grade x Sex x Par. Educ." = ifelse(str_detect(model, "ability:class_year:female:ses"), 1, 0),
         "Prior Perf. x Sex" = ifelse(str_detect(model, "ability:female"), 1, 0),
         "Prior Perf. x Sex x Par. Educ." = ifelse(str_detect(model, "ability:female:ses"), 1, 0),
         "Prior Perf. x Par. Educ." = ifelse(str_detect(model, "ability:ses"), 1, 0),
         "Grade x Sex" = ifelse(str_detect(model, "class_year:female"), 1, 0),
         "Grade x Sex x Par. Educ." = ifelse(str_detect(model, "class_year:female:ses"), 1, 0),
         "Grade x Par. Educ." = ifelse(str_detect(model, "class_year:ses"), 1, 0),
         "Sex x Par. Educ." = ifelse(str_detect(model, "female:ses"), 1, 0))

complete_models <- complete_models %>%
  mutate(outcome = ifelse(substr(model, 1,3)=="ALL", "Composite",
                          ifelse(substr(model, 1,3)=="RW ", "Maths",
                                 ifelse(substr(model, 1,3)=="SP ", "Spelling",
                                        ifelse(substr(model, 1,3)=="TBL", "Reading", NA))))) %>%
  group_by(outcome) %>%
  arrange(estimate) %>%
  mutate(h_order = 1:n())

save(complete_models,
     file = paste0(models_path, "complete_models_", dvs, "_",
                   paste(control_period, collapse=""), "_", treatment_period, ".Rda"))

# # PLOT RESULTS --------------------------------------------------------------------------------
# 
# # TOP PLOT
make_coef_plot <- function(dep_var) {
  # plot specification curve
  g1 <- complete_models %>%
    filter(outcome == dep_var) %>%
    ggplot(aes(x = h_order, y = estimate)) +
    geom_ribbon(aes(fill = outcome,
                    ymin = estimate - 1.96* std.error,
                    ymax = estimate + 1.96* std.error, ), size = 0.2, alpha = 0.5) +
    geom_point(aes(color = outcome), shape = 21, size = 0.2) +
    ylab("Treatment Coefficient") +
    scale_color_aaas(guide = FALSE) +
    scale_fill_aaas(guide = FALSE) +
    geom_hline(yintercept = 0, color = "black", size = 1, linetype = 2) +
    facet_wrap(~outcome, scales = "free_x") +
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank())
  
  if(treatment_period=="20192020"){
    g1 <- g1 +
      ylim(c(-4, 2))
  } else if(treatment_period=="20182019"){
    g1 <- g1 + ylim(c(-4, 2))
  }
  
  if(dep_var=="Maths"){g1 <- g1 +
    scale_color_manual(guide = FALSE, values = c("#EE0000FF")) +
    scale_fill_manual(guide = FALSE, values = c("#EE0000FF"))}
  if(dep_var=="Reading"){g1 <- g1 +
    scale_color_manual(guide = FALSE, values = c("#008B45FF")) +
    scale_fill_manual(guide = FALSE, values = c("#008B45FF"))}
  if(dep_var=="Spelling"){g1 <- g1 +
    scale_color_manual(guide = FALSE, values = c("#631879FF")) +
    scale_fill_manual(guide = FALSE, values = c("#631879FF"))}
  return(g1)
}

# BOTTOM PLOT
# Function to create a specification plot for a single category.
make_spec_plot <- function(category, dep_var = "Composite") {
  # category = spec_cols[1] # DEBUG
  specs <- complete_models %>%
    filter(outcome==dep_var) %>%
    dplyr::select(h_order, category) %>%
    pivot_longer(starts_with(category), names_prefix = paste0(category, "_")) %>%
    mutate(name = factor(name, levels = rev(unique(name))))
  
  if(is.numeric(specs$value)){
    spec_plot <- ggplot(specs, aes(x = h_order, y = name, alpha = value)) +
      geom_point(shape = "|", size = 0.7) +
      scale_alpha_continuous(guide = FALSE, range = c(0, 1)) +
      scale_fill_aaas(guide = FALSE) +
      theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.text.x = element_blank()) +
      theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())
    
  } else {
    spec_plot <- ggplot(specs, aes(x = h_order, y = value)) +
      geom_point(shape = "|", size = 0.7) +
      scale_alpha_continuous(guide = FALSE, range = c(0, 1)) +
      scale_fill_aaas(guide = FALSE) +
      theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.text.x = element_blank()) +
      theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())
  }
}

if(dvs == "ALL") {
  # # COMPOSITE
  coef_plot_all <- make_coef_plot("Composite")
  spec_plots_all <- lapply(list(
    c("Parental Education", "Sex", "Prior Performance",
      "School Grade", "Year", "Days between tests"),
    names(complete_models[str_detect(names(complete_models), " x ")]),
    c("type"),
    c("period")), make_spec_plot, dep_var = "Composite")
  
  combined_plot_all <- plot_grid(plotlist = c(list(coef_plot_all), spec_plots_all),
                                 labels = c("", "Controls", "Interactions", "Fixed-Effects", "Sample Period"),
                                 label_size = 12, label_fontface = "italic", vjust = 0.5, hjust = -0.1,
                                 rel_heights = c(1.7, 0.6, 1.1, 0.3, 0.2), align = "v", ncol = 1)
  
  ggsave(combined_plot_all,
         file = paste0(plots_path, "spec_curve_all_", paste(control_period, collapse = ""),
                       "_", treatment_period, ".pdf"), width = 10, height = 12)
  
} else if(dvs =="RW"){
  coef_plot_rw <- make_coef_plot("Maths")
  spec_plots_rw <- lapply(list(
    c("Parental Education", "Sex", "Prior Performance",
      "School Grade", "Year", "Days between tests"),
    names(complete_models[str_detect(names(complete_models), " x ")]),
    c("type"),
    c("period")), make_spec_plot, dep_var = "Maths")
  
  combined_plot_rw <- plot_grid(plotlist = c(list(coef_plot_rw), spec_plots_rw),
                                labels = c("", "Controls", "Interactions", "Fixed-Effects", "Sample Period"),
                                label_size = 12, label_fontface = "italic", vjust = 0.5, hjust = -0.1,
                                rel_heights = c(1.7, 0.6, 1.1, 0.3, 0.2), align = "v", ncol = 1)
  
  ggsave(combined_plot_rw,
         file = paste0(plots_path, "spec_curve_rw_",
                       paste(control_period, collapse = ""), "_", treatment_period, ".pdf"),
         width = 10, height = 12)
  
} else if(dvs=="SP") {
  coef_plot_sp <- make_coef_plot("Spelling")
  
  spec_plots_sp <- lapply(list(
    c("Parental Education", "Sex", "Prior Performance",
      "School Grade", "Year", "Days between tests"),
    names(complete_models[str_detect(names(complete_models), " x ")]),
    c("type"),
    c("period")), make_spec_plot, dep_var = "Spelling")
  
  combined_plot_sp <- plot_grid(plotlist = c(list(coef_plot_sp), spec_plots_sp),
                                labels = c("", "Controls", "Interactions", "Fixed-Effects", "Sample Period"),
                                label_size = 12, label_fontface = "italic", vjust = 0.5, hjust = -0.1,
                                rel_heights = c(1.7, 0.6, 1.1, 0.3, 0.2), align = "v", ncol = 1)
  
  ggsave(combined_plot_sp,
         file = paste0(plots_path, "spec_curve_sp_",
                       paste(control_period, collapse = ""), "_", treatment_period, ".pdf"),
         width = 10, height = 12)
  
} else if(dvs=="TBL") {
  # READING
  coef_plot_tbl <- make_coef_plot("Reading")
  spec_plots_tbl <- lapply(list(
    c("Parental Education", "Sex", "Prior Performance",
      "School Grade", "Year", "Days between tests"),
    names(complete_models[str_detect(names(complete_models), " x ")]),
    c("type"),
    c("period")), make_spec_plot, dep_var = "Reading")
  
  combined_plot_tbl <- plot_grid(plotlist = c(list(coef_plot_tbl), spec_plots_tbl),
                                 labels = c("", "Controls", "Interactions", "Fixed-Effects", "Sample Period"),
                                 label_size = 12, label_fontface = "italic", vjust = 0.5, hjust = -0.1,
                                 rel_heights = c(1.7, 0.6, 1.1, 0.3, 0.2), align = "v", ncol = 1)
  ggsave(combined_plot_tbl,
         file = paste0(plots_path, "spec_curve_tbl_",
                       paste(control_period, collapse = ""), "_", treatment_period, ".pdf"),
         width = 10, height = 12)
  
}
