rm(list = ls())

library(tidyverse)
library(lubridate)
# theming
library(ggsci)
library(cowplot)


# SPECIFY TREATMENT AND CONTROL PERIODS
control_period <- c("20162017", '20172018', "20182019")
treatment_period <- "20192020"

# GENERATE PLOTTING FUNCTIONS

# read in and edit data
gen_model_df <- function(path_to_rds_file, sibling_FE = FALSE, type_name = NULL) {
  
  read_and_edit <- function(path_to_rds_file, type_name) {
    data <- readRDS(path_to_rds_file) %>%
      mutate(type = type_name)
    return(data)
  }
  
  full_models <- c()
  
  model_types <- c("overall", "grade", "ses", "female", "ability")
  
  
  for(i in model_types) {
    print(i)
    if(sibling_FE) {
      data <- read_and_edit(paste0(models_path, "models_", i, "_", 
                                   paste0(control_period, collapse = ""), "_", treatment_period, ".rds"), 
                            type_name = i) %>%
        select(term, estimate, std.error, statistic, p.value, model, type) %>%
        mutate_at(vars(-term, -model, -type), as.numeric)
      
      full_models <- bind_rows(full_models, data)
    } else {
      data <- read_and_edit(paste0(models_path, "models_", i, "_", 
                                   paste0(control_period, collapse = ""), "_", treatment_period, ".rds"), 
                            type_name = i) %>%
        mutate(statistic = as.numeric(statistic), p.value = as.numeric(p.value), 
               conf.low = as.numeric(conf.low), conf.high = as.numeric(conf.high), df = as.numeric(df), 
               nobs = as.numeric(nobs), nclusters = as.numeric(nclusters))
      
      full_models <- bind_rows(full_models, data)
    }
  }
  
  return(full_models %>%
           mutate(type = ifelse(type == "overall" & model!="ALL", model, type)) %>%
           filter(term=="treat"|str_detect(term, "full_treat")) %>%
           mutate(label = ifelse(model=="ALL" & type=="overall", "",
                                 ifelse(type=="RW", "Maths", 
                                        ifelse(type=="TBL", "Reading", 
                                               ifelse(type=="SP", "Spelling", 
                                                      ifelse(type=="DMT", "Info. Processing",
                                                             ifelse(str_detect(model, "7"), "Age 11", 
                                                                    ifelse(str_detect(model, "6"), "Age 10", 
                                                                           ifelse(str_detect(model, "5"), "Age 9", 
                                                                                  ifelse(str_detect(model, "4"), "Age 8", 
                                                                                         ifelse(type=="ses" & term=="treat", "High", 
                                                                                                ifelse(type=="ses" & term=="full_treat:seslow", "Low", 
                                                                                                       ifelse(type=="ses" & term=="full_treat:seslowest", "Lowest",   
                                                                                                              ifelse(type=="female" & term=="treat",  "Boys",   
                                                                                                                     ifelse(type=="female" & term=="full_treat:female", "Girls",   
                                                                                                                            ifelse(type=="ability" & term=="treat", "Top", 
                                                                                                                                   ifelse(type=="ability" & term=="full_treat:abilitymiddle", 
                                                                                                                                          "Middle", 
                                                                                                                                          ifelse(type=="ability" & term=="full_treat:abilitybottom", 
                                                                                                                                                 "Bottom", NA))))))))))))))))), 
                  label = factor(label, levels = c("", "Total","High", "Low", "Lowest", "Boys", "Girls", 
                                                   "Maths", "Spelling", "Reading", "Top", "Middle", "Bottom", 
                                                   "Age 8", "Age 9", "Age 10", "Age 11")), 
                  type = ifelse(type == "overall", "Total",
                                ifelse(type %in% c('RW', 'TBL', 'SP'), "Subject",
                                       ifelse(type == "ses", "Parental Educ.",
                                              ifelse(type == "female", "Sex",
                                                     ifelse(type == "ability", "Prior Perf.",
                                                            ifelse(type == "grade", "School Grade", NA)))))),
                  type = factor(type, levels = c("", "Total", "Parental Educ.", "Sex",
                                                 "Prior Perf.","Subject", "School Grade"))))
}


# generate plots
make_plot <- function(full_models, matched = FALSE, sibling_FE = FALSE, DMT = FALSE, dodge = 0,
                      alt_treat_years = FALSE) {
  
  if (DMT) {
    full_models_all <- full_models %>%
      filter(!type == "Subject")
    g_start <- full_models_all %>%
      ggplot(aes(x = reorder(label, -as.numeric(label)), y = estimate, color = type, shape = type,
                 alpha = group)) +
      geom_point(size = 2.5, position = position_dodge(width=dodge)) +
      geom_errorbar(aes(ymin = 
                          estimate - 1.96 * std.error, 
                        ymax = estimate + 1.96 * std.error), width = 0, size = 0.7, 
                    position = position_dodge(width=dodge)) +
      scale_alpha_discrete(range = c(0.3, 1))
  } else {
    full_models_all <- full_models %>%
      filter(str_detect(model, "ALL") | type == "Subject") %>%
      mutate(group = "all")
    
    g_start <- full_models_all %>%
      ggplot(aes(x = reorder(label, -as.numeric(label)), y = estimate, color = type, shape = type)) +
      geom_point(size = 2.5) +
      geom_errorbar(aes(ymin = 
                          estimate - 1.96 * std.error, 
                        ymax = estimate + 1.96 * std.error), width = 0, size = 0.7)
  }
  
  g1 <- g_start +
    geom_hline(yintercept = 0, linetype = 2, color = "darkgrey") +
    cowplot::theme_cowplot(font_size = 17) +
    coord_flip() +
    scale_color_aaas() +
    scale_fill_aaas() +
    ylab("Learning loss (percentiles)") +
    xlab("")
  
  # ADD FACETS
  if (matched) {
    g1 <- g1 +
      facet_grid(type ~ model_type, scales = "free", space = "free", switch="y") +
      scale_y_continuous(minor_breaks = seq(-5.5 , 1.5, 1), 
                         breaks = round(seq(-5, 1.5, 1), 2),
                         limits = c(-5.2, 1.2)) 
  } else if (sibling_FE) {
    g1 <- g1 +
      facet_grid(type ~ ., scales = "free", space = "free", switch="y") +
      scale_y_continuous(minor_breaks = seq(-10 , 2, 1), 
                         breaks = round(seq(-10, 2, 1), 2),
                         limits = c(-10, 2)) 
  } else if (alt_treat_years) {
    g1 <- g1 +
      facet_grid(type ~ ., scales = "free", space = "free", switch="y") +
      scale_y_continuous(minor_breaks = seq(-5.5 , 2, 1), 
                         breaks = round(seq(-5, 2, 1), 2),
                         limits = c(-5.2, 2)) 
  } else {
    g1 <- g1 +
      facet_grid(type ~ ., scales = "free", space = "free", switch="y") +
      scale_y_continuous(minor_breaks = seq(-5.5 , 1.5, 1), 
                         breaks = round(seq(-5, 1.5, 1), 2),
                         limits = c(-5.2, 1.2)) 
  }
  
  plot <- g1 +    
    theme(panel.grid.major.x = element_line(size = 0.5, linetype = 'dotted',
                                            colour = "lightgrey"), 
          panel.grid.minor.x = element_line(size = 0.25, linetype = 'dotted',
                                            colour = "lightgrey"),
          strip.placement = "outside", 
          strip.text.y = element_text(face = "bold", hjust=0.5, vjust=0.5),
          strip.background=element_rect(fill = NA, color = "black", size = 1.2),
          legend.position = "none", 
          panel.spacing.x=unit(1, "lines"),
          panel.spacing.y=unit(0.1, "lines"),
          panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5), 
          plot.margin=grid::unit(c(0.5,0.5,0.1,0.1), "mm"))
  return(plot)
}

# COEF PLOT (MAIN RESULTS): FIGURE 2  ------------------------------------------
models_path <- "data/final/main/"
plots_path <- "plots/main/"

full_models <- gen_model_df(models_path)
make_plot(full_models = full_models)

ggsave(paste0(plots_path, "ll_full_", paste(control_period, collapse = ""),
              "_", treatment_period, ".pdf"),  width = 7, height = 9)

# COEF PLOT (ALL CONTROLS): APPENDIX -------------------------------------------
models_path <- "data/final/supp/r_all_controls/"
plots_path <- "plots/supp/r_all_controls/"

full_models <- gen_model_df(models_path)
make_plot(full_models = full_models)

ggsave(paste0(plots_path, "ll_full_", paste(control_period, collapse = ""),
              "_", treatment_period, ".pdf"),  width = 7, height = 9)


# COEF PLOT (MATCHED): APPENDIX ------------------------------------------------
models_path <- "data/final/supp/r_matching/"
plots_path <- "plots/supp/r_matching/"

full_models <- gen_model_df(models_path) %>% 
  mutate(model_type = recode(model_type, "P-Score" = "Propensity Score", "E-Balance" = "Entropy Balance"))
make_plot(full_models = full_models, matched = T)

ggsave(paste0(plots_path, "ll_full_", paste(control_period, collapse = ""),
              "_", treatment_period, ".pdf"),  width = 7, height = 9)

# COEF PLOT (SIBLING FIXED EFFECTS): APPENDIX ----------------------------------
models_path <- "data/final/supp/r_sibling_FE/"
plots_path <- "plots/supp/r_sibling_FE/"

full_models <- gen_model_df(models_path)
make_plot(full_models = full_models, sibling_FE = T)

ggsave(paste0(plots_path, "ll_full_", paste(control_period, collapse = ""),
              "_", treatment_period, ".pdf"),  width = 7, height = 9)

# COEF PLOT (SCHOOL FIXED EFFECTS): APPENDIX -----------------------------------
models_path <- "data/final/supp/r_schools_FE/"
plots_path <- "plots/supp/r_schools_FE/"

full_models <- gen_model_df(models_path)
make_plot(full_models = full_models)

ggsave(paste0(plots_path, "ll_full_", paste(control_period, collapse = ""),
              "_", treatment_period, ".pdf"),  width = 7, height = 9)


# COEF PLOT (NOT CONTROLLING FOR DAYS BETWEEN TESTS): APPENDIX -----------------------------------
models_path <- "data/final/supp/r_exclude_days/"
plots_path <- "plots/supp/r_exclude_days/"

full_models <- gen_model_df(models_path)
make_plot(full_models = full_models)

ggsave(paste0(plots_path, "ll_full_", paste(control_period, collapse = ""),
              "_", treatment_period, ".pdf"),  width = 7, height = 9)


# COEF PLOT (SCHOOLS IN NEIGHBOURHOODS WITH LOW SHARE OF NON-WESTERN RESIDENTS): APPENDIX ----------
models_path <- "data/final/supp/r_low_pnw/"
plots_path <- "plots/supp/r_low_pnw/"

full_models <- gen_model_df(models_path)
make_plot(full_models = full_models)

ggsave(paste0(plots_path, "ll_full_", paste(control_period, collapse = ""),
              "_", treatment_period, ".pdf"),  width = 7, height = 9)


# COEF PLOT (ONLY STUDENTS WITH NON-MISSING DVS): APPENDIX ----------
models_path <- "data/final/supp/r_nonmissing_dvs/"
plots_path <- "plots/supp/r_nonmissing_dvs/"

full_models <- gen_model_df(models_path)
make_plot(full_models = full_models)

ggsave(paste0(plots_path, "ll_full_", paste(control_period, collapse = ""),
              "_", treatment_period, ".pdf"),  width = 7, height = 9)


# COEF PLOT (ONLY SCHOOLS WHERE >=75% OF STUDENTS TOOK A TEST DURING PANDEMIC): APPENDIX ----------
models_path <- "data/final/supp/r_schools_75/"
plots_path <- "plots/supp/r_schools_75/"

full_models <- gen_model_df(models_path)
make_plot(full_models = full_models)

ggsave(paste0(plots_path, "ll_full_", paste(control_period, collapse = ""),
              "_", treatment_period, ".pdf"),  width = 7, height = 9)

# COEF PLOT (ALTERNATIVE CONTROL YEARS): APPENDIX -------------------------------------------
models_path <- "data/final/supp/r_alt_control_years/"
plots_path <- "plots/supp/r_alt_control_years/"

control_periods <- list(c("20162017", "20172018"), 
                        c("20162017", "20182019"), 
                        c("20172018", "20182019"), 
                        c("20162017"), c("20172018"), c("20182019"))

treatment_period <- "20192020"

for(control_period in control_periods) {
  full_models <- gen_model_df(models_path)
  print(make_plot(full_models = full_models))
  
  ggsave(paste0(plots_path, "ll_full_", paste(control_period, collapse = ""),
                "_", treatment_period, ".pdf"),  width = 7, height = 9)
}

# COEF PLOT (ALTERNATIVE TREATMENT YEARS): APPENDIX -------------------------------------------
models_path <- "data/final/supp/p_alt_treat_years/"
plots_path <- "plots/supp/p_alt_treat_years/"

control_periods <- list(c("20172018", "20182019"), 
                        c("20162017", "20182019"), 
                        c("20162017", "20172018"))

treatment_periods <- list(c("20162017"),c("20172018"), c("20182019"))

for(i in 1:length(treatment_periods)) {
  models_path <- "data/final/supp/p_alt_treat_years/"
  plots_path <- "plots/supp/p_alt_treat_years/"
  
  control_period <- control_periods[[i]]
  treatment_period <- treatment_periods[[i]]
  
  full_models <- gen_model_df(models_path)
  print(make_plot(full_models = full_models, alt_treat_years = TRUE))
  
  ggsave(paste0(plots_path, "ll_full_", paste(control_period, collapse = ""),
                "_", treatment_period, ".pdf"),  width = 7, height = 9)
}

# RE-CLASSIFY CORRECT TREATMENT AND CONTROL YEARS
control_period <- c("20162017", "20172018", "20182019")
treatment_period <- c("20192020")

# NON-CURRICULAR CONTENT (DMT TESTS) APPENDIX ------------------------------------------------------

# COMPARISON OF EFFECT OF PANDEMIC FOR TESTS OF CURRICULAR VS NON-CURRICULAR CONTENT
models_path <- "data/final/main/"

full_models <- gen_model_df(models_path) %>%
  mutate(group = "all")

full_models_dmt <- full_models %>%
  filter(str_detect(model, "DMT")) %>%
  mutate(type = replace_na(type, "Total"), 
         label = replace_na(label, "")) %>%
  mutate(group =  "dmt")

full_models_dmt_all <- rbind(full_models_dmt, full_models %>%
                               filter(str_detect(model, "ALL")))

make_plot(full_models = full_models_dmt_all, DMT = T, dodge = 0.5)

# save this plot in new folder, since it doesnt feature in main analysis
models_path <- "data/final/supp/r_dmt/"
plots_path <- "plots/supp/r_dmt/"

ggsave(paste0(plots_path, "dmt_full_", paste(control_period, collapse = ""),
              "_", treatment_period, ".pdf"),  width = 9, height = 7.8)


# NON-CURRICULAR CONTENT (DMT TESTS), SES ONLY (FIG 4) ---------------------------------------------

# COMPARISON OF EFFECT OF PANDEMIC FOR TESTS OF CURRICULAR VS NON-CURRICULAR CONTENT
models_path <- "data/final/main/"
plots_path <- "plots/main/"

full_models <- gen_model_df(models_path) %>%
  filter(type %in% c(NA, "Total", "Parental Educ.")) %>%
  mutate(type = ifelse(type=="Parental Educ.", "Parental Education", as.character(type))) %>%
  mutate(type = factor(type, levels = c("Total", "Parental Education"))) %>%
  mutate(group = "all")

full_models_dmt <- full_models %>%
  filter(str_detect(model, "DMT")) %>%
  mutate(type = replace_na(type, "Total"), 
         label = replace_na(label, "")) %>%
  mutate(group =  "dmt")

full_models_dmt_all <- rbind(full_models_dmt, full_models %>%
                               filter(str_detect(model, "ALL")))

make_plot(full_models = full_models_dmt_all, DMT = T) +
  theme(axis.line = element_line(colour = 'black', size = 0.2),
        panel.spacing.x=unit(0.04, "lines"),
        panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
        strip.text = element_text(size = 11),
        strip.background=element_rect(fill = NA, color = "black", size = 0.8),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12))


# save
ggsave(paste0(plots_path, "ll_dmt_ses_", paste(control_period, collapse = ""),
              "_", treatment_period, ".pdf"), width = 4.5, height = 3)

# NON-CURRICULAR CONTENT (DMT TESTS), IN PREVIOUS YEARS: APPENDIX ----------------------------------

# COMPARISON OF EFFECT OF PANDEMIC FOR TESTS OF CURRICULAR VS NON-CURRICULAR CONTENT

control_periods <- list(c("20172018", "20182019"), 
                        c("20162017", "20182019"), 
                        c("20162017", "20172018"))

treatment_periods <- list(c("20162017"),c("20172018"), c("20182019"))

for(i in 1:length(treatment_periods)) {
  # load data 
  models_path <- "data/final/supp/p_alt_treat_years/"
  plots_path <- "plots/supp/p_alt_treat_years/"
  
  control_period <- control_periods[[i]]
  treatment_period <- treatment_periods[[i]]
  
  full_models <- gen_model_df(models_path) %>%
    mutate(group = "all")
  
  full_models_dmt <- full_models %>%
    filter(str_detect(model, "DMT")) %>%
    mutate(type = replace_na(type, "Total"), 
           label = replace_na(label, "")) %>%
    mutate(group =  "dmt")
  
  full_models_dmt_all <- rbind(full_models_dmt, full_models %>%
                                 filter(str_detect(model, "ALL")))
  
  
  print(make_plot(full_models = full_models_dmt_all, DMT = T, dodge = 0.5, 
                  alt_treat_years = T))
  
  # save this plot in new folder, since it is mainly focused on non-curricular content
  models_path <- "data/final/supp/r_dmt/"
  plots_path <- "plots/supp/r_dmt/"
  
  ggsave(paste0(plots_path, "dmt_full_", paste(control_period, collapse = ""),
                "_", treatment_period, ".pdf"),  width = 9, height = 7.8)
}


# SCHOOL RANDOM EFFECTS: APPENDIX ----------------------------------------------
models_path <- "data/final/supp/r_schools_ml/"
plots_path <- "plots/supp/r_schools_ml/"

df <- readRDS(paste0(models_path, "models_overall_201620172017201820182019_20192020.rds"))

plot_re <- function(df, re_type = "controls") {
  ggplot(df %>% filter(RE_model == re_type) , aes(x = School, y = Treat, color = Sig)) + geom_point(size=0.3) +
    geom_errorbar(aes(x = School, ymin = lb_treat, ymax = ub_treat), size=0.2,
                  alpha=0.4) +
    geom_hline(yintercept=0, linetype='dashed') +
    theme_bw() + scale_color_aaas() +
    labs(title=paste0("Estimated school-level treatment effects including 95% CI"),
         x="Schools", y="Estimated school-level treatment effect") + cowplot::theme_cowplot(font_size = 15) + theme(legend.position = "none")
}


ggsave(paste0(plots_path, "school_re_woc.pdf"), plot_re(df, "base"), width=10, height=7)
ggsave(paste0(plots_path, "school_re_wc.pdf"), plot_re(df, "controls"), width=10, height=7)

schools <- readRDS("data/raw/schools.rds")

base_df_school <- df %>%
  filter(RE_model == "base") %>%
  left_join(schools)

controls_df_school <- df %>%
  filter(RE_model == "controls") %>%
  left_join(schools)

## for proportion non-western
var = "school_weight"

bins = 21
gen_binned_scatter <- function(df, var = "prop_non_western", bins = 25, xlabel="x",
                               xlim = c(0, 0.8), ylim = c(-6, -1.5)) {
  df$var <- df[[var]]
  bins_var <- quantile(df$var, seq(0, 1, 1 / bins), na.rm=T)
  bins_var <- bins_var[!duplicated(bins_var)]
  
  gg <- ggplot(df, aes(y = Treat, x = var)) +
    stat_smooth(method='lm')
  gg_data <- data.frame(ggplot_build(gg)$data[[1]])
  
  return(ggplot(df, aes(y = Treat, x = var)) + stat_summary_bin(fun.y="mean", breaks = bins_var, color="black") +
           geom_smooth(method = "lm", color="black", fill = NA) + cowplot::theme_cowplot(font_size= 19.5) +
           geom_ribbon(data=gg_data, aes(x=x, ymin = ymin, ymax = ymax), fill = "NA", linetype = 2, color="black", inherit.aes = F)  +
           labs(x=xlabel, y="Estimated school-level treatment effect",
                title = "") + coord_cartesian(xlim = xlim, ylim = ylim))
}

ggsave(paste0(plots_path, "school_pnw_ALL_25_woc.pdf"),
       gen_binned_scatter(base_df_school, "prop_non_western", 25, xlabel = "Proportion non-Western",
                          xlim = c(0, 0.8), ylim = c(-6, -1.5)))

ggsave(paste0(plots_path, "school_pnw_ALL_25_wc.pdf"),
       gen_binned_scatter(controls_df_school, "prop_non_western", 25, xlabel = "Proportion non-Western",
                          xlim = c(0, 0.8), ylim = c(-6, -1.5)))

ggsave(paste0(plots_path, "school_sw_ALL_21_woc.pdf"),
       gen_binned_scatter(base_df_school, "school_weight", 21, xlabel = "School disadvantage",
                          xlim = c(18, 40), ylim = c(-6, -1.5)))

ggsave(paste0(plots_path, "school_sw_ALL_21_wc.pdf"),
       gen_binned_scatter(controls_df_school, "school_weight", 21, xlabel = "School disadvantage",
                          xlim = c(18, 40), ylim = c(-6, -1.5)))



# RE-CLASSIFY CORRECT TREATMENT AND CONTROL YEARS
control_period <- c("20162017", "20172018", "20182019")
treatment_period <- c("20192020")



# PLOT THE EFFECT OF SES ACROSS ALL SPECIFICATIONS --------------------------------------------

control_period <- c("20162017", "20172018", "20182019")
treatment_period <- c("20192020")

# Main model
models_path <- "data/final/main/"
plots_path <- "plots/main/"

models_main <- gen_model_df(models_path) %>%
  mutate(spec = "Baseline")

# Full controls 
models_path <- "data/final/supp/r_all_controls/"

models_control <- gen_model_df(models_path) %>%
  mutate(spec = "All Controls")

# Matched 
models_path <- "data/final/supp/r_matching/"

models_matched <- gen_model_df(models_path) %>%
  mutate(spec = model_type, 
         spec = ifelse(spec == "E-Balance", "Entropy Bal.", 
                       ifelse(spec == "P-Score", "Prop-Score", spec)))

# Schools with 75% Test Attendance
models_path <- "data/final/supp/r_schools_75/"

models_schools_75 <- gen_model_df(models_path) %>%
  mutate(spec = "75% Schools")

# School Fixed Effects
models_path <- "data/final/supp/r_schools_FE/"

models_schools_FE <- gen_model_df(models_path) %>%
  mutate(spec = "School FE")

# Family FE
models_path <- "data/final/supp/r_sibling_FE/"

models_sibling_FE <- gen_model_df(models_path) %>%
  mutate(spec = "Family FE")

models <- bind_rows(models_main, models_control, models_matched, models_schools_75, 
                    models_schools_FE, models_sibling_FE)

# PLOT MODEL
g1 <- models %>%
  filter(term=="treat"|str_detect(term, "full_treat")) %>%
  filter(str_detect(model, "ALL")) %>%
  filter(type %in% c("Total", "Parental Educ.")) %>%
  mutate(type = recode(type, "Parental Educ." = "Parental Education"),
         spec = factor(spec, levels = c("Baseline", "All Controls", "Entropy Bal.", "Prop-Score", 
                                        "75% Schools", "School FE", "Family FE"))) %>%
  ggplot(aes(x = reorder(label, -as.numeric(label)), y = estimate, color = spec, 
             group = spec,
             shape = spec)) +
  geom_point(size = 2.5, position = position_dodge(width=-0.8)) +
  geom_errorbar(aes(ymin = 
                      estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error), width = 0, size = 0.7, 
                position = position_dodge(width=-0.8)) +
  geom_hline(yintercept = 0, linetype = 2, color = "darkgrey") +
  facet_grid(type ~ ., scales = "free", space = "free", switch="y") +
  scale_shape_manual(values=1:length(unique(models$spec))) +
  scale_y_continuous(minor_breaks = seq(-6.5 , 1.5, 0.5), 
                     breaks = round(seq(-6, 1, 1), 2),
                     limits = c(-6.3, 1.2)) +
  cowplot::theme_cowplot(font_size = 23) +
  coord_flip() +
  scale_color_aaas() +
  scale_fill_aaas() +
  ylab("Learning loss (percentiles)") +
  xlab("") 

g1 +
  theme(panel.grid.major.x = element_line(size = 0.5, linetype = 'dotted',
                                          colour = "lightgrey"), 
        panel.grid.minor.x = element_line(size = 0.25, linetype = 'dotted',
                                          colour = "lightgrey"),
        strip.placement = "outside", 
        strip.text.y = element_text(face = "bold", hjust=0.5, vjust=0.5),
        strip.background=element_rect(fill = NA, color = "black", size = 1.5),
        legend.position = "right", 
        legend.title = element_blank(),
        panel.spacing.x=unit(0.08, "lines"),
        panel.spacing.y=unit(0.1, "lines"),
        panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
        plot.margin=grid::unit(c(0.5,0.5,0.1,0.1), "mm"))

# save plot
ggsave(paste0(plots_path, "ll_ses_", paste(control_period, collapse = ""), "_", treatment_period, ".pdf"),
       width = 9, height = 8)




# SPECIFICATION CURVES (APPENDIX) ------------------------------------------------------------------

# FUNCTIONS TO CREATE TOP AND BOTTOM PLOT OF SPECIFICATION CURVE 
make_coef_plot <- function(data, dep_var) {
  # plot specification curve
  g1 <- data %>%
    filter(str_detect(outcome, dep_var)) %>%
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
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 16),
          axis.title.y = element_text(size = 14),
          strip.background=element_rect(fill = NA, color = "black", size = 0.8))
  
  
  if(treatment_period=="20192020"){
    g1 <- g1 +
      ylim(c(-4.15, 2))
  } else if(treatment_period=="20182019"){
    g1 <- g1 + ylim(c(-4.1, 2))
  }
  
  if(str_detect(dep_var, "Maths")){g1 <- g1 +
    scale_color_manual(guide = FALSE, values = c("#EE0000FF")) +
    scale_fill_manual(guide = FALSE, values = c("#EE0000FF"))}
  if(str_detect(dep_var, "Reading")){g1 <- g1 +
    scale_color_manual(guide = FALSE, values = c("#008B45FF")) +
    scale_fill_manual(guide = FALSE, values = c("#008B45FF"))}
  if(str_detect(dep_var, "Spelling")){g1 <- g1 +
    scale_color_manual(guide = FALSE, values = c("#631879FF")) +
    scale_fill_manual(guide = FALSE, values = c("#631879FF"))}
  return(g1)
}

# BOTTOM PLOT
# Function to create a specification plot for a single category.
make_spec_plot <- function(data, category, dep_var = "Composite") {
  # category = spec_cols[1] # DEBUG
  specs <- data %>%
    filter(outcome==dep_var) %>%
    dplyr::select(h_order, category) %>%
    pivot_longer(starts_with(category), names_prefix = paste0(category, "_")) %>%
    mutate(name = factor(name, levels = rev(unique(name))))
  
  if(is.numeric(specs$value)){
    spec_plot <- ggplot(specs, aes(x = h_order, y = name, alpha = value)) +
      geom_point(shape = "|", size = 0.7) +
      scale_alpha_continuous(guide = FALSE, range = c(0, 1)) +
      scale_fill_aaas(guide = FALSE) +
      theme_bw() +
      theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(), 
            axis.text.y = element_text(size = 13),
            strip.text = element_text(size = 16))
    
    
  } else {
    spec_plot <- ggplot(specs, aes(x = h_order, y = value)) +
      geom_point(shape = "|", size = 0.7) +
      scale_alpha_continuous(guide = FALSE, range = c(0, 1)) +
      scale_fill_aaas(guide = FALSE) +
      theme_bw() +
      theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(), 
            axis.text.y = element_text(size = 13),
            strip.text = element_text(size = 16))
  }
}

# RUN ANALYSIS  ------------------------

# Specify location of data and plots destination
models_path <- "data/final/supp/r_spec_curve/"
plots_path <- "plots/supp/r_spec_curve/"

# specify periods to loop over
control_periods <- list( c("20162017", "20172018"), 
                         c("20162017", "20172018", "20182019")) 

treatment_periods <- list(c("20182019"), c("20192020"))


for(i in 1:length(control_periods)){
  
  # loop over main and placebo models
  control_period <- control_periods[[i]]
  treatment_period <- treatment_periods[[i]]
  
  # load data
  load(paste0("data/final/supp/r_spec_curve/complete_models_", 
              paste(control_period, collapse = ""), "_", treatment_period, ".Rda"))
  
  
  dep_vars <- c("Composite", "Maths", "Reading", "Spelling") 
  
  for(dv in dep_vars) {
    coef_plot_all <- make_coef_plot(complete_models, dv)
    
    if(treatment_period=="20182019") {
      dv <- paste0("Placebo (", dv, ")")
      
      # correct coding mistake in sample period from 2020 to 2019
      complete_models <- complete_models %>%
        mutate(period = gsub("2019-2020", "2018-2019", period), 
               period = gsub("2017-2020", "2017-2019", period))
    }
    
    spec_plots_all <- lapply(list(
      c("Parental Education", "Sex", "Prior Performance",
        "School Grade", "Year", "Days between tests"),
      names(complete_models[str_detect(names(complete_models), " x ")]),
      c("type"),
      c("period")), make_spec_plot, data = complete_models, dep_var = dv)
    
    combined_plot <- plot_grid(plotlist = c(list(coef_plot_all), spec_plots_all),
                               labels = c("", "Controls", "Interactions", "Fixed-Effects", "Sample Period"),
                               label_size = 14, label_fontface = "italic", vjust = 0.5, hjust = -0.1,
                               rel_heights = c(1.7, 0.6, 1.1, 0.3, 0.2), align = "v", ncol = 1)
    
    ggsave(combined_plot,
           file = paste0(plots_path, "spec_curve_", dv, "_", paste(control_period, collapse = ""),
                         "_", treatment_period, ".pdf"), width = 10, height = 12)
  }
}
