# ---
# RE SCHOOL LEVEL ANALYSIS
# ---

library(tidyverse)
library(broom)
library(texreg)
library(lme4)

#SPECIFY PATH FOR TABLES AND PLOTS
tables_path <- "tables/supp/r_schools_ml/"
plots_path <- "plots/supp/r_schools_ml/"
models_path <- "data/final/supp/r_schools_ml/"

# SPECIFY TREATMENT AND CONTROL PERIODS
control_period <- c("20162017", "20172018", "20182019")
treatment_period <- "20192020"

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

#  MULTILEVEL MODELS --------------------------------------------------------------------------
# -- Multilevel models

generate_rel_ids <- function(df, subject = "RW", min_n = 10) {
  count_schools <- count(df, school_id)
  df$rel_var <- df[, subject]
  school_df <- df %>%
    group_by(treat, school_id) %>%
    summarise(count_rw = sum(!is.na(rel_var)))
  
  treat_ids <- school_df %>%
    filter(count_rw > 10,
           treat == 1)
  control_ids <- school_df %>%
    filter(count_rw > 10,
           treat == 0)
  
  return(unique(treat_ids$school_id[treat_ids$school_id %in% control_ids$school_id]))
}

generate_ranef_df <- function(model, overall_intercept) {
  ranef_se <- arm::se.ranef(model)
  df <- data.frame(school = 1 : length(coef(model)$school_id[, 1]),
                   intercept = coef(model)$school_id[, 1],
                   treat = coef(model)$school_id[, 2],
                   se_int = ranef_se$school_id[, 1],
                   se_treat = ranef_se$school_id[, 2],
                   school_id = rownames(ranef_se$school_id))
  names(df) <- c("School", "Intercept", "Treat", "SE_Intercept", "SE_Treat", "school_id")
  
  df <- df %>%
    mutate(lb_intercept = -2 * SE_Intercept + Intercept,
           ub_intercept = 2 * SE_Intercept + Intercept,
           lb_treat = -2 * SE_Treat + Treat,
           ub_treat = 2 * SE_Treat + Treat)
  
  return(df[order(df$Intercept), ] %>%
           mutate(School = 1 : dim(df)[1],
                  Sig = ifelse((lb_intercept > overall_intercept) | (ub_intercept < overall_intercept),
                               "Excl. intercept", "Incl. intercept")))
}

## Estimate models based on outcome of interest

dvs <- c("ALL", "RW", "TBL", "SP")

## Estimate RE models without pupil-level controls (note random intercept added by default)
fit_base<- lapply(dvs, FUN = function(x) {lmer(formula(paste0(x," ~ treat + (treat | school_id) + year_s + days_between_all_s")),
                                                       data = total_df %>% filter(school_id %in% generate_rel_ids(total_df, subject = x)))})

## Estimate RE models with pupil-level controls (note random intercept added by default)
fit_controls <- lapply(dvs, FUN = function(x) {lmer(formula(paste0(x," ~ treat + (treat | school_id) + year_s + days_between_all_s + female + ses + ability")),
                                                    data = total_df %>% filter(school_id %in% generate_rel_ids(total_df, subject = x)))})
names(fit_base) <- dvs
names(fit_controls) <- dvs

screenreg(fit_base)
screenreg(fit_controls)

## Save output

texreg(fit_base,
       caption = "Overall learning loss (School RE), no controls", 
       custom.coef.map = var_names,
       center = TRUE,
       include.ci = FALSE,
       label = "table:school_re_base",
       file = paste0(tables_path, "ll_school_RE_base_", 
                     paste(control_period, collapse = ""), "_",
                     treatment_period, ".tex"))

texreg(fit_controls,
       caption = "Overall learning loss (School RE), full controls", 
       custom.coef.map = var_names,
       center = TRUE,
       include.ci = FALSE,
       label = "table:school_re_controls",
       file = paste0(tables_path, "ll_school_RE_controls_", 
                     paste(control_period, collapse = ""), "_",
                     treatment_period, ".tex"))


## Estimate and plot random effects on school level without and with controls
overall_intercept <- fixef(fit_base[[1]])[1]
ranef_df_base <- generate_ranef_df(fit_base[[1]], overall_intercept = 0)
ranef_df_base <- ranef_df_base[order(ranef_df_base$Treat), ] %>%
  mutate(School = 1 : dim(ranef_df_base)[1],
         Sig = ifelse((lb_treat > 0) | (ub_treat < 0),
                      "Excl. fixed effect", "Incl. fixed effect")) %>%
  mutate(RE_model = "base")

overall_intercept <- fixef(fit_controls[[1]])[1]
ranef_df_controls <- generate_ranef_df(fit_controls[[1]], overall_intercept = 0)
ranef_df_controls <- ranef_df_controls[order(ranef_df_controls$Treat), ] %>%
  mutate(School = 1 : dim(ranef_df_controls)[1],
         Sig = ifelse((lb_treat > 0) | (ub_treat < 0),
                      "Excl. fixed effect", "Incl. fixed effect")) %>%
  mutate(RE_model = "controls")

## Save model data for visualisation
full_df <- rbind(ranef_df_base, ranef_df_controls)
saveRDS(full_df, paste0(models_path, "models_overall_",
                        paste(control_period, collapse = ""), "_",
                        treatment_period, ".rds"))

ggplot(ranef_df_controls, aes(x = School, y = Treat, color = Sig)) + geom_point(size=0.3) +
  geom_errorbar(aes(x = School, ymin = lb_treat, ymax = ub_treat), size=0.2,
                alpha=0.4) +
  geom_hline(yintercept=overall_treat, linetype='dashed') +
  theme_bw() + scale_color_aaas() +
  labs(title=paste0("Estimated school-level treatment effects including 95% CI"),
       x="Schools", y="Estimated school-level treatment effect") + cowplot::theme_cowplot(font_size = 15) + theme(legend.position = "none")

ggsave(paste0("plots/desc/", dep_var, "/school_RE_", dep_var, "_", uni_str, "_", uni_str, "_woc.pdf"), last_plot(), width=10, height=7)

overall_intercept <- fixef(me2)[1]
overall_treat <- 0
ranef_df <- generate_ranef_df(me2, overall_intercept = 0)
ranef_df <- ranef_df[order(ranef_df$Treat), ] %>%
  mutate(School = 1 : dim(ranef_df)[1],
         Sig = ifelse((lb_treat > overall_treat) | (ub_treat < overall_treat),
                      "Excl. fixed effect", "Incl. fixed effect"))
ggplot(ranef_df, aes(x = School, y = Treat, color = Sig)) + geom_point(size=0.3) +
  geom_errorbar(aes(x = School, ymin = lb_treat, ymax = ub_treat), size=0.2,
                alpha=0.4) +
  geom_hline(yintercept=overall_treat, linetype='dashed') +
  theme_bw() + scale_color_aaas() +
  labs(title=paste0("Estimated school-level treatment effects including 95% CI"),
       x="Schools", y="Estimated school-level treatment effect") + cowplot::theme_cowplot(font_size = 15) + theme(legend.position = "none")

ggsave(paste0("plots/desc/", dep_var, "/school_RE_", dep_var, "_", uni_str, "_wc.pdf"), last_plot(), width=10, height=7)


## -- Analyze random effects on school level
schools <- readRDS("data/edit/schools.rds")
ranef_df_m1 <- generate_ranef_df(me1, overall_intercept = 0)
infer_school_m1 <- ranef_df_m1  %>%
  left_join(schools)
ranef_df_m2 <- generate_ranef_df(me2, overall_intercept = 0)
infer_school_m2 <- ranef_df_m2  %>%
  left_join(schools)

## for proportion non-western
for (bins in c(10, 25, 50)) {
  bins_pnw <- quantile(infer_school_m1$prop_non_western, seq(0, 1, 1 / bins), na.rm=T)
  
  gg <- ggplot(infer_school_m1, aes(y = Treat, x = prop_non_western)) +
    stat_smooth(method='lm')
  gg_data <- data.frame(ggplot_build(gg)$data[[1]])
  
  ggplot(infer_school_m1, aes(y = Treat, x = prop_non_western)) + stat_summary_bin(fun.y="mean", breaks = bins_pnw, color=plot_color) +
    geom_smooth(method = "lm", color=plot_color, fill = NA) + cowplot::theme_cowplot(font_size= 19.5) +
    geom_ribbon(data=gg_data, aes(x=x, ymin = ymin, ymax = ymax), fill = "NA", linetype = 2, color="black", inherit.aes = F)  +
    labs(x="Neighborhood proportion of inhabitants with non-western background", y="Estimated school-level treatment effect",
         title = "") + coord_cartesian(xlim = c(0, 0.8), ylim = c(-6, -1.5))

  ggsave(paste0("plots/desc/", dep_var, "/school_pnw_no_scatter_", dep_var, "_", uni_str, "_", bins, "_woc.pdf"), last_plot(), width=10, height=7)
  
  gg <- ggplot(infer_school_m2, aes(y = Treat, x = prop_non_western)) +
    stat_smooth(method='lm')
  gg_data <- data.frame(ggplot_build(gg)$data[[1]])
  
  bins_pnw <- quantile(infer_school_m2$prop_non_western, seq(0, 1, 1 / bins), na.rm=T)
  ggplot(infer_school_m2, aes(y = Treat, x = prop_non_western)) + stat_summary_bin(fun.y="mean", breaks = bins_pnw, color=plot_color) +
    geom_smooth(method = "lm", color = plot_color, fill = NA) + cowplot::theme_cowplot(font_size= 19.5) +
    geom_ribbon(data=gg_data, aes(x=x, ymin = ymin, ymax = ymax), fill = "NA", linetype = 2, color="black", inherit.aes = F)  +
    labs(x="Neighborhood proportion of inhabitants with non-western background", y="Estimated school-level treatment effect", title = "") +
    coord_cartesian(xlim = c(0, 0.8), ylim = c(-6, -1.5))
  ggsave(paste0("plots/desc/", dep_var, "/school_pnw_no_scatter_", dep_var, "_", uni_str, "_",  bins, "_wc.pdf"), last_plot(), width=10, height=7)
}
## for proportion school_weight
for (bins in c(10, 15, 25, 50)) {
  bins_sw <- unique(quantile(infer_school_m1$school_weight, seq(0, 1, 1 / bins), na.rm=T))
  bins <- length(bins_sw)
  
  gg <- ggplot(infer_school_m1, aes(y = Treat, x = school_weight)) +
    stat_smooth(method='lm')
  gg_data <- data.frame(ggplot_build(gg)$data[[1]])
  
  ggplot(infer_school_m1, aes(y = Treat, x = school_weight)) + stat_summary_bin(fun.y="mean", breaks = bins_sw, color=plot_color) +
    geom_smooth(method = "lm", color = plot_color, fill = NA) +
    geom_ribbon(data=gg_data, aes(x=x, ymin = ymin, ymax = ymax), fill = "NA", linetype = 2, color="black", inherit.aes = F)  +
    cowplot::theme_cowplot(font_size= 19.5) + labs(x="School indicator of SES (higher school weight indicates lower SES)", y="Estimated school-level treatment effect",
                                    title = "") + coord_cartesian(xlim = c(18, 40), ylim = c(-6, -1.5))
  ggsave(paste0("plots/desc/", dep_var, "/school_sw_no_scatter_", dep_var, "_", uni_str, "_", bins, "_woc.pdf"), last_plot(), width=10, height=7)
  
  gg <- ggplot(infer_school_m2, aes(y = Treat, x = school_weight)) +
    stat_smooth(method='lm')
  gg_data <- data.frame(ggplot_build(gg)$data[[1]])
  
  ggplot(infer_school_m2, aes(y = Treat, x = school_weight)) + stat_summary_bin(fun.y="mean", breaks = bins_sw, color=plot_color) +
    geom_smooth(method = "lm", color = plot_color, fill=NA) +
    geom_ribbon(data=gg_data, aes(x=x, ymin = ymin, ymax = ymax), fill = "NA", linetype = 2, color="black", inherit.aes = F)  +
    cowplot::theme_cowplot(font_size= 19.5) + labs(x="School indicator of SES (higher school weight indicates lower SES)", y="Estimated school-level treatment effect",
                                    title = "") +
    coord_cartesian(xlim = c(18, 40), ylim = c(-6, -1.5))
  ggsave(paste0("plots/desc/", dep_var, "/school_sw_no_scatter_", dep_var, "_", uni_str, "_", bins, "_wc.pdf"), last_plot(), width=10, height=7)
}
## for neighborhood deprivation
for (bins in c(5, 6, 7, 8, 9, 20, 50)) {
  bins_kl <- unique(quantile(infer_school_m1$KL18, seq(0, 1, 1 / bins), na.rm=T))
  bins <- length(bins_kl)
  
  gg <- ggplot(infer_school_m1, aes(y = Treat, x = KL18)) +
    stat_smooth(method='lm')
  gg_data <- data.frame(ggplot_build(gg)$data[[1]])
  
  ggplot() + stat_summary_bin(data = infer_school_m1, fun.y="mean", breaks = bins_kl, color=plot_color, aes(y = Treat, x = KL18)) +
    geom_smooth(data = infer_school_m1, aes(y = Treat, x = KL18),method = "lm", color = plot_color, fill=NA) +
    geom_ribbon(data=gg_data, aes(x=x, ymin = ymin, ymax = ymax), fill = "NA", linetype = 2, color="black")  +
    cowplot::theme_cowplot(font_size= 17) + labs(x="Neighborhood indicator of social deprivation (higher value indicate less deprivation)", y="Estimated school-level treatment effect",
                                    title = "")

  ggsave(paste0("plots/desc/", dep_var, "/school_kl_no_scatter_", dep_var, "_", uni_str, "_", bins, "_woc.pdf"), last_plot(), width=10, height=7)
  
  gg <- ggplot(infer_school_m2, aes(y = Treat, x = KL18)) +
    stat_smooth(method='lm')
  gg_data <- data.frame(ggplot_build(gg)$data[[1]])
  
  ggplot(infer_school_m2, aes(y = Treat, x = KL18)) + stat_summary_bin(fun.y="mean", breaks = bins_kl, color=plot_color) +
    geom_smooth(method = "lm", color = plot_color, fill = NA) +
    geom_ribbon(data=gg_data, aes(x=x, ymin = ymin, ymax = ymax), fill = "NA", linetype = 2, color="black", inherit.aes = F)  +
    cowplot::theme_cowplot(font_size= 17) + labs(x="Neighborhood indicator of social deprivation (higher values indicate less deprivation)", y="Estimated school-level treatment effect",
                                    title = "")
  ggsave(paste0("plots/desc/", dep_var, "/school_kl_no_scatter_", dep_var, "_", uni_str, "_", bins, "_wc.pdf"), last_plot(), width=10, height=7)
}

school_group_m1 <- infer_school_m1 %>%
  group_by(DEN) %>%
  summarise(mean = mean(Treat, na.rm=T),
            sd = sd(Treat, na.rm=T),
            den = unique(DEN),
            n = n())
ggplot(school_group_m1, aes(y = mean, x = DEN)) +
  geom_bar(stat ="identity") + geom_errorbar(aes(ymin = mean - 2*sd / sqrt(n), ymax = mean + 2*sd / sqrt(n)), size = 0.2, width=0.4) + cowplot::theme_cowplot() +
  labs(x="School denomination", y="Estimated school-level treatment effect",
       title = "School estimated treatment effect by school denomination")
ggsave(filename=paste0("plots/desc/", dep_var, "/school_den_effects_", dep_var, "_", uni_str, "_woc.pdf"), last_plot(), width=10, height=7)
school_group_m2 <- infer_school_m2 %>%
  group_by(DEN) %>%
  summarise(mean = mean(Treat, na.rm=T),
            sd = sd(Treat, na.rm=T),
            den = unique(DEN),
            n = n())
ggplot(school_group_m2, aes(y = mean, x = DEN)) +
  geom_bar(stat ="identity") + geom_errorbar(aes(ymin = mean - 2*sd / sqrt(n), ymax = mean + 2*sd / sqrt(n)), size = 0.2, width=0.4) + cowplot::theme_cowplot() +
  labs(x="School denomination", y="Estimated school-level treatment effect",
       title = "School estimated treatment effect by school denomination")
ggsave(filename=paste0("plots/desc/", dep_var, "/school_den_effects_", dep_var, "_", uni_str, "_woc.pdf"), last_plot(), width=10, height=7)

## -- Other stuff
infer_school %>%
  filter(!is.na(SWEIGHT)) %>%
  group_by(SWEIGHT) %>%
  summarise(mean_treat = mean(Treat, na.rm=T),
            sd_treat = sd(Treat, na.rm=T))
infer_school %>%
  filter(!is.na(PNW)) %>%
  group_by(PNW) %>%
  summarise(mean_treat = mean(Treat, na.rm=T),
            sd_treat = sd(Treat, na.rm=T))
infer_school %>%
  filter(!is.na(DEN)) %>%
  group_by(DEN) %>%
  summarise(mean_treat = mean(Treat, na.rm=T),
            sd_treat = sd(Treat, na.rm=T))
infer_school %>%
  filter(!is.na(DEN)) %>%
  group_by(DEN) %>%
  summarise(mean_treat = mean(Treat, na.rm=T),
            sd_treat = sd(Treat, na.rm=T))
lm1 <- lm(RW ~ -1 + school_id + school_id * treat - treat, data = total_df %>% filter(school_id %in% rel_ids))
lm2 <- lm(RW ~ 1 + treat, data = total_df %>% filter(school_id %in% rel_ids))
overall_intercept <- summary(lm2)$coefficients[2, 1]
generate_fe_df <- function(model, overall_intercept) {
  df <- data.frame(School = 1:length(summary(model)$coefficients[, 1]),
                   Coef = summary(model)$coefficients[, 1],
                   SE = summary(model)$coefficients[, 2],
                   Type = ifelse(grepl("treat", rownames(summary(model)$coefficients)),
                                 "Treat", "Intercept"))
  
  names(df) <- c("School", "Intercept", "SE", "Type")
  
  df <- df %>%
    mutate(lb_intercept = -2 * SE + Intercept,
           ub_intercept = 2 * SE + Intercept)
  
  return(df[order(df$Intercept), ] %>%
           mutate(School = 1 : dim(df)[1],
                  Sig = ifelse((lb_intercept > overall_intercept) | (ub_intercept < overall_intercept),
                               "Excl. intercept", "Incl. intercept")))
}
fe_df <- generate_fe_df(model, overall_intercept = 0) %>%
  filter(Type == "Treat")
df_test <- data.frame(coef = summary(model)$coefficients[, 1])
df_test %>%
  filter(grepl("treat", rownames(df_test)))
rownames(df_test)[grepl("treat", rownames(df_test))]
library(reshape2)
means <- total_df %>% filter(school_id %in% rel_ids) %>%
  filter(school_id %in% rel_ids) %>%
  filter(class_year == 7) %>%
  group_by(treat, school_id) %>%
  summarise(mean_rw = mean(RW, na.rm=T)) %>%
  pivot_wider(id_cols = school_id, names_from = treat, values_from = mean_rw) %>%
  mutate(diff = `1` - `0`)
ggplot(means) + geom_histogram(aes(x = diff))
mean(fe_df$Intercept)
check <- summary(model)$coefficients %>%
  as.data.frame() %>%
  filter(grepl("treat", rownames(summary(model)$coefficients)))
summary(check$Estimate)
ggplot(check) + geom_density(aes(x = Estimate))
ggplot(fe_df, aes(x = School, y = Intercept, color = Sig)) + geom_point(size=0.3) +
  geom_errorbar(aes(x = School, ymin = lb_intercept, ymax = ub_intercept), size=0.2,
                alpha=0.4) +
  geom_hline(yintercept=overall_intercept, linetype='dashed') +
  theme_bw() + scale_color_brewer(palette = "Set1") +
  labs(title="Estimated treatment effect (school level), including 95% CI",
       x="School", y="Estimate")
