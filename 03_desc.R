# ---
# GENERATE DESCRIPTIVE STATISTICS / PLOTS
# ---

library(tidyverse)
library(ggsci)
library(gridExtra)
library(cowplot)

# Test data
tests <- read_rds("data/edit/tests_relevant.rds")
schools <- readRDS("data/raw/schools.rds")
pupils_raw <- readRDS("data/raw/pupil4.rds") %>%
  mutate(school_id = as.character(school_id))

analysis_df <- readRDS("data/edit/analysis.rds") %>%
  mutate(treat = ifelse(year=="20192020", 1, 0)) %>%
  mutate(school_id = factor(school_id))

## TABLES

# SUMMARY TABLE ----------------------------------------------------------------

labs <- data.frame(ALL = '\\Delta Composite',
                   RW = '\\Delta Maths',
                   TBL = '\\Delta Reading', 
                   SP = '\\Delta Spelling', 
                   days_between_all = "Days between tests", 
                   female = "Sex", 
                   class_year = "School Grade", 
                   ses = "Parental Education", 
                   ability = "Prior Performance", 
                   school_weight = "School Disadvantage", 
                   DEN = "School Denomination",
                   prop_non_western = "% Non-Western")

analysis_df %>% 
  filter(!is.na(female),
         !is.na(ses),
         !is.na(days_between_all),
         !is.na(year),
         !is.na(ability)) %>%
  dplyr::select(ALL, RW, TBL, SP, treat, days_between_all, ses, female, ability,
                class_year, school_weight, DEN, prop_non_western) %>%
  mutate(treat = ifelse(treat ==0, "Control", ifelse(treat==1, "Treated", NA)), 
         female = ifelse(female == 0, "Male", "Female"), 
         class_year = factor(class_year, levels=c('4','5','6', '7'), 
                                labels = c("Age 8", "Age 9", "Age 10", "Age 11"))) %>%
  vtable::sumtable(group = 'treat', 
                   group.test = list(star.cutoffs = c(0.001, 0.01, 0.05), 
                                     format = '${pval}^{{stars}}$ ({name})'), 
                   digits = 2, factor.percent = FALSE, factor.counts = FALSE, 
                   labels = labs, out = "latex", file = "tables/desc/summary_tab.tex")

## ABILTY BY SUB-GROUP

analysis_df$E_ALL <- rowMeans(analysis_df[, c("E_RW", "E_SP", "E_TBL")], na.rm = T)
analysis_df$M_ALL <- rowMeans(analysis_df[, c("M_RW", "M_SP", "M_TBL")], na.rm = T)

missingness_df <- analysis_df$family_id

gen_missing <- function(df) {
  n_obs <- dim(df)[1]
  
  return(df %>%
           group_by(year, class_year) %>%
           summarise(na_female = mean(is.na(female)),
                     na_ses = mean(is.na(ses)),
                     # na_class_year = mean(is.na(class_year)),
                     na_ability = mean(is.na(ability)),
                     na_school = mean(is.na(school_id)),
                     na_school_disadvantage = mean(is.na(school_weight)),
                     na_school_denomination = mean(is.na(DEN)),
                     na_school_nonwestern = mean(is.na(prop_non_western)),
                     na_family = mean(is.na(family_id)),
                     na_days_between_tests = mean(is.na(days_between_all)),
                     na_M_ALL = mean(is.na(M_ALL)),
                     na_E_ALL = mean(is.na(E_ALL)),
                     na_ALL = mean(is.na(ALL)),
                     na_M_RW = mean(is.na(M_RW)),
                     na_E_RW = mean(is.na(E_RW)),
                     na_RW = mean(is.na(RW)),
                     na_M_TBL = mean(is.na(M_TBL)),
                     na_E_TBL = mean(is.na(E_TBL)),
                     na_TBL = mean(is.na(TBL)),
                     na_M_SP = mean(is.na(M_SP)),
                     na_E_SP = mean(is.na(E_SP)),
                     na_SP = mean(is.na(SP)),
                     na_M_DMT = mean(is.na(M_DMT)),
                     na_E_DMT = mean(is.na(E_DMT)),
                     na_DMT = mean(is.na(DMT)),
                     n = n()) %>%
           t())
}

all <- gen_missing(analysis_df)
df_all <- all %>% as.data.frame() %>%
  mutate(var = rownames(all)) %>%
  select(var, everything())
writexl::write_xlsx(df_all,
                    "tables/desc/FULL_MISSINGNESS_TABLE.xlsx")


# missingness by test
analysis_df %>%
  filter(!is.na(E_RW)) %>%
  summarise(na_ses = sum(is.na(ses)) / n(),
            na_female = sum(is.na(female)) / n(),
            na_class_year = sum(is.na(class_year)) / n(),
            na_ability = sum(is.na(ability)) / n())



## GENERATE M ABILITY BY SUBGROUPS

generate_cross <- function(df) {
  cross_df <- df %>%
    filter(!is.na(E_ALL),
           !is.na(M_ALL),
           !is.na(ability),
           !is.na(ALL),
           !is.na(ses),
           !is.na(female),
           !is.na(ability))
  
  cor_df <- as.data.frame(cor(cross_df[, c("raw_ability", "M_ALL", "E_ALL", "ALL")],
                              method = "pearson", use = "complete.obs"))
  cor_df <- rbind(cor_df, c(dim(cross_df)[1], NA, NA, NA))
  rownames(cor_df)[5] <- "N"
  return(cor_df)
}
write.csv(generate_cross(analysis_df), "tables/desc/E_M_ALL_ABILTY_CORRELATIONS_all.csv")
write.csv(generate_cross(analysis_df %>% filter(treat == 0)),
          "tables/desc/E_M_ALL_ABILTY_CORRELATIONS_control.csv")
write.csv(generate_cross(analysis_df %>% filter(treat == 1)),
          "tables/desc/E_M_ALL_ABILTY_CORRELATIONS_treat.csv")

gen_ability <- function(df, var = "ses", no_group = F) {
  
  if (no_group) {
    df$grouping_var <- "all"
    var = "all"
  } else {
    df$grouping_var <- df[[var]]  
  }
  
  return(df %>%
           filter(!is.na(grouping_var)) %>%
           dplyr::select(grouping_var, M_RW, E_RW, RW,
                         M_TBL, E_TBL, TBL,
                         M_SP, E_SP, SP,
                         M_DMT, E_DMT, DMT,
                         M_ALL, E_ALL, ALL) %>%
           group_by(grouping_var) %>%
           summarise_all(list(mean = mean), na.rm = TRUE) %>%
           mutate(var = paste0(var, "_", grouping_var)) %>%
           dplyr::select(-grouping_var))
}

complete_desc <- function(df, year_subset) {
  df <- df %>% filter(year == year_subset)
  return(rbind(c(year_subset, rep(NA, 16)),
               gen_ability(df, "ses"),
               gen_ability(df, "female"),
               gen_ability(df, "ability"),
               gen_ability(df, no_group = T)) %>%
           as.data.frame() %>%
           select(var, everything()))
}

full_test_desc <- 
  rbind(complete_desc(analysis_df, year_subset = "20162017"),
        complete_desc(analysis_df, year_subset = "20172018"),
        complete_desc(analysis_df, year_subset = "20182019"),
        complete_desc(analysis_df, year_subset = "20192020"))

write.csv(rbind(gen_m_ability(analysis_df, "ses"),
                gen_m_ability(analysis_df, "female"),
                gen_m_ability(analysis_df, "ability"),
                gen_m_ability(analysis_df, no_group = T),
                c("N", dim(analysis_df)[1], NA, NA, NA, NA, NA, NA, NA)),
          "tables/desc/M_PERF_BY_CHAR.csv")

writexl::write_xlsx(full_test_desc, "tables/desc/FULL_TEST_DESC.xlsx")

df <- analysis_df

## Lost to follow-up
gen_row <- function(df, treat_bin = 1, var = "female") {
  df$var <- df[[var]]
  df <- df %>%
   filter(treat == treat_bin) %>%
   filter(!is.na(var)) %>%
   group_by(var) %>%
   summarise(n_M_RW = sum(!is.na(M_RW)),
             n_E_RW = sum(!is.na(E_RW)),
             n_ANY_RW = sum(!is.na(M_RW) | !is.na(E_RW)),
             n_M_TBL = sum(!is.na(M_TBL)),
             n_E_TBL = sum(!is.na(E_TBL)),
             n_ANY_TBL = sum(!is.na(M_TBL) | !is.na(E_TBL)),
             n_M_SP = sum(!is.na(M_SP)),
             n_E_SP = sum(!is.na(E_SP)),
             n_ANY_SP = sum(!is.na(M_SP) | !is.na(E_SP)),
             n_M_ALL = sum(!is.na(M_ALL)),
             n_E_ALL = sum(!is.na(E_ALL)),
             n_ANY_ALL = sum(!is.na(M_ALL) | !is.na(E_ALL))) %>%
    ungroup()
  return(df)
}

df_treat <- rbind(gen_row(analysis_df, treat_bin = 1, var = "treat"),
                  gen_row(analysis_df, treat_bin = 1, var = "female"),
                  gen_row(analysis_df, treat_bin = 1, var = "ability"),
                  gen_row(analysis_df, treat_bin = 1, var = "class_year"))
df_treat$var <- c("All", "Boys", "Girls", "Ability: Top", "Ability: Middle", "Ability: Low",
                  "Age: 8", "Age: 9", "Age: 10", "Age: 11")

df_control <- rbind(gen_row(analysis_df, treat_bin = 0, var = "treat"),
                    gen_row(analysis_df, treat_bin = 0, var = "female"),
                    gen_row(analysis_df, treat_bin = 0, var = "ability"),
                    gen_row(analysis_df, treat_bin = 0, var = "class_year"))
df_control$var <- c("All", "Boys", "Girls", "Ability: Top", "Ability: Middle", "Ability: Low",
                    "Age: 8", "Age: 9", "Age: 10", "Age: 11")

write.csv(df_treat, "tables/desc/LOSS_TO_FOLLOW_UP_TREAT.csv")
write.csv(df_control, "tables/desc/LOSS_TO_FOLLOW_UP_CONTROL.csv")

# AVERAGE TEST SCORES M, E PER YEAR

analysis_df %>%
  select(starts_with("M_"), starts_with("E_"), RW, SP, TBL, DMT, ALL, year) %>%
  group_by(year) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  write.csv("tables/desc/MEAN_SCORES_PER_YEAR_AND_PERIOD_alt.csv")

## PLOTS

# AVERAGE TEST SCORE PER YEAR --------------------------------------------------
df <- readxl::read_xlsx("tables/desc/FULL_TEST_DESC.xlsx") %>%
  filter(var == "all_all") %>%
  mutate(year = c("2017", "2018", "2019", "2020")) %>%
  reshape2::melt(id.vars = c("var", "year")) %>%
  mutate(type = gsub("M_|E_", "", variable),
         period = ifelse(grepl("M_", variable), "Middle of year test",
                         ifelse(grepl("E_", variable), "End of year test", "Difference in scores")), 
         period = factor(period, levels = c("Middle of year test", "End of year yest", "Difference in scores")))

M_E <- ggplot(df %>% filter(period != "Difference in scores" & !str_detect(type, "DMT")), 
              aes(x = year, y = as.numeric(value), color = type, group = type)) + 
  geom_point(size = 2.5) +
  stat_summary(geom="line") +
  facet_wrap(~period, ncol = 1) + 
  ylim(49, 53.5) +
  ylab("Average test score") +
  cowplot::theme_cowplot(font_size = 26) + ggplot2::theme(legend.position = "top") +
  scale_color_aaas(name = "Test type", labels = c("Composite", "Maths", "Spelling", "Reading")) +
  scale_size_manual(values=c(0.75, 0.75, 0.75, 1.1), guide = "none") +
  scale_linetype_manual(values=c(2, 2, 2, 1), guide = "none") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        strip.text = element_text(face = "bold", hjust=0.5, vjust=0.5),
        strip.background=element_rect(fill = NA, color = "black", size = 1.2),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"), 
        plot.margin=grid::unit(c(0.5,0.5,0.1,0.1), "mm")) + background_grid(major="y", minor="y")

Delta <- ggplot(df %>% filter(period == "Difference in scores" & !str_detect(type, "DMT")), 
                aes(x = year, y = as.numeric(value), color = type, group = type)) + 
  geom_point(size = 2.5) +
  stat_summary(geom="line") +
  ylim(-2.0, 2.0) +
  ylab("Difference between first and second test") +
  cowplot::theme_cowplot(font_size = 26) + ggplot2::theme(legend.position = "top") +
  scale_color_aaas(name = "Test type", labels =  c("Composite", "Maths", "Spelling", "Reading")) +
  scale_size_manual(values=c(1, 1, 1, 1.3), guide = "none") +
  scale_linetype_manual(values=c(2, 2, 2, 1), guide = "none") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        strip.text = element_text(face = "bold", hjust=0.5, vjust=0.5),
        strip.background=element_rect(fill = NA, color = "black", size = 1.2),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"), 
        plot.margin=grid::unit(c(0.5,0.5,0.1,0.1), "mm")) + background_grid(major="y", minor="y")

M_E
ggsave(filename = "plots/desc/M_E_2017-2020.pdf",
       last_plot(), width = 15, height = 8)

Delta
ggsave(filename = "plots/desc/delta_2017-2020.pdf",
       last_plot(), width = 15, height = 8)

# NUMBER OF TESTS ACROSS YEARS -------------------------------------------------

# set year of date to 2020 to allow consistent plotting
tests_align <- tests %>%
  mutate(date_taken = as.Date(gsub("\\d{4}", "2020", date_taken))) %>%
  filter(date_taken <= as.Date("2020-09-01"))  # cap the graph at the end of the school year

ggplot(tests_align) +  # scale number of tests taken on a 0 to 1 scale
  geom_density(aes(y = ..scaled.., x = date_taken, color = as.factor(school_year),
                   size = as.factor(school_year), linetype = as.factor(school_year))) +
  labs(y = "Weekly tests taken relative to annual maximum", x = "", title = "") +
  geom_text(data = data.frame(x = as.Date("2020-03-15"), y = 0.890,
                              label = "Schools close"),
            aes(x, y, label = label), angle = 270, vjust = -1, size = 6) +
  geom_text(data = data.frame(x = as.Date("2020-05-25"), y = 0.860,
                              label = "Schools re-open"),
            aes(x, y, label = label), angle = 270, vjust = -1, size = 6) +
  geom_vline(xintercept = as.Date("2020-03-15"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-05-25"), linetype = "dashed") +
  xlim(as.Date("2020-01-01"), as.Date("2020-08-01")) +
  scale_x_date(date_breaks = "months" , date_labels = "%d-%b") +
  cowplot::theme_cowplot(font_size = 19) + ggplot2::theme(legend.position = "top") +
  scale_color_aaas(name = "", labels = c("2017", "2018", "2019", "2020")) +
  scale_size_manual(values=c(0.75, 0.75, 0.75, 1.1), guide = "none") +
  scale_linetype_manual(values=c(2, 2, 2, 1), guide = "none")

ggsave(filename = "plots/desc/date_taken_all_years_rel.pdf",
       last_plot(), width = 15, height = 8)

# DESCRIPTIVE PLOT OF Y  -------------------------------------------------------

treat_melt <- analysis_df %>%
  dplyr::select(pupil_id, RW, TBL, SP, year, ALL) %>%
  reshape2::melt(id.vars = c("pupil_id", "year"), measure.vars = c("RW", "TBL", "SP", "ALL")) %>%  # reshape long
  mutate(variable = ifelse(variable=="ALL", "Composite",
                           ifelse(variable=="RW", "Maths",
                                  ifelse(variable=="TBL", "Reading",
                                         ifelse(variable=="SP", "Spelling", NA)))),  # rename test names
         year = factor(gsub("^\\d{4}", "", year), c("2017", "2018", "2019", "2020")))  # only use latter year

ggplot() +
  geom_density(data = treat_melt, aes(x = value, color = as.factor(year),
                                      linetype = as.factor(year),
                                      size = as.factor(year))) +
  facet_wrap(~variable, ncol = 1) +
  labs(y = "Density",
       x = "Difference between first and second test") +
  cowplot::theme_cowplot(font_size = 26) +
  scale_color_aaas(name = "Year", labels = c("2017", "2018", "2019", "2020")) +
  scale_size_manual(values=c(0.75, 0.75, 0.75, 1.1), guide = "none") +
  scale_linetype_manual(values=c(2, 2, 2, 1), guide = "none") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold", hjust=0.5, vjust=0.5),
        strip.background=element_rect(fill = NA, color = "black", size = 1.2),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"), 
        plot.margin=grid::unit(c(0.5,0.5,0.1,0.1), "mm")) +
  scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30),
                     labels = c("-30", "-20", "-10", "0", "10", "20", "30"),
                     limits = c(-30, 30))

ggsave(filename = "plots/desc/y_descriptives.pdf",
       last_plot(), width = 10, height = 13)

# SAMPLE REPRESENTATION --------------------------------------------------------
# below script is generated internally by Data Provider to evaluate in-sample
# school level information with population CBS data from 2018-2019

## SCHOOL SIZE
# calculate size of schools in 2018 by counting unique pupils
school_sizes <- pupils_raw %>%
  filter(school_year == "2018") %>%
  filter(!duplicated(pupil_id)) %>%
  filter(!is.na(school_id)) %>%
  count(school_id)

# categorize into bins used by CBS
school_df <- schools %>%
  left_join(school_sizes) %>%
  filter(!is.na(n)) %>%
  mutate(size = ifelse(n < 101, "<=100",
                       ifelse(n < 201, "101-200",
                              ifelse(n < 501, "201-500","500+")
                       )
  )) %>%
  select(-n)

# calculate proportion of schools in each bin
size <- count(school_df, size, wt = NULL) %>%
  mutate(prop = n / dim(school_df)[1]) %>%
  dplyr::select(-n)

# include population-level proprtions of each bin (CBS 2018-2019)
size$NL <- c(0.17, 0.33, 0.45, 0.05)
names(size) <- c("Var", "Sample", "Population")


## SCHOOL DENOMINATION
denom <- school_df %>%
  count(DEN) %>%
  mutate(prop = n / dim(school_df)[1]) %>%
  dplyr::select(-n)

# include population-level proprtions of each bin (CBS 2018-2019)
denom$NL <- c(0.318 + 0.296, 0.081, 0.305)
names(denom) <- c("Var", "Sample", "Population")
 

## SCHOOL URBANITY
ste <- school_df %>%
  mutate(ste_mvs = abs(ste_mvs - 6)) %>%
  count(ste_mvs) %>%
  mutate(pop = n / dim(school_df)[1]) %>%
  dplyr::select(-n)

# include population-level proprtions of each bin (CBS 2018-2019)
ste$NL <- c(0.077, 0.214, 0.168, 0.309, 0.233)
names(ste) <- c("Var", "Sample", "Population")


## PUPIL PARENTAL EDUCATION
pupil_pe <- pupils_raw %>%
  filter(school_year == 2019) %>%
  filter(!is.na(pupil_inspection_weight)) %>%
  filter(!duplicated(pupil_id)) %>%
  group_by(pupil_inspection_weight) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  dplyr::select(-n) %>%
  filter(pupil_inspection_weight != 0)

pupil_pe$NL <- c(0.0438, 0.0440)
names(pupil_pe) <- c("Var", "Sample", "Population")

comparison <- rbind(size, denom, ste, pupil_pe)
comparison$Type <- c(rep("Size", 4), rep("Denomination", 3),
                     rep("Urbanity", 5), rep("Parental education", 2))

comp_melt <- comparison %>%
  reshape2::melt(id.vars=c("Var", "Type")) %>%
  mutate(value = as.numeric(value))

order_c <- c("<=100", "101-200", "201-500", "500+",
             "Christian", "Other", "Public", 1, 2, 3, 4, 5, "0.3", "1.2")


own_theme <- cowplot::theme_cowplot(font_size = 18)+
  ggplot2::theme(legend.position = "top")

## SCHOOL WEIGHT
sw <- school_df %>%
  count(school_weight) %>%
  mutate(pop = n / dim(school_df)[1]) %>%
  dplyr::select(-n)

cbs_sw <- read.csv2("data/raw/CBS_school_weights.csv")
#names(cbs_sw) <- gsub("?..", "", names(cbs_sw))
sw_df <- rbind(data.frame(sample = "Population",
                          sw = cbs_sw$pop_school_weight),
               data.frame(sample = "Sample",
                          sw = school_df[, "school_weight"]))


## NON-IMMIGRANT 
pnw <- school_df %>%
  count(prop_non_western) %>%
  mutate(pop = n / dim(school_df)[1]) %>%
  dplyr::select(-n)

## SCHOOL-NEIGHBOURHOOD PROPORTION NON-WESTERN
cbs_pnw <- read.csv2("data/raw/CBS_prop_nw.csv", dec = ",") %>%
  mutate(prop_nw = as.numeric(prop_nw),
         g_afs_sc = as.numeric(g_afs_sc)) %>%
  # only use neighbourhoods with sufficient size who have a school within 1km
  filter(g_afs_sc <= 1) %>%
  filter(a_inw >= 1000)

pnw_df <- rbind(data.frame(sample = "Population",
                           pnw = cbs_pnw$prop_nw),
                data.frame(sample = "Sample",
                           pnw = school_df[, "prop_non_western"]))

plot_nw <- ggplot(data = pnw_df %>% filter(pnw <= 0.4), 
       aes(x = pnw*100, color = sample, fill = sample)) +
  geom_density(size = 1) +
  scale_color_manual(values = pal_aaas()(2)[2:1], name = "") +
  scale_fill_manual(values = pal_aaas(alpha = 0.3)(2)[2:1], name = "") +
  labs(x="% non-Western", y="Density", title="Share non-Western") +
  own_theme +
  background_grid(major="y", minor="y") +
  ylim(0, 0.13)


plot_sw <- ggplot(data = sw_df, aes(x = sw, color = sample, fill = sample)) +
  geom_density(size = 1) +
  scale_color_manual(values = pal_aaas()(2)[2:1], name = "") +
  scale_fill_manual(values = pal_aaas(alpha = 0.3)(2)[2:1], name = "") +
  labs(x="School disadvantage (higher = more disadvantaged)", y="Density", title="School disadvantage") + own_theme +
  background_grid(major="y", minor="y") +
  ylim(0, 0.13)


plot_size <- ggplot(comp_melt, 
                    aes(x=value, y=factor(Var, levels=order_c), fill=variable)) +
  geom_bar(data=comp_melt %>% filter(Type == "Size"), stat="identity", position="dodge",
           color="black") +
  geom_text(data=comp_melt %>% filter(Type == "Size"), 
            aes(label=formatC(value, format="f", digits=2)),
            position=position_dodge(width=0.9), color="black", vjust=-0.3, size=4) +
  coord_flip() +   
  scale_fill_aaas(name = "") +
  scale_y_discrete(labels=c("Less than\n100 pupils", "101-200\npupils", "201-500\npupils", "500+\npupils")) +
  labs(x="Proportion", y="", title="School size") + 
  own_theme + 
  xlim(0, 0.7) + 
  background_grid(major="y", minor="y")

plot_denom <- ggplot(comp_melt, aes(x=value, y=factor(Var, levels=order_c), fill=variable)) +
  geom_bar(data=comp_melt %>% filter(Type == "Denomination"), stat="identity", position="dodge",
           color="black") +
  geom_text(data=comp_melt %>% filter(Type == "Denomination"), 
            aes(label=formatC(value, format="f", digits=2)),
            position=position_dodge(width=0.9), color="black", vjust=-0.3, size=4) +
  coord_flip() + 
  scale_fill_aaas(name = "School Denomination") +
  scale_y_discrete(labels=c("Christian", "Other", "Public")) +
  labs(y="", x="", title = "School denomination") + 
  own_theme + 
  xlim(0, 0.7) + 
  background_grid(major="y", minor="y")

plot_urb <- ggplot(comp_melt, aes(x=value, y=factor(Var, levels=order_c), fill=variable)) +
  geom_bar(data=comp_melt %>% filter(Type == "Urbanity"), stat="identity", position="dodge",
           color="black") +
  geom_text(data=comp_melt %>% filter(Type == "Urbanity"), aes(label=formatC(value, format="f", digits=2)),
            position=position_dodge(width=0.9), color="black", vjust=-0.3, size=4) +
  coord_flip() + 
  scale_fill_aaas(name = "School Urbanity") +
  scale_y_discrete(labels=c("Very\nlow", "Low", "Medium", "High", "Very\nhigh")) +
  labs(y="", x="Proportion", title = "School urbanity") + 
  own_theme + 
  xlim(0, 0.7) +
  background_grid(major="y", minor="y")

plot_weight <- ggplot(comp_melt, aes(x=value, y=factor(Var, levels=order_c), fill=variable)) +
  geom_bar(data=comp_melt %>% filter(Type == "Parental education"), stat="identity", position="dodge",
           color="black") +
  geom_text(data=comp_melt %>% filter(Type == "Parental education"), aes(label=formatC(value, format="f", digits=2)),
            position=position_dodge(width=0.9), color="black", vjust=-0.3, size=4) +
  coord_flip() + 
  scale_fill_aaas(name = "Pupil Weight") +
  scale_y_discrete(labels=c("Low", "Lowest")) +
  labs(y="", x="", title = "Parental education") +
  own_theme + 
  xlim(0, 0.7) + 
  background_grid(major="y", minor="y")



plots <- plot_grid(plot_size + theme(legend.position = "none"),
          plot_denom + theme(legend.position = "none"),
          plot_urb + theme(legend.position = "none"),
          plot_weight + theme(legend.position = "none"),
          plot_sw + theme(legend.position = "none"),
          plot_nw + theme(legend.position = "none"),
          ncol = 2)

legend <- get_legend(plot_size +
                       guides(color = guide_legend(nrow = 1)) +
                       theme(legend.position = "top", legend.justification = "center"))

plot_grid(legend, rel_heights = c(.05, .95), plots, nrow=2, align="h", axis="t")

ggsave("plots/desc/sample_representativeness.pdf",
       height=14, width=11,
       last_plot())

