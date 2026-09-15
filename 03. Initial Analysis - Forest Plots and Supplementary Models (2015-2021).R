# ------------------------------------------------------------------------------
# Rural-Urban Divide in Risk Perception of LSD (NSDUH 2015-2021)
# Bradley, M., Grossman, D., Simonsson, O., Copes, H., & Hendricks, P. S. (2025). Rural-urban divide in risk perception of LSD: Implications for psychedelic-assisted therapy. The Journal of Rural Health, 41(1), e12906.
# https://doi.org/10.1111/jrh.12906
#
# Script: 03. Initial Analysis - Forest Plots and Supplementary Models (2015-2021).R
#   Pre-revision analysis retained for transparency: original model specifications,
#   VIF checks, odds-ratio tables, and forest plots. Superseded by script 01 for the
#   published estimates.
#
# Data: NSDUH public-use R data files, downloaded from SAMHSA (see README.md).
#   Place NSDUH_2015.RDATA ... NSDUH_2021.RDATA in Data/ or change `data_dir` below.
#   Run from the repository root (open rural-urban-lsd-risk.Rproj in RStudio).
# ------------------------------------------------------------------------------

data_dir   <- "Data"
output_dir <- "Outputs"
if (!dir.exists(output_dir)) dir.create(output_dir)

library(tidyverse)
library(survey)
library(srvyr)
library(ggplot2)
library(scales)
library(reshape2)
library(car)
library(broom)
library(knitr)
library(kableExtra)
library(svydiags)

options(scipen = 999)

# 2015-2019 Analysis

load(file.path(data_dir, "NSDUH_2015.RDATA"))
load(file.path(data_dir, "NSDUH_2016.RDATA"))
load(file.path(data_dir, "NSDUH_2017.RDATA"))
load(file.path(data_dir, "NSDUH_2018.RDATA"))
load(file.path(data_dir, "NSDUH_2019.RDATA"))

assign("nsduh2015", PUF2015_021518)
assign("nsduh2016", PUF2016_022818)
assign("nsduh2017", PUF2017_100918)
assign("nsduh2018", PUF2018_100819)
assign("nsduh2019", PUF2019_100920)

nsduh2015$year = 2015
nsduh2016$year = 2016
nsduh2017$year = 2017
nsduh2018$year = 2018
nsduh2019$year = 2019

nsduh20152019 <- bind_rows(nsduh2015, nsduh2016, nsduh2017, nsduh2018, nsduh2019)
names(nsduh20152019) <- tolower(names(nsduh20152019))
nsduh20152019 <- nsduh20152019[complete.cases(nsduh20152019$grsklsdtry), ]

# Create Insurance composite variable (2015-2019) 

nsduh20152019$insurance <- ifelse(nsduh20152019$irmedicr == 1, 1,
                                  ifelse(nsduh20152019$irmcdchp == 1, 2,
                                         ifelse(nsduh20152019$irchmpus == 1, 3,
                                                ifelse(nsduh20152019$irprvhlt == 1, 4,
                                                       ifelse(nsduh20152019$irothhlt == 1, 5,
                                                          ifelse(nsduh20152019$irinsur4 == 2, 6, NA))))))

# Create Rural Status variable for regression (2015-2019)

nsduh20152019$rural_status <- ifelse(nsduh20152019$coutyp4 == 3, 1, 0)
nsduh20152019$sm_met_status <- ifelse(as.numeric(nsduh20152019$coutyp4) == 2, 1, 0)
nsduh20152019$addprev <- ifelse(nsduh20152019$addprev == 1, 1, 0)
nsduh20152019$mjever <- ifelse(nsduh20152019$mjever == 1, 1, 0)

nsduh20152019$grsklsdtry <- factor(nsduh20152019$grsklsdtry, levels = c(0, 1))
nsduh20152019$coutyp4 <- factor(nsduh20152019$coutyp4)

# Integrate 2015 martial status variable with 2016-2019 marital status variable

nsduh20152019 <- nsduh20152019 %>%
  mutate(irmarit = ifelse(year == 2015, irmaritstat, irmarit))

# Recode perceived risk of LSD variable (2015-2019)

nsduh20152019$rsklsdtry <- factor(nsduh20152019$rsklsdtry, levels = c(1, 2, 3, 4),
                               labels = c("No Risk", "Slight Risk", "Moderate Risk", "Great Risk"))

# Recode perceived risk of Marijuana variable (2015-2019)

nsduh20152019$rskmrjmon <- factor(nsduh20152019$rskmrjmon, levels = c(1, 2, 3, 4),
                                  labels = c("No Risk", "Slight Risk", "Moderate Risk", "Great Risk"))


# Recode county metro/non-metro status variable (2015-2019)

nsduh20152019$coutyp4 <- factor(nsduh20152019$coutyp4, levels = c(1, 2, 3),
                                labels = c("Large Metro", "Small Metro", "Non Metro"))

# Recode religious services variable (2015-2019)

nsduh20152019$snrlgsvc[nsduh20152019$snrlgsvc %in% c(85, 89, 94, 97, 98, 99)] <- 9

# Recode mental health coverage (private insurance) variable (2015-2019)

nsduh20152019$hltinmnt[nsduh20152019$hltinmnt %in% c(85, 97, 98, 99)] <- 9

# Recode LSD access variable (2015-2019)

nsduh20152019$difobtlsd[nsduh20152019$difobtlsd == 9] <- NA
nsduh20152019$difobtlsd <- factor(nsduh20152019$difobtlsd)

# Recode marijuana access variable (2015-2019)

nsduh20152019$difobtmrj[nsduh20152019$difobtmrj == 9] <- NA
nsduh20152019$difobtmrj <- factor(nsduh20152019$difobtmrj)

# Code analysis weight (2015-2019)

nsduh20152019$newanalwt <- nsduh20152019$analwt_c/5

nsduh20152019_sub <- nsduh20152019[, c("grsklsdtry", "coutyp4", "rural_status", "catag6",
                               "irsex", "sm_met_status", "eduhighcat", "insurance", "verep", "vestr", "newanalwt",
                               "rskmrjmon", "lsdflag", "newrace2", "rsklsdtry", "lsdyr", "year", "irmarit",
                               "wrkstatwk2", "spdmon", "addprev", "coutyp4", "grskmrjmon", "mjever", 
                               "grskmrjwk", "grskhertry", "grsklsdwk", "irfamin3", "snrlgsvc",
                               "difobtlsd", "difobtmrj", "hltinmnt", "rsklsdwk", "rskmrjwk")]

# Set other unordered categorical variables as factor

categorical_vars_20152019 <- c("irmarit", "mjever", "lsdflag", "irsex", "newrace2",
                      "insurance", "hltinmnt")

nsduh20152019_sub <- nsduh20152019_sub %>%
  mutate(across(all_of(categorical_vars_20152019), as.factor))

# Create survey design object (2015-2019)

nsduh.design <-  svydesign(
  ids = ~verep,
  strata= ~vestr,
  weights= ~newanalwt, 
  data = nsduh20152019_sub,
  nest = TRUE)

# Check missing values of variables (2015-2019)

variables <- c("grsklsdtry", "coutyp4", "rural_status", "catag6", "irsex", "sm_met_status",
               "eduhighcat", "insurance", "verep", "vestr", "newanalwt", "rskmrjmon", "lsdflag",
               "newrace2", "rsklsdtry", "lsdyr", "year", "irmarit", "wrkstatwk2", "spdmon", 
               "addprev", "coutyp4", "grskmrjmon", "mjever", "grskmrjwk",
               "grsklsdwk", "irfamin3", "snrlgsvc", "difobtlsd", "difobtmrj", "hltinmnt", "rsklsdwk")

missing_values <- sapply(variables, function(var) sum(is.na(nsduh.design$variables[[var]])))
missing_values <- data.frame(Variable = variables, Missing_Values = missing_values)
print(missing_values)

# Population Estimates - 2015-2019 (ns followed by %s, Rao-Scott tests for table)

# Create a tbl_svysummary object
nsduh_table <- tbl_svysummary(
  data = nsduh.design,
  by = "rural_status",
  percent = "column",
  label = list(
    irsex = "Gender",
    newrace2 = "Race",
    catag6 = "Age",
    eduhighcat = "Education",
    irfamin3 = "Family Income Level",
    irmarit = "Marital Status",
    insurance = "Insurance",
    hltinmnt = "Mental Health Insurance Coverage",
    snrlgsvc = "Yearly Religious Service Attendance",
    lsdflag = "LSD Lifetime Use",
    grsklsdtry = "Great Risk LSD - Lifetime",
    difgetlsd_recode = "LSD Ease of Access", 
    mjever = "Cannabis Lifetime Use",
    grskmrjmon = "Great Risk Cannabis - 1-2x Month Risk",
    grskmrjwk = "Great Risk Cannabis - 1-2x Week",
    difgetmrj_recode = "Marijuana Ease of Access"
  )
) %>%
  add_p(all_categorical() ~ "svy.adj.chisq.test") %>% 
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman") %>%
  gt::gtsave(file.path(output_dir, "nsduh_table.rtf"))

# LSD Use Table (2015-2019)

prop.table(table(nsduh20152019$lsd, nsduh20152019$coutyp4), margin = 2)*100 

  # ^^ ratios for lifetime use are almost identical

# Population Estimates (ns followed by %s, Rao-Scott tests for table)

  # Population Estimate for entire survey/survey range
 # svytotal(~ 1, nsduh.design) Not working, use below
  
  # Rural-Urban Continnum Code--total n
  svytotal(~ coutyp4, nsduh.design, na.rm = TRUE, vartype = se)
  
  # Gender (2015-2019)
  svyby(~coutyp4, ~irsex, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + irsex, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + irsex, nsduh.design)
  
  # Race (2015-2019)
  svyby(~coutyp4, ~newrace2, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + newrace2, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + newrace2, nsduh.design)
  
  # Age (2015-2019)
  svyby(~coutyp4, ~catag6, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + catag6, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + catag6, nsduh.design)
  
  # Education (2015-2019)
  svyby(~coutyp4, ~eduhighcat, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + eduhighcat, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + eduhighcat, nsduh.design)
  
  # Family Income Level (2015-2019)
  svyby(~coutyp4, ~irfamin3, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + irfamin3, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + irfamin3, nsduh.design)
  
  # Marital Status (2015-2019)
  svyby(~coutyp4, ~irmarit, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + irmarit, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + irmarit, nsduh.design)
  
  # Insurance (2015-2019)
  svyby(~coutyp4, ~insurance, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + insurance, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + insurance, nsduh.design)
  
  # Does insurance cover mental or emotional difficulties (2015-2019)
  svyby(~coutyp4, ~hltinmnt, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + hltinmnt, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + hltinmnt, nsduh.design)
  
  # Influence of Religious Beliefs on Decisions (2015-2019)
  svyby(~coutyp4, ~snrlgsvc, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + snrlgsvc, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + snrlgsvc, nsduh.design)
  
  # LSD Lifetime Use (2015-2019)
  svyby(~coutyp4, ~lsdflag, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + lsdflag, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + lsdflag, nsduh.design) 
  
  # LSD Lifetime Risk (2015-2019)
  svyby(~coutyp4, ~rsklsdtry, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + rsklsdtry, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + rsklsdtry, nsduh.design) 
  
  # LSD 1-2x Week Risk (2015-2019) 
  svyby(~coutyp4, ~rsklsdwk, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + rsklsdwk, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + rsklsdwk, nsduh.design) 
  
  # LSD Access - Dichotomized (2015-2019)
  svyby(~coutyp4, ~difobtlsd, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + difobtlsd, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + difobtlsd, nsduh.design) 
  
  # Marijuana Lifetime Use (2015-2019)
  svyby(~coutyp4, ~mjever, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + mjever, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + mjever, nsduh.design) 
  
  # Marijuana Month Risk (2015-2019)
  svyby(~coutyp4, ~rskmrjmon, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + rskmrjmon, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + rskmrjmon, nsduh.design) 
  
  # Marijuana 1-2x Week Risk (2015-2019) 
  svyby(~coutyp4, ~rskmrjwk, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + rskmrjwk, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + rskmrjwk, nsduh.design) 
  
  # Marijuana Access - Dichotomized (2015-2019)
  svyby(~coutyp4, ~difobtmrj, nsduh.design, svytotal)
  prop.table(svytable(~coutyp4 + difobtmrj, nsduh.design), margin=1)*100
  svychisq(~ coutyp4 + difobtmrj, nsduh.design) 
  
  
# Rural-Metro Code by LSD Risk (2015-2019) 

rsklsdtry_coutyp4_by_year <- svyby(~ rsklsdtry, ~ coutyp4 + year, design = nsduh.design, FUN = svymean, na.rm = TRUE)
rsklsdtry_coutyp4_by_year <- svyby(~ rsklsdtry, ~ coutyp4 + year, design = nsduh.design, 
                                   FUN = svymean, na.rm = TRUE, 
                                   keep.variance = TRUE, 
                                   weights = nsduh.design$newanalwt)

rsklsdtry_coutyp4_by_year_melted <- melt(rsklsdtry_coutyp4_by_year, id.vars = c("coutyp4", "year"))

ggplot(rsklsdtry_coutyp4_by_year_melted %>%
         filter(variable %in% c("rsklsdtryNo Risk", "rsklsdtrySlight Risk", "rsklsdtryModerate Risk", "rsklsdtryGreat Risk")) %>%
         mutate(variable = case_when(
           variable == "rsklsdtryNo Risk" ~ "No Risk",
           variable == "rsklsdtrySlight Risk" ~ "Slight Risk",
           variable == "rsklsdtryModerate Risk" ~ "Moderate Risk",
           variable == "rsklsdtryGreat Risk" ~ "Great Risk",
           TRUE ~ as.character(variable)
         )),
       aes(x = year, y = value, color = coutyp4)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Year", y = "Weighted Percentage", color = "County Type", 
       title = "Figure 1. Risk Perception of LSD, Single Use, by Rural/Urban Continuum Code, 2015-2019") +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# Linear trend analysis (Haven't gotten this to work yet--really need to...)

    # ^^ Build this using this guide: https://www.r-bloggers.com/2015/11/
    # statistically-significant-trends-with-multiple-years-of-complex-survey-data/


# Rural-Metro Code by Marijuana Risk (2015-2019) 

rskmrjmon_coutyp4_by_year <- svyby(~ rskmrjmon, ~ coutyp4 + year, design = nsduh.design, FUN = svymean, na.rm = TRUE)

rsklmrjmon_coutyp4_by_year <- svyby(~ rskmrjmon, ~ coutyp4 + year, design = nsduh.design, 
                                   FUN = svymean, na.rm = TRUE, 
                                   keep.variance = TRUE, 
                                   weights = nsduh.design$newanalwt)

rskmrjmon_coutyp4_by_year_melted <- melt(rskmrjmon_coutyp4_by_year, id.vars = c("coutyp4", "year"))

ggplot(rskmrjmon_coutyp4_by_year_melted %>%
         filter(variable %in% c("rskmrjmonNo Risk", "rskmrjmonSlight Risk", 
                                "rskmrjmonModerate Risk", "rskmrjmonGreat Risk")) %>%
         mutate(variable = case_when(
           variable == "rskmrjmonNo Risk" ~ "No Risk",
           variable == "rskmrjmonSlight Risk" ~ "Slight Risk",
           variable == "rskmrjmonModerate Risk" ~ "Moderate Risk",
           variable == "rskmrjmonGreat Risk" ~ "Great Risk",
           TRUE ~ as.character(variable)
         )),
       aes(x = year, y = value, color = coutyp4)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Year", y = "Weighted Percentage", color = "County Type", 
       title = "Figure 2. Risk Perception for Cannabis, Monthly Use, by Rural/Urban Continuum Code, 2015-2019") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# LSD Risk Regression Model (2015-2019)
 
lsdrisktry_logreg <- svyglm(grsklsdtry ~ rural_status + catag6 + eduhighcat + newrace2 
                         + irsex + lsdflag + irmarit + snrlgsvc + insurance + difobtlsd, 
                 design = nsduh.design, family = quasibinomial())

lsdriskwk_logreg <- svyglm(grsklsdwk ~ rural_status + catag6 + eduhighcat + newrace2 
                            + irsex + lsdflag + irmarit + irfamin3 + snrlgsvc + difobtlsd, 
                            design = nsduh.design, family = quasibinomial())

# LSD Access Model (2015-2019)

difobtlsd_logreg <- svyglm(difobtlsd ~ rural_status + catag6 + eduhighcat + newrace2 
                           + irsex + lsdflag + irmarit + irfamin3 + snrlgsvc + grsklsdwk, 
                           design = nsduh.design, family = quasibinomial())

# LSD Use Model (2015-2019)

lsdflag_logreg <- svyglm(lsdflag ~ rural_status + catag6 + eduhighcat + irmarit + newrace2 
                         + irsex + grsklsdtry + spdmon + irfamin3 + snrlgsvc, 
                         design = nsduh.design, family = quasibinomial())

# Cannabis Models (2015-2019)

mjmonrisk_logreg <- svyglm(grskmrjmon ~ rural_status + catag6 + eduhighcat + newrace2 
                           + irsex + mjever + irmarit + irfamin3 + snrlgsvc + difobtmrj, 
                         design = nsduh.design, family = quasibinomial())

mjwkrisk_logreg <- svyglm(grskmrjwk ~ rural_status + catag6 + eduhighcat + newrace2 + irsex + mjever + irmarit + irfamin3 + snrlgsvc + difobtmrj, 
                           design = nsduh.design, family = quasibinomial())

mjever_logreg <- svyglm(mjever ~ rural_status + catag6 + eduhighcat + newrace2 + irsex + grskmrjwk + irmarit + irfamin3 + snrlgsvc + difobtmrj, 
                          design = nsduh.design, family = quasibinomial())

difobtmj_logreg <- svyglm(difobtmrj ~ rural_status + catag6 + eduhighcat + newrace2 + irsex + grskmrjwk + irmarit + irfamin3 + snrlgsvc, 
                        design = nsduh.design, family = quasibinomial())


# Check for multicolinearity using VIF (2015-2019) 

vif(lsdrisktry_logreg)
vif(lsdriskwk_logreg)
vif(lsdflag_logreg)
vif(mjmonrisk_logreg)
vif(mjever_logreg)
vif(difobtlsd_logreg)
vif(difobtmj_logreg)


models <- list(
  lsdrisktry_logreg,
  lsdriskwk_logreg,
  lsdflag_logreg,
  difobtlsd_logreg,
  mjmonrisk_logreg,
  mjwkrisk_logreg,
  mjever_logreg,
  difobtmj_logreg
)

# Prep Models for Forest Plot + aOR Table (2015-2019)

results <- lapply(models, tidy)

results_df <- bind_rows(results, .id = "model")

odds_ratios <- results_df %>%
  filter(term != "(Intercept)") %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error),
         p.value = format.pval(p.value, eps = 0.001)) %>%
  mutate(model = recode_factor(model,
                               "1" = "lsdrisktry_logreg",
                               "2" = "lsdriskwk_logreg",
                               "3" = "lsdflag_logreg",
                               "4" = "difobtlsd_logreg",
                               "5" = "mjmonrisk_logreg",
                               "6" = "mjwkrisk_logreg",
                               "7" = "mjever_logreg",
                               "8" = "difobtmj_logreg"
  ))

or_table <- odds_ratios %>% filter(term == "rural_status")

# Forest Plot of Odds Ratios (2015-2019)

ggplot(data = or_table, aes(x = odds_ratio, y = model)) +
  geom_pointrange(aes(xmin = lower_ci, xmax = upper_ci), size = 1.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds Ratio", y = "", title = "Figure 3. Lifetime Use, Access and Risk Perception of LSD and Cannabis by Rural County Status, 2015-2019") +
  scale_y_discrete(labels = c("LSD 1-2x Lifetime Great Risk", "LSD 1-2x Week Great Risk", "Lifetime LSD Use", "LSD Easy to Obtain", "Cannabis 1x Month Great Risk", "Cannabis 1-2x Week Great Risk", "Lifetime Cannabis Use", "Cannabis Easy to Obtain")) +
  theme_classic() 

or_table <- or_table %>% mutate_all(as.numeric)
or_table$p.value <- ifelse(is.na(or_table$p.value), "0.0001", or_table$p.value)
or_table$names <- c("LSD 1-2x Lifetime Great Risk", "LSD 1-2x Week Great Risk", "Lifetime LSD Use", "LSD Easy to Obtain", "Cannabis 1x Month Great Risk", "Cannabis 1-2x Week Great Risk", "Lifetime Cannabis Use", "Cannabis Easy to Obtain")
or_table$ci <- paste0("(", round(or_table$lower_ci, 2), ", ", round(or_table$upper_ci, 2), ")")


# aOR Table (2015-2019)

table_data <- or_table[, c("names", "estimate", "odds_ratio", "ci", "p.value")]

kable(table_data, format = "markdown", col.names = c("Term", "Coefficient", "Adjusted Odds Ratio", "95% Confidence Interval", "P-Value")) %>%
     kable_styling(bootstrap_options = "striped")

# Rewriten Analysis----

# LSD Models

# Model 1 (Crude)

LSD_Model1 <- svyglm(grsklsdtry ~ rural_status, design = nsduh.design, family = quasibinomial())

# Model 2 (Sociodemographically Adjusted)

LSD_Model2 <- svyglm(grsklsdtry ~ rural_status + catag6 + eduhighcat + newrace2 
                     + irsex + irmarit + irfamin3 + snrlgsvc, 
                     design = nsduh.design, family = quasibinomial())

# Model 3 (Adjusted for Lifetime Use + Past Year Use + Ease of Access)

LSD_Model3 <- svyglm(grsklsdtry ~ rural_status + catag6 + eduhighcat + newrace2 
                     + irsex + irmarit + irfamin3 + snrlgsvc + difobtlsd + 
                       lsdflag, 
                     design = nsduh.design, family = quasibinomial())

# Cannabis Models

# Model 1 (Crude)

Cannabis_Model1 <- svyglm(grskmrjmon ~ rural_status, design = nsduh.design, family = quasibinomial())

# Model 2 (Sociodemographically Adjusted)

Cannabis_Model2 <- svyglm(grskmrjmon ~ rural_status + catag6 + eduhighcat + newrace2 
                          + irsex + irmarit + irfamin3 + snrlgsvc, 
                          design = nsduh.design, family = quasibinomial())

# Model 3 (Adjusted for Lifetime Use + Past Year Use + Ease of Access)

Cannabis_Model3 <- svyglm(grskmrjmon ~ rural_status + catag6 + eduhighcat + newrace2 
                          + irsex + irmarit + irfamin3 + snrlgsvc + difobtmrj + 
                            mjever, 
                          design = nsduh.design, family = quasibinomial())

# Check for multicolinearity using VIF (2015-2019) 

vif(LSD_Model2)
vif(LSD_Model3)
vif(Cannabis_Model2)
vif(Cannabis_Model3)



models <- list(
  LSD_Model1,
  LSD_Model2,
  LSD_Model3,
  Cannabis_Model1,
  Cannabis_Model2,
  Cannabis_Model3,
)

# Prep Models for Forest Plot + aOR Table (2015-2019)

results <- lapply(models, tidy)

results_df <- bind_rows(results, .id = "model")

odds_ratios <- results_df %>%
  filter(term != "(Intercept)") %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error),
         p.value = format.pval(p.value, eps = 0.001)) %>%
  mutate(model = recode_factor(model,
                               "1" = "LSD_Model1",
                               "2" = "LSD_Model2",
                               "3" = "LSD_Model3",
                               "4" = "Cannabis_Model1",
                               "5" = "Cannabis_Model2",
                               "6" = "Cannabis_Model3",
  ))

or_table <- odds_ratios %>% filter(term == "rural_status")


# Check for multicolinearity using VIF (2015-2019) 

vif(lsdrisktry_logreg)
vif(lsdriskwk_logreg)
vif(lsdflag_logreg)
vif(mjmonrisk_logreg)
vif(mjever_logreg)
vif(difobtlsd_logreg)
vif(difobtmj_logreg)


models <- list(
  lsdrisktry_logreg,
  lsdriskwk_logreg,
  lsdflag_logreg,
  difobtlsd_logreg,
  mjmonrisk_logreg,
  mjwkrisk_logreg,
  mjever_logreg,
  difobtmj_logreg
)

# Prep Models for Forest Plot + aOR Table (2015-2019)

results <- lapply(models, tidy)

results_df <- bind_rows(results, .id = "model")

odds_ratios <- results_df %>%
  filter(term != "(Intercept)") %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error),
         p.value = format.pval(p.value, eps = 0.001)) %>%
  mutate(model = recode_factor(model,
                               "1" = "lsdrisktry_logreg",
                               "2" = "lsdriskwk_logreg",
                               "3" = "lsdflag_logreg",
                               "4" = "difobtlsd_logreg",
                               "5" = "mjmonrisk_logreg",
                               "6" = "mjwkrisk_logreg",
                               "7" = "mjever_logreg",
                               "8" = "difobtmj_logreg"
  ))

or_table <- odds_ratios %>% filter(term == "rural_status")

# Forest Plot of Odds Ratios (2015-2019)

ggplot(data = or_table, aes(x = odds_ratio, y = model)) +
  geom_pointrange(aes(xmin = lower_ci, xmax = upper_ci), size = 1.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds Ratio", y = "", title = "Figure 3. Lifetime Use, Access and Risk Perception of LSD and Cannabis by Rural County Status, 2015-2019") +
  scale_y_discrete(labels = c("LSD 1-2x Lifetime Great Risk", "LSD 1-2x Week Great Risk", "Lifetime LSD Use", "LSD Easy to Obtain", "Cannabis 1x Month Great Risk", "Cannabis 1-2x Week Great Risk", "Lifetime Cannabis Use", "Cannabis Easy to Obtain")) +
  theme_classic() 

or_table <- or_table %>% mutate_all(as.numeric)
or_table$p.value <- ifelse(is.na(or_table$p.value), "0.0001", or_table$p.value)
or_table$names <- c("LSD 1-2x Lifetime Great Risk", "LSD 1-2x Week Great Risk", "Lifetime LSD Use", "LSD Easy to Obtain", "Cannabis 1x Month Great Risk", "Cannabis 1-2x Week Great Risk", "Lifetime Cannabis Use", "Cannabis Easy to Obtain")
or_table$ci <- paste0("(", round(or_table$lower_ci, 2), ", ", round(or_table$upper_ci, 2), ")")


# aOR Table (2015-2019)

table_data <- or_table[, c("names", "estimate", "odds_ratio", "ci", "p.value")]

kable(table_data, format = "markdown", col.names = c("Term", "Coefficient", "Adjusted Odds Ratio", "95% Confidence Interval", "P-Value")) %>%
  kable_styling(bootstrap_options = "striped")


# Serious Psychological Distress Regression Model 

# spdmon_logreg <- svyglm(spdmon ~ rural_status + sm_met_status + catag6 + eduhighcat + irmarit + newrace2 + irsex + grsklsdtry + wrkstatwk2,
                      #   design = nsduh.design, family = quasibinomial())


# Adult Depression Regression Model (Not Controlling for LSD Variables)

# addprev_logreg <- svyglm(addprev ~ rural_status + sm_met_status + catag6 + eduhighcat 
                       # + irmarit + newrace2 + irsex + wrkstatwk2, 
                      #  design = nsduh.design, family = quasibinomial())

# 2020 Analysis

load(file.path(data_dir, "NSDUH_2020.RDATA"))

names(NSDUH_2020) <- tolower(names(NSDUH_2020))

NSDUH_2020$rural_status <- ifelse(as.numeric(NSDUH_2020$coutyp4) == 3, 1, 0)
NSDUH_2020$sm_met_status <- ifelse(as.numeric(NSDUH_2020$coutyp4) == 2, 1, 0)
NSDUH_2020$mjever <- ifelse(NSDUH_2020$mjever == 1, 1, 0)

NSDUH_2020$grsklsdtry <- ifelse(as.numeric(NSDUH_2020$rsklsdtry) == 4, 1, 0)

# Create Insurance composite variable (2020) 

NSDUH_2020$insurance <- ifelse(NSDUH_2020$irmedicr == 1, 1,
                               ifelse(NSDUH_2020$irmcdchp == 1, 2,
                                      ifelse(NSDUH_2020$irchmpus == 1, 3,
                                             ifelse(NSDUH_2020$irprvhlt == 1, 4,
                                                    ifelse(NSDUH_2020$irothhlt == 1, 5,
                                                           ifelse(NSDUH_2020$irinsur4 == 2, 6, NA))))))

# Recode perceived risk of LSD variable (2020) 

NSDUH_2020$rsklsdtry <- factor(NSDUH_2020$rsklsdtry, levels = c(1, 2, 3, 4),
                               labels = c("No Risk", "Slight Risk", "Moderate Risk", "Great Risk"))

# Subset relevant variables (2020) 
nsduh2020_sub <- NSDUH_2020[, c("grsklsdtry", "coutyp4", "rural_status", "catag6",
                                "irsex", "sm_met_status", "eduhighcat", "verep", "vestrq1q4_c", "analwtq1q4_c",
                                "rskmrjmon", "lsdflag", "newrace2", "rsklsdtry", "lsdyr", "nomarr2",
                                "wrkstatwk2", "spdmon", "addprev", "grskmrjmon", "mjever", 
                                "grskmrjwk", "grskhertry", "grsklsdwk", "irfamin3", "snrlgsvc",
                                "difobtmrj", "rskmrjwk", "difobtlsd", "rsklsdwk", "hltinmnt", "insurance",
                                "irmarit")]

# Set other unordered categorical variables as factor (2020)

categorical_vars_2020 <- c("irmarit", "mjever", "lsdflag", "irsex", "newrace2",
                      "insurance", "hltinmnt")

nsduh2020_sub <- nsduh2020_sub %>%
  mutate(across(all_of(categorical_vars_2020), as.factor))

nsduh2020.design <-  svydesign(
  ids = ~verep,
  strata= ~vestrq1q4_c,
  weights= ~analwtq1q4_c, 
  data = nsduh2020_sub,
  nest = TRUE)

nsduh2020.design<-subset(nsduh2020.design,!is.na(grsklsdtry))

rsklsdtry_by_coutyp4_2020 <- svyby(~rsklsdtry, ~coutyp4, design = nsduh2020.design, FUN = svymean, na.rm = TRUE)

colnames(rsklsdtry_by_coutyp4_2020) <- gsub(" ", "", colnames(rsklsdtry_by_coutyp4_2020))



# Composite bar charts for descriptive statistics 
# 
# #rsklsdtry_by_coutyp4_long <- rsklsdtry_by_coutyp4 %>%
# #  gather(key = "risk_level", value = "weighted_percentage", rsklsdtryNoRisk:rsklsdtryGreatRisk)
# 
# #rsklsdtry_by_coutyp4_long$risk_level <- factor(rsklsdtry_by_coutyp4_long$risk_level,
#                                                levels = c("rsklsdtryGreatRisk", 
#                                                           "rsklsdtryModerateRisk", 
#                                                           "rsklsdtrySlightRisk", 
#                                                           "rsklsdtryNoRisk"))
# 
# 
# ggplot(rsklsdtry_by_coutyp4_long, aes(x = coutyp4, y = weighted_percentage, fill = risk_level)) +
#   geom_bar(stat = "identity") +
#   xlab("County Metro/Non-Metro Status") +
#   ylab("Weighted Percentage") +
#   ggtitle("Figure 4. Perceived Risk of Single Use of LSD by County Metro/Non-Metro Status, 2020") +
#   scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61"),
#                     limits = c("rsklsdtryNoRisk", "rsklsdtrySlightRisk", "rsklsdtryModerateRisk", "rsklsdtryGreatRisk"),
#                     labels = c("No Risk", "Slight Risk", "Moderate Risk", "Great Risk")) +
#   theme(legend.title = element_blank(), legend.position = "bottom")

# Population Estimates 2020 (ns followed by %s, Rao-Scott tests for table)

# Population Estimate for entire survey/survey range (2020) 
svytotal(~ 1, nsduh2020.design)

# Rural-Urban Continnum Code--total n (2020) 
svytotal(~ irsex, nsduh2020.design, na.rm = TRUE, vartype = se)

# Gender (2020)
svyby(~coutyp4, ~irsex, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + irsex, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + irsex, nsduh2020.design)

# Race (2020)
svyby(~coutyp4, ~newrace2, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + newrace2, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + newrace2, nsduh2020.design)

# Age (2020)
svyby(~coutyp4, ~catag6, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + catag6, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + catag6, nsduh2020.design)

# Education (2020)
svyby(~coutyp4, ~eduhighcat, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + eduhighcat, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + eduhighcat, nsduh2020.design)

# Family Income Level (2020)
svyby(~coutyp4, ~irfamin3, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + irfamin3, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + irfamin3, nsduh2020.design)

# Marital Status (2020)
svyby(~coutyp4, ~irmarit, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + irmarit, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + irmarit, nsduh2020.design)

# Insurance (2020)
svyby(~coutyp4, ~insurance, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + insurance, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + insurance, nsduh2020.design)

# Does insurance cover mental or emotional difficulties (2020)
svyby(~coutyp4, ~hltinmnt, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + hltinmnt, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + hltinmnt, nsduh2020.design)

# Influence of Religious Beliefs on Decisions (2020)
svyby(~coutyp4, ~snrlgsvc, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + snrlgsvc, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + snrlgsvc, nsduh2020.design)

# LSD Lifetime Use (2020)
svyby(~coutyp4, ~lsdflag, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + lsdflag, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + lsdflag, nsduh2020.design) 

# LSD Lifetime Risk (2020)
svyby(~coutyp4, ~rsklsdtry, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + rsklsdtry, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + rsklsdtry, nsduh2020.design) 

# LSD 1-2x Week Risk (2020)
svyby(~coutyp4, ~rsklsdwk, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + rsklsdwk, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + rsklsdwk, nsduh2020.design) 

# LSD Access - Dichotomized (2020)
svyby(~coutyp4, ~difobtlsd, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + difobtlsd, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + difobtlsd, nsduh2020.design) 

# Marijuana Lifetime Use (2020)
svyby(~coutyp4, ~mjever, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + mjever, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + mjever, nsduh2020.design) 

# Marijuana Month Risk (2020)
svyby(~coutyp4, ~rskmrjmon, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + rskmrjmon, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + rskmrjmon, nsduh2020.design) 

# Marijuana 1-2x Week Risk (2020)
svyby(~coutyp4, ~rskmrjwk, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + rskmrjwk, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + rskmrjwk, nsduh2020.design) 

# Marijuana Access - Dichotomized (2020)
svyby(~coutyp4, ~difobtmrj, nsduh2020.design, svytotal)
prop.table(svytable(~coutyp4 + difobtmrj, nsduh2020.design), margin=1)*100
svychisq(~ coutyp4 + difobtmrj, nsduh2020.design) 


# Rewriten Analysis (2020)----

# LSD Models

# Model 1 (Crude)

LSD_Model1 <- svyglm(grsklsdtry ~ rural_status, design = nsduh.design, family = quasibinomial())

# Model 2 (Sociodemographically Adjusted)

LSD_Model2 <- svyglm(grsklsdtry ~ rural_status + catag6 + eduhighcat + newrace2 
                     + irsex + irmarit + irfamin3 + snrlgsvc, 
                     design = nsduh.design, family = quasibinomial())

# Model 3 (Adjusted for Lifetime Use + Past Year Use + Ease of Access)

LSD_Model3 <- svyglm(grsklsdtry ~ rural_status + catag6 + eduhighcat + newrace2 
                     + irsex + irmarit + irfamin3 + snrlgsvc + difobtlsd + 
                       lsdflag, 
                     design = nsduh.design, family = quasibinomial())

# Cannabis Models

# Model 1 (Crude)

Cannabis_Model1 <- svyglm(grskmrjmon ~ rural_status, design = nsduh.design, family = quasibinomial())

# Model 2 (Sociodemographically Adjusted)

Cannabis_Model2 <- svyglm(grskmrjmon ~ rural_status + catag6 + eduhighcat + newrace2 
                          + irsex + irmarit + irfamin3 + snrlgsvc, 
                          design = nsduh.design, family = quasibinomial())

# Model 3 (Adjusted for Lifetime Use + Past Year Use + Ease of Access)

Cannabis_Model3 <- svyglm(grskmrjmon ~ rural_status + catag6 + eduhighcat + newrace2 
                          + irsex + irmarit + irfamin3 + snrlgsvc + difobtmrj + 
                            mjever, 
                          design = nsduh.design, family = quasibinomial())

# Check for multicolinearity using VIF (2015-2019) 

vif(LSD_Model2)
vif(LSD_Model3)
vif(Cannabis_Model2)
vif(Cannabis_Model3)



models <- list(
  LSD_Model1,
  LSD_Model2,
  LSD_Model3,
  Cannabis_Model1,
  Cannabis_Model2,
  Cannabis_Model3,
)

# Prep Models for Forest Plot + aOR Table (2015-2019)

results <- lapply(models, tidy)

results_df <- bind_rows(results, .id = "model")

odds_ratios <- results_df %>%
  filter(term != "(Intercept)") %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error),
         p.value = format.pval(p.value, eps = 0.001)) %>%
  mutate(model = recode_factor(model,
                               "1" = "LSD_Model1",
                               "2" = "LSD_Model2",
                               "3" = "LSD_Model3",
                               "4" = "Cannabis_Model1",
                               "5" = "Cannabis_Model2",
                               "6" = "Cannabis_Model3",
  ))

or_table <- odds_ratios %>% filter(term == "rural_status")

# Regression Models + Odds Ratios + Forest Plot (2020)

# LSD Risk Regression Models (2020)

lsdrisktry2020_logreg <- svyglm(grsklsdtry ~ rural_status + catag6 + eduhighcat + newrace2 
                                + irsex + lsdflag + irmarit + snrlgsvc + insurance + difobtlsd, 
                                design = nsduh2020.design, family = quasibinomial())

lsdriskwk2020_logreg <- svyglm(grsklsdwk ~ rural_status + catag6 + eduhighcat + newrace2 
                               + irsex + lsdflag + irmarit + irfamin3 + snrlgsvc + difobtlsd, 
                               design = nsduh2020.design, family = quasibinomial())

# LSD Access Model (2020)

difobtlsd2020_logreg <- svyglm(difobtlsd ~ rural_status + catag6 + eduhighcat + newrace2 
                               + irsex + lsdflag + irmarit + irfamin3 + snrlgsvc + grsklsdwk, 
                               design = nsduh2020.design, family = quasibinomial())

# LSD Use Model (2020)

lsdflag2020_logreg <- svyglm(lsdflag ~ rural_status + catag6 + eduhighcat + irmarit + newrace2 
                             + irsex + grsklsdtry + spdmon + irfamin3 + snrlgsvc, 
                             design = nsduh2020.design, family = quasibinomial())

# Cannabis Models (2020)

mjmonrisk2020_logreg <- svyglm(grskmrjmon ~ rural_status + catag6 + eduhighcat + newrace2 
                               + irsex + mjever + irmarit + irfamin3 + snrlgsvc + difobtmrj, 
                               design = nsduh2020.design, family = quasibinomial())

mjwkrisk2020_logreg <- svyglm(grskmrjwk ~ rural_status + catag6 + eduhighcat + newrace2 + irsex + mjever + irmarit + irfamin3 + snrlgsvc + difobtmrj, 
                              design = nsduh2020.design, family = quasibinomial())

mjever2020_logreg <- svyglm(mjever ~ rural_status + catag6 + eduhighcat + newrace2 + irsex + grskmrjwk + irmarit + irfamin3 + snrlgsvc + difobtmrj, 
                            design = nsduh2020.design, family = quasibinomial())

difobtmj2020_logreg <- svyglm(difobtmrj ~ rural_status + catag6 + eduhighcat + newrace2 + irsex + grskmrjwk + irmarit + irfamin3 + snrlgsvc, 
                              design = nsduh2020.design, family = quasibinomial())

# Check for multicolinearity using VIF (2020) 

vif(lsdrisktry2020_logreg)
vif(lsdriskwk2020_logreg)
vif(lsdflag2020_logreg)
vif(mjmonrisk2020_logreg)
vif(mjwkrisk2020_logreg)
vif(mjever2020_logreg)
vif(difobtlsd2020_logreg)
vif(difobtmj2020_logreg)

# Prep for Forest Plot (2020)

models2020 <- list(
  lsdrisktry2020_logreg,
  lsdriskwk2020_logreg,
  lsdflag2020_logreg,
  difobtlsd2020_logreg,
  mjmonrisk2020_logreg,
  mjwkrisk2020_logreg,
  mjever2020_logreg,
  difobtmj2020_logreg
)

results2020 <- lapply(models2020, tidy)

results_df_2020 <- bind_rows(results2020, .id = "model")

odds_ratios_2020 <- results_df %>%
  filter(term != "(Intercept)") %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error),
         p.value = format.pval(p.value, eps = 0.001)) %>%
  mutate(model = recode_factor(model,
                               "1" = "lsdrisktry2020_logreg",
                               "2" = "lsdriskwk2020_logreg",
                               "3" = "lsdflag2020_logreg",
                               "4" = "difobtlsd2020_logreg",
                               "5" = "mjmonrisk2020_logreg",
                               "6" = "mjwkrisk2020_logreg",
                               "7" = "mjever2020_logreg",
                               "8" = "difobtmj2020_logreg"
  ))

or_table_2020 <- odds_ratios_2020 %>% filter(term == "rural_status")


# Forest Plot of Odds Ratios (2020)

ggplot(data = or_table_2020, aes(x = odds_ratio, y = model)) +
  geom_pointrange(aes(xmin = lower_ci, xmax = upper_ci), size = 1.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds Ratio", y = "", title = "Figure 4. Odds Ratios for Lifetime Use and Risk Perception by Rural Status, 2020") +
  scale_y_discrete(labels = c("LSD 1-2x Lifetime Great Risk", "LSD 1-2x Week Great Risk", "Lifetime LSD Use", "LSD Easy to Obtain", "Cannabis 1x Month Great Risk", "Cannabis 1-2x Week Great Risk", "Lifetime Cannabis Use", "Cannabis Easy to Obtain")) +
  theme_classic() +
  xlim(0.6, NA)


or_table_2020 <- or_table_2020 %>% mutate_all(as.numeric)
or_table_2020$p.value <- ifelse(is.na(or_table_2020$p.value), "0.0001", or_table_2020$p.value)
or_table_2020$names <- c("LSD 1-2x Lifetime Great Risk", "LSD 1-2x Week Great Risk", "Lifetime LSD Use", "LSD Easy to Obtain", "Cannabis 1x Month Great Risk", "Cannabis 1-2x Week Great Risk", "Lifetime Cannabis Use", "Cannabis Easy to Obtain")
or_table_2020$ci <- paste0("(", round(or_table_2020$lower_ci, 2), ", ", round(or_table_2020$upper_ci, 2), ")")


# aOR Table (2020)

table_data2020 <- or_table_2020[, c("names", "estimate", "odds_ratio", "ci", "p.value")]

kable(table_data2020, format = "markdown", col.names = c("Term", "Coefficient", "Adjusted Odds Ratio", "95% Confidence Interval", "P-Value")) %>%
  kable_styling(bootstrap_options = "striped")

#2021 Analysis

load(file.path(data_dir, "NSDUH_2021.RDATA"))

assign("NSDUH_2021", PUF2021_100622)

names(NSDUH_2021) <- tolower(names(NSDUH_2021))

NSDUH_2021$rural_status <- ifelse(as.numeric(NSDUH_2021$coutyp4) == 3, 1, 0)
NSDUH_2021$sm_met_status <- ifelse(as.numeric(NSDUH_2021$coutyp4) == 2, 1, 0)
NSDUH_2021$mjever <- ifelse(NSDUH_2021$mjever == 1, 1, 0)

NSDUH_2021$grsklsdtry <- ifelse(as.numeric(NSDUH_2021$rsklsdtry) == 4, 1, 0)

# Recode county metro/non-metro status variable
NSDUH_2021$coutyp4 <- factor(NSDUH_2021$coutyp4, levels = c(1, 2, 3),
                             labels = c("Large Metro", "Small Metro", "Non Metro"))

# Create Insurance composite variable 

NSDUH_2021$insurance <- ifelse(NSDUH_2021$irmedicr == 1, 1,
                               ifelse(NSDUH_2021$irmcdchp == 1, 2,
                                      ifelse(NSDUH_2021$irchmpus == 1, 3,
                                             ifelse(NSDUH_2021$irprvhlt == 1, 4,
                                                    ifelse(NSDUH_2021$irothhlt == 1, 5,
                                                           ifelse(NSDUH_2021$irinsur4 == 2, 6, NA))))))

# Recode perceived risk of LSD variable
NSDUH_2021$rsklsdtry <- factor(NSDUH_2021$rsklsdtry, levels = c(1, 2, 3, 4),
                               labels = c("No Risk", "Slight Risk", "Moderate Risk", "Great Risk"))

nsduh2021_sub <- NSDUH_2021[, c("grsklsdtry", "coutyp4", "rural_status", "catag6",
                                "irsex", "sm_met_status", "eduhighcat", "verep", "vestr_c", "analwt_c",
                                "rskmrjmon", "lsdflag", "newrace2", "rsklsdtry", "lsdyr", "nomarr2",
                                "wrkstatwk2", "addprev", "grskmrjmon", "mjever", 
                                "grskmrjwk", "herflag", "grskhertry", "grsklsdwk", "irfamin3", "snrlgsvc",
                                "difobtmrj", "rskmrjwk", "difobtlsd", "rsklsdwk", "hltinmnt", "insurance",
                                "irmarit")]

# Set other unordered categorical variables as factor (2021)

categorical_vars_2021 <- c("irmarit", "mjever", "lsdflag", "irsex", "newrace2",
                           "insurance", "hltinmnt")

nsduh2021_sub <- nsduh2021_sub %>%
  mutate(across(all_of(categorical_vars_2021), as.factor))

nsduh2021.design <-  svydesign(
  ids = ~verep,
  strata= ~vestr_c,
  weights= ~analwt_c, 
  data = nsduh2021_sub,
  nest = TRUE)

nsduh2021.design<-subset(nsduh2021.design,!is.na(grsklsdtry))

# Composite bar chart - LSD Risk Perception

# rsklsdtry_by_coutyp4 <- svyby(~rsklsdtry, ~coutyp4, design = nsduh2021.design, FUN = svymean, na.rm = TRUE)
# 
# colnames(rsklsdtry_by_coutyp4) <- gsub(" ", "", colnames(rsklsdtry_by_coutyp4))
# 
# rsklsdtry_by_coutyp4_long <- rsklsdtry_by_coutyp4 %>%
#   gather(key = "risk_level", value = "weighted_percentage", rsklsdtryNoRisk:rsklsdtryGreatRisk)
# 
# rsklsdtry_by_coutyp4_long$risk_level <- factor(rsklsdtry_by_coutyp4_long$risk_level,
#                                                levels = c("rsklsdtryGreatRisk", 
#                                                           "rsklsdtryModerateRisk", 
#                                                           "rsklsdtrySlightRisk", 
#                                                           "rsklsdtryNoRisk"))
# 
# 
# ggplot(rsklsdtry_by_coutyp4_long, aes(x = coutyp4, y = weighted_percentage, fill = risk_level)) +
#   geom_bar(stat = "identity") +
#   xlab("County Metro/Non-Metro Status") +
#   ylab("Weighted Percentage") +
#   ggtitle("Figure 6. Perceived Risk of LSD by County Metro/Non-Metro Status, 2021") +
#   scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61"),
#                     limits = c("rsklsdtryNoRisk", "rsklsdtrySlightRisk", "rsklsdtryModerateRisk", "rsklsdtryGreatRisk"),
#                     labels = c("No Risk", "Slight Risk", "Moderate Risk", "Great Risk")) +
#   theme(legend.title = element_blank(), legend.position = "bottom")
# 
# # Composite bar chart - Marijuana Risk Perception
# 
# rskmrjmon_by_coutyp4 <- svyby(~rskmrjmon, ~coutyp4, design = nsduh2021.design, FUN = svymean, na.rm = TRUE)
# 
# colnames(rskmrjmon_by_coutyp4) <- gsub(" ", "", colnames(rskmrjmon_by_coutyp4))
# 
# rskmrjmon_by_coutyp4_long <- rsklsdtry_by_coutyp4 %>%
#   gather(key = "risk_level", value = "weighted_percentage", rsklsdtryNoRisk:rsklsdtryGreatRisk)
# 
# rsklsdtry_by_coutyp4_long$risk_level <- factor(rsklsdtry_by_coutyp4_long$risk_level,
#                                                levels = c("rskmrjmonGreatRisk", 
#                                                           "rskmrjmonModerateRisk", 
#                                                           "rskmrjmonSlightRisk", 
#                                                           "rskmrjmonNoRisk"))
# 
# 
# ggplot(rskmrjmon_by_coutyp4_long, aes(x = coutyp4, y = weighted_percentage, fill = risk_level)) +
#   geom_bar(stat = "identity") +
#   xlab("County Metro/Non-Metro Status") +
#   ylab("Weighted Percentage") +
#   ggtitle("Figure 7. Perceived Risk of Monthly Marijuana Use by County Metro/Non-Metro Status, 2021") +
#   scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61"),
#                     limits = c("rskmrjmonNoRisk", "rskmrjmonSlightRisk", "rskmrjmonModerateRisk", "rskmrjmonGreatRisk"),
#                     labels = c("No Risk", "Slight Risk", "Moderate Risk", "Great Risk")) +
#   theme(legend.title = element_blank(), legend.position = "bottom")

# Population Estimate for entire survey/survey range
# svytotal(nsduh2021.design) <-- this isn't working?

# Rural-Urban Continnum Code--total n
svytotal(~ coutyp4, nsduh2021.design, na.rm = TRUE, vartype = se)

# Gender (2021)
svyby(~coutyp4, ~irsex, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + irsex, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + irsex, nsduh2021.design)

# Race (2021)
svyby(~coutyp4, ~newrace2, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + newrace2, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + newrace2, nsduh2021.design)

# Age (2021)
svyby(~coutyp4, ~catag6, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + catag6, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + catag6, nsduh2021.design)

# Education (2021)
svyby(~coutyp4, ~eduhighcat, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + eduhighcat, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + eduhighcat, nsduh2021.design)

# Family Income Level (2021)
svyby(~coutyp4, ~irfamin3, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + irfamin3, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + irfamin3, nsduh2021.design)

# Marital Status (2021)
svyby(~coutyp4, ~irmarit, nsduh2021.design, svytotal)

svychisq(~ coutyp4 + irmarit, nsduh2021.design)

# Insurance (2021)
svyby(~coutyp4, ~insurance, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + insurance, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + insurance, nsduh2021.design)

# Does insurance cover mental or emotional difficulties (2021)
svyby(~coutyp4, ~hltinmnt, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + hltinmnt, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + hltinmnt, nsduh2021.design)

# Influence of Religious Beliefs on Decisions (2021)
svyby(~coutyp4, ~snrlgsvc, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + snrlgsvc, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + snrlgsvc, nsduh2021.design)

# LSD Lifetime Use (2021)
svyby(~coutyp4, ~lsdflag, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + lsdflag, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + lsdflag, nsduh2021.design) 

# LSD Lifetime Risk (2021)
svyby(~coutyp4, ~rsklsdtry, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + rsklsdtry, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + rsklsdtry, nsduh2021.design) 

# LSD 1-2x Week Risk (2021)
svyby(~coutyp4, ~rsklsdwk, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + rsklsdwk, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + rsklsdwk, nsduh2021.design) 

# LSD Access - Dichotomized (2021)
svyby(~coutyp4, ~difobtlsd, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + difobtlsd, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + difobtlsd, nsduh2021.design) 

# Marijuana Lifetime Use (2021)
svyby(~coutyp4, ~mjever, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + mjever, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + mjever, nsduh2021.design) 

# Marijuana Month Risk (2021)
svyby(~coutyp4, ~rskmrjmon, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + rskmrjmon, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + rskmrjmon, nsduh2021.design) 

# Marijuana 1-2x Week Risk (2021)
svyby(~coutyp4, ~rskmrjwk, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + rskmrjwk, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + rskmrjwk, nsduh2021.design) 

# Marijuana Access - Dichotomized (2021)
svyby(~coutyp4, ~difobtmrj, nsduh2021.design, svytotal)
prop.table(svytable(~coutyp4 + difobtmrj, nsduh2021.design), margin=1)*100
svychisq(~ coutyp4 + difobtmrj, nsduh2021.design) 

# Regression Models + Odds Ratios + Forest Plot (2021)

# LSD Risk Regression Models (2021)

lsdrisktry2021_logreg <- svyglm(grsklsdtry ~ rural_status + catag6 + eduhighcat + newrace2 
                                + irsex + lsdflag + irmarit + snrlgsvc + insurance + difobtlsd, 
                                design = nsduh2021.design, family = quasibinomial())

lsdriskwk2021_logreg <- svyglm(grsklsdwk ~ rural_status + catag6 + eduhighcat + newrace2 
                               + irsex + lsdflag + irmarit + irfamin3 + snrlgsvc + difobtlsd, 
                               design = nsduh2021.design, family = quasibinomial())

# LSD Access Model (2021)

difobtlsd2021_logreg <- svyglm(difobtlsd ~ rural_status + catag6 + eduhighcat + newrace2 
                               + irsex + lsdflag + irmarit + irfamin3 + snrlgsvc + grsklsdwk, 
                               design = nsduh2021.design, family = quasibinomial())

# LSD Use Model (2021)

lsdflag2021_logreg <- svyglm(lsdflag ~ rural_status + catag6 + eduhighcat + irmarit + newrace2 
                             + irsex + grsklsdtry + irfamin3 + snrlgsvc, 
                             design = nsduh2021.design, family = quasibinomial())

# Cannabis Models (2021)

mjmonrisk2021_logreg <- svyglm(grskmrjmon ~ rural_status + catag6 + eduhighcat + newrace2 
                               + irsex + mjever + irmarit + irfamin3 + snrlgsvc + difobtmrj, 
                               design = nsduh2021.design, family = quasibinomial())

mjwkrisk2021_logreg <- svyglm(grskmrjwk ~ rural_status + catag6 + eduhighcat + newrace2 + irsex + mjever + irmarit + irfamin3 + snrlgsvc + difobtmrj, 
                              design = nsduh2021.design, family = quasibinomial())

mjever2021_logreg <- svyglm(mjever ~ rural_status + catag6 + eduhighcat + newrace2 + irsex + grskmrjwk + irmarit + irfamin3 + snrlgsvc + difobtmrj, 
                            design = nsduh2021.design, family = quasibinomial())

difobtmj2021_logreg <- svyglm(difobtmrj ~ rural_status + catag6 + eduhighcat + newrace2 + irsex + grskmrjwk + irmarit + irfamin3 + snrlgsvc, 
                              design = nsduh2021.design, family = quasibinomial())

# Check for multicolinearity using VIF (2021) 

vif(lsdrisktry2021_logreg)
vif(lsdriskwk2021_logreg)
vif(lsdflag2021_logreg)
vif(mjmonrisk2021_logreg)
vif(mjwkrisk2021_logreg)
vif(mjever2021_logreg)
vif(difobtlsd2021_logreg)
vif(difobtmj2021_logreg)

# Prep Models for Forest Plot + aOR Table (2021)

models2021 <- list(
  lsdrisktry2021_logreg,
  lsdriskwk2021_logreg,
  lsdflag2021_logreg,
  difobtlsd2021_logreg,
  mjmonrisk2021_logreg,
  mjwkrisk2021_logreg,
  mjever2021_logreg,
  difobtmj2021_logreg
)

results_2021 <- lapply(models2021, tidy)

results_2021_df <- bind_rows(results_2021, .id = "model")

odds_ratios_2021 <- results_2021_df %>%
  filter(term != "(Intercept)") %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error),
         p.value = format.pval(p.value, eps = 0.001)) %>%
  mutate(model = recode_factor(model,
                               "1" = "lsdrisktry2021_logreg",
                               "2" = "lsdriskwk2021_logreg",
                               "3" = "lsdflag2021_logreg",
                               "4" = "difobtlsd2021_logreg",
                               "5" = "mjmonrisk2021_logreg",
                               "6" = "mjwkrisk2021_logreg",
                               "7" = "mjever2021_logreg",
                               "8" = "difobtmj2021_logreg"
  ))

or_table_2021 <- odds_ratios_2021 %>% filter(term == "rural_status")


# Forest Plot of Odds Ratios (2021)

ggplot(data = or_table_2021, aes(x = odds_ratio, y = model)) +
  geom_pointrange(aes(xmin = lower_ci, xmax = upper_ci), size = 1.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds Ratio", y = "", title = "Figure 5. Odds Ratios for Lifetime Use and Risk Perception by Rural Status, 2021") +
  scale_y_discrete(labels = c("LSD 1-2x Lifetime Great Risk", "LSD 1-2x Week Great Risk", "Lifetime LSD Use", "LSD Easy to Obtain", "Cannabis 1x Month Great Risk", "Cannabis 1-2x Week Great Risk", "Lifetime Cannabis Use", "Cannabis Easy to Obtain")) +
  theme_classic() +
  xlim(0.6, NA)


or_table_2021 <- or_table_2021 %>% mutate_all(as.numeric)
or_table_2021$p.value <- ifelse(is.na(or_table_2021$p.value), "0.0001", or_table_2021$p.value)
or_table_2021$names <- c("LSD 1-2x Lifetime Great Risk", "LSD 1-2x Week Great Risk", "Lifetime LSD Use", "LSD Easy to Obtain", "Cannabis 1x Month Great Risk", "Cannabis 1-2x Week Great Risk", "Lifetime Cannabis Use", "Cannabis Easy to Obtain")
or_table_2021$ci <- paste0("(", round(or_table_2021$lower_ci, 2), ", ", round(or_table_2021$upper_ci, 2), ")")


# aOR Table (2021)

table_data2021 <- or_table_2021[, c("names", "estimate", "odds_ratio", "ci", "p.value")]

kable(table_data2021, format = "markdown", col.names = c("Term", "Coefficient", "Adjusted Odds Ratio", "95% Confidence Interval", "P-Value")) %>%
  kable_styling(bootstrap_options = "striped")




