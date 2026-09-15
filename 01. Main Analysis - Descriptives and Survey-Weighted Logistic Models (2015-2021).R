# ------------------------------------------------------------------------------
# Rural-Urban Divide in Risk Perception of LSD (NSDUH 2015-2021)
# Bradley, M., Grossman, D., Simonsson, O., Copes, H., & Hendricks, P. S. (2025). Rural-urban divide in risk perception of LSD: Implications for psychedelic-assisted therapy. The Journal of Rural Health, 41(1), e12906.
# https://doi.org/10.1111/jrh.12906
#
# Script: 01. Main Analysis - Descriptives and Survey-Weighted Logistic Models (2015-2021).R
#   Final analysis for the published manuscript: Table 1 descriptives with Rao-Scott tests,
#   and crude, sociodemographically adjusted, and fully adjusted survey-weighted logistic
#   models of rural status on perceived great risk of LSD and cannabis (2015-2019 pooled,
#   2020, and 2021), plus a year x rural status interaction test.
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
library(gt)
library(scales)
library(reshape2)
library(car)
library(broom)
library(knitr)
library(kableExtra)
library(gtsummary)

options(scipen = 999)

# (working directory: repository root; see header)
# 2015-2019 Analysis----

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

nsduh20152019$grsklsdtry[nsduh20152019$grsklsdtry == 9] <- NA
nsduh20152019 <- nsduh20152019[complete.cases(nsduh20152019$grsklsdtry), ]
nsduh20152019 <- nsduh20152019[complete.cases(nsduh20152019$grskmrjmon), ]

# Create Insurance composite variable (2015-2019) 

nsduh20152019$insurance <- ifelse(nsduh20152019$irmedicr == 1, 1,
                                  ifelse(nsduh20152019$irmcdchp == 1, 2,
                                         ifelse(nsduh20152019$irchmpus == 1, 3,
                                                ifelse(nsduh20152019$irprvhlt == 1, 4,
                                                       ifelse(nsduh20152019$irothhlt == 1, 5,
                                                              ifelse(nsduh20152019$irinsur4 == 2, 6, NA))))))

nsduh20152019$insurance <- factor(nsduh20152019$insurance, levels = c(1, 2, 3, 4, 5, 6),
                                  labels = c("Medicare", "Medicaid/CHIP", "Military",
                                             "Private", "Other", "Uninsured"))


# Create Rural Status variable for regression (2015-2019)

nsduh20152019$rural_status <- ifelse(nsduh20152019$coutyp4 == 3, 1, 0)
nsduh20152019$rural_status <- factor(nsduh20152019$rural_status, levels = c(0, 1),
                                     labels = c("Urban", "Rural"))

# Integrate 2015 martial status variable with 2016-2019 marital status variable

nsduh20152019 <- nsduh20152019 %>%
  mutate(irmarit = ifelse(year == 2015, irmaritstat, irmarit))

# Label Age Variable

nsduh20152019$catag6 <- factor(nsduh20152019$catag6, levels = c(1, 2, 3, 4, 5, 6),
                               labels = c("12-17 Years Old", "18-25 Years Old", "26-34 Years Old",
                                          "35-49 Years Old", "50-64 Years Old", "65 or Older"))
# Label Gender Variable

nsduh20152019$irsex <- factor(nsduh20152019$irsex, levels = c(1, 2),
                              labels = c("Male", "Female"))

# Label Race Variable

nsduh20152019$newrace2 <- factor(nsduh20152019$newrace2, levels = c(1, 2, 3, 4, 5, 6, 7),
                                 labels = c("White (Non-Hispanic)", "Black (Non-Hispanic)", "American Indian/Alaska Native",
                                            "Native Hawaiian/Other Pacific Islander", "Asian", "Multiracial", "Hispanic"))

# Label Martial Status Variable

nsduh20152019$irmarit <- factor(nsduh20152019$irmarit, levels = c(1, 2, 3, 4),
                                labels = c("Married", "Widowed", "Divorced or Separated",
                                           "Never Been Married"))
# Label Family Income Variable

nsduh20152019$irfamin3 <- factor(nsduh20152019$irfamin3, levels = c(1, 2, 3, 4, 5, 6, 7), 
                                 labels = c("Less than $10,000", "$10,000-$19,999",
                                            "$20,000-$29,999", "$30,000-$39,999",
                                            "$40,000-$49,999", "$50,000-$74,999",
                                            "75,000 or more"))

# Recode Education variable

nsduh20152019$eduhighcat <- factor(nsduh20152019$eduhighcat, levels = c(1, 2, 3, 4, 5),
                                   labels = c("Less Than High School", "High School",
                                              "Some College or Associate's Degree",
                                              "College Graduate", "12-17 Year Olds"))

# Recode Lifetime LSD Use Variable

nsduh20152019$lsdflag <- factor(nsduh20152019$lsdflag, levels = c(0, 1),
                                labels = c("No", "Yes"))

# Recode Marijuana Use Variable

nsduh20152019$mjever <- ifelse(nsduh20152019$mjever == 1, 1, 0)
nsduh20152019$mjever <- factor(nsduh20152019$mjever, levels = c(0, 1),
                               labels = c("No", "Yes"))

# Recode perceived great risk of LSD variable (2015-2019)

nsduh20152019$grsklsdtry <- factor(nsduh20152019$grsklsdtry, levels = c(0, 1),
                                   labels = c("No Great Risk", "Great Risk"))

# Recode perceived great risk of Marijuana 1-2x Month variable (2015-2019)

nsduh20152019$grskmrjmon <- factor(nsduh20152019$grskmrjmon, levels = c(0, 1),
                                   labels = c("No Great Risk", "Great Risk"))

# Recode religious services variable (2015-2019)

nsduh20152019$snrlgsvc[nsduh20152019$snrlgsvc %in% c(85, 89, 94, 97, 98, 99)] <- 9
nsduh20152019$snrlgsvc <- factor(nsduh20152019$snrlgsvc, levels = c(1, 2, 3, 4, 5, 6, 9),
                                 labels = c("0 times", "1-2 times", "3-5 times", "6-24 times",
                                            "25-52 times", "More than 52 times", "< 18 years old Or Legitimate Skip"))

# Recode mental health coverage (private insurance) variable (2015-2019)

nsduh20152019$hltinmnt [nsduh20152019$hltinmnt == 99] <- 4
nsduh20152019$hltinmnt[nsduh20152019$hltinmnt %in% c(85, 97, 98)] <- 3
nsduh20152019$hltinmnt <- factor(nsduh20152019$hltinmnt, levels = c(1, 2, 3, 4),
                                 labels = c("Yes", "No", "Don't Know or No Answer",
                                            "Not Privately Insured"))

# Recode LSD access variable (2015-2019)

nsduh20152019$difobtlsd[nsduh20152019$difobtlsd == 9] <- NA
nsduh20152019$difobtlsd <- factor(nsduh20152019$difobtlsd)

# Recode marijuana access variable (2015-2019)

nsduh20152019$difobtmrj[nsduh20152019$difobtmrj == 9] <- NA
nsduh20152019$difobtmrj <- factor(nsduh20152019$difobtmrj)

# Code analysis weight (2015-2019)

nsduh20152019$newanalwt <- nsduh20152019$analwt_c/5


nsduh20152019_sub <- nsduh20152019[, c("grsklsdtry", "rural_status", "catag6", "irsex", "eduhighcat",
                                       "insurance", "verep", "vestr", "newanalwt", "lsdflag",
                                       "newrace2", "year", "irmarit", "rsklsdtry",
                                       "grskmrjmon", "mjever", "irfamin3",
                                       "snrlgsvc", "difobtlsd", "difobtmrj", "hltinmnt")]

# Create survey design object (2015-2019)

nsduh.design <-  svydesign(
  ids = ~verep,
  strata= ~vestr,
  weights= ~newanalwt, 
  data = nsduh20152019_sub,
  nest = TRUE)

# Demographics + Tests of Association (2015-2019)----

nsduh_table_20152019 <- tbl_svysummary(
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
    difobtlsd = "LSD Ease of Access", 
    mjever = "Cannabis Lifetime Use",
    grskmrjmon = "Great Risk Cannabis - 1-2x Month Risk",
    grskmrjwk = "Great Risk Cannabis - 1-2x Week",
    difobtmrj = "Marijuana Ease of Access"
  )
) %>%
  add_p(all_categorical() ~ "svy.adj.chisq.test") %>% 
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman") %>%
  gt::gtsave(file.path(output_dir, "nsduh_table2015-2019.rtf"))

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

# Model 3 + Year Interaction Term

LSD_Model3_Interaction <- svyglm(grsklsdtry ~ year * rural_status + catag6 + eduhighcat + newrace2 
                                 + irsex + irmarit + irfamin3 + snrlgsvc + difobtlsd + lsdflag, 
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


# Prep Models for Forest Plot + aOR Table (2015-2019)
models <- list(
  LSD_Model1,
  LSD_Model2,
  LSD_Model3,
  Cannabis_Model1,
  Cannabis_Model2,
  Cannabis_Model3
)

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

or_table <- odds_ratios %>% filter(term == "rural_statusRural")


# 2020 Analysis----

load(file.path(data_dir, "NSDUH_2020.RDATA"))

names(NSDUH_2020) <- tolower(names(NSDUH_2020))

NSDUH_2020$grsklsdtry[NSDUH_2020$grsklsdtry == 9] <- NA
NSDUH_2020 <- NSDUH_2020[complete.cases(NSDUH_2020$grsklsdtry), ]
NSDUH_2020 <- NSDUH_2020[complete.cases(NSDUH_2020$grskmrjmon), ]


# Create Rural Status variable for regression 

NSDUH_2020$rural_status <- ifelse(NSDUH_2020$coutyp4 == 3, 1, 0)
NSDUH_2020$rural_status <- factor(NSDUH_2020$rural_status, levels = c(0, 1),
                                     labels = c("Urban", "Rural"))


# Create Insurance composite variable (2020) 

NSDUH_2020$insurance <- ifelse(NSDUH_2020$irmedicr == 1, 1,
                               ifelse(NSDUH_2020$irmcdchp == 1, 2,
                                      ifelse(NSDUH_2020$irchmpus == 1, 3,
                                             ifelse(NSDUH_2020$irprvhlt == 1, 4,
                                                    ifelse(NSDUH_2020$irothhlt == 1, 5,
                                                           ifelse(NSDUH_2020$irinsur4 == 2, 6, NA))))))

# Label Age Variable

NSDUH_2020$catag6 <- factor(NSDUH_2020$catag6, levels = c(2, 3, 4, 5, 6),
                               labels = c("18-25 Years Old", "26-34 Years Old",
                                          "35-49 Years Old", "50-64 Years Old", "65 or Older"))
# Label Gender Variable

NSDUH_2020$irsex <- factor(NSDUH_2020$irsex, levels = c(1, 2),
                              labels = c("Male", "Female"))

# Label Race Variable

NSDUH_2020$newrace2 <- factor(NSDUH_2020$newrace2, levels = c(1, 2, 3, 4, 5, 6, 7),
                                 labels = c("White (Non-Hispanic)", "Black (Non-Hispanic)", "American Indian/Alaska Native",
                                            "Native Hawaiian/Other Pacific Islander", "Asian", "Multiracial", "Hispanic"))

# Label Martial Status Variable

NSDUH_2020$irmarit <- factor(NSDUH_2020$irmarit, levels = c(1, 2, 3, 4),
                                labels = c("Married", "Widowed", "Divorced or Separated",
                                           "Never Been Married"))
# Label Family Income Variable

NSDUH_2020$irfamin3 <- factor(NSDUH_2020$irfamin3, levels = c(1, 2, 3, 4, 5, 6, 7), 
                                 labels = c("Less than $10,000", "$10,000-$19,999",
                                            "$20,000-$29,999", "$30,000-$39,999",
                                            "$40,000-$49,999", "$50,000-$74,999",
                                            "75,000 or more"))

# Recode Education variable

NSDUH_2020$eduhighcat <- factor(NSDUH_2020$eduhighcat, levels = c(1, 2, 3, 4, 5),
                                   labels = c("Less Than High School", "High School",
                                              "Some College or Associate's Degree",
                                              "College Graduate", "12-17 Year Olds"))

# Recode Lifetime LSD Use Variable

NSDUH_2020$lsdflag <- factor(NSDUH_2020$lsdflag, levels = c(0, 1),
                                labels = c("No", "Yes"))

# Recode Marijuana Use Variable

NSDUH_2020$mjever <- ifelse(NSDUH_2020$mjever == 1, 1, 0)
NSDUH_2020$mjever <- factor(NSDUH_2020$mjever, levels = c(0, 1),
                               labels = c("No", "Yes"))

# Recode perceived great risk of LSD variable (2020)

NSDUH_2020$grsklsdtry <- factor(NSDUH_2020$grsklsdtry, levels = c(0, 1),
                                   labels = c("No Great Risk", "Great Risk"))

# Recode perceived great risk of Marijuana 1-2x Month variable (2020)

NSDUH_2020$grskmrjmon <- factor(NSDUH_2020$grskmrjmon, levels = c(0, 1),
                                   labels = c("No Great Risk", "Great Risk"))

# Recode religious services variable (2020)

NSDUH_2020$snrlgsvc[NSDUH_2020$snrlgsvc %in% c(85, 94, 97, 98, 99)] <- 9
NSDUH_2020$snrlgsvc <- factor(NSDUH_2020$snrlgsvc, levels = c(1, 2, 3, 4, 5, 6),
                                 labels = c("0 times", "1-2 times", "3-5 times", "6-24 times",
                                            "25-52 times", "More than 52 times"))

# Recode LSD access variable (2020)

NSDUH_2020$difobtlsd[NSDUH_2020$difobtlsd == 9] <- NA
NSDUH_2020$difobtlsd <- factor(NSDUH_2020$difobtlsd)

# Recode marijuana access variable (2020)

NSDUH_2020$difobtmrj[NSDUH_2020$difobtmrj == 9] <- NA
NSDUH_2020$difobtmrj <- factor(NSDUH_2020$difobtmrj)

# Subset relevant variables (2020) 

nsduh2020_sub <- NSDUH_2020[, c("grsklsdtry", "rural_status", "catag6", "irsex", "eduhighcat",
                                "insurance", "verep", "vestrq1q4_c", "analwtq1q4_c", "lsdflag",
                                "newrace2", "irmarit", "rsklsdtry",
                                "grskmrjmon", "mjever", "irfamin3",
                                "snrlgsvc", "difobtlsd", "difobtmrj")]

# Set other unordered categorical variables as factor (2020)

categorical_vars_2020 <- c("irmarit", "mjever", "lsdflag", "irsex", "newrace2",
                           "insurance")

nsduh2020_sub <- nsduh2020_sub %>%
  mutate(across(all_of(categorical_vars_2020), as.factor))

nsduh2020.design <-  svydesign(
  ids = ~verep,
  strata= ~vestrq1q4_c,
  weights= ~analwtq1q4_c,
  data = nsduh2020_sub,
  nest = TRUE)

options(survey.lonely.psu="adjust")

# Demographics + Tests of Association (2020)----

nsduh_table_2020 <- tbl_svysummary(
  data = nsduh2020.design,
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
    difobtlsd = "LSD Ease of Access", 
    mjever = "Cannabis Lifetime Use",
    grskmrjmon = "Great Risk Cannabis - 1-2x Month Risk",
    grskmrjwk = "Great Risk Cannabis - 1-2x Week",
    difobtmrj = "Marijuana Ease of Access"
  )
) %>%
  add_p(all_categorical() ~ "svy.adj.chisq.test") %>% 
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman") %>%
  gt::gtsave(file.path(output_dir, "nsduh_table2020.rtf"))

  # LSD Models
  
  # Model 1 (Crude)
  
  LSD_Model1_2020 <- svyglm(grsklsdtry ~ rural_status, design = nsduh2020.design, family = quasibinomial())
  
  # Model 2 (Sociodemographically Adjusted)
  
  LSD_Model2_2020 <- svyglm(grsklsdtry ~ rural_status + catag6 + eduhighcat + newrace2 
                       + irsex + irmarit + irfamin3 + snrlgsvc, 
                       design = nsduh2020.design, family = quasibinomial())
  
  # Model 3 (Adjusted for Lifetime Use + Past Year Use + Ease of Access)
  
  LSD_Model3_2020 <- svyglm(grsklsdtry ~ rural_status + catag6 + eduhighcat + newrace2 
                       + irsex + irmarit + irfamin3 + snrlgsvc + difobtlsd + 
                         lsdflag, 
                       design = nsduh2020.design, family = quasibinomial())
  
  # Cannabis Models
  
  # Model 1 (Crude)
  
  Cannabis_Model1_2020 <- svyglm(grskmrjmon ~ rural_status, design = nsduh2020.design, family = quasibinomial())
  
  # Model 2 (Sociodemographically Adjusted)
  
  Cannabis_Model2_2020 <- svyglm(grskmrjmon ~ rural_status + catag6 + eduhighcat + newrace2 
                            + irsex + irmarit + irfamin3 + snrlgsvc, 
                            design = nsduh2020.design, family = quasibinomial())
  
  # Model 3 (Adjusted for Lifetime Use + Past Year Use + Ease of Access)
  
  Cannabis_Model3_2020 <- svyglm(grskmrjmon ~ rural_status + catag6 + eduhighcat + newrace2 
                            + irsex + irmarit + irfamin3 + snrlgsvc + difobtmrj + 
                              mjever, 
                          design = nsduh2020.design, family = quasibinomial())

# Check for multicolinearity using VIF (2020) 

vif(LSD_Model2_2020)
vif(LSD_Model3_2020)
vif(Cannabis_Model2_2020)
vif(Cannabis_Model3_2020)

models_2020 <- list(
  LSD_Model1_2020,
  LSD_Model2_2020,
  LSD_Model3_2020,
  Cannabis_Model1_2020,
  Cannabis_Model2_2020,
  Cannabis_Model3_2020
)

# Prep Models for Forest Plot + aOR Table (2020)

results_2020 <- lapply(models_2020, tidy)

results_df_2020 <- bind_rows(results_2020, .id = "model")

odds_ratios_2020 <- results_df_2020 %>%
  filter(term != "(Intercept)") %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error),
         p.value = format.pval(p.value, eps = 0.001)) %>%
  mutate(model = recode_factor(model,
                               "1" = "LSD_Model1_2020",
                               "2" = "LSD_Model2_2020",
                               "3" = "LSD_Model3_2020",
                               "4" = "Cannabis_Model1_2020",
                               "5" = "Cannabis_Model2_2020",
                               "6" = "Cannabis_Model3_2020"
  ))

or_table_2020 <- odds_ratios_2020 %>% filter(term == "rural_statusRural")

# 2021 Analysis----

load(file.path(data_dir, "NSDUH_2021.RDATA"))

assign("NSDUH_2021", PUF2021_100622)

names(NSDUH_2021) <- tolower(names(NSDUH_2021))
NSDUH_2021 <- NSDUH_2021[complete.cases(NSDUH_2021$grsklsdtry), ]

# Create Rural Status variable for regression 

NSDUH_2021$rural_status <- ifelse(NSDUH_2021$coutyp4 == 3, 1, 0)
NSDUH_2021$rural_status <- factor(NSDUH_2021$rural_status, levels = c(0, 1),
                                  labels = c("Urban", "Rural"))


# Create Insurance composite variable

NSDUH_2021$insurance <- ifelse(NSDUH_2021$irmedicr == 1, 1,
                               ifelse(NSDUH_2021$irmcdchp == 1, 2,
                                      ifelse(NSDUH_2021$irchmpus == 1, 3,
                                             ifelse(NSDUH_2021$irprvhlt == 1, 4,
                                                    ifelse(NSDUH_2021$irothhlt == 1, 5,
                                                           ifelse(NSDUH_2021$irinsur4 == 2, 6, NA))))))

# Label Age Variable

NSDUH_2021$catag6 <- factor(NSDUH_2021$catag6, levels = c(1, 2, 3, 4, 5, 6),
                            labels = c("12-17 Years Old", "18-25 Years Old", "26-34 Years Old",
                                       "35-49 Years Old", "50-64 Years Old", "65 or Older"))
# Label Gender Variable

NSDUH_2021$irsex <- factor(NSDUH_2021$irsex, levels = c(1, 2),
                           labels = c("Male", "Female"))

# Label Race Variable

NSDUH_2021$newrace2 <- factor(NSDUH_2021$newrace2, levels = c(1, 2, 3, 4, 5, 6, 7),
                              labels = c("White (Non-Hispanic)", "Black (Non-Hispanic)", "American Indian/Alaska Native",
                                         "Native Hawaiian/Other Pacific Islander", "Asian", "Multiracial", "Hispanic"))

# Label Martial Status Variable

NSDUH_2021$irmarit <- factor(NSDUH_2021$irmarit, levels = c(1, 2, 3, 4),
                             labels = c("Married", "Widowed", "Divorced or Separated",
                                        "Never Been Married"))
# Label Family Income Variable

NSDUH_2021$irfamin3 <- factor(NSDUH_2021$irfamin3, levels = c(1, 2, 3, 4, 5, 6, 7), 
                              labels = c("Less than $10,000", "$10,000-$19,999",
                                         "$20,000-$29,999", "$30,000-$39,999",
                                         "$40,000-$49,999", "$50,000-$74,999",
                                         "75,000 or more"))

# Recode Education variable

NSDUH_2021$eduhighcat <- factor(NSDUH_2021$eduhighcat, levels = c(1, 2, 3, 4, 5),
                                labels = c("Less Than High School", "High School",
                                           "Some College or Associate's Degree",
                                           "College Graduate", "12-17 Year Olds"))

# Recode Lifetime LSD Use Variable

NSDUH_2021$lsdflag <- factor(NSDUH_2021$lsdflag, levels = c(0, 1),
                             labels = c("No", "Yes"))

# Recode Marijuana Use Variable

NSDUH_2021$mjever <- ifelse(NSDUH_2021$mjever == 1, 1, 0)
NSDUH_2021$mjever <- factor(NSDUH_2021$mjever, levels = c(0, 1),
                            labels = c("No", "Yes"))

# Recode perceived great risk of LSD variable 

NSDUH_2021$grsklsdtry <- factor(NSDUH_2021$grsklsdtry, levels = c(0, 1),
                                labels = c("No Great Risk", "Great Risk"))

# Recode perceived great risk of Marijuana 1-2x Month variable 

NSDUH_2021$grskmrjmon <- factor(NSDUH_2021$grskmrjmon, levels = c(0, 1),
                                labels = c("No Great Risk", "Great Risk"))

# Recode religious services variable

NSDUH_2021$snrlgsvc[NSDUH_2021$snrlgsvc %in% c(85, 94, 97, 98, 99)] <- 9
NSDUH_2021$snrlgsvc <- factor(NSDUH_2021$snrlgsvc, levels = c(1, 2, 3, 4, 5, 6),
                              labels = c("0 times", "1-2 times", "3-5 times", "6-24 times",
                                         "25-52 times", "More than 52 times"))

# Recode LSD access variable

NSDUH_2021$difobtlsd[NSDUH_2021$difobtlsd == 9] <- NA
NSDUH_2021$difobtlsd <- factor(NSDUH_2021$difobtlsd)

# Recode marijuana access variable 

NSDUH_2021$difobtmrj[NSDUH_2021$difobtmrj == 9] <- NA
NSDUH_2021$difobtmrj <- factor(NSDUH_2021$difobtmrj)

# Subset relevant variables

nsduh2021_sub <- NSDUH_2021[, c("grsklsdtry", "rural_status", "catag6", "irsex", "eduhighcat",
                                "insurance", "verep", "vestr_c", "analwt_c", "lsdflag",
                                "newrace2", "irmarit", "rsklsdtry",
                                "grskmrjmon", "mjever", "irfamin3",
                                "snrlgsvc", "difobtlsd", "difobtmrj")]

# Set other unordered categorical variables as factor 

categorical_vars_2021 <- c("irmarit", "mjever", "lsdflag", "irsex", "newrace2",
                           "insurance")

nsduh2021_sub <- nsduh2021_sub %>%
  mutate(across(all_of(categorical_vars_2021), as.factor))

nsduh2021_sub <- nsduh2021_sub %>%
  mutate(across(all_of(categorical_vars_2021), as.factor))

nsduh2021.design <-  svydesign(
  ids = ~verep,
  strata= ~vestr_c,
  weights= ~analwt_c, 
  data = nsduh2021_sub,
  nest = TRUE)

# Demographics + Tests of Association (2021)----

nsduh_table_2021 <- tbl_svysummary(
  data = nsduh2021.design,
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
    difobtlsd = "LSD Ease of Access", 
    mjever = "Cannabis Lifetime Use",
    grskmrjmon = "Great Risk Cannabis - 1-2x Month Risk",
    grskmrjwk = "Great Risk Cannabis - 1-2x Week",
    difobtmrj = "Marijuana Ease of Access"
  )
) %>%
  add_p(all_categorical() ~ "svy.adj.chisq.test") %>% 
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman") %>%
  gt::gtsave(file.path(output_dir, "nsduh_table2021.rtf"))

# LSD Models

# Model 1 (Crude)

LSD_Model1_2021 <- svyglm(grsklsdtry ~ rural_status, design = nsduh2021.design, family = quasibinomial())

# Model 2 (Sociodemographically Adjusted)

LSD_Model2_2021 <- svyglm(grsklsdtry ~ rural_status + catag6 + eduhighcat + newrace2 
                          + irsex + irmarit + irfamin3 + snrlgsvc, 
                          design = nsduh2021.design, family = quasibinomial())

# Model 3 (Adjusted for Lifetime Use + Past Year Use + Ease of Access)

LSD_Model3_2021 <- svyglm(grsklsdtry ~ rural_status + catag6 + eduhighcat + newrace2 
                          + irsex + irmarit + irfamin3 + snrlgsvc + difobtlsd + 
                            lsdflag, 
                          design = nsduh2021.design, family = quasibinomial())

# Cannabis Models

# Model 1 (Crude)

Cannabis_Model1_2021 <- svyglm(grskmrjmon ~ rural_status, design = nsduh2021.design, family = quasibinomial())

# Model 2 (Sociodemographically Adjusted)

Cannabis_Model2_2021 <- svyglm(grskmrjmon ~ rural_status + catag6 + eduhighcat + newrace2 
                               + irsex + irmarit + irfamin3 + snrlgsvc, 
                               design = nsduh2021.design, family = quasibinomial())

# Model 3 (Adjusted for Lifetime Use + Past Year Use + Ease of Access)

Cannabis_Model3_2021 <- svyglm(grskmrjmon ~ rural_status + catag6 + eduhighcat + newrace2 
                               + irsex + irmarit + irfamin3 + snrlgsvc + difobtmrj + 
                                 mjever, 
                               design = nsduh2021.design, family = quasibinomial())

# Check for multicolinearity using VIF (2015-2019) 

vif(LSD_Model2_2021)
vif(LSD_Model3_2021)
vif(Cannabis_Model2_2021)
vif(Cannabis_Model3_2021)

models_2021 <- list(
  LSD_Model1_2021,
  LSD_Model2_2021,
  LSD_Model3_2021,
  Cannabis_Model1_2021,
  Cannabis_Model2_2021,
  Cannabis_Model3_2021
)

# Prep Models for Forest Plot + aOR Table (2015-2019)

results_2021 <- lapply(models_2021, tidy)

results_df_2021 <- bind_rows(results_2021, .id = "model")

odds_ratios_2021 <- results_df_2021 %>%
  filter(term != "(Intercept)") %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(estimate - 1.96 * std.error),
         upper_ci = exp(estimate + 1.96 * std.error),
         p.value = format.pval(p.value, eps = 0.001)) %>%
  mutate(model = recode_factor(model,
                               "1" = "LSD_Model1_2021",
                               "2" = "LSD_Model2_2021",
                               "3" = "LSD_Model3_2021",
                               "4" = "Cannabis_Model1_2021",
                               "5" = "Cannabis_Model2_2021",
                               "6" = "Cannabis_Model3_2021"
  ))

or_table_2021 <- odds_ratios_2021 %>% filter(term == "rural_statusRural")

