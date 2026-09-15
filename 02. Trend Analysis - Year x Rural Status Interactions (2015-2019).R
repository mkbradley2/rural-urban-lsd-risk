# ------------------------------------------------------------------------------
# Rural-Urban Divide in Risk Perception of LSD (NSDUH 2015-2021)
# Bradley, M., Grossman, D., Simonsson, O., Copes, H., & Hendricks, P. S. (2025). Rural-urban divide in risk perception of LSD: Implications for psychedelic-assisted therapy. The Journal of Rural Health, 41(1), e12906.
# https://doi.org/10.1111/jrh.12906
#
# Script: 02. Trend Analysis - Year x Rural Status Interactions (2015-2019).R
#   Supplementary trend models: year x rural status interactions for each level of
#   perceived LSD risk (great, moderate, slight, none), 2015-2019 pooled.
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

# Create LSD Moderate Risk variable for regression (2015-2019)

nsduh20152019$modlsdtry <- ifelse(nsduh20152019$rsklsdtry == 3, 1, 0)
nsduh20152019$modlsdtry <- factor(nsduh20152019$modlsdtry, levels = c(0, 1),
                                     labels = c("Other Than Moderate Risk", "Moderate Risk"))

# Create LSD Slight Risk variable for regression (2015-2019)

nsduh20152019$slightlsdtry <- ifelse(nsduh20152019$rsklsdtry == 2, 1, 0)
nsduh20152019$slightlsdtry <- factor(nsduh20152019$slightlsdtry, levels = c(0, 1),
                                  labels = c("Other Than Slight Risk", "Slight Risk"))

# Create LSD No Risk variable for regression (2015-2019)

nsduh20152019$nolsdtry <- ifelse(nsduh20152019$rsklsdtry == 1, 1, 0)
nsduh20152019$nolsdtry <- factor(nsduh20152019$nolsdtry, levels = c(0, 1),
                                     labels = c("Other Than No Risk", "No Risk"))

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
                                       "snrlgsvc", "difobtlsd", "difobtmrj", "hltinmnt",
                                       "year", "nolsdtry", "slightlsdtry", "modlsdtry")]

# Create survey design object (2015-2019)

nsduh.design <-  svydesign(
  ids = ~verep,
  strata= ~vestr,
  weights= ~newanalwt, 
  data = nsduh20152019_sub,
  nest = TRUE)

# Model 3.5: LSD Great Risk x Year Interaction 

LSD_Model3.5_Great <- svyglm(grsklsdtry ~ year * rural_status + catag6 + eduhighcat + newrace2 
                                 + irsex + irmarit + irfamin3 + snrlgsvc + difobtlsd + lsdflag, 
                                 design = nsduh.design, family = quasibinomial())

summary(LSD_Model3.5_Great)

# Model 3.5 + LSD Moderate Risk x Year Interaction

LSD_Model3.5_Moderate <- svyglm(modlsdtry ~ year * rural_status + catag6 + eduhighcat + newrace2 
                                 + irsex + irmarit + irfamin3 + snrlgsvc + difobtlsd + lsdflag, 
                                 design = nsduh.design, family = quasibinomial())

summary(LSD_Model3.5_Moderate)

# Model 3.5 + LSD Slight Risk x Year Interaction

LSD_Model3.5_Slight <- svyglm(slightlsdtry ~ year * rural_status + catag6 + eduhighcat + newrace2 
                                + irsex + irmarit + irfamin3 + snrlgsvc + difobtlsd + lsdflag, 
                                design = nsduh.design, family = quasibinomial())

summary(LSD_Model3.5_Slight)

# Model 3.5 + LSD No Risk x Year Interaction

LSD_Model3.5_No <- svyglm(nolsdtry ~ year * rural_status + catag6 + eduhighcat + newrace2 
                              + irsex + irmarit + irfamin3 + snrlgsvc + difobtlsd + lsdflag, 
                              design = nsduh.design, family = quasibinomial())

summary(LSD_Model3.5_No)

