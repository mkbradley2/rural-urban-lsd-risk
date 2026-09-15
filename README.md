# Rural-Urban Divide in Risk Perception of LSD: Implications for Psychedelic-Assisted Therapy
### Created by: Melissa Bradley
### Version: 09/14/2026

## Background
This repository contains the R code used to analyze rural-urban differences in perceived risk of LSD and cannabis use in the United States using the National Survey on Drug Use and Health (NSDUH), 2015-2021. The findings of these analyses are available [here](https://doi.org/10.1111/jrh.12906).

## Overview

- **Data:** NSDUH public-use files, 2015-2021. Survey years 2015-2019 are pooled; 2020 and 2021 are analyzed separately.

- **Exposure:** Rural (nonmetropolitan) vs. metropolitan county of residence

- **Primary Outcomes:**
  - Perceived great risk of trying LSD once or twice
  - Perceived great risk of using cannabis once or twice a month

- **Statistical Models and Tests:**
  - Survey-weighted descriptive statistics with Rao-Scott chi-square tests
  - Survey-weighted logistic regression (crude, sociodemographically adjusted, and adjusted for lifetime use and ease of access)
  - Year x rural status interaction terms to assess trends across 2015-2019
  - Variance inflation factors to check for multicollinearity

## Data
NSDUH public-use files are not included in this repository. The R-format data file for each survey year can be downloaded from the [SAMHSA Data Archive](https://www.datafiles.samhsa.gov/) and saved in the **/Data/** folder as **NSDUH_2015.RDATA** through **NSDUH_2021.RDATA**. The scripts load each file by the object name assigned by SAMHSA at the time of analysis (e.g., `PUF2015_021518`); if a file has since been re-released under a different object name, update the corresponding `assign()` line.

## Scripts

- **01. Main Analysis - Descriptives and Survey-Weighted Logistic Models (2015-2021).R**  
  Data preparation and recoding, Table 1 descriptives, and the crude, sociodemographically adjusted, and fully adjusted logistic models for 2015-2019 (pooled), 2020, and 2021. Produces the estimates reported in the manuscript.

- **02. Trend Analysis - Year x Rural Status Interactions (2015-2019).R**  
  Supplementary trend models fitting year x rural status interactions for each level of perceived LSD risk (great, moderate, slight, none).

- **03. Initial Analysis - Forest Plots and Supplementary Models (2015-2021).R**  
  Pre-revision analysis retained for transparency, including the original model specifications, odds ratio tables, and forest plots. Superseded by script 01 for the published estimates.
