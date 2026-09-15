# Rural-Urban Divide in Risk Perception of LSD: Implications for Psychedelic-Assisted Therapy
### Created by: Melissa Bradley
### Version: 09/14/2026

## Background
This repository contains the statistical analysis code for:

> Bradley, M., Grossman, D., Simonsson, O., Copes, H., & Hendricks, P. S. (2025). Rural-urban divide in risk perception of LSD: Implications for psychedelic-assisted therapy. The Journal of Rural Health, 41(1), e12906. https://doi.org/10.1111/jrh.12906

The study uses the National Survey on Drug Use and Health (NSDUH) to compare perceived risk of LSD and cannabis use between rural and metropolitan residents in the United States, 2015-2021.

## Overview

- **Data:** NSDUH public-use files, 2015-2021. Survey years 2015-2019 are pooled; 2020 and 2021 are analyzed separately.
- **Exposure:** Rural (nonmetropolitan) vs. metropolitan county of residence (`COUTYP4`).
- **Primary Outcomes:**
  - Perceived great risk of trying LSD once or twice (`GRSKLSDTRY`)
  - Perceived great risk of using cannabis once or twice a month (`GRSKMRJMON`)
- **Statistical Models and Tests:**
  - Survey-weighted descriptive statistics with Rao-Scott chi-square tests (`gtsummary::tbl_svysummary`)
  - Survey-weighted logistic regression (`survey::svyglm`, quasibinomial family) in three nested models:
    1. Crude
    2. Adjusted for age, sex, race/ethnicity, education, marital status, family income, and religious service attendance
    3. Model 2 plus lifetime use and perceived ease of access to the substance
  - Year x rural status interaction terms to test trends across 2015-2019
  - Variance inflation factors for multicollinearity checks
- **Survey design:** Strata `VESTR`, primary sampling units `VEREP`, nested. For the pooled 2015-2019 analysis, the analysis weight `ANALWT_C` is divided by 5.

## Scripts

- **01. Main Analysis - Descriptives and Survey-Weighted Logistic Models (2015-2021).R**
  Final analysis for the published manuscript. Data preparation and recoding, Table 1 descriptives, and the crude, sociodemographically adjusted, and fully adjusted models for 2015-2019 (pooled), 2020, and 2021. Also fits the year x rural status interaction reported in the manuscript. Writes descriptive tables to `Outputs/`.

- **02. Trend Analysis - Year x Rural Status Interactions (2015-2019).R**
  Supplementary trend models fitting year x rural status interactions for each level of perceived LSD risk (great, moderate, slight, none).

- **03. Initial Analysis - Forest Plots and Supplementary Models (2015-2021).R**
  Pre-revision analysis retained for transparency. Includes the original model specifications, odds-ratio tables, forest plots, and additional descriptive breakdowns. Superseded by script 01 for the published estimates.

## Data

NSDUH public-use files are not redistributed in this repository. Download the R-format data file for each survey year from the SAMHSA Data Archive (https://www.datafiles.samhsa.gov/) and save them in `Data/` with the following names. The scripts expect the object names SAMHSA assigns to each file, as of the release used for the manuscript:

| File | Object name in `.RDATA` |
|---|---|
| `NSDUH_2015.RDATA` | `PUF2015_021518` |
| `NSDUH_2016.RDATA` | `PUF2016_022818` |
| `NSDUH_2017.RDATA` | `PUF2017_100918` |
| `NSDUH_2018.RDATA` | `PUF2018_100819` |
| `NSDUH_2019.RDATA` | `PUF2019_100920` |
| `NSDUH_2020.RDATA` | `NSDUH_2020` |
| `NSDUH_2021.RDATA` | `PUF2021_100622` |

If SAMHSA has re-released a file under a different object name, update the corresponding `assign()` line near the top of each analysis section.

## Requirements

R 4.x with the following packages: `tidyverse`, `survey`, `srvyr`, `gtsummary`, `gt`, `broom`, `car`, `scales`, `reshape2`, `knitr`, `kableExtra`, `svydiags`.

## Citation

If you use this code, please cite the paper above.
