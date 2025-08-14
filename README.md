# Socioeconomic Status, Identity, and Mindset: A Path Analysis of Community College STEM Achievement

This project examines whether Pell Grant eligibility, gender, or generational status moderate the relationships between students’ meritocratic beliefs, STEM identity, and growth mindset, and whether these beliefs help explain academic performance (GPA) at a two-year Hispanic-Serving Institution (HSI). Rather than focusing solely on academic metrics, this analysis explores how students’ beliefs and identities may influence their academic outcomes.

## Overview

Survey data was collected from over 400 STEM students at an HSI community college in California using the **Culturally Contextual STEM Identity (CCSI v5)** survey.  
Three composite indices were created:

- **Meritocratic Beliefs** — The belief that success is determined solely by individual ability and effort.
- **STEM Identity** — A sense of belonging, engagement, and identification within STEM.
- **Growth Mindset** — The belief that abilities can be developed through effort, strategies, and support.

A multi-group structural equation model (SEM) was used to test whether the relationships between these beliefs and cumulative GPA differ by Pell Grant eligibility, gender, or generational college status.

## Key Results

- **No statistically significant moderation** was found for Pell Grant eligibility, gender, or generational status using chi-square difference tests.
- **Female students** with higher growth mindset scores had significantly higher GPAs (`β = 0.184`, `p = 0.034`).
- **Non-Pell students** with higher meritocratic beliefs tended to have lower GPAs (`β = -0.137`, `p = 0.095`), though this result was marginal.
- All models explained **little variance in GPA** (highest R² = 0.026), suggesting that psychosocial beliefs alone do not strongly predict GPA.

## Limitations

This analysis is based on cross-sectional self-reported survey data, which means results show associations but cannot establish causality. Responses may be subject to bias if participants misunderstood questions or responded in socially desirable ways. The study was conducted at a single HSI community college, so results may not generalize to other institutions or contexts. Academic, structural, and personal factors not included in this model likely play a larger role in predicting GPA.

## Key Files

- `reports/report.qmd` — Quarto report with full methods, results, and discussion
- `reports/report.html` — Rendered HTML version of the report
- `reports/references.bib` — Bibliography for citations
- `outputs/` — Figures and tables (Cronbach’s alpha plots, correlation heatmaps, path diagrams)
- `scripts/` — R scripts for data cleaning, reliability checks, modeling, and visualization
- `README.md` — Project overview

## Data Privacy

Raw survey data has been excluded from this repository in order to protect participant confidentiality. Only cleaned and de-identified processed data are included in `data/processed/`. The analysis is fully documented and reproducible with a dataset that follows the same structure.

## View the Report

*(https://josephclayton41.github.io/Socioeconomic-Status-Identity-and-Mindset-A-Path-Analysis-of-Community-College-STEM-Achievement/)*

[LinkedIn](https://www.linkedin.com/in/josephpclayton)
