# BUS-390 Final Research Project  
**A Statistical Approach to Understanding Plan Size and Its Effect on HEDIS Performance in Medicare Advantage**

## Project Overview
This project explores whether Medicare Advantage (MA) plan size is associated with differences in HEDIS (Healthcare Effectiveness Data and Information Set) performance across categories of care. Using linear regression and quartile-based descriptive statistics, we analyze the relationship between plan enrollment size and quality performance across dozens of care categories.

---

## Dataset Files

The following files were obtained from CMS.gov’s [MA HEDIS Public Use Files](https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/ma-hedis-public-use-files/ma-hedis-public-use-files-0):

- `HEDIS2024Documentation.docx` – Documentation describing HEDIS rates and categories of care.
- `MADictionary2024.xlsx` – Maps indicator keys to specific HEDIS measures.
- `HEDIS2024.xlsx` – Raw dataset with HEDIS performance data and plan metadata.
- `hedis_measures.csv` – Contains HEDIS rate data at the contract level.
- `general.csv` – Contains plan-level metadata, including enrollment (plan size).

---

## R Code Files and Outputs

### Regression Analysis and Table Creation

- `RegressionResultsForIndicator.R`, `DescriptiveStatistics.R`, `NewTable1.R`  
  Used together to produce `Results - 95% CI (1).csv`, which contains regression results for each significant category of care.  
  - **Output:** Table 2 (Regression Results by Category)

### Plan Size Quartile Analysis

- `PlanSizeByQuartile.R`  
  Computes plan size distribution by quartile and generates `plan_size_by_quartile.csv`.  
  - **Output:** Table A (Plan Size by Quartile), Table 3 (Totals by Quartile)

- `RateByQuartile.R`  
  Aggregates average HEDIS rates and number of significant results by quartile into `rate_by_quartile.csv`.  
  - **Output:** Table 4 (HEDIS Rate by Quartile)

- `QuartileFiltered.R`  
  Filters and summarizes regression results to include only statistically significant findings, grouped by quartile, into `hedis_quartiles_significant.csv`.  
  - **Output:** Table 5 (Significant Results by Quartile)

---

## Use of AI Assistance

All R scripts used in this project were originally generated with the assistance of **ChatGPT (OpenAI)**. ChatGPT was used to:
- Structure R scripts for regression modeling, data wrangling, and table generation
- Debug code and adapt syntax to the structure of the Medicare Advantage HEDIS datasets
- Improve code readability, efficiency, and reproducibility

The scripts were subsequently reviewed and manually edited to ensure they aligned with the dataset structure and research objectives. Final outputs were validated using visual inspection and descriptive checks.

---

## Figures

- **Figure 1**  
  A bar chart displaying the distribution of HEDIS rate changes across categories of care.  
  - **Source File:** `Results - 95% CI (1).csv`  
  - **Created in:** Tableau Public

---

## Citation

**Data Source:**  
Centers for Medicare & Medicaid Services. *MA HEDIS Public Use Files*.  
Available at: [CMS.gov](https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/ma-hedis-public-use-files/ma-hedis-public-use-files-0)
