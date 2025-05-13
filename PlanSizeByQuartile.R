library(dplyr)
library(broom)
library(readxl)
library(tidyr)

# Load data
hedis_measures <- read_excel("HEDIS2024.xlsx", sheet = "hedis_measures")
general_info <- read_excel("HEDIS2024.xlsx", sheet = "general")

# Indicator keys of interest
target_keys <- c(
  "200004_20", "203430_10", "203431_10", "203432_10", "203433_10",
  "200659_20", "202534_10", "203602_10", "200704_20", "200700_20",
  "200740_20", "210077_10", "210096_10", "200747_20", "200757_20",
  "200767_20", "202548_10", "200705_20", "202551_10", "201175_20",
  "201179_20", "201181_20", "200707_20", "200711_20", "202527_10",
  "210505_10", "210087_10", "201828_20", "201958_20", "210088_10",
  "201962_20", "201964_20", "210067_10", "202143_20", "202446_20",
  "202451_20", "202455_20", "202457_20", "202517_10", "202520_10",
  "202523_10", "202526_10", "202530_10"
)

# Clean general info
general_vars <- general_info %>%
  select(CMSContractNumber = `CMS Contract Number`,
         Enrollment = `General-0050`,
         PlanType = `General-0011`,
         SpecialNeedsPlan = `General-0014`,
         PartD = `General-0016`,
         Region = `General-0060`)

# Filter and clean HEDIS data
dataset <- hedis_measures %>%
  filter(IndicatorKey %in% target_keys, !Rate %in% c("NA", "NR", "NB", "BR"), !is.na(Rate)) %>%
  left_join(general_vars, by = "CMSContractNumber") %>%
  mutate(
    Rate = as.numeric(Rate),
    Denominator = as.numeric(Denominator),
    PlanSize = as.numeric(Enrollment),
    LogPlanSize = log(PlanSize),
    SpecialNeedsPlan = ifelse(SpecialNeedsPlan == "Yes", 1, 0),
    PartD = ifelse(PartD == "Yes", 1, 0),
    PlanType = as.factor(PlanType),
    Region = as.factor(Region)
  ) %>%
  filter(!is.na(Rate), !is.na(LogPlanSize), !is.na(PlanType), !is.na(SpecialNeedsPlan), !is.na(PartD), !is.na(Region))

# Add quartile info
dataset <- dataset %>%
  group_by(IndicatorKey) %>%
  mutate(EnrollmentQuartile = ntile(PlanSize, 4)) %>%
  ungroup()

# Regression results per IndicatorKey × Quartile
regression_results <- dataset %>%
  group_by(IndicatorKey, EnrollmentQuartile) %>%
  do({
    model <- lm(Rate ~ LogPlanSize + PlanType + SpecialNeedsPlan + PartD + Region, data = .)
    tidy(model) %>%
      filter(term == "LogPlanSize") %>%
      select(estimate, std.error, p.value)
  }) %>%
  ungroup() %>%
  rename(
    Coefficient = estimate,
    StdError = std.error,
    PValue = p.value
  )

# Descriptive statistics per IndicatorKey × Quartile
descriptive_stats <- dataset %>%
  group_by(IndicatorKey, EnrollmentQuartile) %>%
  summarise(
    MeanPlanSize = mean(PlanSize, na.rm = TRUE),
    MeanDenominator = mean(Denominator, na.rm = TRUE),
    MeanRate = mean(Rate, na.rm = TRUE),
    NumContracts = n_distinct(CMSContractNumber),
    .groups = "drop"
  )

# Combine regression and descriptive stats
final_table <- left_join(regression_results, descriptive_stats,
                         by = c("IndicatorKey", "EnrollmentQuartile"))



# Step A: Create a lookup table
care_categories <- tibble::tibble(
  IndicatorKey = c(
    "200004_20", "203430_10", "203431_10", "203432_10", "203433_10",
    "200659_20", "202534_10", "203602_10", "200704_20", "200700_20",
    "200740_20", "210077_10", "210096_10", "200747_20", "200757_20",
    "200767_20", "202548_10", "200705_20", "202551_10", "201175_20",
    "201179_20", "201181_20", "200707_20", "200711_20", "202527_10",
    "210505_10", "210087_10", "201828_20", "201958_20", "210088_10",
    "201962_20", "201964_20", "210067_10", "202143_20", "202446_20",
    "202451_20", "202455_20", "202457_20", "202517_10", "202520_10",
    "202523_10", "202526_10", "202530_10"
  ),
  CareCategory = c(
    "AAP – Adults' Access to Preventive/Ambulatory Services",
    "AIS-E – Adult Immunization Status (Influenza (19–65))",
    "AIS-E – Adult Immunization Status (Pneumococcal (66+))",
    "AIS-E – Adult Immunization Status (Td/Tdap (19–65))",
    "AIS-E – Adult Immunization Status (Zoster (50–65))",
    "AMM – Antidepressant Medication Management",
    "ASF-E – Alcohol Use Screening and Follow-Up",
    "BCS-E – Breast Cancer Screening",
    "BPD – BP Control for Patients with Diabetes",
    "CBP – Controlling High Blood Pressure",
    "COL – Colorectal Cancer Screening",
    "CRE – Cardiac Rehabilitation",
    "DAE – High-Risk Meds in the Elderly",
    "DDE – Drug-Disease Interactions in Elderly",
    "DMS-E – Use of PHQ-9 for Monitoring Depression",
    "DRR-E – Depression Remission or Response",
    "DSF-E – Depression Screening and Follow-Up",
    "EED – Eye Exam for Patients with Diabetes",
    "FMC – Follow-Up for High-Risk Chronic Conditions",
    "FUA – Follow-Up for Alcohol/Drug Dependence",
    "FUH – Follow-Up After Hospitalization (Mental Health)",
    "FUM – Follow-Up After ED Visit (Mental Health)",
    "HBD – HbA1c Control for Patients with Diabetes",
    "HBD – HbA1c Control for Patients with Diabetes (poor)",
    "HDO – High-Dosage Opioid Use",
    "IET – Initiation/Engagement in AOD Treatment",
    "KED – Kidney Health Evaluation",
    "LDM – Language Diversity of Membership",
    "OMW – Osteoporosis Management Post-Fracture",
    "OSW – Osteoporosis Screening in Older Women",
    "PBH – Persistence of Beta-Blockers After AMI",
    "PCE – COPD Exacerbation Pharmacotherapy",
    "POD – Opioid Disorder Pharmacotherapy",
    "PSA – Non-Recommended PSA-Based Screening in Older Men",
    "SAA – Antipsychotic Adherence (Schizophrenia)",
    "SPC – Statin Use for Cardiovascular Disease",
    "SPD – Statin Use for Diabetes",
    "SPR – Spirometry Testing for COPD",
    "TRC – Transitions of Care (Medication Reconciliation Post-Discharge (Total))",
    "TRC – Transitions of Care (Notification of Inpatient Admission (Total))",
    "TRC – Transitions of Care (Patient Engagement After Inpatient Discharge (Total))",
    "TRC – Transitions of Care (Receipt of Discharge Information (Total))",
    "UOP – Opioid Use from Multiple Providers"
  )
)

final_table_named <- final_table %>%
  mutate(Effect_10pct = Coefficient * log(1.1)) %>%
  left_join(care_categories, by = "IndicatorKey") %>%
  relocate(CareCategory, .after = IndicatorKey) 

# View updated table
print(final_table_named, n = 200)


library(tidyr)
library(dplyr)

# Step 1: Pivot plan size wide across quartiles
plan_size_wide <- descriptive_stats %>%
  select(IndicatorKey, EnrollmentQuartile, MeanPlanSize) %>%
  pivot_wider(
    names_from = EnrollmentQuartile,
    values_from = MeanPlanSize,
    names_prefix = "Quartile_"
  )

# Step 2: Join with care category names
plan_size_named <- plan_size_wide %>%
  left_join(care_categories, by = "IndicatorKey") %>%
  select(CareCategory, starts_with("Quartile_")) %>%
  arrange(CareCategory)

# Step 3: Optional — format with flextable
library(flextable)

flextable(plan_size_named) %>%
  set_caption("Average Plan Size by Quartile for Each HEDIS Measure") %>%
  colformat_double(digits = 0) %>%
  fontsize(size = 10, part = "all") %>%
  autofit() %>%
  align(j = 1, align = "left") %>%
  align(j = 2:5, align = "center")

write.csv(plan_size_named, "plan_size_by_quartile.csv", row.names = FALSE)




