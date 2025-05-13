library(readxl)
library(dplyr)
library(tidyr)

# 1. Load the HEDIS data
hedis_measures <- read_excel("HEDIS2024.xlsx", sheet = "hedis_measures")
general_info <- read_excel("HEDIS2024.xlsx", sheet = "general")

# 2. Choose a Rate to study
# Example: Breast Cancer Screening (BCS-E)
# Find IndicatorKey for BCS-E (you need to check documentation or column names)
head(hedis_measures)
# Assume the IndicatorKey is '203602_10' for BCS-E total rate (replace if needed)
target_measure <- hedis_measures %>%
  filter(IndicatorKey == "210524_10") %>%
  select(CMSContractNumber, Rate)

head(general_info)
# 3. Prepare general plan info
general_vars <- general_info %>%
  select(CMSContractNumber = `CMS Contract Number`,
         Enrollment = `General-0050`,
         PlanType = `General-0011`,
         SpecialNeedsPlan = `General-0014`,
         PartD = `General-0016`,
         Region = `General-0060`)
# 4. Merge measure rates with plan info
dataset <- target_measure %>%
  left_join(general_vars, by = "CMSContractNumber")

# 5. Clean data
dataset_clean <- dataset %>%
  filter(!is.na(Rate), Rate != "NA", Rate != "NR", Rate != "NB", Rate != "BR") %>%  # Remove missing/bad Rate
  mutate(
    Rate = as.numeric(Rate),               # Convert Rate to numeric
    PlanSize = as.numeric(Enrollment),     # Rename Enrollment to PlanSize
    SpecialNeedsPlan = ifelse(SpecialNeedsPlan == "Yes", 1, 0),
    PartD = ifelse(PartD == "Yes", 1, 0),
    PlanType = as.factor(PlanType),
    Region = as.factor(Region)
  ) %>%
  drop_na(Rate, PlanSize, PlanType, SpecialNeedsPlan, PartD, Region) # Drop if still missing

dataset

dataset_clean
# 6. Optional: log-transform Plan Size if skewed
dataset_clean <- dataset_clean %>%
  mutate(LogPlanSize = log(PlanSize))

# 7. Run regression
model <- lm(Rate ~ LogPlanSize  + PlanType + SpecialNeedsPlan + PartD + Region, data = dataset_clean)

# 8. Output regression results
summary(model)
