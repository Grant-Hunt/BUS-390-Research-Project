library(readxl)
library(dplyr)
library(tidyr)

# 1. Load data
hedis_measures <- read_excel("HEDIS2024.xlsx", sheet = "hedis_measures")
general_info <- read_excel("HEDIS2024.xlsx", sheet = "general")

# 2. Specify IndicatorKey list
target_keys <- c(
  "200004_20", "203430_10", "203431_10", "203432_10", "203433_10",
  "200659_20", "202534_10", "203602_10", "200704_20", "200700_20",
  "200740_20", "210077_10", "210096_10", "200747_20", "200757_20",
  "200767_20", "202548_10", "200705_20", "202551_10", "201175_20",
  "201179_20", "201181_20", "200707_20", "200711_20", "202527_10",
  "210505_10", "210087_10", "201828_20", "201958_20", "210088_10",
  "201962_20", "201964_20", "202143_20", "210067_10", "202446_20",
  "202451_20", "202455_20", "202457_20", "202517_10", "202520_10",
  "202523_10", "202526_10", "202530_10"
)

# 3. Clean and merge general info
general_vars <- general_info %>%
  select(CMSContractNumber = `CMS Contract Number`,
         Enrollment = `General-0050`)

# 4. Filter and clean HEDIS measures
hedis_clean <- hedis_measures %>%
  filter(IndicatorKey %in% target_keys, !Rate %in% c("NA", "NR", "NB", "BR"), !is.na(Rate)) %>%
  mutate(
    Rate = as.numeric(Rate),
    Denominator = as.numeric(Denominator)
  ) %>%
  left_join(general_vars, by = "CMSContractNumber") %>%
  mutate(
    PlanSize = as.numeric(Enrollment)
  ) %>%
  drop_na(Rate, PlanSize, Denominator)

# 5. Group by IndicatorKey and compute descriptive statistics
descriptive_stats <- hedis_clean %>%
  group_by(IndicatorKey) %>%
  summarise(
    AvgDenominator = mean(Denominator, na.rm = TRUE),
    AvgEnrollment = mean(PlanSize, na.rm = TRUE),
    AvgRate = mean(Rate, na.rm = TRUE),
    NumPlans = n_distinct(CMSContractNumber),
    .groups = "drop"
  ) %>%
  arrange(IndicatorKey)

# 6. View result
print(n=50,descriptive_stats)

# 7. Export to CSV for Excel
write.csv(descriptive_stats, "hedis_descriptive_stats.csv", row.names = FALSE)




