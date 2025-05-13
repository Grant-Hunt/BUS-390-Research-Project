

# Load libraries
library(readr)
library(dplyr)
library(flextable)

# Load the CSV file
results_table <- read_csv("Results - 95% CI (1).csv")

# Clean and prepare table
results_table <- results_table %>%
  mutate(
    Coefficient = round(Coefficient, 2),
    `10% Increase` = round(`10% Increase`, 2),
    AvgDenominator = round(AvgDenominator),
    AvgEnrollment = round(AvgEnrollment),
    AvgRate = round(AvgRate, 1),
    NumPlans = as.integer(NumPlans)
  ) %>%
  rename(
    `HEDIS Measure` = `HEDIS Measure Category`,
    `10% Size Effect` = `10% Increase`,
    `Avg Denominator` = AvgDenominator,
    `Plan Avg Enrollment` = AvgEnrollment,
    `Avg Rate` = AvgRate,
    `Number of Plans` = NumPlans
  ) %>%
  select(-Code)  # Remove Indicator Key column

# Create flextable
flextable(results_table) %>%
  set_caption("Table 1 - Summary of Care Categories with Regression Results and Descriptive Statistics") %>%
  fontsize(size = 10, part = "all") %>%
  autofit() %>%
  bold(i = ~ Significance == "***", j = "Significance") %>%
  align(j = 1, align = "left", part = "all") %>%  # Align first column left
  align(j = 2:ncol(results_table), align = "center", part = "all")  # Rest centered

