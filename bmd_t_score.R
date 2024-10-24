rm(list = ls())
options(scipen = 999)

library(dplyr)
library(tidyverse)
library(haven)
library(ggplot2)
library(tidyr)
library(janitor)
library(pander)
library(purrr)
library(readr)
library(naniar)
library(ggfortify)
library(knitr)
library(foreign)

# Load the demographic data for NHANES 2010-2011 (reference group) with age/race/ethnicity
demo_2010 <- read_xpt('DEMO_F.XPT')
demo_2010 <- demo_2010 %>%
  filter(RIDAGEYR >= 20 & RIDAGEYR <= 30) %>%     # 20-30 year olds
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1)      # Include race and ethnicity

# Merge the demographic data with femur BMD data (NHANES 2010-2011 reference group)
dxa_femur_2010 <- read_xpt('DXXFEM_F.XPT')      
dxa_femur_ref <- demo_2010 %>%
  inner_join(dxa_femur_2010, by = 'SEQN')

# Calculate means and SDs for Neck and Wards Triangle BMD by gender and race (neck of femur and ward triangle was significant initially)
age_gender_race_means_ref <- dxa_femur_ref %>%
  group_by(RIAGENDR, RIDRETH1) %>%                # Group by gender and race
  summarise(
    MEAN_REF_NKBMD = mean(DXXNKBMD, na.rm = TRUE),
    SD_REF_NKBMD = sd(DXXNKBMD, na.rm = TRUE),
    MEAN_REF_WDBMD = mean(DXXWDBMD, na.rm = TRUE),
    SD_REF_WDBMD = sd(DXXWDBMD, na.rm = TRUE)
  )

# Load the demographic data for NHANES 2017-2020 (age 50+)
demo_2017 <- read_xpt('P_DEMO.XPT')
demo_2017 <- demo_2017 %>%
  filter(RIDAGEYR >= 50) %>%  # 50+ years
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1)

# Merge the demographic data with femur BMD data (NHANES 2017-2020)
dxa_femur_2017 <- read_xpt('P_DXXFEM.XPT')        # Ensure you're reading the correct BMD data
dxa_femur_merged <- demo_2017 %>%
  inner_join(dxa_femur_2017, by = "SEQN") %>%
  left_join(age_gender_race_means_ref, by = c("RIAGENDR", "RIDRETH1"))  # Join by gender and race

# Calculate Z-scores and T-scores for each individual
dxa_femur_merged <- dxa_femur_merged %>%
  mutate(
    T_score_WDBMD = (DXXWDBMD - MEAN_REF_WDBMD) / SD_REF_WDBMD,  # T-score using reference group (20-30 years)
    T_score_NKBMD = (DXXNKBMD - MEAN_REF_NKBMD) / SD_REF_NKBMD
  )

# select only valid data and filter final dataset to use
dxa_femur_merged <- dxa_femur_merged %>%
  filter(DXAFMRST == 1) %>%                   # only valid data
  select(SEQN, T_score_WDBMD, T_score_NKBMD)

# Save the dataset with T-scores
write.csv(dxa_femur_merged, "dexa_t_scores.csv", row.names = FALSE)

