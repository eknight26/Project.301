# NHANES dataset 2017-2020

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

#### demographics
demo <- read_xpt("P_DEMO.XPT")
demo <- demo %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3, INDFMPIR)

#### osteoporosis, create target variable column hx_fracture where a respondent has had a fracture
#### either on their hip, wrist or spine in the part and drop those original columns
osteo <- read_xpt("P_OSQ.XPT") %>%
  select(SEQN, OSQ060, OSQ080, OSQ130, OSQ150, OSQ170, OSQ200, OSQ010A, OSQ010B, OSQ010C) %>%
  mutate(hx_fracture = if_else(rowSums(across(c('OSQ010A', 'OSQ010B', 'OSQ010C')) == 1) > 0, 1, 0))

# merge demographic data and osteoporosis data (where fracture as response variable is located)
fracture <- demo %>%
  inner_join(osteo, by = join_by(SEQN))
  
# removes the 3 columns used to make target variable
main_data <- fracture %>% select(-OSQ010A, -OSQ010B, -OSQ010C)

#### measurements
bp <- read_xpt('P_BPXO.XPT')            # blood pressure readings
bmx <- read_xpt('P_BMX.XPT')            # to obtain BMI
dexa_spine <- read_xpt('P_DXXSPN.XPT')  # spine BMD   
dexa_femur <- read_xpt('P_DXXFEM.XPT')  # femur BMD

# blood pressure (systolic BP (mean)); created a new column for systolic mean BP
bp <- bp %>% 
  mutate(BPXMSYS = rowMeans(select(., BPXOSY1, BPXOSY2, BPXOSY3))) %>%
  select(SEQN, BPXMSYS)

# body measurements (BMI), valid data filtered (coded as 1)
bmx <- bmx %>%
  filter(BMDSTATS == 1) %>%
  select(SEQN, BMXBMI)

# dexa femur, valid data filtered (coded as 1); only selected BMD columns
dexa_femur <- dexa_femur %>%
  filter(DXAFMRST == 1) %>%
  select(c(SEQN, DXXOFBMD, DXXNKBMD, DXXTRBMD, DXXINBMD, DXXWDBMD))

# dexa spine, completed scan filtered (coded as 1); 
# The main reasons for completed, but invalid (coded as 2), spine scans were an insufficient scan area or partial scan, 
# degenerative disease/severe scoliosis, and sclerotic spine/spinal fusion/laminectomy (these were included as these are commonly found in older adults and
# including this is more representative of the population)
# only selected BMD columns
dexa_spine <- dexa_spine %>%
  filter(DXASPNST == 1 | DXASPNST == 2) %>%
  select(c(SEQN, DXXOSBMD, DXXL1BMD, DXXL2BMD, DXXL3BMD, DXXL4BMD))


#### blood tests
# folate
folate <- read_xpt('P_FOLFMS.XPT')
folate <- folate %>%
  select(SEQN, LBDFOTSI)

# standard biochemistry blood tests (ALP, phosphate, calcium only)
biochem <- read_xpt('P_BIOPRO.XPT') %>% 
  select(SEQN, LBXSAPSI, LBDSPHSI, LBDSCASI)

# blood levels of heavy metals (Lead, Cadmium, Mercury, Selenium, Manganese)
metals <- read_xpt('P_PBCD.XPT')
metals <- metals %>%
  select(SEQN, LBDBPBSI, LBDBCDSI, LBDTHGSI, LBDBSESI, LBDBMNSI)


# alcohol use, if ever drank 1, else 0; if drinking, ave number of drinks per day in the last 12 mos
alc_intake <- read_xpt('P_ALQ.XPT') %>%
  select(SEQN, ALQ111, ALQ130) %>%
  mutate(alcohol_consumed = ifelse(ALQ111 == 1, ALQ130, 0)) %>%
  select(SEQN, alcohol_consumed)

# has high blood pressure
bpressure <- read_xpt('P_BPQ.XPT') 
bpressure <- bpressure %>%
  select(SEQN, BPQ020, BPQ080)

# has diabetes
diabetes <- read_xpt('P_DIQ.XPT') %>%
  select(SEQN, DIQ010)


#### various medical conditions (asthma, heart, diabetes, lung, liver, etc)
med_cond <- read_xpt('P_MCQ.XPT') %>%
  select(SEQN, MCQ160A, MCQ160B, MCQ160C, MCQ160E, MCQ160F, MCQ160L, MCQ160M, MCQ160P, MCQ080, MCQ010, MCQ220)


#### physical activity amount, represents sedentary lifestyle (amount of time spent sitting per day in minutes)
activity <- read_xpt("P_PAQ.XPT") %>%
  select(SEQN, PAD680)


#### smoking, if smoked in the last 5 days 1, else 0; if yes, number of cigs smoked per day in the last 5 days
smoking <- read_xpt("P_SMQRTU.XPT") %>%
  select(SEQN, SMQ681, SMQ720) %>%
  mutate(cigarettes_smoked = ifelse(SMQ681 == 1, SMQ720, 0)) %>%
  select(SEQN, cigarettes_smoked)


# merge all datasets to main data, 'left_join' to maintain main set structure
main_data <- main_data %>%
  left_join(dexa_femur, by = join_by(SEQN)) %>%
  left_join(dexa_spine, by = join_by(SEQN)) %>%
  left_join(bmx, by = join_by(SEQN)) %>%
  left_join(bp, by = join_by(SEQN)) %>%
  left_join(folate, by = join_by(SEQN)) %>%
  left_join(biochem, by = join_by(SEQN)) %>%
  left_join(metals, by = join_by(SEQN)) %>%
  left_join(alc_intake, by = join_by(SEQN)) %>%
  left_join(bpressure, by = join_by(SEQN)) %>%
  left_join(diabetes, by = join_by(SEQN)) %>%
  left_join(med_cond, by = join_by(SEQN)) %>%
  left_join(activity, by = join_by(SEQN)) %>%
  left_join(smoking, by = join_by(SEQN))

# check dimension/shape of dataset; 4987 instances, 50 columns
dim(main_data)

# all numeric, some are categorical encoded as numbers (to convert as factor)
str(main_data)

# check numeric and categorical columns, all numeric
sapply(main_data, class)

# summary for numerical variables
# checking for values, min, max, unusual entries, IQR and NAs
pander(summary(main_data))

# check if all respondents are all unique (no duplicates)
length(unique(main_data$SEQN)) == dim(main_data)[1]

# check few instances
head(main_data)

# get column names
names(main_data)

# check class label for imbalance; 0 = 4260, 1 = 726
pander(table(main_data$hx_fracture))


#### MISSING VALUES
# get total number of NA values
total_na_count <- colSums(is.na(main_data))

# calculate the percentage of missing values
na_proportion <- colSums(is.na(main_data)) / nrow(main_data) * 100

# create a data frame
na_proportion <- data.frame(
  Feature = names(na_proportion),
  Count = total_na_count,
  Percent_Missing = na_proportion
)

# sort descending
na_proportion <- na_proportion[order(na_proportion$Percent_Missing, decreasing = TRUE), ]

# save as .csv
write.csv(na_proportion, "na_proportion.csv", row.names = FALSE)


# filter the data to only include features with missing values
na_proportion_filtered <- na_proportion[na_proportion$Percent_Missing > 0, ]

# create a bar plot of missing values
ggplot(na_proportion_filtered, aes(x = reorder(Feature, Percent_Missing), y = Percent_Missing)) +
  geom_bar(stat = "identity", fill = "grey50") +          # Greyscale fill for the bars
  coord_flip() +
  labs(x = "Variable Names", y = "Missing Values (%)",
       title = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),  # Customize the title
    axis.title.x = element_text(size = 12),              # Customize the x-axis label
    axis.title.y = element_text(size = 12),              # Customize the y-axis label
    axis.text = element_text(color = "black")            # Keep axis text black for visibility
  )


# there is also one NA entry on 'hx_fracture' column, will remove this instance
main_data <- main_data %>% drop_na(hx_fracture)

# a row with NA in target column is now removed
sum(is.na(main_data$hx_fracture))

main_data$hx_fracture <- as.factor(main_data$hx_fracture)

# save raw data as .csv
write_csv(main_data, 'main_data.csv')


###### MERGING with t_score dataset for BMD

# load data
dexa_t_score <- read_csv('dexa_t_scores.csv', show_col_types = FALSE)

# merge to main dataset
main_data <- main_data %>%
  left_join(dexa_t_score, by = join_by(SEQN))

# save another copy
write_csv(main_data, 'main_data_t_score.csv')