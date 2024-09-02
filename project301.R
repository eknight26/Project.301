# NHANES dataset 2017-2020
# Predictors of fractures

rm(list= ls())

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

#### demographics
demo <- read_xpt("P_DEMO.XPT")
demo <- demo %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2, DMDMARTZ, INDFMPIR)

#### osteoporosis
osteo <- read_xpt("P_OSQ.XPT") %>%
  select(SEQN, OSQ060, OSQ080, OSQ130, OSQ150, OSQ170, OSQ200, OSQ010A, OSQ010B, OSQ010C)

#### main_data
# merge demographic data and osteoporosis data (where fracture as response variable is located)
fracture <- demo %>%
  inner_join(osteo, by = join_by(SEQN))

# creates new column as response variable (ever had fractures - either hip, wrist or spine)
main_data <- fracture %>% 
  mutate(has_fracture = if_else(rowSums(across(c('OSQ010A', 'OSQ010B', 'OSQ010C')) == 1) > 0, 1, 2))

# removes the 3 columns used to make response var column
main_data <- main_data %>% select(-OSQ010A, -OSQ010B, -OSQ010C)


#### dietary

nutr_tot_intake1 <- read_xpt('P_DR1TOT.XPT')
nutr_tot_intake1 <- nutr_tot_intake1 %>% 
  filter(DR1DRSTZ == 1) %>%   # filter only reliable and data that meet minimum criteria
  select(SEQN, DRQSDIET, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR, DR1TFIBE, DR1TTFAT,
         DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, DR1TATOC, DR1TRET, DR1TVARA, 
         DR1TACAR, DR1TBCAR, DR1TCRYP, DR1TLYCO, DR1TLZ, DR1TVB1, DR1TVB2,
         DR1TNIAC, DR1TVB6, DR1TFOLA, DR1TFA, DR1TFDFE, DR1TCHL, DR1TVB12, 
         DR1TVD, DR1TVC, DR1TVK, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON, DR1TZINC,
         DR1TCOPP, DR1TSODI, DR1TPOTA, DR1TSELE, DR1TCAFF, DR1TTHEO, DR1TALCO)

nutr_supp <- read_xpt('P_DSQTOT.XPT') %>%
  select(SEQN, DSDCOUNT, DSD010, DSD010AN, DSQTVB1, DSQTVB2, DSQTNIAC, DSQTVB6, DSQTFA,
         DSQTFDFE, DSQTCHL, DSQTVB12, DSQTVC, DSQTVK, DSQTVD, DSQTCALC, DSQTPHOS,
         DSQTMAGN, DSQTIRON, DSQTZINC, DSQTCOPP, DSQTSODI, DSQTPOTA, DSQTSELE,
         DSQTCAFF, DSQTIODI)

# join both dietary datasets
dietary <- nutr_tot_intake1 %>%
  inner_join(nutr_supp, by = join_by(SEQN))


#### measurements
bp <- read_xpt('P_BPXO.XPT')
bmx <- read_xpt('P_BMX.XPT')
dexa_spine <- read_xpt('P_DXXSPN.XPT')
dexa_femur <- read_xpt('P_DXXFEM.XPT')

# blood pressure (systolic BP (mean))
bp <- bp %>% 
  mutate(BPXMSYS = rowMeans(select(., BPXOSY1, BPXOSY2, BPXOSY3))) %>%
  select(SEQN, BPXMSYS)

# body measurements (BMI)
bmx <- bmx %>%
  filter(BMDSTATS == 1) %>%
  select(SEQN, BMXBMI)

# dexa femur
dexa_femur <- dexa_femur %>%
  filter(DXAFMRST == 1) %>%
  select(-c(DXAFMRST, DXXFMBCC))

# dexa spine
dexa_spine <- dexa_spine %>%
  filter(DXASPNST == 1) %>%
  select(-c(DXASPNST, DXXOSBCC, DXXL1BCC, DXXL2BCC, DXXL3BCC, DXXL4BCC))


#### blood tests
# folate
folate <- read_xpt('P_FOLFMS.XPT')
folate <- folate %>%
  select(SEQN, LBDFOTSI)

# standard biochemistry blood tests (ALP, phosphate, calcium)
biochem <- read_xpt('P_BIOPRO.XPT') %>% 
  select(SEQN, LBXSAPSI, LBDSPHSI, LBDSCASI)

# blood levels of heavy metals (Lead, Cadmium, Mercury, Selenium, Manganese)
metals <- read_xpt('P_PBCD.XPT')
metals <- metals %>%
  select(SEQN, LBDBPBSI, LBDBCDSI, LBDTHGSI, LBDBSESI, LBDBMNSI)


#### alcohol use, blood_pressure, diet behaviour and milk consumption
alc_intake <- read_xpt('P_ALQ.XPT') %>%
  select(SEQN, ALQ111, ALQ130)

bpressure <- read_xpt('P_BPQ.XPT') 
bpressure <- bpressure %>%
  select(SEQN, BPQ020, BPQ080)
  
diabetes <- read_xpt('P_DIQ.XPT') %>%
  select(SEQN, DIQ010)

diet_behave <- read_xpt('P_DBQ.XPT') %>%
  select(SEQN, DBQ700, DBQ197)


#### various medical conditions (asthma, heart, diabetes, lung, liver, etc)
med_cond <- read_xpt('P_MCQ.XPT') %>%
  select(SEQN, MCQ160A, MCQ160B, MCQ160C, MCQ160E, MCQ160F, MCQ160L, MCQ160M, MCQ160P, MCQ080, MCQ010, MCQ220)


#### physical activity
activity <- read_xpt("P_PAQ.XPT") %>%
  select(SEQN, PAQ605, PAQ620, PAQ635, PAQ650, PAQ665, PAD680)


#### smoking
smoking <- read_xpt("P_SMQ.XPT") %>%
  select(SEQN, SMQ020, SMQ040)


# merge all datasets to main data, 'left_join' to maintain main set structure
# a large reduction in number of observations was lost when using inner_join
main_data <- main_data %>%
  left_join(dexa_femur, by = join_by(SEQN)) %>%
  left_join(dexa_spine, by = join_by(SEQN)) %>%
  left_join(bmx, by = join_by(SEQN)) %>%
  left_join(bp, by = join_by(SEQN)) %>%
  left_join(dietary, by = join_by(SEQN)) %>%
  left_join(folate, by = join_by(SEQN)) %>%
  left_join(biochem, by = join_by(SEQN)) %>%
  left_join(metals, by = join_by(SEQN)) %>%
  left_join(alc_intake, by = join_by(SEQN)) %>%
  left_join(bpressure, by = join_by(SEQN)) %>%
  left_join(diabetes, by = join_by(SEQN)) %>%
  left_join(diet_behave, by = join_by(SEQN)) %>%
  left_join(med_cond, by = join_by(SEQN)) %>%
  left_join(activity, by = join_by(SEQN)) %>%
  left_join(smoking, by = join_by(SEQN))

# check dimension/shape of dataset
dim(main_data)

# all numeric, some are categorical encoded as numbers (to convert as factor)
str(main_data)

# check numeric and categorical columns
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

# check class label for imbalance
pander(table(main_data$has_fracture))


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

# Create a bar plot of missing values
ggplot(na_proportion, aes(x = reorder(Feature, Percent_Missing), y = Percent_Missing)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(x = "Column", y = "Missing Values (%)",
    title = "Percentage of Missing Values by Feature") +
  theme_minimal()

# remove features over 60% missing data
na_proportion_reduced <- as.data.frame(na_proportion[na_proportion$Percent_Missing > 60,])
columns_to_drop <- na_proportion_reduced$Feature

# update dataset
main_data <- main_data %>% 
  select(-all_of(columns_to_drop))

# there is also one NA entry on 'has_fracture' column, will remove this instance
main_data <- main_data %>% drop_na(has_fracture)

# a row with NA in target column is now removed
sum(is.na(main_data$has_fracture))
