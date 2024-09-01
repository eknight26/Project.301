# NHANES dataset 2017-2020

rm(list= ls())

library(dplyr)
library(haven)
library(ggplot2)
library(tidyr)
library(janitor)

#### demographics
demo <- read_xpt("P_DEMO.XPT")
demog <- demo %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2, DMDMARTZ, INDFMPIR)

#### osteoporosis
osteo <- read_xpt("P_OSQ.XPT") %>%
  select(SEQN, OSQ060, OSQ080, OSQ130, OSQ150, OSQ170, OSQ200, OSQ010A, OSQ010B, OSQ010C)

#### main_data
# merge demographic data and osteoporosis data (where fracture as response variable is located)
fracture <- demog %>%
  inner_join(osteo, by = join_by(SEQN))

# creates new column as response variable (ever had fractures - either hip, wrist or spine)
main_data <- fracture %>% 
  mutate(has_fracture = if_else(rowSums(across(c('OSQ010A', 'OSQ010B', 'OSQ010C')) == 1) > 0, 1, 2))

# removes the 3 columns used to make response var column
main_data <- main_data %>% select(-OSQ010A, -OSQ010B, -OSQ010C)

# convert columns to factors (as categorical)
main_data <- main_data %>% mutate(across(c(RIAGENDR, RIDRETH3, DMDEDUC2, DMDMARTZ, OSQ060,
                                           OSQ080, OSQ130, OSQ150, OSQ170, OSQ200), as.factor))

#### dietary

nutr_tot_intake1 <- read_xpt('P_DR1TOT.XPT')
nutr_tot_intake1 <- nutr_tot_intake1 %>% 
  filter(DR1DRSTZ == 1) %>%
  select(SEQN, DRQSDIET, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TSUGR, DR1TFIBE, DR1TTFAT,
         DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TCHOL, DR1TATOC, DR1TRET, DR1TVARA, 
         DR1TACAR, DR1TBCAR, DR1TCRYP, DR1TLYCO, DR1TLZ, DR1TVB1, DR1TVB2,
         DR1TNIAC, DR1TVB6, DR1TFOLA, DR1TFA, DR1TFDFE, DR1TCHL, DR1TVB12, 
         DR1TVD, DR1TVC, DR1TVK, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON, DR1TZINC,
         DR1TCOPP, DR1TSODI, DR1TPOTA, DR1TSELE, DR1TCAFF, DR1TTHEO, DR1TALCO)

nutr_supp <- read_xpt('P_DSQTOT.XPT') %>%
  select(SEQN, DSDCOUNT, DSD010AN, DSQTVB1, DSQTVB2, DSQTNIAC, DSQTVB6, DSQTFA,
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

bp <- bp %>% 
  mutate(BPXMSYS = rowMeans(select(., BPXOSY1, BPXOSY2, BPXOSY3))) %>%
  select(SEQN, BPXMSYS)

bmx <- bmx %>%
  filter(BMDSTATS == 1) %>%
  select(SEQN, BMXBMI)

dexa_femur <- dexa_femur %>%
  filter(DXAFMRST == 1) %>%
  select(-c(DXAFMRST, DXXFMBCC))

dexa_spine <- dexa_spine %>%
  filter(DXASPNST == 1) %>%
  select(-c(DXASPNST, DXXOSBCC, DXXL1BCC, DXXL2BCC, DXXL3BCC, DXXL4BCC))


#### blood tests
folate <- read_xpt('P_FOLFMS.XPT')
folate <- folate %>%
  select(SEQN, LBDFOTSI)

biochem <- read_xpt('P_BIOPRO.XPT') %>% 
  select(SEQN, LBXSAPSI, LBDSPHSI, LBDSCASI)

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


#### medical conditions
med_cond <- read_xpt('P_MCQ.XPT') %>%
  select(SEQN, MCQ160A, MCQ160B, MCQ160C, MCQ160E, MCQ160F, MCQ160L, MCQ160M, MCQ160P, MCQ080, MCQ010, MCQ220)


#### physical activity
activity <- read_xpt("P_PAQ.XPT") %>%
  select(SEQN, PAQ605, PAQ620, PAQ635, PAQ650, PAQ665, PAD680)


#### smoking
smoking <- read_xpt("P_SMQ.XPT") %>%
  select(SEQN, SMQ020, SMQ040)


# merge all datasets to main data
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

# all numeric, some are categorical encoded as numbers (to convert as factor)
str(main_data)

# check if all respondents are all unique (no duplicates)
length(unique(main_data$SEQN)) == dim(main_data)[1]

# summary for numerical variables
# checking for values, min, max, unusual entries, IQR and NAs
pander(summary(main_data))

# check few instances
head(main_data)

# get column names
names(main_data)

# check class
sapply(main_data, class)

# check class label for imbalance
pander(table(main_data$has_fracture))

# bar plot to check 'gender' distribution 
ggplot(main_data, aes(has_fracture, fill = RIAGENDR)) +
  geom_bar(position = "stack") +
  labs(x = "Ever had a fracture",
       y = "Frequency",
       fill = "Gender")

main_data <- main_data %>% drop_na(has_fracture)

# a row with NA in target column is now removed
sum(is.na(main_data$has_fracture))

# save as .csv
write_csv(main_data, 'main_data.csv')
