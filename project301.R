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

# there is 1 NA on response variable
# sum(is.na(main_data$has_fracture))
