library(dplyr)
library(tidyverse)
library(haven)
library(ggplot2)
library(tidyr)
library(janitor)
library(pander)
library(purrr)
library(readr)

main_prepr <- read.csv('main_data.csv')

# summary for numerical variables
# checking for values, min, max, unusual entries, IQR and NAs
pander(summary(main_prepr))


#### EXAMINING COLUMNS AND converting to NAs the 'refused' and 'don't know' answers
# demographic and osteoporosis
main_prepr$DMDEDUC2[main_prepr$DMDEDUC2 %in% c(7, 9)] <- NA
main_prepr$DMDMARTZ[main_prepr$DMDMARTZ %in% c(77, 99)] <- NA
main_prepr$OSQ060[main_prepr$OSQ060 %in% c(7, 9)] <- NA
main_prepr$OSQ080[main_prepr$OSQ080 %in% c(7, 9)] <- NA
main_prepr$OSQ130[main_prepr$OSQ130 %in% c(7, 9)] <- NA
main_prepr$OSQ150[main_prepr$OSQ150 %in% c(7, 9)] <- NA
main_prepr$OSQ170[main_prepr$OSQ170 %in% c(7, 9)] <- NA
main_prepr$OSQ200[main_prepr$OSQ200 %in% c(7, 9)] <- NA

# dietary
main_prepr$DRQSDIET[main_prepr$DRQSDIET %in% c(7, 9)] <- NA
main_prepr$DSDCOUNT[main_prepr$DSDCOUNT %in% c(77, 99)] <- NA
main_prepr$DSD010[main_prepr$DSD010 %in% c(7, 9)] <- NA
main_prepr$DSD010AN[main_prepr$DSD010AN %in% c(7, 9)] <- NA


# alcohol
main_prepr$ALQ130[main_prepr$ALQ130 %in% c(777, 999)] <- NA

# BP
main_prepr$BPQ020[main_prepr$BPQ020 %in% c(7, 9)] <- NA
main_prepr$BPQ080[main_prepr$BPQ080 %in% c(7, 9)] <- NA

# diabetes
main_prepr$DIQ010[main_prepr$DIQ010 %in% c(3, 7, 9)] <- NA

# dietary behaviour
main_prepr$DBQ700[main_prepr$DBQ700 %in% c(7, 9)] <- NA
main_prepr$DBQ197[main_prepr$DBQ197 %in% c(7, 9)] <- NA

# medical condition
main_prepr$MCQ160A[main_prepr$MCQ160A %in% c(7, 9)] <- NA
main_prepr$MCQ160B[main_prepr$MCQ160B %in% c(7, 9)] <- NA
main_prepr$MCQ160C[main_prepr$MCQ160C %in% c(7, 9)] <- NA
main_prepr$MCQ160E[main_prepr$MCQ160E %in% c(7, 9)] <- NA
main_prepr$MCQ160F[main_prepr$MCQ160F %in% c(7, 9)] <- NA
main_prepr$MCQ160L[main_prepr$MCQ160L %in% c(7, 9)] <- NA
main_prepr$MCQ160M[main_prepr$MCQ160M %in% c(7, 9)] <- NA
main_prepr$MCQ160P[main_prepr$MCQ160P %in% c(7, 9)] <- NA
main_prepr$MCQ080[main_prepr$MCQ080 %in% c(7, 9)] <- NA
main_prepr$MCQ010[main_prepr$MCQ010 %in% c(7, 9)] <- NA
main_prepr$MCQ220[main_prepr$MCQ220 %in% c(7, 9)] <- NA

# physical activity
main_prepr$PAQ605[main_prepr$PAQ605 %in% c(7, 9)] <- NA
main_prepr$PAQ620[main_prepr$PAQ620 %in% c(7, 9)] <- NA
main_prepr$PAQ635[main_prepr$PAQ635 %in% c(7, 9)] <- NA
main_prepr$PAQ650[main_prepr$PAQ650 %in% c(7, 9)] <- NA
main_prepr$PAQ665[main_prepr$PAQ665 %in% c(7, 9)] <- NA
main_prepr$PAD680[main_prepr$PAD680 %in% c(7777, 9999)] <- NA

# smoking
main_prepr$SMQ020[main_prepr$SMQ020 %in% c(7, 9)] <- NA
main_prepr$SMQ040[main_prepr$SMQ040 %in% c(7, 9)] <- NA

# define a dataset to revert back to for EDA
main_for_eda <- main_prepr


#### IMPUTE NAs
# define a dataset for imputing
main_to_impute <- main_prepr
pander(summary(main_to_impute))

# impute with most frequent
nominal_cols <- c('DMDEDUC2', 'DMDMARTZ', 'OSQ060', 'OSQ080', 'OSQ130', 'OSQ150', 'OSQ170', 'OSQ200', 'DRQSDIET',
                  'DSD010', 'DSD010AN', 'ALQ111', 'ALQ130', 'BPQ020', 'BPQ080', 'DIQ010', 'DBQ700', 'DBQ197', 'MCQ160A', 
                  'MCQ160B', 'MCQ160C', 'MCQ160E', 'MCQ160F', 'MCQ160L', 'MCQ160M', 'MCQ160P', 'MCQ080', 'MCQ010', 'MCQ220',
                  'PAQ605', 'PAQ620', 'PAQ635', 'PAQ650', 'PAQ665', 'SMQ020', 'SMQ040')


# impute with mean
numeric_cols <- c('INDFMPIR', 'DXXOFBMD', 'DXXOFBMC', 'DXXOFA', 'DXXNKBMD', 'DXXNKBMC', 'DXXNKA', 'DXXTRBMD', 'DXXTRBMC', 'DXXTRA', 
                  'DXXINBMD', 'DXXINBMC', 'DXXINA', 'DXXWDBMD', 'DXXWDBMC', 'DXXWDA', 'DXXOSBMD', 'DXXOSBMC', 'DXXOSA', 
                  'DXXL1BMD', 'DXXL1BMC', 'DXXL1A', 'DXXL2BMD', 'DXXL2BMC', 'DXXL2A', 'DXXL3BMD', 'DXXL3BMC', 'DXXL3A', 
                  'DXXL4BMD', 'DXXL4BMC', 'DXXL4A', 'DXAFMRK', 'DXAFMRD0', 'DXASPNK', 'DXASPND0', 'BMXBMI', 'BPXMSYS', 
                  'DR1TKCAL', 'DR1TPROT', 'DR1TCARB', 'DR1TSUGR', 'DR1TFIBE', 'DR1TTFAT', 'DR1TSFAT', 'DR1TMFAT', 'DR1TPFAT', 
                  'DR1TCHOL', 'DR1TATOC', 'DR1TRET', 'DR1TVARA', 'DR1TACAR', 'DR1TBCAR', 'DR1TCRYP', 'DR1TLYCO', 'DR1TLZ', 
                  'DR1TVB1', 'DR1TVB2', 'DR1TNIAC', 'DR1TVB6', 'DR1TFOLA', 'DR1TFA', 'DR1TFDFE', 'DR1TCHL', 'DR1TVB12', 
                  'DR1TVD', 'DR1TVC', 'DR1TVK', 'DR1TCALC', 'DR1TPHOS', 'DR1TMAGN', 'DR1TIRON', 'DR1TZINC', 'DR1TCOPP', 
                  'DR1TSODI', 'DR1TPOTA', 'DR1TSELE', 'DR1TCAFF', 'DR1TTHEO', 'DR1TALCO', 'DSDCOUNT', 'DSQTVD', 
                  'LBDFOTSI', 'LBXSAPSI', 'LBDSPHSI', 'LBDSCASI', 'LBDBPBSI', 'LBDBCDSI', 'LBDTHGSI', 
                  'LBDBSESI', 'LBDBMNSI', 'PAD680')

# impute numeric cols with mean
main_to_impute <- main_to_impute %>%
  mutate(across(all_of(numeric_cols), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# impute categ cols with mode
main_to_impute <- main_to_impute %>%
  mutate(across(all_of(nominal_cols), ~ ifelse(is.na(.), as.numeric(names(sort(table(.), decreasing = TRUE)[1])), .)))


# check for remaining NAs
colSums(is.na(main_to_impute))



#### FEATURE SELECTION ####

main_feat_selection <- main_to_impute

# find highly correlated pairs and keep one that is less correlated with the rest
library(caret)
corr_matrix <- cor(main_feat_selection)
highly_correlated <- findCorrelation(corr_matrix, cutoff = 0.8, names = TRUE, verbose = TRUE)
redundant_cols <- names(main_feat_selection[, highly_correlated]) # 34 columns to remove

library(randomForest)

# main_dropped_na <- drop_na(main_to_impute)
# there will only be 91 observations when NAs are dropped

# fit the Random Forest model
# Exclude the SEQN column from the predictors
rf_model <- randomForest(as.factor(has_fracture) ~ . - SEQN, data = main_feat_selection, importance = TRUE)

# extract feature importance
feature_importance <- importance(rf_model)
important_features <- names(sort(feature_importance, decreasing = TRUE))

df_feature_importance <- as.data.frame(feature_importance)
df_feature_importance$Feature <- rownames(df_feature_importance)  

# plot feature importance
ggplot(df_feature_importance, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Feature") +
  ylab("Mean Decrease in Gini") +
  ggtitle("Feature Importance from Random Forest")

#### select top 15% of important features to reduce feature space
num_features_to_keep <- ceiling(0.15 * nrow(df_feature_importance))

# get the top 15% of features by MDG ==== 20 features
top_features <- df_feature_importance[order(df_feature_importance$MeanDecreaseGini, decreasing = TRUE)[1:num_features_to_keep], ]


#### create new data frame with top features (reverting back to dataframe prior to imputing) ####
# also moving class label column at the rightmost column
main_final <- main_for_eda %>%
  select(all_of(top_features$Feature))
main_final$has_fracture <- as.factor(main_prepr$has_fracture)

# convert columns to factors as categorical variables
main_final$OSQ080 <- as.factor(main_final$OSQ080)
main_final$RIDRETH3 <- as.factor(main_final$RIDRETH3)

# save to .csv file
write_csv(main_final, 'main_final.csv')
