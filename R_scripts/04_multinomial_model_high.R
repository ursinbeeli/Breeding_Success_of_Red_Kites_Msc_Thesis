library(here)
library(dplyr)
library(nnet)
library(caret)



# disabling scientific notation
options(scipen=999)

# LOADING DATA -----------------------------------------------------------------
milvus <- read.csv(here("data/modified/03_multinomial/06_milvus_parameters_daily_ground_truth.csv"))



# DATA PREPARATION -------------------------------------------------------------
# cast dependend variable to factor
milvus$breeding_status <- as.factor(milvus$breeding_status)

# keep only relevant columns
milvus <- milvus %>%
  select(
    year_id, sex, age,
    max_residence_time, min_residence_time, max_revisits, sl_mean, t_mean,
    nest_dist_max, nest_dist_min, nest_dist_mean, nest_dist_var,
    residence_time_nest, revisits_nest, nest_elevation,
    kde_area_95, kde_area_95_7day, kde_area_50_7day, kde_area_95_5day,
    kde_area_50_5day, kde_area_95_3day, kde_area_95_mw, kde_area_95_mw_diff,
    locations_per_day,
    breeding_status
  )



# SPLIT DATA FRAME RANDOMLY INTO TWO--------------------------------------------
set.seed(102)
milvus_id <- unique(milvus$year_id)
milvus_id_train <- sample(milvus_id, round(length(milvus_id)*.7))

milvus_train <- milvus %>%
  filter(year_id %in% milvus_id_train) %>%
  select(-year_id)
milvus_test <- milvus %>%
  filter(!year_id %in% milvus_id_train) %>%
  select(-year_id)



# MODEL BUILDING ---------------------------------------------------------------
breeding_model <- multinom(breeding_status~., data=milvus_train, maxit=500, trace=T)

# finding out which variables are most important
most_imp_vars <- varImp(breeding_model)
most_imp_vars$Variables <- row.names(most_imp_vars)
most_imp_vars <- most_imp_vars[order(-most_imp_vars$Overall),]

#' MOST IMPORTANT VARIABLES
#' sex:                 9.302990291783
#' kde_area_50_7day:    7.895342425246
#' kde_area_50_5day:    5.479789835513
#' locations_per_day:   0.885418875829
#' kde_area_95_mw:      0.732379695242
#' kde_area_95_7day:    0.676352919813
#' kde_area_95:         0.627471303103
#' age:                 0.584564171843
#' kde_area_95_5day:    0.554659752877
#' max_revisits:        0.485700207788
#' t_mean:              0.265129052636
#' kde_area_95_mw_diff: 0.261555896168
#' kde_area_95_3day:    0.134553062269
#' revisits_nest:       0.129474372994
#' min_residence_time:  0.112761720429
#' nest_dist_min:       0.043426083967
#' nest_elevation:      0.017832818214
#' residence_time_nest: 0.007443849535
#' nest_dist_mean:      0.006773977754
#' max_residence_time:  0.003429780638
#' sl_mean:             0.003333859481
#' nest_dist_max:       0.000297568353
#' nest_dist_var:       0.000003800926

# predict breeding status on test data set
predictions_test <- predict(breeding_model, type="class", newdata=milvus_test)

# creating a confusion matrix to analyse model performance
confusionMatrix(data = predictions_test, milvus_test$breeding_status)

# Accuracy: 0.5548
# Kappa:    0.2992

# analysing the amount of predicted days/breeding status
predictions_test_df <- data.frame(predictions_test)
table(predictions_test_df$predictions_test)
table(milvus_test$breeding_status)

#' proportion of truly predicted to actual truth
#' feeding:       57    / 335
#' incubating:    25    / 254
#' non breeding:  64    / 797


