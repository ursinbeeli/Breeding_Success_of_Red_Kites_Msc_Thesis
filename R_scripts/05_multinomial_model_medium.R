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
    kde_area_50_7day,
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
#' sex:                 6.752170966087
#' kde_area_50_7day:    1.000116025352
#' age:                 0.544861132145
#' max_revisits:        0.288344659667
#' t_mean:              0.223409917356
#' revisits_nest:       0.128028565398
#' nest_dist_min:       0.062181261751
#' min_residence_time:  0.025704961852
#' residence_time_nest: 0.007485318818
#' max_residence_time:  0.006453821346
#' nest_dist_mean:      0.006335972023
#' nest_elevation:      0.006236277145
#' sl_mean:             0.003191564943
#' nest_dist_max:       0.000801835762
#' nest_dist_var:       0.000001214899

# predict breeding status on test data set
predictions_test <- predict(breeding_model, type="class", newdata=milvus_test)

# creating a confusion matrix to analyse model performance
confusionMatrix(data = predictions_test, milvus_test$breeding_status)

# Accuracy: 0.6027
# Kappa:    0.3293

# analysing the amount of predicted days/breeding status
predictions_test_df <- data.frame(predictions_test)
table(predictions_test_df$predictions_test)
table(milvus_test$breeding_status)

#' proportion of truly predicted to actual truth
#' feeding:       53    / 335
#' incubating:    2     / 254
#' non breeding:  91    / 797


