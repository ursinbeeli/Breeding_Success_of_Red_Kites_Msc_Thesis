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
    t_mean,
    nest_dist_min,
    residence_time_nest, revisits_nest,
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
#' sex:                 6.463626942
#' kde_area_50_7day:    1.533402187
#' age:                 0.462626753
#' revisits_nest:       0.229617400
#' t_mean:              0.225531495
#' nest_dist_min:       0.054183002
#' residence_time_nest: 0.007323113

# predict breeding status on test data set
predictions_test <- predict(breeding_model, type="class", newdata=milvus_test)

# creating a confusion matrix to analyse model performance
confusionMatrix(data = predictions_test, milvus_test$breeding_status)

# Accuracy: 0.6096
# Kappa:    0.3703

# analysing the amount of predicted days/breeding status
predictions_test_df <- data.frame(predictions_test)
table(predictions_test_df$predictions_test)
table(milvus_test$breeding_status)

#' proportion of truly predicted to actual truth
#' feeding:       70    / 335
#' incubating:    8     / 254
#' non breeding:  68    / 797


