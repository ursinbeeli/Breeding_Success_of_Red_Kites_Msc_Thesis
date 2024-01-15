library(here)
library(dplyr)
library(brms)



# LOADING DATA -----------------------------------------------------------------
# Movement data
milvus <- read.csv(here("../Data/german_birds.csv"))
# Multinomial Logistic Regression Model
brood_model_15 <- readRDS(here("../Data/Output/08_mlrm_model/brood_model_15.rds"))



# DATA PREPARATION -------------------------------------------------------------
# Casting dependent variable and sex to factor
milvus$breeding_status <- factor(milvus$breeding_status, levels = c("nonbreeding", "incubating", "feeding"))
milvus$sex <- factor(milvus$sex, levels = c(0,1))



# CREATING TABLE OF MODEL PERFORMANCE
# Creating directory to save results
if (!dir.exists(here("../Data/Output/11_mlrm_performance_de"))) {
  if (!dir.exists(here("../Data/Output"))) {
    dir.create("../Data/Output")
  }
  dir.create("../Data/Output/11_mlrm_performance_de")
}

# Predicting unknown data
model_prediction <- predict(brood_model_15, newdata = milvus, allow_new_levels = T) %>%
  as.data.frame()

# Storing raw output with probabilities
model_prediction_raw <- model_prediction

# Setting highest probability to 1 and rest to 0
for (i in 1:nrow(model_prediction)) {
  model_prediction[i,as.integer(which.max(model_prediction[i,]))] <- 1
}
model_prediction[model_prediction < 1] <- 0

# Adding ground truth to prediction
milvus_test_sub <- milvus %>%
  select(year_id, sex = sex_chr, date, breeding_status)
model_prediction <- bind_cols(model_prediction, milvus_test_sub) %>%
  mutate(p_feeding = `P(Y = feeding)`,
         p_incubating = `P(Y = incubating)`,
         p_nonbreeding = `P(Y = nonbreeding)`) %>%
  select(-`P(Y = feeding)`, -`P(Y = incubating)`, -`P(Y = nonbreeding)`)
model_prediction_raw <- bind_cols(model_prediction_raw, milvus_test_sub) %>%
  mutate(p_feeding = `P(Y = feeding)`,
         p_incubating = `P(Y = incubating)`,
         p_nonbreeding = `P(Y = nonbreeding)`) %>%
  select(-`P(Y = feeding)`, -`P(Y = incubating)`, -`P(Y = nonbreeding)`)

# Adding columns to determine per row if prediction is correct
model_prediction$accuracy_feeding <- 0
model_prediction$accuracy_incubating <- 0
model_prediction$accuracy_nonbreeding <- 0
model_prediction$accuracy <- 0
for (i in 1:nrow(model_prediction)) {
  if (model_prediction[i,]$p_feeding == 1 &
      model_prediction[i,]$breeding_status == "feeding") {
    model_prediction[i,]$accuracy_feeding <- 1
  }
  if (model_prediction[i,]$p_incubating == 1 &
      model_prediction[i,]$breeding_status == "incubating") {
    model_prediction[i,]$accuracy_incubating <- 1
  }
  if (model_prediction[i,]$p_nonbreeding == 1 &
      model_prediction[i,]$breeding_status == "nonbreeding") {
    model_prediction[i,]$accuracy_nonbreeding <- 1
  }
}
model_prediction$accuracy <- model_prediction$accuracy_feeding +
  model_prediction$accuracy_incubating + model_prediction$accuracy_nonbreeding

write.csv(model_prediction_raw, here("../Data/Output/11_mlrm_performance_de/01_bm_15_predictions_raw.csv"),
          row.names = F)

write.csv(model_prediction, here("../Data/Output/11_mlrm_performance_de/02_bm_15_predictions.csv"),
          row.names = F)



# THRESHOLD VALUES -------------------------------------------------------------
duration_incubating <- 28
duration_feeding <- 42
duration_breeding <- duration_incubating + duration_feeding
duration_total <- as.integer(as.Date("2022-07-31") - as.Date("2022-03-10")) + 1



# RAW OUTPUT ANALYSIS ----------------------------------------------------------
a_raw_output_analysis <- model_prediction %>%
  group_by(year_id) %>%
  summarise(n_days_incubating = sum(p_incubating),
            n_days_feeding = sum(p_feeding),
            n_days_nonbreeding = sum(p_nonbreeding),
            n_days_total = n()) %>%
  mutate(success = NA,
         validity = NA)

# Evaluating the breeding success
for (i in 1:nrow(a_raw_output_analysis)) {
  if (a_raw_output_analysis[i,]$n_days_incubating >= duration_incubating && a_raw_output_analysis[i,]$n_days_feeding >= duration_feeding) {
    a_raw_output_analysis[i,]$success <- T
  }
}

# Calculating the validity
a_raw_output_analysis$validity <- a_raw_output_analysis$n_days_total/duration_total

# Saving data frame
write.csv(a_raw_output_analysis, here("../Data/Output/11_mlrm_performance_de/03_brood_phase_prediction_raw.csv"),
          row.names = F)

# Calculating stats for confusion matrix
n_inc_TP <- model_prediction[model_prediction$accuracy_incubating == 1 ,]
n_fee_TP <- model_prediction[model_prediction$accuracy_feeding == 1 ,]
n_non_TN <- model_prediction[model_prediction$accuracy_nonbreeding == 1 ,]

n_inc_tot <- model_prediction[model_prediction$breeding_status == "incubating" ,]
n_fee_tot <- model_prediction[model_prediction$breeding_status == "feeding" ,]
n_non_tot <- model_prediction[model_prediction$breeding_status == "nonbreeding" ,]

n_inc_predicted <- model_prediction[model_prediction$p_incubating == 1 ,]
n_fee_predicted <- model_prediction[model_prediction$p_feeding == 1 ,]
n_non_predicted <- model_prediction[model_prediction$p_nonbreeding == 1 ,]

n_inc_FP_fee <- model_prediction[model_prediction$p_incubating == 1 & model_prediction$breeding_status == "feeding" ,]
n_inc_FP_non <- model_prediction[model_prediction$p_incubating == 1 & model_prediction$breeding_status == "nonbreeding" ,]
n_fee_FP_inc <- model_prediction[model_prediction$p_feeding == 1 & model_prediction$breeding_status == "incubating" ,]
n_fee_FP_non <- model_prediction[model_prediction$p_feeding == 1 & model_prediction$breeding_status == "nonbreeding" ,]
n_non_FN_inc <- model_prediction[model_prediction$p_nonbreeding == 1 & model_prediction$breeding_status == "incubating" ,]
n_non_FN_fee <- model_prediction[model_prediction$p_nonbreeding == 1 & model_prediction$breeding_status == "feeding" ,]









# MERGED CATEGORIES ------------------------------------------------------------
# Merging incubating + feeding = breeding
b_merged_categories <- model_prediction_raw %>%
  mutate(p_breeding = p_incubating + p_feeding) %>%
  select(-p_incubating, -p_feeding)

# If p > 0.5 then this category applies
b_merged_categories[b_merged_categories$p_nonbreeding > .5 ,]$p_nonbreeding <- 1
b_merged_categories[b_merged_categories$p_nonbreeding < 1 ,]$p_nonbreeding <- 0
b_merged_categories[b_merged_categories$p_breeding >= .5 ,]$p_breeding <- 1
b_merged_categories[b_merged_categories$p_breeding < 1 ,]$p_breeding <- 0

# Adapting ground truth
b_merged_categories <- b_merged_categories %>%
  mutate(breeding_status = as.character(breeding_status))
b_merged_categories[b_merged_categories$breeding_status == "incubating" ,]$breeding_status <- "breeding"
b_merged_categories[b_merged_categories$breeding_status == "feeding" ,]$breeding_status <- "breeding"

# Counting days per individual
b_merged_categories_grouped <- b_merged_categories %>%
  group_by(year_id) %>%
  summarise(n_days_breeding = sum(p_breeding),
            n_days_nonbreeding = sum(p_nonbreeding),
            n_days_total = n()) %>%
  mutate(success = NA,
         validity = NA)

# Evaluating the breeding success
for (i in 1:nrow(b_merged_categories_grouped)) {
  if (b_merged_categories_grouped[i,]$n_days_breeding >= duration_breeding) {
    b_merged_categories_grouped[i,]$success <- T
  }
}

# Calculating the validity
b_merged_categories_grouped$validity <- b_merged_categories_grouped$n_days_total/duration_total

# Saving data frame
write.csv(b_merged_categories_grouped, here("../Data/Output/11_mlrm_performance_de/04_brood_phase_prediction_merged_categories.csv"),
          row.names = F)

# Calculating stats for confusion matrix
n_bre_TP <- b_merged_categories[b_merged_categories$p_breeding == 1 & b_merged_categories$breeding_status == "breeding" ,]
n_non_TN <- b_merged_categories[b_merged_categories$p_nonbreeding == 1 & b_merged_categories$breeding_status == "nonbreeding" ,]

n_bre_tot <- b_merged_categories[b_merged_categories$breeding_status == "breeding" ,]
n_non_tot <- b_merged_categories[b_merged_categories$breeding_status == "nonbreeding" ,]

n_bre_predicted <- b_merged_categories[b_merged_categories$p_breeding == 1 ,]
n_non_predicted <- b_merged_categories[b_merged_categories$p_nonbreeding == 1 ,]

n_bre_FP <- b_merged_categories[b_merged_categories$p_breeding == 1 & b_merged_categories$breeding_status == "nonbreeding" ,]
n_non_FN <- b_merged_categories[b_merged_categories$p_nonbreeding == 1 & b_merged_categories$breeding_status == "breeding" ,]









# RULE BASED OUTPUT ADAPTIATION

#' - Every date before first day where p_incubation > p_nonbreeding & p_incubation > p_feeding equals "nonbreeding".
#' - First day of incubation must not be isolated (it must followed by five other incubation days)
#' - After this date, ever day day with p_incubation + p_feeding > p_nonbreeding equals "breeding" (incubating/feeding).
#' - Date of first incubation + 37 days is the first possible "feeding" date. <<--------------------------------------------- PROBLEMATIC!!!
#' - After date of first incubation + 87 days no more "incubation" days possible.
# ASSUMPTION: Max 37d incubation and 50d feeding

c_rule_based_adaptation <- model_prediction %>%
  mutate(predicted_breeding_status = NA,
         date = as.Date(date, format ="%Y-%m-%d", tz = "UTC"))

for (i in 1:nrow(c_rule_based_adaptation)) {
  if (c_rule_based_adaptation[i,]$p_incubating == 1) {
    c_rule_based_adaptation[i,]$predicted_breeding_status <- "incubating"
  }
  if (c_rule_based_adaptation[i,]$p_feeding == 1) {
    c_rule_based_adaptation[i,]$predicted_breeding_status <- "feeding"
  }
  if (c_rule_based_adaptation[i,]$p_nonbreeding == 1) {
    c_rule_based_adaptation[i,]$predicted_breeding_status <- "nonbreeding"
  }
}

# Aetting "feeding" to "incubating"
c_rule_based_adaptation[c_rule_based_adaptation$predicted_breeding_status == "feeding" ,]$predicted_breeding_status <- "incubating"

for (i in unique(c_rule_based_adaptation$year_id)) {
  individual_bird <- c_rule_based_adaptation[c_rule_based_adaptation$year_id == i ,]
  
  # Checking if there is an incubation day in the year_id
  if (!is.na(first(individual_bird[individual_bird$predicted_breeding_status == "incubating" ,]$date))) {
    
    # Defining the first breeding day
    first_incubating_day <- first(individual_bird[individual_bird$predicted_breeding_status == "incubating" ,]$date)
    
    # Days after the first incubation date
    days_after_first_incubation_day <- c(first_incubating_day+1, first_incubating_day+2, first_incubating_day+3,
                                         first_incubating_day+4, first_incubating_day+5)
    
    # Checking if there are enough incubation days after the first one
    while (nrow(c_rule_based_adaptation[c_rule_based_adaptation$year_id == i &
                                        c_rule_based_adaptation$date %in% days_after_first_incubation_day &
                                        c_rule_based_adaptation$predicted_breeding_status == "incubating" ,]) < 3) {
      # Otherwise take the next incubation day and check it again
      individual_bird[individual_bird$date == first_incubating_day ,]$predicted_breeding_status <- "nonbreeding"
      
      if (!is.na(first(individual_bird[individual_bird$predicted_breeding_status == "incubating" ,]$date))) {
        first_incubating_day <- first(individual_bird[individual_bird$predicted_breeding_status == "incubating" ,]$date)
        days_after_first_incubation_day <- c(first_incubating_day+1, first_incubating_day+2, first_incubating_day+3,
                                             first_incubating_day+4, first_incubating_day+5)
      }
      else {
        break
      }
    }
    
    # Defining other changing points in the time line
    last_incubating_day <- first_incubating_day + 36
    last_feeding_day <- last_incubating_day + 50
    
    # Nonbreeding before first incubation day
    if (nrow(c_rule_based_adaptation[c_rule_based_adaptation$year_id == i & c_rule_based_adaptation$date < first_incubating_day ,]) > 0) {
      c_rule_based_adaptation[c_rule_based_adaptation$year_id == i & c_rule_based_adaptation$date < first_incubating_day ,]$predicted_breeding_status <- "nonbreeding"
    }
    
    # Between first incubating day and last feeding day only non_breeding if p_nonbreeding > 75%
    if (nrow(c_rule_based_adaptation[c_rule_based_adaptation$year_id == i & c_rule_based_adaptation$date > first_incubating_day &
                                     c_rule_based_adaptation$date < last_feeding_day &
                                     c_rule_based_adaptation$p_nonbreeding < 0.75 ,]) > 0) {
      c_rule_based_adaptation[c_rule_based_adaptation$year_id == i & c_rule_based_adaptation$date > first_incubating_day &
                                c_rule_based_adaptation$date < last_feeding_day &
                                c_rule_based_adaptation$p_nonbreeding < 0.75 ,]$predicted_breeding_status <- "incubating"
    }
    
    # After first incubation day everything equals "incubating" if p_incubating + p_feeding > p_nonbreeding
    if (nrow(c_rule_based_adaptation[c_rule_based_adaptation$year_id == i & c_rule_based_adaptation$date > first_incubating_day &
                                     c_rule_based_adaptation$p_feeding + c_rule_based_adaptation$p_incubating >
                                     c_rule_based_adaptation$p_nonbreeding ,]) > 0) {
      c_rule_based_adaptation[c_rule_based_adaptation$year_id == i & c_rule_based_adaptation$date > first_incubating_day &
                                c_rule_based_adaptation$p_feeding + c_rule_based_adaptation$p_incubating >
                                c_rule_based_adaptation$p_nonbreeding ,]$predicted_breeding_status <- "incubating"
    }
    
    # After last incubation day every day that equals "incubating" now equals "feeding"
    if (nrow(c_rule_based_adaptation[c_rule_based_adaptation$year_id == i & c_rule_based_adaptation$date > last_incubating_day &
                                     c_rule_based_adaptation$predicted_breeding_status == "incubating" ,]) > 0) {
      c_rule_based_adaptation[c_rule_based_adaptation$year_id == i & c_rule_based_adaptation$date > last_incubating_day &
                                c_rule_based_adaptation$predicted_breeding_status == "incubating" ,]$predicted_breeding_status <- "feeding"
    }
    
    # After last feeding day equals "nonbreeding"
    if (nrow(c_rule_based_adaptation[c_rule_based_adaptation$year_id == i & c_rule_based_adaptation$date > last_feeding_day ,]) > 0) {
      c_rule_based_adaptation[c_rule_based_adaptation$year_id == i & c_rule_based_adaptation$date > last_feeding_day ,]$predicted_breeding_status <- "nonbreeding"
    }
  }
}

c_rule_based_adaptation$p_incubating <- 0
c_rule_based_adaptation$p_feeding <- 0
c_rule_based_adaptation$p_nonbreeding <- 0

for (i in 1:nrow(c_rule_based_adaptation)) {
  if (c_rule_based_adaptation[i,]$predicted_breeding_status == "incubating") {
    c_rule_based_adaptation[i,]$p_incubating <- 1
  }
  if (c_rule_based_adaptation[i,]$predicted_breeding_status == "feeding") {
    c_rule_based_adaptation[i,]$p_feeding <- 1
  }
  if (c_rule_based_adaptation[i,]$predicted_breeding_status == "nonbreeding") {
    c_rule_based_adaptation[i,]$p_nonbreeding <- 1
  }
}

# Counting days per individual
c_rule_based_adaptation_grouped <- c_rule_based_adaptation %>%
  group_by(year_id) %>%
  summarise(n_days_incubating = sum(p_incubating),
            n_days_feeding = sum(p_feeding),
            n_days_nonbreeding = sum(p_nonbreeding),
            n_days_total = n()) %>%
  mutate(success = NA,
         validity = NA)

# Evaluating the breeding success
for (i in 1:nrow(c_rule_based_adaptation_grouped)) {
  if (c_rule_based_adaptation_grouped[i,]$n_days_incubating >= duration_incubating && c_rule_based_adaptation_grouped[i,]$n_days_feeding >= duration_feeding) {
    c_rule_based_adaptation_grouped[i,]$success <- T
  }
}

# Calculating the validity
c_rule_based_adaptation_grouped$validity <- c_rule_based_adaptation_grouped$n_days_total/duration_total

# Saving data frame
write.csv(c_rule_based_adaptation_grouped, here("../Data/Output/11_mlrm_performance_de/05_brood_phase_prediction_rule_based_adaptation.csv"),
          row.names = F)

# Calculating Stats for confusion matrix
n_inc_TP <- c_rule_based_adaptation[c_rule_based_adaptation$p_incubating == 1 & c_rule_based_adaptation$breeding_status == "incubating" ,]
n_fee_TP <- c_rule_based_adaptation[c_rule_based_adaptation$p_feeding == 1 & c_rule_based_adaptation$breeding_status == "feeding" ,]
n_non_TN <- c_rule_based_adaptation[c_rule_based_adaptation$p_nonbreeding == 1 & c_rule_based_adaptation$breeding_status == "nonbreeding" ,]

n_inc_tot <- c_rule_based_adaptation[c_rule_based_adaptation$breeding_status == "incubating" ,]
n_fee_tot <- c_rule_based_adaptation[c_rule_based_adaptation$breeding_status == "feeding" ,]
n_non_tot <- c_rule_based_adaptation[c_rule_based_adaptation$breeding_status == "nonbreeding" ,]

n_inc_predicted <- c_rule_based_adaptation[c_rule_based_adaptation$p_incubating == 1 ,]
n_fee_predicted <- c_rule_based_adaptation[c_rule_based_adaptation$p_feeding == 1 ,]
n_non_predicted <- c_rule_based_adaptation[c_rule_based_adaptation$p_nonbreeding == 1 ,]

n_inc_FP_fee <- c_rule_based_adaptation[c_rule_based_adaptation$p_incubating == 1 & c_rule_based_adaptation$breeding_status == "feeding" ,]
n_inc_FP_non <- c_rule_based_adaptation[c_rule_based_adaptation$p_incubating == 1 & c_rule_based_adaptation$breeding_status == "nonbreeding" ,]
n_fee_FP_inc <- c_rule_based_adaptation[c_rule_based_adaptation$p_feeding == 1 & c_rule_based_adaptation$breeding_status == "incubating" ,]
n_fee_FP_non <- c_rule_based_adaptation[c_rule_based_adaptation$p_feeding == 1 & c_rule_based_adaptation$breeding_status == "nonbreeding" ,]
n_non_FN_inc <- c_rule_based_adaptation[c_rule_based_adaptation$p_nonbreeding == 1 & c_rule_based_adaptation$breeding_status == "incubating" ,]
n_non_FN_fee <- c_rule_based_adaptation[c_rule_based_adaptation$p_nonbreeding == 1 & c_rule_based_adaptation$breeding_status == "feeding" ,]


