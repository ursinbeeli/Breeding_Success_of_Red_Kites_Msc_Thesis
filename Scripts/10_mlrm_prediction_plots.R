# LOADING PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)
library(brms)
library(ggplot2)
library(ggpubr)



# LOADING DATA -----------------------------------------------------------------
# Movement data
milvus <- read.csv(here("../Data/Output/07_mlrm_parameters/milvus_mlrm_parameters.csv"))
# Multinomial Logistic Regression Model
brood_model_15 <- readRDS(here("../Data/Output/08_mlrm_model/brood_model_15.rds"))



# DATA PREPARATION -------------------------------------------------------------
# Casting dependent variable and sex to factor
milvus$breeding_status <- factor(milvus$breeding_status, levels = c("nonbreeding", "incubating", "feeding"))
milvus$sex <- factor(milvus$sex, levels = c(0,1))



# SPLITTING DATA FRAME RANDOMLY INTO TWO ---------------------------------------
set.seed(107)
milvus_id <- unique(milvus$year_id)
milvus_id_train <- sample(milvus_id, round(length(milvus_id)*.7))

# Training data
milvus_train <- milvus %>%
  filter(year_id %in% milvus_id_train)
# Testing data
milvus_test <- milvus %>%
  filter(!year_id %in% milvus_id_train)



# CALCULATING PREDICTIONS ------------------------------------------------------
model_prediction <- predict(brood_model_15, newdata = milvus_test, allow_new_levels = T) %>%
  as.data.frame()

model_prediction_combined <- milvus_test %>%
  select(year_id, date, breeding_status) %>%
  bind_cols(model_prediction) %>%
  mutate(p_feeding = `P(Y = feeding)`,
         p_incubating = `P(Y = incubating)`,
         p_nonbreeding = `P(Y = nonbreeding)`,
         estimated_breeding_status = "") %>%
  select(-`P(Y = feeding)`, -`P(Y = incubating)`, -`P(Y = nonbreeding)`)

for (j in 1:nrow(model_prediction_combined)) {
  if (model_prediction_combined[j,]$p_feeding > model_prediction_combined[j,]$p_incubating &
      model_prediction_combined[j,]$p_feeding > model_prediction_combined[j,]$p_nonbreeding) {
    model_prediction_combined[j,]$estimated_breeding_status <- "feeding"
  }
  if (model_prediction_combined[j,]$p_incubating > model_prediction_combined[j,]$p_feeding &
      model_prediction_combined[j,]$p_incubating > model_prediction_combined[j,]$p_nonbreeding) {
    model_prediction_combined[j,]$estimated_breeding_status <- "incubating"
  }
  if (model_prediction_combined[j,]$p_nonbreeding > model_prediction_combined[j,]$p_feeding &
      model_prediction_combined[j,]$p_nonbreeding > model_prediction_combined[j,]$p_incubating) {
    model_prediction_combined[j,]$estimated_breeding_status <- "nonbreeding"
  }
  if (model_prediction_combined[j,]$p_incubating == model_prediction_combined[j,]$p_feeding &
      model_prediction_combined[j,]$p_incubating > model_prediction_combined[j,]$p_nonbreeding) {
    model_prediction_combined[j,]$estimated_breeding_status <- "incubating"
  }
  if (model_prediction_combined[j,]$p_feeding == model_prediction_combined[j,]$p_nonbreeding &
      model_prediction_combined[j,]$p_feeding > model_prediction_combined[j,]$p_incubating) {
    model_prediction_combined[j,]$estimated_breeding_status <- "feeding"
  }
  if (model_prediction_combined[j,]$p_incubating == model_prediction_combined[j,]$p_nonbreeding &
      model_prediction_combined[j,]$p_incubating > model_prediction_combined[j,]$p_feeding) {
    model_prediction_combined[j,]$estimated_breeding_status <- "incubating"
  }
  if (model_prediction_combined[j,]$p_incubating == model_prediction_combined[j,]$p_nonbreeding &
      model_prediction_combined[j,]$p_incubating == model_prediction_combined[j,]$p_feeding) {
    model_prediction_combined[j,]$estimated_breeding_status <- "incubating"
  }
} 

model_prediction_bm_15 <- model_prediction_combined



# PLOTTING PREDICTIONS ---------------------------------------------------------
# Creating directory for plots
if (!dir.exists(here("../Data/Output/Plots/10_mlrm_prediction_plots"))) {
  if (!dir.exists(here("../Data/Output"))) {
    dir.create("../Data/Output")
  }
  if (!dir.exists(here("../Data/Output/Plots"))) {
    dir.create("../Data/Output/Plots")
  }
  dir.create("../Data/Output/Plots/10_mlrm_prediction_plots")
}

milvus$sex_plot <- as.numeric(milvus$sex)
milvus[milvus$sex_plot == 1,]$sex_plot <- "m"
milvus[milvus$sex_plot == 2,]$sex_plot <- "f"

model_prediction <- model_prediction_bm_15
# Plotting predictions separately for every year_id
for (k in unique(model_prediction$year_id)) {
  sex <- first(milvus[milvus$year_id == k, ]$sex_plot)
  assign(paste0("plot_", k),
         ggplot(data = model_prediction[model_prediction$year_id == k,], aes(x = as.Date(date))) +
           geom_rect(aes(ymin = .5, ymax = 1,
                         xmin = as.Date(date) - .5, xmax = as.Date(date) + .5,
                         fill = estimated_breeding_status), alpha = .6) +
           geom_col(aes(y = .5, fill = breeding_status), alpha = .6, width = 1) +
           geom_line(aes(y = p_nonbreeding), color = "gray10", size = .5) +
           geom_line(aes(y = p_incubating), color = "darkslategray2", size = .5) +
           geom_line(aes(y = p_feeding), color = "turquoise4", size = .5) +
           scale_fill_manual(values = c("nonbreeding" = "gray10",
                                        "incubating" = "darkslategray2",
                                        "feeding" = "turquoise4")) +
           scale_y_continuous(
             # Adding a second axis and specify its features
             sec.axis = sec_axis(trans=~.*1, name="Predicted brood phase                 Observed brood phase")
           ) +
           labs(x = element_blank(), y = "Probability of predicted brood phase (lines)",
                title = paste0("Year ID: ", k, ", Sex: ", sex), fill = "Brood phase") +
           theme(plot.title = element_text(hjust = 0.5),
                 panel.background = element_blank(),
                 panel.grid.major = element_line(colour = "gray70"),
                 panel.grid.minor = element_line(colour = "gray90"))
  )
}

# Arranging first page
plot_list <- list()
for (l in 1:36) {
  m <- unique(model_prediction$year_id)[l]
  plot_list[[l]] <- get(paste0("plot_", m))
}
plot <- ggarrange(plotlist = c(plot_list),
                  ncol = 4, nrow = 9)

# Adding main title and y axis title
annotate_figure(plot)

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/10_mlrm_prediction_plots/01_predictions_test_data_a.pdf")),
       width = 9920, height = 14032, units = "px", dpi = 300,
       limitsize = F)

# Arranging second page
plot_list <- list()
for (l in 37:67) {
  m <- unique(model_prediction$year_id)[l]
  plot_list[[l]] <- get(paste0("plot_", m))
}
plot <- ggarrange(plotlist = c(plot_list),
                  ncol = 4, nrow = 9)

# Adding main title and y axis title
annotate_figure(plot)

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/10_mlrm_prediction_plots/02_predictions_test_data_b.pdf")),
       width = 9920, height = 14032, units = "px", dpi = 300,
       limitsize = F)









# EXAMPLE BIRD TO COMPARE RAW PREDICTION WITH RULE BASED ADAPTATION ------------
# Assuption: Max 37d incubation and 50d feeding
model_prediction <- model_prediction_bm_15

model_prediction$date <- as.Date(model_prediction$date, format ="%Y-%m-%d", tz = "UTC")

for (i in unique(model_prediction$year_id)) {
  individual_bird <- model_prediction[model_prediction$year_id == i ,]
  
  # Checking if there is an incubation day in the year_id
  if (!is.na(first(individual_bird[individual_bird$estimated_breeding_status == "incubating" ,]$date))) {
    
    # Defining the first breeding day
    first_incubating_day <- first(individual_bird[individual_bird$estimated_breeding_status == "incubating" ,]$date)
    
    # Days after the first incubation date
    days_after_first_incubation_day <- c(first_incubating_day+1, first_incubating_day+2, first_incubating_day+3,
                                         first_incubating_day+4, first_incubating_day+5)
    
    # Checking if there are enough incubation days after the first one
    while (nrow(model_prediction[model_prediction$year_id == i &
                                 model_prediction$date %in% days_after_first_incubation_day &
                                 model_prediction$estimated_breeding_status == "incubating" ,]) < 3) {
      # Otherwise take the next incubation day and check it again
      individual_bird[individual_bird$date == first_incubating_day ,]$estimated_breeding_status <- "nonbreeding"
      
      if (!is.na(first(individual_bird[individual_bird$estimated_breeding_status == "incubating" ,]$date))) {
        first_incubating_day <- first(individual_bird[individual_bird$estimated_breeding_status == "incubating" ,]$date)
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
    if (nrow(model_prediction[model_prediction$year_id == i & model_prediction$date < first_incubating_day ,]) > 0) {
      model_prediction[model_prediction$year_id == i & model_prediction$date < first_incubating_day ,]$estimated_breeding_status <- "nonbreeding"
    }
    
    # Between first incubating day and last feeding day only non_breeding if p_nonbreeding > 75%
    model_prediction[model_prediction$year_id == i & model_prediction$date > first_incubating_day &
                       model_prediction$date < last_feeding_day &
                       model_prediction$p_nonbreeding < 0.75 ,]$estimated_breeding_status <- "incubating"
    
    # After first incubation day everything equals "incubating" if p_incubating + p_feeding > p_nonbreeding
    if (nrow(model_prediction[model_prediction$year_id == i & model_prediction$date > first_incubating_day &
                              model_prediction$p_feeding + model_prediction$p_incubating >
                              model_prediction$p_nonbreeding ,]) > 0) {
      model_prediction[model_prediction$year_id == i & model_prediction$date > first_incubating_day &
                         model_prediction$p_feeding + model_prediction$p_incubating >
                         model_prediction$p_nonbreeding ,]$estimated_breeding_status <- "incubating"
    }
    
    # After last incubation day every day that equals "incubating" now equals "feeding"
    if (nrow(model_prediction[model_prediction$year_id == i & model_prediction$date > last_incubating_day &
                              model_prediction$estimated_breeding_status == "incubating" ,]) > 0) {
      model_prediction[model_prediction$year_id == i & model_prediction$date > last_incubating_day &
                         model_prediction$estimated_breeding_status == "incubating" ,]$estimated_breeding_status <- "feeding"
    }
    
    # After last feeding day equals "nonbreeding"
    if (nrow(model_prediction[model_prediction$year_id == i & model_prediction$date > last_feeding_day ,]) > 0) {
      model_prediction[model_prediction$year_id == i & model_prediction$date > last_feeding_day ,]$estimated_breeding_status <- "nonbreeding"
    }
  }
}

model_prediction_rules <- model_prediction



# Plotting example bird with raw predictions
k <-  "2021_490"
y <- model_prediction_bm_15[model_prediction_bm_15$year_id == k,]
z <- model_prediction_rules[model_prediction_rules$year_id == k,]
sex <- first(milvus[milvus$year_id == k, ]$sex_plot)

# Raw prediction
ggplot(data = y[y$year_id == k,], aes(x = as.Date(date))) +
  geom_rect(aes(ymin = .5, ymax = 1,
                xmin = as.Date(date) - .5, xmax = as.Date(date) + .5,
                fill = estimated_breeding_status), alpha = .6) +
  geom_col(aes(y = .5, fill = breeding_status), alpha = .6, width = 1) +
  geom_line(aes(y = p_nonbreeding), color = "gray10", size = 1) +
  geom_line(aes(y = p_incubating), color = "darkslategray2", size = 1) +
  geom_line(aes(y = p_feeding), color = "turquoise4", size = 1) +
  scale_fill_manual(values = c("nonbreeding" = "gray10",
                               "incubating" = "darkslategray2",
                               "feeding" = "turquoise4")) +
  labs(x = element_blank(), y = "Probability of predicted brood phase (lines)",
       title = "Raw model prediction approach") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray70"),
        panel.grid.minor = element_line(colour = "gray90"))

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/10_mlrm_prediction_plots/03_prediction_example_raw.pdf")),
       width = 2500, height = 1500, units = "px", dpi = 300,
       limitsize = F)



# Plotting example bird with rule based adaptation
ggplot(data = z[z$year_id == k,], aes(x = as.Date(date))) +
  geom_rect(aes(ymin = .5, ymax = 1,
                xmin = as.Date(date) - .5, xmax = as.Date(date) + .5,
                fill = estimated_breeding_status), alpha = .6) +
  geom_col(aes(y = .5, fill = breeding_status), alpha = .6, width = 1) +
  geom_line(aes(y = p_nonbreeding), color = "gray10", size = 1) +
  geom_line(aes(y = p_incubating), color = "darkslategray2", size = 1) +
  geom_line(aes(y = p_feeding), color = "turquoise4", size = 1) +
  scale_fill_manual(values = c("nonbreeding" = "gray10",
                               "incubating" = "darkslategray2",
                               "feeding" = "turquoise4")) +
  scale_y_continuous(
    # Adding a second axis and specify its features
    sec.axis = sec_axis(trans=~.*1, name="Predicted brood phase                Observed brood phase")
  ) +
  labs(x = element_blank(), y = element_blank(),
       title = "Rule-based adaptation approach", fill = "Brood phase") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray70"),
        panel.grid.minor = element_line(colour = "gray90"))

# Saving Plot
ggsave(here(paste0("../Data/Output/Plots/10_mlrm_prediction_plots/04_prediction_example_rule.pdf")),
       width = 3000, height = 1500, units = "px", dpi = 300,
       limitsize = F)


