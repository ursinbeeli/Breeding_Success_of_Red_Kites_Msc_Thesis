# LOADING PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)
library(brms)
library(ggplot2)
library(ggpubr)



# LOADING DATA -----------------------------------------------------------------
# Movement data of German birds
milvus <- read.csv(here("../Data/german_birds.csv"))
# Multinomial Logistic Regression Model
brood_model_15 <- readRDS(here("../Data/Output/08_mlrm_model/brood_model_15.rds"))



# DATA PREPARATION -------------------------------------------------------------
# Casting dependent variable and sex to factor
milvus$breeding_status <- factor(milvus$breeding_status, levels = c("nonbreeding", "incubating", "feeding"))
milvus$sex <- factor(milvus$sex, levels = c(0,1))



# CALCULATING PREDICTIONS ------------------------------------------------------
model_prediction <- predict(brood_model_15, newdata = milvus, allow_new_levels = T) %>%
  as.data.frame()

model_prediction_combined <- milvus %>%
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
# Creating directory for plot
if (!dir.exists(here("../Data/Output/Plots/12_mlrm_prediction_plots_de"))) {
  if (!dir.exists(here("../Data/Output"))) {
    dir.create("../Data/Output")
  }
  if (!dir.exists(here("../Data/Output/Plots"))) {
    dir.create("../Data/Output/Plots")
  }
  dir.create("../Data/Output/Plots/12_mlrm_prediction_plots_de")
}

model_prediction <- model_prediction_bm_15
# Plotting predictions separately for every year_id
for (k in unique(model_prediction$year_id)) {
  sex <- first(milvus[milvus$year_id == k, ]$sex_chr)
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

# Arranging page
plot_list <- list()
for (l in 1:28) {
  m <- unique(model_prediction$year_id)[l]
  plot_list[[l]] <- get(paste0("plot_", m))
}
plot <- ggarrange(plotlist = c(plot_list),
                  ncol = 4, nrow = 9)

# Adding main title and y axis title
annotate_figure(plot)

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/12_mlrm_prediction_plots_de/predictions_german_birds.pdf")),
       width = 9920, height = 14032, units = "px", dpi = 300,
       limitsize = F)


