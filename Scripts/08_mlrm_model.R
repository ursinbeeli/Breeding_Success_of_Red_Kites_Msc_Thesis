# LOADING PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)
library(brms)
library(ggplot2)



# Running this script took around 2 hours with a late 2016 MacBook Pro with 16GB Ram



# LOADING DATA -----------------------------------------------------------------
# Movement data
milvus <- read.csv(here("../Data/Output/07_mlrm_parameters/milvus_mlrm_parameters.csv"))



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



# MODEL BUILDING ---------------------------------------------------------------
# Creating directory to save model
if (!dir.exists(here("../Data/Output/08_mlrm_model"))) {
  if (!dir.exists(here("../Data/Output"))) {
    dir.create("../Data/Output")
  }
  dir.create("../Data/Output/08_mlrm_model")
}

# Building and training the multinomial logistic regression model
brood_model_15 <- brm(bf(breeding_status ~ sex +
                           mcp_area_95_mw_7day^2 + nest_dist_mean_mw_7day +
                           residence_time_nest_mw_7day + revisits_nest_mw_7day +
                           sex * (mcp_area_95_mw_7day^2 +                                # model interactions
                                    nest_dist_mean_mw_7day +
                                    residence_time_nest_mw_7day +
                                    revisits_nest_mw_7day) +
                           residence_time_nest_mw_7day * revisits_nest_mw_7day +
                           locations_per_day + (1 | year_id)),                           # control variable (left) & random effect (right)
                      data = milvus_train, family = categorical())

# Saving model
saveRDS(brood_model_15, here("../Data/Output/08_mlrm_model/brood_model_15.rds"))



# PLOTTING ESTIMATES -----------------------------------------------------------
# Creating directory to save plot
if (!dir.exists(here("../Data/Output/Plots/08_mlrm_model"))) {
  if (!dir.exists(here("../Data/Output"))) {
    dir.create("../Data/Output")
  }
  if (!dir.exists(here("../Data/Output/Plots"))) {
    dir.create("../Data/Output/Plots")
  }
  dir.create("../Data/Output/Plots/08_mlrm_model")
}

bm_summary <- summary(brood_model_15)$fixed %>%
  mutate(Parameters = row.names(summary(brood_model_15)$fixed))

bm_incubating <- bm_summary %>%
  slice(3:13) %>%
  mutate(Category = "Incubating")
bm_incubating$Parameters <- c("Sex", "95% MCP",
                              "Distance to nest", "Residence time",
                              "Revisitations", "Locations per day",
                              "95% MCP of females",
                              "Distance to nest of females", "Residence time of females",
                              "Revisitations of females", "Residence time - Revisitations")

bm_feeding <- bm_summary %>%
  slice(14:24) %>%
  mutate(Category = "Feeding")
bm_feeding$Parameters <- c("Sex", "95% MCP",
                           "Distance to nest", "Residence time",
                           "Revisitations", "Locations per day",
                           "95% MCP of females",
                           "Distance to nest of females", "Residence time of females",
                           "Revisitations of females", "Residence time - Revisitations")

bm_combined <- bind_rows(bm_incubating, bm_feeding)

bm_combined$Category <- factor(bm_combined$Category, levels=c("Incubating", "Feeding"))

bm_combined %>%
  ggplot(aes(y = Parameters)) +
  geom_vline(xintercept = 0, linetype = 3, color = "#8B9DAF") +
  geom_point(aes(y = Parameters, x = Estimate, color = Category), size = 1) +  
  geom_errorbarh(aes(xmax = `u-95% CI`, xmin = `l-95% CI`, height = .12, color = Category), linewidth = 0.5) +
  scale_color_manual(labels = c("Incubating", "Feeding"),
                     values = c("darkslategray2", "turquoise4"),
                     name = "Brood Phase") +
  labs(y = "Variable", x = "Estimate (± 95% Conficence Interval)") +
  theme(plot.title = element_text(hjust = 0.275),
        # legend.position = "none",
        legend.key = element_rect("white"),
        axis.ticks.y    = element_blank(),
        axis.ticks.x    = element_line(colour = "gray90"),
        axis.text.y     = element_text(hjust = 0),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "gray90"))

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/08_mlrm_model/Coefplot_bm_15.pdf")),
       width = 3000, height = 1500, units = "px", dpi = 320,)


