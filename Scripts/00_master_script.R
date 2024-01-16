# LOADING PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)
library(sf)
library(amt)
library(recurse)
library(rnaturalearth)
library(suncalc)
library(ggplot2)
library(ggpubr)
library(brms)



# CAUTION: Running this script takes around 10 hours with a late 2016 MacBook Pro with 16GB Ram



# RUNNING SCRIPTS --------------------------------------------------------------
# Creating a data set with validation information about home range and nest for each individual and year
source("01_validation.R")

# Preparing the tracking data in a form that it can be optimally used in later calculations
source("02_preparation.R") 

# Calculating relevant movement metrics for home range detection
source("03_parameters.R")

# Finding threshold values in movement metrics to separate birds with a home range from birds without a home range
source("04_home_range.R")

# Calculating relevant parameters for nest detection and finding threshold values to separate birds with a nest from birds without a nest
source("05_nest.R")

# Creating a data set with validation information about daily breeding behaviour during brood cycle
source("06_mlrm_validation.R")

# Preparing the movement data set for a multinomial logistic regression model
source("07_mlrm_parameters.R")

# Building a multinomial logistic regression model with the training data and plotting its estimates
source("08_mlrm_model.R")

# Analysing the performance of the model and calculating statistics (Swiss data)
source("09_mlrm_performance.R")

# Plotting the model predictions on the Swiss testing data
source("10_mlrm_prediction_plots.R")

# Analysing the performance of the model and calculating statistics (German data)
source("11_mlrm_performance_de.R")

# Plotting the model predictions on the German data
source("12_mlrm_prediction_plots_de.R")


