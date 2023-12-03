# LOADING PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)
library(amt)
library(recurse)
library(sf)
library(rnaturalearth)
library(ggplot2)

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

# Preparing the data set for a multinomial logistic regression model
source("06_model_preparation.R")

# Defining the optimal multinomial logistic regression model
source("07_model_fine_tuning.R")

# Validating the multinomial logistic regression model
source("08_model_validation.R copy.R")