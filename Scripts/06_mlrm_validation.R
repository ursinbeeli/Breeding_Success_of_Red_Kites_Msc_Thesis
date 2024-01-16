# LOADING PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)



# LOADING DATA -----------------------------------------------------------------
# Movement data
milvus <- read.csv(here("../Data/Output/02_preprocessed_data/milvus.csv"))
# Life history data
ilh <- read.csv(here("../Data/Individual_life_history_2015-2022.csv"))
# Breeding parameters data
bp <- read.csv(here("../Data/breeding_parameters_2015-2022.csv"))
# Basic nest list data
nl <- read.csv(here("../Data/Basic_nest_list_2015_2022.csv"))



# PREPARATION ------------------------------------------------------------------
# Defining years to receive  ground truth
years <- c(2016:2022)
# Excluding birds that are not available in gsm and milsar data set
ilh <- ilh[ilh$bird_id %in% unique(milvus$bird_id) ,]
# Excluding first brood of one bird that had a replacement brood
bp <- bp[bp$replacement_brood != "no" ,]

# Selecting only necessary columns for breeding parameters
bp <- bp %>%
  select(nest_id = ID,
         year,
         year_nest_id = year_id,
         incubation = incubation.yes.no.,
         hatchlings = hatchlings.yes.no.,
         egg_laying_date,
         hatching_date = Hatching_date,
         earliest_empty,
         latest_empty) %>%
  # Keeping only birds with enough information
  filter(!is.na(incubation) & !is.na(hatchlings))

# Selecting only necessary columns for individual life history
ilh <- ilh %>%
  select(bird_id,
         sex = sex_compiled,
         hatch_year,
         age,
         nest_ID_2016 = nest_ID_16,
         nest_ID_2017 = nest_ID_17,
         nest_ID_2018 = nest_ID_18,
         nest_ID_2019 = nest_ID_19,
         nest_ID_2020 = nest_ID_20,
         nest_ID_2021 = nest_ID_21,
         nest_ID_2022 = nest_ID_22,
         territoryID_2016,
         territoryID_2017,
         territoryID_2018,
         territoryID_2019,
         territoryID_2020,
         territoryID_2021,
         territoryID_2022) %>%
  # Casting nest_id columns to integer
  mutate(nest_ID_2016 = suppressWarnings(as.integer(nest_ID_2016)),
         nest_ID_2017 = suppressWarnings(as.integer(nest_ID_2017)),
         nest_ID_2018 = suppressWarnings(as.integer(nest_ID_2018)),
         nest_is_2019 = suppressWarnings(as.integer(nest_ID_2019)),
         nest_ID_2020= suppressWarnings(as.integer(nest_ID_2020)),
         nest_ID_2021 = suppressWarnings(as.integer(nest_ID_2021)),
         nest_ID_2022 = suppressWarnings(as.integer(nest_ID_2022)),
         territoryID_2022 = suppressWarnings(as.character(territoryID_2022)))

# Changing format of table (years split in rows rather than columns)
for (i in years) {
  assign(paste0("ilh_", i),
         ilh %>%
           select(bird_id, sex, hatch_year, age,
                  territory = paste0("territoryID_", i),
                  nest_id = paste0("nest_ID_", i)) %>%
           mutate(year = i)
  )
}

# Sewing individual data frames together
ilh <- get(paste0("ilh_", years[1]))
for (i in years[2:length(years)]) {
  ilh <- bind_rows(ilh, get(paste0("ilh_", i)))
}

# Removing rows without a nest_id
ilh <- ilh %>%
  filter(!is.na(nest_id))

# Preparing for join
ilh <- ilh %>%
  mutate(year_id = paste(year, bird_id, sep = "_"),
         year_nest_id = paste(year, nest_id, sep = "_")) %>%
  select(-nest_id, -year)

# Joining breeding parameters with individual life history
ground_truth <- left_join(ilh, bp, by = "year_nest_id")

# Selecting only necessary columns for nest list
nl <- nl %>%
  select(nest_id = ID,
         nest_lat = latitude,
         nest_long = longitude,
         nest_elevation = eu_dem_v11)
nl <- nl[!is.na(nl$nest_id) ,]

# Joining ground truth with nest information
ground_truth <- left_join(ground_truth, nl, by = "nest_id")

# Removing birds with no incubation
ground_truth <- ground_truth %>%
  filter(incubation == "yes")

# Changing dates to date format to set all other entries to NA
ground_truth$egg_laying_date <- as.Date(ground_truth$egg_laying_date, format ="%d.%m.%y", tz = "UTC")
ground_truth$hatching_date <- as.Date(ground_truth$hatching_date, format ="%d.%m.%y", tz = "UTC")
ground_truth$earliest_empty <- as.Date(ground_truth$earliest_empty, format ="%d.%m.%y", tz = "UTC")
ground_truth$latest_empty <- as.Date(ground_truth$latest_empty, format ="%d.%m.%y", tz = "UTC")

# Removing birds that have no observed egg laying date or hatching date
ground_truth <- ground_truth %>%
  filter(!is.na(egg_laying_date) & !is.na(hatching_date))

# Removing birds that have no hatchlings but have an observed hatching date
ground_truth <- ground_truth %>%
  filter(!(hatchlings == "no" & !is.na(hatching_date)))

# Creating one column that indicates the date when the nest was empty
ground_truth <- ground_truth %>%
  mutate(empty_date = as.Date(NA, format=c("%m-%d-%Y")))
for (i in 1:nrow(ground_truth)) {
  if (!is.na(ground_truth[i,]$earliest_empty) & is.na(ground_truth[i,]$latest_empty)) {
    ground_truth[i,]$empty_date <- ground_truth[i,]$earliest_empty
  }
  if (is.na(ground_truth[i,]$earliest_empty) & !is.na(ground_truth[i,]$latest_empty)) {
    ground_truth[i,]$empty_date <- ground_truth[i,]$latest_empty
  }
  if (!is.na(ground_truth[i,]$earliest_empty) & !is.na(ground_truth[i,]$latest_empty)) {
    if ((ground_truth[i,]$latest_empty - ground_truth[i,]$earliest_empty) > 1) {
      ground_truth[i,]$empty_date <-
        mean.Date(c(ground_truth[i,]$earliest_empty,
                    ground_truth[i,]$latest_empty))
    }
    else {
      ground_truth[i,]$empty_date <- ground_truth[i,]$latest_empty
    }
  }
}
ground_truth <- ground_truth %>%
  select(-earliest_empty, -latest_empty)

# Removing empty_date entries that occur before hatching_date
for (i in 1:nrow(ground_truth)) {
  if (!is.na(ground_truth[i,]$empty_date) & !is.na(ground_truth[i,]$hatching_date)) {
    if (as.integer(ground_truth[i,]$empty_date - ground_truth[i,]$hatching_date) < 0) {
      ground_truth[i,]$empty_date <- NA
    }
  }
}

# Removing birds with no empty_date
ground_truth <- ground_truth[!is.na(ground_truth$empty_date) ,]

# Changing too late empty_date (egg_laying_date + 37d incubation + 50d feeding)
for(i in 1:nrow(ground_truth)) {
  if (as.integer(ground_truth[i,]$empty_date - ground_truth[i,]$egg_laying_date) > 87) {
    ground_truth[i,]$empty_date <- ground_truth[i,]$egg_laying_date + 87
  }
}

# Calculating age for each bird and year
ground_truth$hatch_year <- suppressWarnings(as.integer(ground_truth$hatch_year))
ground_truth <- ground_truth %>%
  mutate(age_cy = year - hatch_year + 1)
ground_truth[is.na(ground_truth$hatch_year) ,]$age_cy <-
  ground_truth[is.na(ground_truth$hatch_year) ,]$age
ground_truth$age_cy <- as.numeric(gsub(".*?([0-9]+).*", "\\1", ground_truth$age_cy))  
ground_truth[ground_truth$age_cy > 3 ,]$age_cy <- 3
ground_truth <- ground_truth %>%
  select(-hatch_year, -age)

# Changing "unknown" to NA in territory column
ground_truth[!is.na(ground_truth$territory) &
               ground_truth$territory == "unknown",]$territory <- NA

# Reordering columns
ground_truth <- ground_truth %>%
  select(bird_id, year, year_id, sex, age_cy, territory, incubation, hatchlings,
         egg_laying_date, hatching_date, empty_date,
         nest_id, year_nest_id, nest_elevation, nest_lat, nest_long)



# SAVING DATA ------------------------------------------------------------------
# Creating directory
if (!dir.exists(here("../Data/Output/06_mlrm_validation_data"))) {
  if (!dir.exists(here("../Data/Output"))) {
    dir.create("../Data/Output")
  }
  dir.create("../Data/Output/06_mlrm_validation_data")
}

write.csv(ground_truth,
          here("../Data/Output/06_mlrm_validation_data/milvus_brood_validation.csv"),
          row.names = F)


