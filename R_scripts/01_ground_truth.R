library(here)
library(dplyr)

# Loading GPS Data
milvus_gsm <- read.csv(here("data/Milvusmilvus_GSM_SOI.csv"))
milvus_milsar <- read.csv(here("data/Milvusmilvus_Milsar_SOI_final.csv"))
# Loading Life History Data
ilh <- read.csv(here("data/Individual_life_history_2015-2022.csv"))
# Loading Breeding Parameters Data
bp <- read.csv(here("data/breeding_parameters_2015-2022.csv"))
# Loading Basic Nest List Data
nl <- read.csv(here("data/Basic_nest_list_2015_2022.csv"))



#-----------------------------PREPARATION---------------------------------------
# defining years to receive  ground truth
years <- c(2016:2022)
# excluding birds that are not available in gsm and milsar data set
ilh <- ilh[ilh$bird_id %in% unique(milvus_milsar$individual.local.identifier) |
             ilh$bird_id %in% unique(milvus_gsm$individual.local.identifier) ,]
bp <- bp[bp$ID %in% unique(milvus_milsar$individual.local.identifier) |
           bp$ID %in% unique(milvus_gsm$individual.local.identifier) ,]
# excluding first brood of one bird that had a replacement brood
bp <- bp[bp$replacement_brood != "no" ,]

# selecting only necessary columns for breeding parameters
bp <- bp %>%
  select(bird_id = ID,
         year,
         year_id,
         incubation = incubation.yes.no.,
         hatchlings = hatchlings.yes.no.,
         egg_laying_date,
         hatching_date = Hatching_date,
         earliest_empty,
         latest_empty) %>%
  # keep only birds with enough information
  filter(!is.na(incubation) & !is.na(hatchlings))

# selecting only necessary columns for individual life history
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
  # casting nest_id columns to integer
  mutate(nest_ID_2016 = suppressWarnings(as.integer(nest_ID_2016)),
         nest_ID_2017 = suppressWarnings(as.integer(nest_ID_2017)),
         nest_ID_2018 = suppressWarnings(as.integer(nest_ID_2018)),
         nest_is_2019 = suppressWarnings(as.integer(nest_ID_2019)),
         nest_ID_2020= suppressWarnings(as.integer(nest_ID_2020)),
         nest_ID_2021 = suppressWarnings(as.integer(nest_ID_2021)),
         nest_ID_2022 = suppressWarnings(as.integer(nest_ID_2022)))

# changing format of table (years split in rows rather than columns)
for (i in years) {
  assign(paste0("ilh_", i),
         ilh %>%
           select(bird_id, sex, hatch_year, age,
                  territory = paste0("territoryID_", i),
                  nest_id = paste0("nest_ID_", i)) %>%
           mutate(year = i)
  )
}

# sewing individual data frames together
ilh <- get(paste0("ilh_", years[1]))
for (i in years[2:length(years)]) {
  ilh <- bind_rows(ilh, get(paste0("ilh_", i)))
}

# preparing for join
ilh <- ilh %>%
  mutate(year_id = paste(year, bird_id, sep = "_")) %>%
  select(-bird_id, -year)

# joining breeding parameters with individual life history
ground_truth <- left_join(bp, ilh, by = "year_id")

# selecting only necessary columns for nest list
nl <- nl %>%
  select(nest_id = ID,
         nest_lat = latitude,
         nest_long = longitude,
         nest_elevation = eu_dem_v11)
nl <- nl[!is.na(nl$nest_id) ,]

# joining ground truth with nest information
ground_truth <- left_join(ground_truth, nl, by = "nest_id")

# removing birds with no known sex
ground_truth <- ground_truth %>%
  filter(!is.na(sex) & sex != "unknown" & sex != "unknown ")

# removing birds with no incubation
ground_truth <- ground_truth %>%
  filter(incubation == "yes")

# changing dates to date format to set all other entries to NA
ground_truth$egg_laying_date <- as.Date(ground_truth$egg_laying_date, format ="%d.%m.%y", tz = "UTC")
ground_truth$hatching_date <- as.Date(ground_truth$hatching_date, format ="%d.%m.%y", tz = "UTC")
ground_truth$earliest_empty <- as.Date(ground_truth$earliest_empty, format ="%d.%m.%y", tz = "UTC")
ground_truth$latest_empty <- as.Date(ground_truth$latest_empty, format ="%d.%m.%y", tz = "UTC")

# removing birds that have been breeding but have no observed egg laying date
ground_truth <- ground_truth %>%
  filter(!(incubation == "yes" & is.na(egg_laying_date)))

# removing birds that have had hatchlings but have no observed hatching date
ground_truth <- ground_truth[!is.na(ground_truth$hatching_date) ,]

# removing birds that have no hatchlings but have an observed hatching date
ground_truth <- ground_truth %>%
  filter(!(hatchlings == "no" & !is.na(hatching_date)))

# creating one column that indicates the date when the nest was empty
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

# removing empty_date entries that occur before hatching_date
for (i in 1:nrow(ground_truth)) {
  if (!is.na(ground_truth[i,]$empty_date) & !is.na(ground_truth[i,]$hatching_date)) {
    if (as.integer(ground_truth[i,]$empty_date - ground_truth[i,]$hatching_date) < 0) {
      ground_truth[i,]$empty_date <- NA
    }
  }
}

# removing birds with same empty_date as egg_laying_date
for(i in 1:nrow(ground_truth)) {
  if (!is.na(ground_truth[i,]$empty_date) & !is.na(ground_truth[i,]$egg_laying_date)) {
    if (ground_truth[i,]$empty_date == ground_truth[i,]$egg_laying_date) {
      ground_truth[i,]$empty_date <- NA
    }
  }
}

# removing birds with no empty_date
ground_truth <- ground_truth[!is.na(ground_truth$empty_date) ,]

# change too late empty_date (egg_laying_date + 37d incubation + 50d feeding)
for(i in 1:nrow(ground_truth)) {
  if (as.integer(ground_truth[i,]$empty_date - ground_truth[i,]$egg_laying_date) > 87) {
    ground_truth[i,]$empty_date <- ground_truth[i,]$egg_laying_date + 87
  }
}

# calculating age for each bird and year
ground_truth$hatch_year <- suppressWarnings(as.integer(ground_truth$hatch_year))
ground_truth <- ground_truth %>%
  mutate(age_cy = year - hatch_year + 1)
ground_truth[is.na(ground_truth$hatch_year) ,]$age_cy <-
  ground_truth[is.na(ground_truth$hatch_year) ,]$age
ground_truth$age_cy <- as.numeric(gsub(".*?([0-9]+).*", "\\1", ground_truth$age_cy))  
ground_truth[ground_truth$age_cy > 3 ,]$age_cy <- 3
ground_truth <- ground_truth %>%
  select(-hatch_year, -age)

# simplifying territory in a binary way to yes or no
ground_truth[grepl("\\d", ground_truth$territory) ,]$territory <- "yes"
ground_truth[!is.na(ground_truth$territory) &
               ground_truth$territory == "none"
             ,]$territory <- "no"
ground_truth[!(is.na(ground_truth$territory) |
                 ground_truth$territory == "yes" |
                 ground_truth$territory == "no")
               ,]$territory <- NA

# reordering columns
ground_truth_nest <- ground_truth %>%
  select(bird_id, year, year_id, sex, age_cy, territory, incubation, hatchlings,
         egg_laying_date, hatching_date, empty_date,
         nest_id, nest_elevation, nest_lat, nest_long) %>%
  filter(!is.na(nest_id))

ground_truth_simple <- ground_truth %>%
  select(bird_id, year, year_id, sex, age_cy, territory, incubation, hatchlings,
         egg_laying_date, hatching_date, empty_date)

#-----------------------------SAVING DATA FRAMES--------------------------------
write.csv(ground_truth_nest,
          here("data/modified/01_ground_truth/milvus_ground_truth_nest.csv"),
          row.names = F)

write.csv(ground_truth_simple,
          here("data/modified/01_ground_truth/milvus_ground_truth_simple.csv"),
          row.names = F)



