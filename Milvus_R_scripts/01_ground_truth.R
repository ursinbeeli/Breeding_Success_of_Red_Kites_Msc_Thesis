library(here)
library(dplyr)

# Loading GPS Data
milvus_gsm <- read.csv(here("data/Milvusmilvus_GSM_SOI.csv"))
milvus_milsar <- read.csv(here("data/Milvusmilvus_Milsar_SOI_final.csv"))
# Loading Life History Data
ilh <- read.csv2(here("data/Individual_life_history_2015-2021.csv"))
# Loading Breeding Parameters Data
bp <- read.csv2(here("data/breeding_parameters_2015-2021.csv"))


#-----------------------------PREPARATION---------------------------------------
# Excluding birds that are not available in gsm and milsar data set
ilh <- ilh[ilh$bird_id %in% unique(milvus_milsar$individual.local.identifier)
           | ilh$bird_id %in% unique(milvus_gsm$individual.local.identifier), ]
bp <- bp[bp$ID %in% unique(milvus_milsar$individual.local.identifier)
         | bp$ID %in% unique(milvus_gsm$individual.local.identifier), ]

# selecting only necessary columns (and keep only birds with enough information)
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
  filter(!is.na(incubation) & !is.na(hatchlings))

# selecting only necessary columns
ilh <- ilh %>%
  select(bird_id,
         sex = sex_compiled,
         hatch_year,
         territoryID_2016,
         territoryID_2017,
         territoryID_2018,
         territoryID_2019,
         territoryID_2020,
         #territoryID_2022,
         territoryID_2021)

# changing format of table (years split in rows rather than columns)
ilh_2016 <- ilh %>%
  select(bird_id, sex, hatch_year, territory = territoryID_2016) %>%
  mutate(year = 2016)
ilh_2017 <- ilh %>%
  select(bird_id, sex, hatch_year, territory = territoryID_2017) %>%
  mutate(year = 2017)
ilh_2018 <- ilh %>%
  select(bird_id, sex, hatch_year, territory = territoryID_2018) %>%
  mutate(year = 2018)
ilh_2019 <- ilh %>%
  select(bird_id, sex, hatch_year, territory = territoryID_2019) %>%
  mutate(year = 2019)
ilh_2020 <- ilh %>%
  select(bird_id, sex, hatch_year, territory = territoryID_2020) %>%
  mutate(year = 2020)
ilh_2021 <- ilh %>%
  select(bird_id, sex, hatch_year, territory = territoryID_2021) %>%
  mutate(year = 2021)
# ilh_2022 <- ilh %>%
#   select(bird_id, sex, hatch_year, territory = territoryID_2022) %>%
#   mutate(year = 2022)

# sewing individual data frames together
ilh <- bind_rows(ilh_2016, ilh_2017, ilh_2018, ilh_2019, ilh_2020,
                 #ilh_2022,
                 ilh_2021)
# preparing for join
ilh <- ilh %>%
  mutate(year_id = paste(year, bird_id, sep = "_")) %>%
  select(-bird_id, -year)

# joining breeding parameters with individual life history
ground_truth <- left_join(bp, ilh, by = "year_id")

# removing birds with no known sex
ground_truth <- ground_truth %>%
  filter(!is.na(sex) & sex != "unknown" & sex != "unknown ")

# changing dates to date format to set all other entries to NA
ground_truth$egg_laying_date <- as.Date(ground_truth$egg_laying_date, format ="%Y-%m-%d", tz = "UTC")
ground_truth$hatching_date <- as.Date(ground_truth$hatching_date, format ="%Y-%m-%d", tz = "UTC")
ground_truth$earliest_empty <- as.Date(ground_truth$earliest_empty, format ="%Y-%m-%d", tz = "UTC")
ground_truth$latest_empty <- as.Date(ground_truth$latest_empty, format ="%Y-%m-%d", tz = "UTC")

# removing birds that have been breeding but have no observed egg laying date
birds_to_delete <- ground_truth %>%
  filter(incubation == "yes" & is.na(egg_laying_date))
ground_truth <- ground_truth %>%
  filter(!year_id %in% birds_to_delete$year_id)

# removing birds that have had hatchlings but have no observed hatching date
birds_to_delete <- ground_truth %>%
  filter(hatchlings == "yes" & is.na(hatching_date))
ground_truth <- ground_truth %>%
  filter(!year_id %in% birds_to_delete$year_id)

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

# calculating age for each bird and year
ground_truth$hatch_year <- as.integer(ground_truth$hatch_year)
ground_truth <- ground_truth %>%
  mutate(age = year - hatch_year)
ground_truth <- ground_truth %>%
  select(-hatch_year)

# simplifying territory in a binary way to yes or no
ground_truth[grepl("\\d", ground_truth$territory),]$territory <- "yes"
ground_truth[!is.na(ground_truth$territory) &
               ground_truth$territory == "none",
]$territory <- "no"
ground_truth[!is.na(ground_truth$territory) &
               (ground_truth$territory == "unknown" |
                  ground_truth$territory == "unknown (see comments)"),
             ]$territory <- NA

# deleting date entries for birds with no incubation
ground_truth[ground_truth$incubation == "no",]$empty_date <- NA

# reordering columns
ground_truth <- ground_truth %>%
  select(bird_id, year, year_id, sex, age, territory, incubation, hatchlings, egg_laying_date, hatching_date, empty_date)



#-----------------------------SAVING DATA FRAMES--------------------------------
write.csv(ground_truth,
          here("data/modified/01_ground_truth/milvus_ground_truth.csv"),
          row.names = F)


