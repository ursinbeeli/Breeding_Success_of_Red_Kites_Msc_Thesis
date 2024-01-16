# LOADING PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)



# LOADING DATA -----------------------------------------------------------------
# Movement data
milvus_gsm <- read.csv(here("../Data/Milvusmilvus_GSM_SOI.csv"))
milvus_milsar <- read.csv(here("../Data/Milvusmilvus_Milsar_SOI_final.csv"))
# Life history data
ilh <- read.csv(here("../Data/Individual_life_history_2015-2022.csv"))



# PREPARATION ------------------------------------------------------------------
# Removing birds from life history that are not available in GSM and Milsar GPS data
ilh <- ilh[ilh$bird_id %in% unique(milvus_milsar$individual.local.identifier)
           | ilh$bird_id %in% unique(milvus_gsm$individual.local.identifier), ]
# Creating a vector with all years where data is available
years <- c(2016:2022)



# SELECTING BIRD IDS OF BIRDS WITH A HOME RANGE --------------------------------
# Selection of birds
for (i in years) {
  assign(paste0("year_", i),
         ilh %>%
         dplyr::filter(!is.na(get(paste0("territoryID_", i))) &
                         get(paste0("territoryID_", i)) != "none" &
                         get(paste0("territoryID_", i)) != "unknown" &
                         get(paste0("territoryID_", i)) != "unknown (see comments)") %>%
         mutate(year = i,
                year_id = paste0(year, "_", bird_id),
                sex = sex_compiled,
                home_range_id = as.integer(get(paste0("territoryID_", i)))) %>%
           select(bird_id, year, year_id, sex, home_range_id) %>%
           filter(sex == "m" | sex == "f")
  )
}

# Combining all data frames to one single
milvus_home_range <- get(paste0("year_", years[1]))
for (i in years[2:length(years)]) {
  milvus_home_range <- bind_rows(milvus_home_range,
                                 get(paste0("year_", i)))
}



# SELECTING BIRD IDS OF BIRDS WITH NO HOME RANGE--------------------------------
# Selection of birds
for (i in years) {
  assign(paste0("year_", i),
         ilh %>%
         dplyr::filter(get(paste0("territoryID_", i)) == "none") %>%
         mutate(year = i,
                year_id = paste0(year, "_", bird_id),
                sex = sex_compiled) %>%
           select(bird_id, year, year_id, sex) %>%
           filter(sex == "m" | sex == "f")
  )
}

# Combining all data frames to one single
milvus_no_home_range <- get(paste0("year_", years[1]))
for (i in years[2:length(years)]) {
  milvus_no_home_range <- bind_rows(milvus_no_home_range,
                                    get(paste0("year_", i)))
}



# SELECTING BIRD IDS OF BIRDS WITH A NEST---------------------------------------
# Selection of birds
for (i in years) {
  assign(paste0("year_", i),
         ilh %>%
         dplyr::filter(!is.na(get(paste0("nest_ID_", i-2000))) &
                         get(paste0("nest_ID_", i-2000)) != "unknown") %>%
         mutate(year = i,
                year_id = paste0(year, "_", bird_id),
                sex = sex_compiled,
                nest_id = as.integer(get(paste0("nest_ID_", i-2000)))) %>%
           select(bird_id, year, year_id, sex, nest_id) %>%
           filter(sex == "m" | sex == "f")
  )
}

# Combining all data frames to one single
milvus_nest <- get(paste0("year_", years[1]))
for (i in years[2:length(years)]) {
  milvus_nest <- bind_rows(milvus_nest,
                           get(paste0("year_", i)))
}



# SELECTING BIRD IDS OF BIRDS WITH NO NEST--------------------------------------
# Selection of birds
for (i in years) {
  assign(paste0("year_", i),
         ilh %>%
         dplyr::filter(is.na(get(paste0("nest_ID_", i-2000)))) %>%
         mutate(year = i,
                year_id = paste0(year, "_", bird_id),
                sex = sex_compiled) %>%
           select(bird_id, year, year_id, sex) %>%
           filter(sex == "m" | sex == "f")
  )
}

# Combining all data frames to one single
milvus_no_nest <- get(paste0("year_", years[1]))
for (i in years[2:length(years)]) {
  milvus_no_nest <- bind_rows(milvus_no_nest,
                                    get(paste0("year_", i)))
}



# REMOVING BIRDS OF NEST LISTS THAT DO NOT OCCUR IN TERRITORY LISTS ------------
# Removing birds from nest list that do not have a home range / territory
milvus_nest <- milvus_nest[milvus_nest$year_id %in%
                             milvus_home_range$year_id ,]
# Removing birds from no nest list that are not sure to have no home range
milvus_no_nest <- milvus_no_nest[milvus_no_nest$year_id %in%
                                   milvus_no_home_range$year_id |
                                   milvus_no_nest$year_id %in%
                                   milvus_home_range$year_id ,]
# Removing birds from home range list that are not sure whether to have a nest or not
milvus_home_range <- milvus_home_range[milvus_home_range$year_id %in%
                                         milvus_nest$year_id |
                                         milvus_home_range$year_id %in%
                                         milvus_no_nest$year_id ,]

# Combining all ground truth tables to one single table with all information
milvus_no_nest$nest_id <- 0
milvus_no_home_range$home_range_id <- 0

milvus_nest_combined <- bind_rows(milvus_nest, milvus_no_nest)
milvus_home_range_combined <- bind_rows(milvus_home_range, milvus_no_home_range)

milvus_home_range_and_nest <- full_join(milvus_nest_combined, milvus_home_range_combined)



# SAVING DATA ------------------------------------------------------------------
# Creating directory
if (!dir.exists(here("../Data/Output/01_validation_data"))) {
  if (!dir.exists(here("../Data/Output"))) {
    dir.create("../Data/Output")
  }
  dir.create("../Data/Output/01_validation_data")
}

write.csv(milvus_home_range_and_nest,
          here("../Data/Output/01_validation_data/milvus_home_range_and_nest.csv"),
          row.names = F)


