# LOADING PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)
library(rnaturalearth)
library(sf)
library(amt)



# LOADING DATA -----------------------------------------------------------------
# Movement data
milvus_gsm <- read.csv(here("../Data/Milvusmilvus_GSM_SOI.csv"))
milvus_milsar <- read.csv(here("../Data/Milvusmilvus_Milsar_SOI_final.csv"))
# Validation information
milvus_validation <-
  read.csv(here("../Data/Output/01_validation_data/milvus_home_range_and_nest.csv"))



# DATA PREPARATION -------------------------------------------------------------
data_sets <- c("milvus_gsm", "milvus_milsar")
for (k in data_sets) {
  # Selecting one of the data sets
  milvus <- get(k)
  
  
  
  # FORMATTING
  # Creating a year_id and removing the ones without validation data
  milvus <- milvus %>%
    mutate(year_id = paste0(format(as.Date(timestamp),
                                   format = "%Y", tz = "UTC"),
                            "_", individual.local.identifier)) %>%
    filter(year_id %in% milvus_validation$year_id)
  
  # Creating several time specific variables
  milvus <- milvus %>%
    mutate(timestamp = as.POSIXct(timestamp, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"),
           date = as.Date(timestamp, tz = "UTC"),
           year = as.integer(format(as.Date(timestamp), format = "%Y", th = "UTC")),
           month = as.integer(format(as.Date(timestamp), format = "%m", th = "UTC")),
           day = as.integer(format(as.Date(timestamp), format = "%d", th = "UTC")),
           week = format(date, format = "%W"),
           year_week = format(date, format = "%Y-%W"),
           year_week_id = paste0(format(date, format = "%Y_%W"), "_", individual.local.identifier),
           date_id = paste0(date, "_", individual.local.identifier))
  
  # Removing entries with NAs in timestamp column
  milvus <- milvus[!is.na(milvus$timestamp),]
  
  # Removing points outside of breeding season
  milvus <- milvus %>%
    dplyr::filter(month > 1 & month < 9 & !(month == 8 & day > 15))
  # Removing points without coordinates
  milvus <- milvus[!is.na(milvus$location.long) & !is.na(milvus$location.lat),]
  
  
  
  # REMOVING DUPLICATES IN TIMESTAMPS
  # Grouping the gps data by bird_id and nesting them
  milvus <- milvus %>%
    nest(data = -"individual.local.identifier")
  # Checking for duplicated time stamps for each bird
  duplicates <- data.frame(id = 1:nrow(milvus), duplicates = F)
  for (i in 1:nrow(milvus)) {
    duplicates[i,]$duplicates <- any(duplicated(milvus$data[[i]]$timestamp))
  }
  # Identifying the locations of duplicates in data frame
  for (i in 1:nrow(milvus)) {
    if (duplicates[i,]$duplicates == T) {
      for (j in 1:nrow(milvus$data[[i]])) {
        if (duplicated(milvus$data[[i]][j,])) {
          milvus$data[[i]][j,]$timestamp <- NA
        }
      }
    }
  }
  # Unnesting tracking data again
  milvus <- milvus %>%
    unnest(cols = c(data))
  # Removing duplicates
  milvus <- milvus[!is.na(milvus$timestamp),]
  
  
  
  # REMOVING SPATIAL OUTLIERS
  # Borders of Europe with a 100km Buffer
  suppressMessages(sf_use_s2(FALSE)) # deactivating spherical geometry s2
  europe <- ne_countries(scale = 50, continent = "europe", returnclass="sf")
  st_crs(europe) <- 4326
  europe <- suppressMessages(suppressWarnings(
    europe %>%
      st_crop(c(xmin = -10, ymin = 30, xmax = 50, ymax = 70)) %>%
      st_union() %>%
      st_buffer(dist = 3) %>% # 1 degree is around 111 km
      st_buffer(dist = -2) %>% # positive and negative buffer to avoid holes 
      st_simplify(dTolerance = 0.5) %>%
      st_as_sf()))
  
  # Creating an sf object and excluding points outside of Europe
  milvus <- st_as_sf(milvus, coords = c("location.long", "location.lat"))
  st_crs(milvus) <- 4326
  milvus$in_europe <- suppressMessages(suppressWarnings(
    st_intersects(milvus, europe, sparse = F)))
  milvus <- milvus[milvus$in_europe == T,]
  milvus <- dplyr::select(milvus, -in_europe)
  suppressMessages(sf_use_s2(TRUE)) # reactivating spherical geometry s2
  
  # Writing coordinates from geometry in to separate columns
  # and transforming to European Environmental Association (EEA) projection
  milvus <- milvus %>%
    dplyr::mutate(long_wgs = st_coordinates(.)[,1],
                  lat_wgs = st_coordinates(.)[,2]) %>%
    st_transform(crs = 3035) %>%
    dplyr::mutate(long_eea = st_coordinates(.)[,1],
                  lat_eea = st_coordinates(.)[,2]) %>%
    st_drop_geometry()
  
  
  
  # RESAMPLING TO 1 HOUR INTERVALS
  # Creating tracks
  milvus_track <- milvus %>%
    make_track(long_wgs, lat_wgs, timestamp,
               bird_id = individual.local.identifier,
               event_id = event.id, crs = 4326)
  
  # Grouping the track by bird_id and nesting them
  milvus_track <- milvus_track %>%
    nest(data = -"bird_id")
  
  # Filtering the data to a sampling rate of around 1 hour
  # & burst with at least 3 locations
  milvus_track <- milvus_track %>% 
    mutate(data = map(data, function(x) 
      x %>%
        track_resample(rate = hours(1), tolerance = minutes(5)) %>%
        filter_min_n_burst(min_n = 3)
    )) %>%
    unnest(cols = data)
  
  # Selecting only resampled entries with 1 hour interval from original df
  milvus <- milvus[is.element(milvus$event.id, milvus_track$event_id),]
  
  # Overwriting original data frame with new one
  assign(k, milvus)
}



# DATA MERGING -----------------------------------------------------------------
# Retaining only necessary columns
milvus_gsm <- milvus_gsm %>%
  select(event_id = event.id,
         bird_id = individual.local.identifier,
         year_id, timestamp, date, year, month, day, week, year_week, year_week_id, date_id, long_wgs, lat_wgs,
         long_eea, lat_eea, external_temperature = external.temperature)
milvus_milsar <- milvus_milsar %>%
  select(event_id = event.id,
         bird_id = individual.local.identifier,
         year_id, timestamp, date, year, month, day, week, year_week, year_week_id, date_id, long_wgs, lat_wgs,
         long_eea, lat_eea, external_temperature = external.temperature)
# Binding both data frames together
milvus <- bind_rows(milvus_gsm, milvus_milsar)
# Cleaning temperature data (removing errors (99 & 999))
milvus <- milvus %>%
  mutate(external_temperature = as.integer(round(external_temperature))) %>%
  dplyr::filter(external_temperature < 99)



# SAVING DATA ------------------------------------------------------------------
# Creating directory
if (!dir.exists(here("../Data/Output/02_preprocessed_data"))) {
  if (!dir.exists(here("../Data/Output"))) {
    dir.create("../Data/Output")
  }
  dir.create("../Data/Output/02_preprocessed_data")
}

write.csv(milvus,
          here("../Data/Output/02_preprocessed_data/milvus.csv"),
          row.names = F)


