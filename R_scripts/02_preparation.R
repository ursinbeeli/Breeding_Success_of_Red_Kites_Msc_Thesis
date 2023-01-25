library(here)
library(dplyr)
library(rnaturalearth)
library(sf)
library(amt)

# disabling scientific notation
options(scipen=999)

# -------------------------------- LOADING DATA --------------------------------
milvus_gsm <- read.csv(here("data/Milvusmilvus_GSM_SOI.csv"))
milvus_milsar <- read.csv(here("data/Milvusmilvus_Milsar_SOI_final.csv"))
ground_truth <- read.csv(here("data/modified/01_ground_truth/milvus_ground_truth.csv"))

# CREATING AN INDIVIDUAL BIRD-YEAR ID
milvus_gsm <- milvus_gsm %>%
  mutate(year_id = paste0(format(as.Date(timestamp),
                                 format = "%Y", tz = "UTC"),
                          "_", individual.local.identifier))
milvus_milsar <- milvus_milsar %>%
  mutate(year_id = paste0(format(as.Date(timestamp),
                                 format = "%Y", tz = "UTC"),
                          "_", individual.local.identifier))

# REMOVING BIRDS WITHOUT AVAILABLE GROUND TRUTH DATA
milvus_gsm <- milvus_gsm[milvus_gsm$year_id %in%
                           unique(ground_truth$year_id) ,]
milvus_milsar <- milvus_milsar[milvus_milsar$year_id %in%
                                 unique(ground_truth$year_id) ,]



# ------------------------------ DATA PREPARATION ------------------------------ 
data_sets <- c("milvus_gsm", "milvus_milsar")
for (k in data_sets) {
  # select one of the data sets
  milvus <- get(k)
  
  # FORMATTING
  # converting timestamp into timestamp format
  milvus$timestamp <- as.POSIXct(milvus$timestamp, format ="%Y-%m-%d %H:%M:%S", tz = "UTC")
  # Removing NAs in timestamp column
  milvus <- milvus[!is.na(milvus$timestamp),]
  # creating a year, a month and a day column
  milvus$year <- format(as.Date(milvus$timestamp), format = "%Y", tz = "UTC")
  milvus$month <- format(as.Date(milvus$timestamp), format = "%m", tz = "UTC")
  milvus$day <- format(as.Date(milvus$timestamp), format = "%d", tz = "UTC")
  # Removing points without coordinates
  milvus <- milvus[!is.na(milvus$location.long) & !is.na(milvus$location.lat),]
  
  
  
  # ---------------------- REMOVING DUPLICATED TIMESTAMPS ---------------------- 
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
  # Unnest tracking data again
  milvus <- milvus %>%
    unnest(cols = c(data))
  # Removing duplicates
  milvus <- milvus[!is.na(milvus$timestamp),]
  
  
  
  # ------------------------ REMOVING SPATIAL OUTLIERS ------------------------- 
  # Borders of Europe with a 100km Buffer
  sf_use_s2(FALSE) # deactivating spherical geometry s2
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
  # creating an sf object and exclude points outside of Europe
  milvus <- st_as_sf(milvus, coords = c("location.long", "location.lat"))
  st_crs(milvus) <- 4326
  milvus$in_europe <- suppressMessages(suppressWarnings(
    st_intersects(milvus, europe, sparse = F)))
  milvus <- milvus[milvus$in_europe == T,]
  milvus <- dplyr::select(milvus, -in_europe)
  
  # writing coordinates from geometry in to separate columns
  # and transforming to European Environmental Association (EEA) projection
  milvus <- milvus %>%
    dplyr::mutate(long_wgs = st_coordinates(.)[,1],
                  lat_wgs = st_coordinates(.)[,2]) %>%
    st_transform(crs = 3035) %>%
    dplyr::mutate(long_eea = st_coordinates(.)[,1],
                  lat_eea = st_coordinates(.)[,2]) %>%
    st_drop_geometry()
  
  
  
  # ----------------------- RESAMPLE TO 1 HOUR INTERVALS ----------------------- 
  # creating tracks
  milvus_track <- milvus %>%
    make_track(long_wgs, lat_wgs, timestamp,
               bird_id = individual.local.identifier,
               event_id = event.id, crs = 4326)
  
  # grouping the track by bird_id and nesting them
  milvus_track <- milvus_track %>%
    nest(data = -"bird_id")
  
  # filtering the data to a sampling rate of around 1 hour
  # & burst with at least 3 locations
  milvus_track <- milvus_track %>% 
    mutate(data = map(data, function(x) 
      x %>%
        track_resample(rate = hours(1), tolerance = minutes(5)) %>%
        filter_min_n_burst(min_n = 3)
    )) %>%
    unnest(cols = data)
  
  # select only resampled entries with 1 hour interval from original df
  milvus <- milvus[is.element(milvus$event.id, milvus_track$event_id),]
  
  # overwrite original data frame with new one
  assign(k, milvus)
}



# ------------------------ REMOVING UNNECESSARY COLUMNS ------------------------ 
milvus_gsm_ready <- milvus_gsm %>%
  select(-visible, -gps.activity.count, -battery.charging.current, -comments,
         -gps.satellite.count, -gsm.gsm.signal.strength, -import.marked.outlier,
         -mw.activity.count, -tag.voltage, -transmission.timestamp, -sensor.type,
         -individual.taxon.canonical.name, -tag.local.identifier, -study.name)

milvus_milsar_ready <- milvus_milsar %>%
  select(-visible, -comments, -gps.hdop, -gps.satellite.count, gps.time.to.fix,
         -gps.vdop, -import.marked.outlier, -manually.marked.outlier,
         -solar.cell.voltage, -tag.voltage, -sensor.type,
         -individual.taxon.canonical.name, -tag.local.identifier, -study.name)



# -------------------------------- SAVING FILE --------------------------------- 
# creating directory
if (!dir.exists(here("data/modified"))) {
  dir.create("data/modified")
}
if (!dir.exists(here("data/modified/02_milvus_preprocessed"))) {
  dir.create("data/modified/02_milvus_preprocessed")
}

write.csv(milvus_gsm_ready,
          here("data/modified/02_milvus_preprocessed/01_milvus_gsm.csv"),
          row.names = F)

write.csv(milvus_milsar_ready,
          here("data/modified/02_milvus_preprocessed/02_milvus_milsar.csv"),
          row.names = F)



# -------------------------------- DATA MERGING -------------------------------- 
# removing unnecessary columns
milvus_gsm <- milvus_gsm %>%
  select(event_id = event.id,
         bird_id = individual.local.identifier,
         year_id, timestamp, year, month, day, long_wgs, lat_wgs,
         long_eea, lat_eea, external_temperature = external.temperature)
milvus_milsar <- milvus_milsar %>%
  select(event_id = event.id,
         bird_id = individual.local.identifier,
         year_id, timestamp, year, month, day, long_wgs, lat_wgs,
         long_eea, lat_eea, external_temperature = external.temperature)
# binding both data frames together
milvus <- bind_rows(milvus_gsm, milvus_milsar)
# cleaning temperature data
milvus$external_temperature <- as.integer(round(milvus$external_temperature))
milvus$external_temperature[milvus$external_temperature >= 99] <- NA
milvus <- milvus[!is.na(milvus$external_temperature) ,]



# -------------------------------- SAVING FILE --------------------------------- 
write.csv(milvus,
          here("data/modified/02_milvus_preprocessed/03_milvus_combined.csv"),
          row.names = F)


