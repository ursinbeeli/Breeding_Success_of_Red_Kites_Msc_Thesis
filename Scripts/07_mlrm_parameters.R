library(here)
library(dplyr)
library(amt)
library(recurse)
library(sf)



# Running this script took around 1.5 hours with a late 2016 MacBook Pro with 16GB Ram



# LOADING DATA -----------------------------------------------------------------
# Movement data
milvus <- read.csv(here("../Data/Output/02_preprocessed_data/milvus.csv"))
# Validation information
milvus_brood_validation <-
  read.csv(here("../Data/Output/06_mlrm_validation_data/milvus_brood_validation.csv"))
milvus_nest <- read.csv(here("../Data/Output/05_nest/milvus_nest_location.csv")) %>%
  select(year_id, sex, nest, nest_id, potential_nest_long, potential_nest_lat)



# DATA PREPARATION -------------------------------------------------------------
# Keeping only data of birds with a potential nest
milvus <- milvus %>%
  filter(year_id %in% milvus_nest$year_id)

# Formatting timestamp and date
milvus <- milvus %>%
  mutate(timestamp = as.POSIXct(timestamp, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"),
         date = as.Date(date, tz = "UTC"))
# Removing entries with NAs in timestamp column (happens during formatting since some entries have only date in timestamp)
milvus <- milvus[!is.na(milvus$timestamp),]

# Keeping only data from 10th of March to July
# (6 days earlier to enable several moving window calculations)
# (earliest egg laying date according to Aebischer & Scherler, 2021, p. 110)
milvus <- milvus %>%
  filter(month > 2 & month < 8 & !(month == 3 & day < 4))

# Removing days with less than three locations
milvus_counts_daily <- milvus %>%
  group_by(bird_id, date) %>%
  summarise(n_counts = n(),
            date_id = first(date_id))
milvus_counts_daily <- milvus_counts_daily %>%
  filter(n_counts < 3)
milvus <- milvus[!(milvus$date_id %in% milvus_counts_daily$date_id),]

# Keeping only birds with enough data in all months (sum of all months: 25) (min: 3 locations/day -> 143 days -> 429 locations)
milvus_counts <- milvus %>%
  group_by(year_id) %>%
  summarise(n_counts = n(),
            sum_months = sum(unique(month)))
milvus_counts <- milvus_counts %>%
  filter(sum_months == 25 & n_counts >= 429)
milvus <- milvus[milvus$year_id %in% milvus_counts$year_id,]

# Keeping only birds with available brood history validation data
milvus <- milvus[milvus$year_id %in% milvus_brood_validation$year_id ,]

# Creating a daily data frame to add information later
milvus_daily <- milvus %>%
  group_by(date_id) %>%
  summarise(bird_id = first(bird_id),
            year_id = first(year_id),
            date = first(date),
            year = first(year),
            month = first(month),
            day = first(day),
            long_wgs = mean(long_wgs),
            lat_wgs = mean(lat_wgs),
            long_eea = mean(long_eea),
            lat_eea = mean(lat_eea),
            locations_per_day = n())

# Adding nest information to whole data frame and daily data frame
milvus <- left_join(milvus, milvus_nest, by = "year_id")
milvus_daily <- left_join(milvus_daily, milvus_nest, by = "year_id")



# MCP --------------------------------------------------------------------------
# Creating a track for minimum convex polygon
milvus_track_mcp <- milvus %>%
  mk_track(
    .x = long_eea,
    .y = lat_eea,
    .t = timestamp,
    id = date_id,
    year_id,
    crs = 3035
  )

# 95% daily MCP
mcp_95 <- milvus_track_mcp %>%
  nest(data = -id) %>%
  mutate(mcp_area = map(data, ~ hr_mcp(., levels = c(0.95)) %>% hr_area)) %>%
  dplyr::select(id, mcp_area) %>%
  unnest(cols = mcp_area) %>%
  mutate(date_id = id,
         mcp_area_95 = area / 1000000) %>%
  select(-id, -level, -what, -area)

# Joining area information to milvus data set
milvus_daily <- left_join(milvus_daily, mcp_95, by = "date_id")

# 95% 7 days
milvus_track_mcp$mcp_area_95_7day <- NA
l <- 0
for (i in unique(milvus_track_mcp$year_id)) {
  milvus_track_individual <- milvus_track_mcp[milvus_track_mcp$year_id == i ,]
  for (j in 7:length(unique(milvus_track_individual$id))) {
    k <- unique(milvus_track_individual$id)[(j-6):j]
    milvus_track_subset <- milvus_track_individual[milvus_track_individual$id %in% k, ]
    mcp_area <- hr_mcp(milvus_track_subset, levels = c(0.95)) %>% hr_area()
    milvus_track_mcp[milvus_track_mcp$year_id == i &
                       milvus_track_mcp$id == unique(milvus_track_individual$id)[j]
                     ,]$mcp_area_95_7day <- mcp_area$area / 1000000
    # this last chunk is for progress report, since function takes a lot of time
    if ((round((l+j)*100/length(unique(milvus_track_mcp$id)), 2))%%1 == 0) {
      print(paste0("Progress: ",
                   round((l+j)*100/length(unique(milvus_track_mcp$id)), 2), "%"))
    }
  }
  l <- l + j
}

# Preparing track for joining
milvus_track_mcp_join <- milvus_track_mcp %>%
  group_by(date_id = id) %>%
  summarise(mcp_area_95_7day = first(mcp_area_95_7day))

# Joining area information to milvus data set
milvus_daily <- left_join(milvus_daily, milvus_track_mcp_join, by = "date_id")

# 95% moving window (mean over seven days)
milvus_daily$mcp_area_95_mw_7day <- NA
for (i in unique(milvus_daily$year_id)) {
  milvus_individual <- milvus_daily[milvus_daily$year_id == i ,]
  for (j in 7:length(milvus_individual$date)) {
    k <- seq(milvus_individual$date[j]-6,milvus_individual[j,]$date, by = "days")
    l <- milvus_individual[milvus_individual$date %in% k ,]
    milvus_daily[milvus_daily$year_id == i &
                   milvus_daily$date_id == unique(milvus_individual$date_id)[j]
                 ,]$mcp_area_95_mw_7day <- 
      mean(l$mcp_area_95, na.rm = T)
  }
}



# RECURSIONS TO NEST -----------------------------------------------------------
# Creating a track for recurse with year_id and potential nest locations
milvus_track_recurse <- milvus %>%
  mk_track(
    .x = long_eea,
    .y = lat_eea,
    .t = timestamp,
    id = date_id,
    year_id,
    potential_nest_long,
    potential_nest_lat,
    crs = 3035
  ) %>%
  data.frame()

# Splitting the track into a list where each element is a unique date_id
milvus_track_recurse_list <- split(milvus_track_recurse,
                                   milvus_track_recurse$id)

# Recursion
milvus_recurse_to_nest <- lapply(milvus_track_recurse_list, function(x)
  getRecursionsAtLocations(x = x[1:4], locations = x[6:7],
                           radius = 50, timeunits = "mins"))

# Finding the time/amount for each date_id
residence_time_nest <- c()
revisits_nest <- c()
for (i in 1:length(milvus_recurse_to_nest)) {
  residence_time_nest[i] <- as.integer(round(first(milvus_recurse_to_nest[[i]]$residenceTime)))
  revisits_nest[i] <- first(milvus_recurse_to_nest[[i]]$revisits)
}

# Combining the IDs with the max time data from the recursion analysis
milvus_recurse_nest <- cbind(data.frame(date_id = names(milvus_track_recurse_list),
                                        residence_time_nest, revisits_nest))

# Joining nest recurse information to milvus data set
milvus_daily <- left_join(milvus_daily, milvus_recurse_nest, by = "date_id")

# Moving window of residence time and revisits (mean over seven days)
milvus_daily$residence_time_nest_mw_7day <- NA
milvus_daily$revisits_nest_mw_7day <- NA
for (i in unique(milvus_daily$year_id)) {
  milvus_individual <- milvus_daily[milvus_daily$year_id == i ,]
  for (j in 7:length(milvus_individual$date)) {
    k <- seq(milvus_individual$date[j]-6,milvus_individual[j,]$date, by = "days")
    l <- milvus_individual[milvus_individual$date %in% k ,]
    milvus_daily[milvus_daily$year_id == i &
                   milvus_daily$date_id == unique(milvus_individual$date_id)[j]
                 ,]$residence_time_nest_mw_7day <- 
      mean(l$residence_time_nest, na.rm = T)
    milvus_daily[milvus_daily$year_id == i &
                   milvus_daily$date_id == unique(milvus_individual$date_id)[j]
                 ,]$revisits_nest_mw_7day <- 
      mean(l$revisits_nest, na.rm = T)
  }
}



# DISTANCE TO NEST -------------------------------------------------------------
# Creating sf objects of bird locations and nest locations
milvus_sf <- milvus %>%
  select(event_id, date_id, long_eea, lat_eea) %>%
  st_as_sf(coords = c("long_eea", "lat_eea"), crs = 3035)
nest_sf <- milvus %>%
  select(event_id, date_id, potential_nest_long, potential_nest_lat) %>%
  st_as_sf(coords = c("potential_nest_long", "potential_nest_lat"), crs = 3035)

# Calculating the distance of each position to respective nest location
sf_use_s2(FALSE) # deactivating spherical geometry s2
milvus_sf$nest_dist <- NA
milvus_sf$nest_dist <- st_distance(milvus_sf, nest_sf, by_element = T)

# Dropping geometry and calculating the mean daily nest distance
milvus_nest_dist <- milvus_sf %>%
  st_drop_geometry() %>%
  group_by(date_id) %>%
  summarise(nest_dist_mean = mean(nest_dist))

# Joining nest distance information to milvus data set
milvus_daily <- left_join(milvus_daily, milvus_nest_dist, by = "date_id")

# Moving window of nest distance (mean of daily mean over seven days)
milvus_daily$nest_dist_mean_mw_7day <- NA
for (i in unique(milvus_daily$year_id)) {
  milvus_individual <- milvus_daily[milvus_daily$year_id == i ,]
  for (j in 7:length(milvus_individual$date)) {
    k <- seq(milvus_individual$date[j]-6,milvus_individual[j,]$date, by = "days")
    l <- milvus_individual[milvus_individual$date %in% k ,]
    milvus_daily[milvus_daily$year_id == i &
                   milvus_daily$date_id == unique(milvus_individual$date_id)[j]
                 ,]$nest_dist_mean_mw_7day <- 
      mean(l$nest_dist_mean, na.rm = T)
  }
}



# CLEANING DATA ----------------------------------------------------------------
# Removing data before 10th of March, they were only necessary for moving window calculations
milvus_daily_final <- milvus_daily %>%
  filter(!(month == 3 & day < 10)) %>%
# Removing remaining NA's and sort the data frame by year_id
  na.omit() %>%
  arrange(year_id)



# ADDING BROOD VALIDATION DATA -------------------------------------------------
# Changing format of date columns to date format
milvus_brood_validation$egg_laying_date <- as.Date(milvus_brood_validation$egg_laying_date, tz = "UTC")
milvus_brood_validation$hatching_date <- as.Date(milvus_brood_validation$hatching_date, tz = "UTC")
milvus_brood_validation$empty_date <- as.Date(milvus_brood_validation$empty_date, tz = "UTC")

# Simplifying brood validation data for joining
milvus_brood_validation <- milvus_brood_validation[milvus_brood_validation$year_id %in% unique(milvus_daily_final$year_id) ,] %>%
  select(-bird_id, -year)

# Adding a new breeding_status column
milvus_daily_final <- milvus_daily_final %>%
  mutate(breeding_status = "nonbreeding")

# Filling in the breeding status
for (i in 1:nrow(milvus_brood_validation)) {
  if (length(milvus_daily_final[milvus_daily_final$year_id == milvus_brood_validation[i,]$year_id &
                                milvus_daily_final$date >= milvus_brood_validation[i,]$egg_laying_date &
                                milvus_daily_final$date < milvus_brood_validation[i,]$hatching_date
                                ,]$breeding_status) > 0) {
    milvus_daily_final[milvus_daily_final$year_id == milvus_brood_validation[i,]$year_id &
                         milvus_daily_final$date >= milvus_brood_validation[i,]$egg_laying_date &
                         milvus_daily_final$date < milvus_brood_validation[i,]$hatching_date
                       ,]$breeding_status <- "incubating"
  }
  if (length(milvus_daily_final[milvus_daily_final$year_id == milvus_brood_validation[i,]$year_id &
                                milvus_daily_final$date >= milvus_brood_validation[i,]$hatching_date &
                                milvus_daily_final$date <= milvus_brood_validation[i,]$empty_date
                                ,]$breeding_status) > 0) {
    milvus_daily_final[milvus_daily_final$year_id == milvus_brood_validation[i,]$year_id &
                         milvus_daily_final$date >= milvus_brood_validation[i,]$hatching_date &
                         milvus_daily_final$date <= milvus_brood_validation[i,]$empty_date
                       ,]$breeding_status <- "feeding"
  }
  print(paste(i, "of", nrow(milvus_brood_validation)))
}



# NORMALISING ALL VARIABLES ----------------------------------------------------
milvus_daily_final_norm <- milvus_daily_final

# Population specific variables
milvus_daily_final_norm$sex_chr <- milvus_daily_final_norm$sex
milvus_daily_final_norm[milvus_daily_final_norm$sex == "f" ,]$sex <- "1"
milvus_daily_final_norm[milvus_daily_final_norm$sex == "m" ,]$sex <- "0"
milvus_daily_final_norm$sex <-
  factor(milvus_daily_final_norm$sex, levels = c(0,1))
milvus_daily_final_norm$locations_per_day <-
  as.numeric(milvus_daily_final_norm$locations_per_day)
milvus_daily_final_norm$locations_per_day <-
  milvus_daily_final_norm$locations_per_day/
  max(milvus_daily_final_norm$locations_per_day)

# Individual specific variables
for (i in unique(milvus_daily_final_norm$year_id)) {
  assign(paste0("milvus_", i),
         milvus_daily_final_norm[milvus_daily_final_norm$year_id == i ,])
}
for (i in unique(milvus_daily_final_norm$year_id)) {
  milvus_individual <- get(paste0("milvus_", i))
  
  milvus_individual$mcp_area_95 <-
    milvus_individual$mcp_area_95/max(milvus_individual$mcp_area_95)
  milvus_individual$mcp_area_95_mw_7day <-
    milvus_individual$mcp_area_95_mw_7day/max(milvus_individual$mcp_area_95_mw_7day)
  milvus_individual$residence_time_nest <-
    milvus_individual$residence_time_nest/max(milvus_individual$residence_time_nest)
  milvus_individual$revisits_nest <-
    milvus_individual$revisits_nest/max(milvus_individual$revisits_nest)
  milvus_individual$residence_time_nest_mw_7day <-
    milvus_individual$residence_time_nest_mw_7day/max(milvus_individual$residence_time_nest_mw_7day)
  milvus_individual$revisits_nest_mw_7day <-
    milvus_individual$revisits_nest_mw_7day/max(milvus_individual$revisits_nest_mw_7day)
  milvus_individual$nest_dist_mean <-
    milvus_individual$nest_dist_mean/max(milvus_individual$nest_dist_mean)
  milvus_individual$nest_dist_mean_mw_7day <-
    milvus_individual$nest_dist_mean_mw_7day/max(milvus_individual$nest_dist_mean_mw_7day)
  
  assign(paste0("milvus_", i), milvus_individual)
}
milvus_complete <- data.frame()
for (i in unique(milvus_daily_final_norm$year_id)) {
  milvus_complete <- bind_rows(milvus_complete, get(paste0("milvus_", i)))
}



# SAVING DATA ------------------------------------------------------------------
# Creating directory
if (!dir.exists(here("../Data/Output/07_mlrm_parameters"))) {
  if (!dir.exists(here("../Data/Output"))) {
    dir.create("../Data/Output")
  }
  dir.create("../Data/Output/07_mlrm_parameters")
}

write.csv(milvus_complete,
          here("../Data/Output/07_mlrm_parameters/milvus_mlrm_parameters.csv"),
          row.names = F)


