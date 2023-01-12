library(here)
library(dplyr)
library(amt)
library(recurse)
library(sf)


# disabling scientific notation
options(scipen=999)

# LOADING DATA -----------------------------------------------------------------
milvus <- read.csv(here("data/modified/02_milvus_preprocessed/03_milvus_combined.csv"))
ground_truth <- read.csv(here("data/modified/01_ground_truth/milvus_ground_truth_nest.csv"))



# DATA PREPARATION -------------------------------------------------------------
# keep only data for February to July may
milvus <- milvus %>%
  filter(month > 1 & month < 8)
# removing rows with NA in temperature data
milvus <- milvus[!is.na(milvus$external_temperature),]
# formatting timestamp to timestamp format
milvus$timestamp <- as.POSIXct(milvus$timestamp, format ="%Y-%m-%d %H:%M:%S", tz = "UTC")
# adding a date column
milvus$date <- as.Date(milvus$timestamp, format ="%Y-%m-%d", tz = "UTC")
# adding a date_id column
milvus <- milvus %>%
  mutate(date_id = paste0(date, "_", bird_id))

# keeping only birds with enough data in all months
# sum of all months (feb-jul): 27
# min: 3 loc/day -> 30 loc/month -> 180 loc
milvus_counts <- milvus %>%
  group_by(year_id) %>%
  summarise(n_counts = n(),
            sum_months = sum(unique(month)))
milvus_counts <- milvus_counts %>%
  filter(sum_months == 27 & n_counts >= 180)
milvus <- milvus[milvus$year_id %in% milvus_counts$year_id,]
# (initially 53 year_id & 37 bird_id, 93207 locations)
# (after     31 year_id & 25 bird_id, 69967 locations)

# removing days with less than three locations
milvus_counts_daily <- milvus %>%
  group_by(bird_id, date) %>%
  summarise(n_counts = n(),
            date_id = first(date_id))
milvus_counts_daily <- milvus_counts_daily %>%
  filter(n_counts < 3)
milvus <- milvus[!(milvus$date_id %in% milvus_counts_daily$date_id),]
# (initially 31 year_id & 25 bird_id, 69967 locations)
# (after     31 year_id & 25 bird_id, 69967 locations)



# CREATING TRACK ---------------------------------------------------------------
milvus_track <- milvus %>%
  mk_track(
    .x = long_eea,
    .y = lat_eea,
    .t = timestamp,
    id = date_id,
    event_id,
    crs = 3035
  ) %>%
  time_of_day(include.crepuscule = T)

# join time_of_day information to milvus data set
milvus_time_of_day <- milvus_track %>%
  select(event_id, time_of_day = tod_)
milvus <- left_join(milvus, milvus_time_of_day, by = "event_id")

# removing event id and order information
milvus_track <- milvus_track %>%
  select(-event_id) %>%
  arrange(id, t_)



# RECURSIONS -------------------------------------------------------------------
# making a data frame and renaming the columns
milvus_track_recurse <- data.frame(milvus_track) %>%
  mutate(.x = x_,
         .y = y_,
         .t = t_) %>%
  select(.x, .y, .t, id)

# split the track into a list where each element is a unique ID-month-year identifier
milvus_track_list <- split(milvus_track_recurse, milvus_track_recurse$id)

# recursion
milvus_revisits <- lapply(milvus_track_list, function(x)
  getRecursions(x = x, radius = 20, timeunits = "mins"))

# find the max time/amount for each date_id
max_residence_time <- c()
min_residence_time <- c()
mean_residence_time <- c()
max_revisits <- c()
min_revisits <- c()
mean_revisits <- c()
for (i in 1:length(milvus_revisits)) {
  max_residence_time[i] <- round(max(milvus_revisits[[i]]$residenceTime))
  min_residence_time[i] <- round(min(milvus_revisits[[i]]$residenceTime))
  mean_residence_time[i] <- round(mean(milvus_revisits[[i]]$residenceTime))
  max_revisits[i] <- max(milvus_revisits[[i]]$revisits)
  min_revisits[i] <- min(milvus_revisits[[i]]$revisits)
  mean_revisits[i] <- round(mean(milvus_revisits[[i]]$revisits))
}

# combine the IDs with the max time data from the recursion analysis
milvus_recurse <- cbind(data.frame(date_id = names(milvus_track_list),
                                   max_residence_time, min_residence_time,
                                   mean_residence_time, max_revisits,
                                   min_revisits, mean_revisits))

# join recurse information to milvus data set
milvus <- left_join(milvus, milvus_recurse, by = "date_id")



# STEP LENGTH ------------------------------------------------------------------
milvus_track_sl <- milvus_track
colnames(milvus_track_sl)[4] <- "burst_"

# calculating steps by burst while burst represents the date_id
milvus_track_sl <- steps_by_burst(milvus_track_sl)

# deleting all entries calculated for time intervals > 1.5 hours
milvus_track_sl <- milvus_track_sl[milvus_track_sl$dt_ < 4500,]

# calculate some statistical measures on a daily basis
milvus_track_sl <- milvus_track_sl %>%
  group_by(burst_) %>%
  summarise(
    sl_max = max(sl_),
    sl_min = min(sl_),
    sl_mean = mean(sl_),
    sl_var = var(sl_)
  )

# join step length information to milvus data set
milvus <- left_join(milvus, milvus_track_sl, by = c("date_id" = "burst_"))



# TEMPERATURE ------------------------------------------------------------------
# calculate some statistical measures on a daily basis
milvus_temp <- milvus %>%
  group_by(date_id) %>%
  summarise(
    t_max = max(external_temperature),
    t_min = min(external_temperature),
    t_mean = mean(external_temperature),
    t_var = var(external_temperature)
  )

# join temperature information to milvus data set
milvus <- left_join(milvus, milvus_temp, by = "date_id")



# DISTANCE TO NEST -------------------------------------------------------------
# creating sf objects of bird locations and nest locations
ground_truth_nest <- ground_truth %>%
  select(year_id, nest_id)
milvus_sf <- left_join(milvus, ground_truth_nest, by = "year_id")
milvus_sf <- milvus_sf %>%
  select(event_id, date_id, nest_id, long_eea, lat_eea) %>%
  st_as_sf(coords = c("long_eea", "lat_eea"), crs = 3035)
nest_sf <- ground_truth %>%
  select(nest_id, nest_elevation, nest_long, nest_lat) %>%
  st_as_sf(coords = c("nest_long", "nest_lat"), crs = 4326) %>%
  st_transform(crs = 3035)

# calculation the distance of each position to respective nest location
milvus_sf$nest_dist <- NA
# ------------------------- This chunk takes 4.8 hours ------------------------- 
for (i in 1:nrow(milvus_sf)) {
  milvus_sf[i ,]$nest_dist <- st_distance(milvus_sf[i ,]$geometry,
                                          nest_sf[nest_sf$nest_id == milvus_sf[i ,]$nest_id ,]$geometry)
  print(i)
}
# ------------------------- This chunk took 4.8 hours -------------------------- 

# drop geometry to save file as in csv format
milvus_nest_dist <- milvus_sf %>%
  st_drop_geometry()

# save file
write.csv(milvus_nest_dist, here("data/modified/03_multinomial/01_milvus_nest_dist.csv"),
          row.names = F)

# calculate some statistical measures on a daily basis
milvus_nest_dist <- milvus_nest_dist %>%
  group_by(date_id) %>%
  summarise(
    nest_dist_max = max(nest_dist),
    nest_dist_min = min(nest_dist),
    nest_dist_mean = mean(nest_dist),
    nest_dist_var = var(nest_dist)
  ) %>%
  select(date_id, nest_dist_max, nest_dist_min, nest_dist_mean, nest_dist_var)

# join nest distance information to milvus data set
milvus <- left_join(milvus, milvus_nest_dist, by = "date_id")

# save file
write.csv(milvus, here("data/modified/03_multinomial/02_milvus_parameters.csv"),
          row.names = F)



# RECURSIONS TO NEST -----------------------------------------------------------
# creating a nest list with coordinates in EEA
ground_truth_nest_eea <- ground_truth %>%
  select(year_id, nest_long, nest_lat) %>%
  st_as_sf(coords = c("nest_long", "nest_lat"), crs = 4326) %>%
  st_transform(crs = 3035) %>%
  dplyr::mutate(nest_long = st_coordinates(.)[,1],
                nest_lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

# creating a track including the year_id to join with nest list
milvus_track_recurse_nest <- milvus %>%
  mk_track(
    .x = long_eea,
    .y = lat_eea,
    .t = timestamp,
    id = date_id,
    year_id,
    crs = 3035
  )

# joining nest coordinates to track
milvus_track_recurse_nest <- left_join(milvus_track_recurse_nest,
                                       ground_truth_nest_eea,
                                       by = "year_id") %>%
  select(-year_id)

# making a data frame and renaming the columns
milvus_track_recurse_nest <- data.frame(milvus_track_recurse_nest) %>%
  mutate(.x = x_,
         .y = y_,
         .t = t_) %>%
  select(.x, .y, .t, id, nest_long, nest_lat)

# split the track into a list where each element is a unique ID-month-year identifier
milvus_track_recurse_nest_list <- split(milvus_track_recurse_nest,
                                        milvus_track_recurse_nest$id)

# recursion
milvus_revisits_nest <- lapply(milvus_track_recurse_nest_list, function(x)
  getRecursionsAtLocations(x = x[1:4], locations = x[5:6],
                           radius = 20, timeunits = "mins"))

# find the max time/amount for each date_id
residence_time_nest <- c()
revisits_nest <- c()
for (i in 1:length(milvus_revisits_nest)) {
  residence_time_nest[i] <- as.integer(round(milvus_revisits_nest[[i]]$residenceTime))
  revisits_nest[i] <- milvus_revisits_nest[[i]]$revisits
}

# combine the IDs with the max time data from the recursion analysis
milvus_recurse_nest <- cbind(data.frame(date_id = names(milvus_track_recurse_nest_list),
                                        residence_time_nest, revisits_nest))

# join nest recurse information to milvus data set
milvus <- left_join(milvus, milvus_recurse_nest, by = "date_id")

# save file
write.csv(milvus, here("data/modified/03_multinomial/03_milvus_parameters.csv"),
          row.names = F)



# KDE --------------------------------------------------------------------------
# creating a track for kernel density estimation with year_id & event_id
milvus_track_kde <- milvus %>%
  mk_track(
    .x = long_eea,
    .y = lat_eea,
    .t = timestamp,
    id = date_id,
    year_id,
    event_id,
    crs = 3035
  )



# 95% daily -----> 1.1 hour <-----
milvus_track_kde_daily <- milvus_track_kde %>%
  select(-year_id)
kde_95 <- milvus_track_kde_daily %>%
  nest(data = -id) %>%
  mutate(kde_area = map(data, ~ hr_akde(., levels = c(0.95)) %>% hr_area)) %>%
  dplyr::select(id, kde_area) %>%
  unnest(cols = kde_area) %>%
  mutate(date_id = id,
         kde_area_95 = area / 1000000) %>%
  dplyr::filter(what == "estimate") %>%
  select(-id, -level, -what, -area)

# Joining area information to milvus data set
milvus <- left_join(milvus, kde_95, by = "date_id")



# 95% 7 days -----> 1.3 hour <-----
milvus_track_kde$kde_area_95_7day <- NA
l <- 0
for (i in unique(milvus_track_kde$year_id)) {
  milvus_track_individual <- milvus_track_kde[milvus_track_kde$year_id == i ,]
  for (j in 7:length(unique(milvus_track_individual$id))) {
    k <- unique(milvus_track_individual$id)[(j-6):j]
    milvus_track_subset <- milvus_track_individual[milvus_track_individual$id %in% k, ]
    kde_area <- hr_akde(milvus_track_subset, levels = c(0.95)) %>% hr_area()
    milvus_track_kde[milvus_track_kde$year_id == i &
                       milvus_track_kde$id == unique(milvus_track_individual$id)[j]
                     ,]$kde_area_95_7day <- 
      kde_area[kde_area$what == "estimate" ,]$area / 1000000
    # this last chunk is for progress report, since function takes a lot of time
    print(paste0("year_id: ", match(i, unique(milvus_track_kde$year_id)),
                 " of ", length(unique(milvus_track_kde$year_id)),
                 " - date_id: ", j, " of ",
                 length(unique(milvus_track_individual$id)),
                 " - total progress: ",
                 round((l+j)*100/length(unique(milvus_track_kde$id)), 2), "%"))
  }
  l <- l + j
}

# preparing track for joining
kde_95_7day <- milvus_track_kde %>%
  select(event_id, kde_area_95_7day)

# Joining area information to milvus data set
milvus <- left_join(milvus, kde_95_7day, by = "event_id")



# 50% 7 days -----> 1.3 hour <-----
milvus_track_kde$kde_area_50_7day <- NA
l <- 0
for (i in unique(milvus_track_kde$year_id)) {
  milvus_track_individual <- milvus_track_kde[milvus_track_kde$year_id == i ,]
  for (j in 7:length(unique(milvus_track_individual$id))) {
    k <- unique(milvus_track_individual$id)[(j-6):j]
    milvus_track_subset <- milvus_track_individual[milvus_track_individual$id %in% k, ]
    kde_area <- hr_akde(milvus_track_subset, levels = c(0.50)) %>% hr_area()
    milvus_track_kde[milvus_track_kde$year_id == i &
                       milvus_track_kde$id == unique(milvus_track_individual$id)[j]
                     ,]$kde_area_50_7day <- 
      kde_area[kde_area$what == "estimate" ,]$area / 1000000
    # this last chunk is for progress report, since function takes a lot of time
    print(paste0("year_id: ", match(i, unique(milvus_track_kde$year_id)),
                 " of ", length(unique(milvus_track_kde$year_id)),
                 " - date_id: ", j, " of ",
                 length(unique(milvus_track_individual$id)),
                 " - total progress: ",
                 round((l+j)*100/length(unique(milvus_track_kde$id)), 2), "%"))
  }
  l <- l + j
}

# preparing track for joining
kde_50_7day <- milvus_track_kde %>%
  select(event_id, kde_area_50_7day)

# Joining area information to milvus data set
milvus <- left_join(milvus, kde_50_7day, by = "event_id")



# 95% 5 days -----> 1.2 hour <-----
milvus_track_kde$kde_area_95_5day <- NA
l <- 0
for (i in unique(milvus_track_kde$year_id)) {
  milvus_track_individual <- milvus_track_kde[milvus_track_kde$year_id == i ,]
  for (j in 5:length(unique(milvus_track_individual$id))) {
    k <- unique(milvus_track_individual$id)[(j-4):j]
    milvus_track_subset <- milvus_track_individual[milvus_track_individual$id %in% k, ]
    kde_area <- hr_akde(milvus_track_subset, levels = c(0.95)) %>% hr_area()
    milvus_track_kde[milvus_track_kde$year_id == i &
                       milvus_track_kde$id == unique(milvus_track_individual$id)[j]
                     ,]$kde_area_95_5day <- 
      kde_area[kde_area$what == "estimate" ,]$area / 1000000
    # this last chunk is for progress report, since function takes a lot of time
    print(paste0("year_id: ", match(i, unique(milvus_track_kde$year_id)),
                 " of ", length(unique(milvus_track_kde$year_id)),
                 " - date_id: ", j, " of ",
                 length(unique(milvus_track_individual$id)),
                 " - total progress: ",
                 round((l+j)*100/length(unique(milvus_track_kde$id)), 2), "%"))
  }
  l <- l + j
}

# preparing track for joining
kde_95_5day <- milvus_track_kde %>%
  select(event_id, kde_area_95_5day)

# Joining area information to milvus data set
milvus <- left_join(milvus, kde_95_5day, by = "event_id")



# 50% 5 days -----> 1.3 hour <-----
milvus_track_kde$kde_area_50_5day <- NA
l <- 0
for (i in unique(milvus_track_kde$year_id)) {
  milvus_track_individual <- milvus_track_kde[milvus_track_kde$year_id == i ,]
  for (j in 5:length(unique(milvus_track_individual$id))) {
    k <- unique(milvus_track_individual$id)[(j-4):j]
    milvus_track_subset <- milvus_track_individual[milvus_track_individual$id %in% k, ]
    kde_area <- hr_akde(milvus_track_subset, levels = c(0.50)) %>% hr_area()
    milvus_track_kde[milvus_track_kde$year_id == i &
                       milvus_track_kde$id == unique(milvus_track_individual$id)[j]
                     ,]$kde_area_50_5day <- 
      kde_area[kde_area$what == "estimate" ,]$area / 1000000
    # this last chunk is for progress report, since function takes a lot of time
    print(paste0("year_id: ", match(i, unique(milvus_track_kde$year_id)),
                 " of ", length(unique(milvus_track_kde$year_id)),
                 " - date_id: ", j, " of ",
                 length(unique(milvus_track_individual$id)),
                 " - total progress: ",
                 round((l+j)*100/length(unique(milvus_track_kde$id)), 2), "%"))
  }
  l <- l + j
}

# preparing track for joining
kde_50_5day <- milvus_track_kde %>%
  select(event_id, kde_area_50_5day)

# Joining area information to milvus data set
milvus <- left_join(milvus, kde_50_5day, by = "event_id")



# 95% 3 days -----> 1.1 hour <-----
milvus_track_kde$kde_area_95_3day <- NA
l <- 0
for (i in unique(milvus_track_kde$year_id)) {
  milvus_track_individual <- milvus_track_kde[milvus_track_kde$year_id == i ,]
  for (j in 3:length(unique(milvus_track_individual$id))) {
    k <- unique(milvus_track_individual$id)[(j-2):j]
    milvus_track_subset <- milvus_track_individual[milvus_track_individual$id %in% k, ]
    kde_area <- hr_akde(milvus_track_subset, levels = c(0.95)) %>% hr_area()
    milvus_track_kde[milvus_track_kde$year_id == i &
                       milvus_track_kde$id == unique(milvus_track_individual$id)[j]
                     ,]$kde_area_95_3day <- 
      kde_area[kde_area$what == "estimate" ,]$area / 1000000
    # this last chunk is for progress report, since function takes a lot of time
    print(paste0("year_id: ", match(i, unique(milvus_track_kde$year_id)),
                 " of ", length(unique(milvus_track_kde$year_id)),
                 " - date_id: ", j, " of ",
                 length(unique(milvus_track_individual$id)),
                 " - total progress: ",
                 round((l+j)*100/length(unique(milvus_track_kde$id)), 2), "%"))
  }
  l <- l + j
}

# preparing track for joining
kde_95_3day <- milvus_track_kde %>%
  select(event_id, kde_area_95_3day)

# Joining area information to milvus data set
milvus <- left_join(milvus, kde_95_3day, by = "event_id")



# 95% moving window (mean over five days) -----> 3 mins <-----
milvus$kde_area_95_mw <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 5:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-4):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(kde_area_95 = first(kde_area_95))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$kde_area_95_mw <- 
      mean(l$kde_area_95, na.rm = T)
  }
}



# 95% moving window difference (diff to mean of previous five days) -----> 3 mins <-----
milvus$kde_area_95_mw_diff <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 2:length(unique(milvus_individual$date_id))) {
    milvus[milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$kde_area_95_mw_diff <- 
    abs(first(milvus[milvus$date_id == unique(milvus_individual$date_id)[j]
                     ,]$kde_area_95)
        - first(milvus[milvus$date_id == unique(milvus_individual$date_id)[j-1]
                       ,]$kde_area_95_mw))
  }
}



# save file
write.csv(milvus, here("data/modified/03_multinomial/04_milvus_parameters.csv"),
          row.names = F)








# FINAL ORGANISATION OF DATA FRAME ---------------------------------------------
milvus_daily <- milvus %>%
  group_by(date_id) %>%
  summarise(
    bird_id = first(bird_id),
    year_id = first(year_id),
    year = first(year),
    month = first(month),
    date = first(date),
    long_wgs = mean(long_wgs),
    lat_wgs = mean(lat_wgs),
    long_eea = mean(long_eea),
    lat_eea = mean(lat_eea),
    max_residence_time = first(max_residence_time),
    min_residence_time = first(min_residence_time),
    mean_residence_time = first(mean_residence_time),
    max_revisits = first(max_revisits),
    min_revisits = first(min_revisits),
    mean_revisits = first(mean_revisits),
    sl_max = first(sl_max),
    sl_min = first(sl_min),
    sl_mean = first(sl_mean),
    sl_var = first(sl_var),
    t_max = first(t_max),
    t_min = first(t_min),
    t_mean = first(t_mean),
    t_var = first(t_var),
    nest_dist_max = first(nest_dist_max),
    nest_dist_min = first(nest_dist_min),
    nest_dist_mean = first(nest_dist_mean),
    nest_dist_var = first(nest_dist_var),
    residence_time_nest = first(residence_time_nest),
    revisits_nest = first(revisits_nest),
    kde_area_95 = first(kde_area_95),
    kde_area_95_7day = first(kde_area_95_7day),
    kde_area_50_7day = first(kde_area_50_7day),
    kde_area_95_5day = first(kde_area_95_5day),
    kde_area_50_5day = first(kde_area_50_5day),
    kde_area_95_3day = first(kde_area_95_3day),
    kde_area_95_mw = first(kde_area_95_mw),
    kde_area_95_mw_diff = first(kde_area_95_mw_diff),
    locations_per_day = n()
    )

# save file
write.csv(milvus_daily, here("data/modified/03_multinomial/05_milvus_parameters_daily.csv"),
          row.names = F)



# ADDING GROUND TRUTH DATA -----------------------------------------------------
# changing format of date columns to date format
ground_truth$egg_laying_date <- as.Date(ground_truth$egg_laying_date, tz = "UTC")
ground_truth$hatching_date <- as.Date(ground_truth$hatching_date, tz = "UTC")
ground_truth$empty_date <- as.Date(ground_truth$empty_date, tz = "UTC")

# simplify ground truth data for joining
ground_truth <- ground_truth[ground_truth$year_id
                             %in% unique(milvus_daily$year_id) ,] %>%
  select(-bird_id, -year)

# adding a new breeding_status column
milvus_daily <- milvus_daily %>%
  mutate(breeding_status = "nonbreeding")

# fill in the breeding status
for (i in 1:nrow(ground_truth)) {
  if (ground_truth[i,]$incubation == "yes") {
    if (!is.na(ground_truth[i,]$hatching_date)) {
      milvus_daily[milvus_daily$year_id == ground_truth[i,]$year_id &
                     milvus_daily$date >= ground_truth[i,]$egg_laying_date &
                     milvus_daily$date <= ground_truth[i,]$hatching_date
                   ,]$breeding_status <- "incubating"
    }
    else {
      milvus_daily[milvus_daily$year_id == ground_truth[i,]$year_id &
                     milvus_daily$date >= ground_truth[i,]$egg_laying_date &
                     milvus_daily$date <= ground_truth[i,]$empty_date
                   ,]$breeding_status <- "incubating"
    }
  }
  if (!is.na(ground_truth[i,]$hatching_date)) {
    milvus_daily[milvus_daily$year_id == ground_truth[i,]$year_id &
                   milvus_daily$date >= ground_truth[i,]$hatching_date &
                   milvus_daily$date <= ground_truth[i,]$empty_date
                 ,]$breeding_status <- "feeding"
  }
}

# make ground truth information ready to join
ground_truth <- ground_truth %>%
  select(year_id, sex, age, nest_elevation)

# join ground truth information to milvus data set
milvus_daily <- left_join(milvus_daily, ground_truth, by = "year_id")

# save file
write.csv(milvus_daily, here("data/modified/03_multinomial/06_milvus_parameters_daily_ground_truth.csv"),
          row.names = F)


