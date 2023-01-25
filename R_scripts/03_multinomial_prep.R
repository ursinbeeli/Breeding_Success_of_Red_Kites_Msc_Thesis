library(here)
library(dplyr)
library(amt)
library(recurse)
library(sf)

# disabling scientific notation
options(scipen=999)

# -------------------------------- LOADING DATA --------------------------------
milvus <- read.csv(here("data/modified/02_milvus_preprocessed/03_milvus_combined.csv"))
ground_truth <- read.csv(here("data/modified/01_ground_truth/milvus_ground_truth.csv"))



# ------------------------------ DATA PREPARATION ------------------------------ 
# formatting timestamp to timestamp format
milvus$timestamp <- as.POSIXct(milvus$timestamp, format ="%Y-%m-%d %H:%M:%S", tz = "UTC")
# adding a date column
milvus$date <- as.Date(milvus$timestamp, format ="%Y-%m-%d", tz = "UTC")
# adding a date_id column
milvus <- milvus %>%
  mutate(date_id = paste0(date, "_", bird_id))

# keep only data from 10th of March to July
# (6 days earlier to enable several moving window calculations)
# (earliest egg laying date according to Aebischer & Scherler, 2021, p. 110)
milvus <- milvus %>%
  filter(month > 2 & month < 8 & !(month == 3 & day < 4))

# keeping only birds with enough data in all months
# sum of all months (mar-jul): 25
# min: 3 loc/day -> 150 days -> 450 loc
milvus_counts <- milvus %>%
  group_by(year_id) %>%
  summarise(n_counts = n(),
            sum_months = sum(unique(month)))
milvus_counts <- milvus_counts %>%
  filter(sum_months == 25 & n_counts >= 450)
milvus <- milvus[milvus$year_id %in% milvus_counts$year_id,]
# (initially 364 year_id & 132 bird_id, 590101 locations)
# (after     224 year_id & 102 bird_id, 484388 locations)

# removing days with less than three locations
milvus_counts_daily <- milvus %>%
  group_by(bird_id, date) %>%
  summarise(n_counts = n(),
            date_id = first(date_id))
milvus_counts_daily <- milvus_counts_daily %>%
  filter(n_counts < 3)
milvus <- milvus[!(milvus$date_id %in% milvus_counts_daily$date_id),]
# (initially 224 year_id & 102 bird_id, 484388 locations)
# (after     224 year_id & 102 bird_id, 484388 locations)



# ------------------------------- CREATING TRACK ------------------------------- 
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




# MCP ------------------------------- 1.75 hours -------------------------------



start_time_MCP <- Sys.time() # --------------------------------------------------------------------- DELETE!!!



# creating a track for minimum convex polygon with year_id & event_id
milvus_track_mcp <- milvus %>%
  mk_track(
    .x = long_eea,
    .y = lat_eea,
    .t = timestamp,
    id = date_id,
    year_id,
    event_id,
    crs = 3035
  )

# creating a new track for daily mcp calculations without year_id
milvus_track_mcp_daily <- milvus_track_mcp %>%
  select(-year_id)

# 95% daily
mcp_95 <- milvus_track_mcp_daily %>%
  nest(data = -id) %>%
  mutate(mcp_area = map(data, ~ hr_mcp(., levels = c(0.95)) %>% hr_area)) %>%
  dplyr::select(id, mcp_area) %>%
  unnest(cols = mcp_area) %>%
  mutate(date_id = id,
         mcp_area_95 = area / 1000000) %>%
  select(-id, -level, -what, -area)

# Joining area information to milvus data set
milvus <- left_join(milvus, mcp_95, by = "date_id")



# 50% daily
mcp_50 <- milvus_track_mcp_daily %>%
  nest(data = -id) %>%
  mutate(mcp_area = map(data, ~ hr_mcp(., levels = c(0.50)) %>% hr_area)) %>%
  dplyr::select(id, mcp_area) %>%
  unnest(cols = mcp_area) %>%
  mutate(date_id = id,
         mcp_area_50 = area / 1000000) %>%
  select(-id, -level, -what, -area)

# Joining area information to milvus data set
milvus <- left_join(milvus, mcp_50, by = "date_id")



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
    print(paste0("year_id: ", match(i, unique(milvus_track_mcp$year_id)),
                 " of ", length(unique(milvus_track_mcp$year_id)),
                 " - date_id: ", j, " of ",
                 length(unique(milvus_track_individual$id)),
                 " - total progress: ",
                 round((l+j)*100/length(unique(milvus_track_mcp$id)), 2), "%"))
  }
  l <- l + j
}

# preparing track for joining
mcp_95_7day <- milvus_track_mcp %>%
  select(event_id, mcp_area_95_7day)

# Joining area information to milvus data set
milvus <- left_join(milvus, mcp_95_7day, by = "event_id")



# 50% 7 days
milvus_track_mcp$mcp_area_50_7day <- NA
l <- 0
for (i in unique(milvus_track_mcp$year_id)) {
  milvus_track_individual <- milvus_track_mcp[milvus_track_mcp$year_id == i ,]
  for (j in 7:length(unique(milvus_track_individual$id))) {
    k <- unique(milvus_track_individual$id)[(j-6):j]
    milvus_track_subset <- milvus_track_individual[milvus_track_individual$id %in% k, ]
    mcp_area <- hr_mcp(milvus_track_subset, levels = c(0.50)) %>% hr_area()
    milvus_track_mcp[milvus_track_mcp$year_id == i &
                       milvus_track_mcp$id == unique(milvus_track_individual$id)[j]
                     ,]$mcp_area_50_7day <- mcp_area$area / 1000000
    # this last chunk is for progress report, since function takes a lot of time
    print(paste0("year_id: ", match(i, unique(milvus_track_mcp$year_id)),
                 " of ", length(unique(milvus_track_mcp$year_id)),
                 " - date_id: ", j, " of ",
                 length(unique(milvus_track_individual$id)),
                 " - total progress: ",
                 round((l+j)*100/length(unique(milvus_track_mcp$id)), 2), "%"))
  }
  l <- l + j
}

# preparing track for joining
mcp_50_7day <- milvus_track_mcp %>%
  select(event_id, mcp_area_50_7day)

# Joining area information to milvus data set
milvus <- left_join(milvus, mcp_50_7day, by = "event_id")



# 95% 5 days
milvus_track_mcp$mcp_area_95_5day <- NA
l <- 0
for (i in unique(milvus_track_mcp$year_id)) {
  milvus_track_individual <- milvus_track_mcp[milvus_track_mcp$year_id == i ,]
  for (j in 5:length(unique(milvus_track_individual$id))) {
    k <- unique(milvus_track_individual$id)[(j-4):j]
    milvus_track_subset <- milvus_track_individual[milvus_track_individual$id %in% k, ]
    mcp_area <- hr_mcp(milvus_track_subset, levels = c(0.95)) %>% hr_area()
    milvus_track_mcp[milvus_track_mcp$year_id == i &
                       milvus_track_mcp$id == unique(milvus_track_individual$id)[j]
                     ,]$mcp_area_95_5day <- mcp_area$area / 1000000
    # this last chunk is for progress report, since function takes a lot of time
    print(paste0("year_id: ", match(i, unique(milvus_track_mcp$year_id)),
                 " of ", length(unique(milvus_track_mcp$year_id)),
                 " - date_id: ", j, " of ",
                 length(unique(milvus_track_individual$id)),
                 " - total progress: ",
                 round((l+j)*100/length(unique(milvus_track_mcp$id)), 2), "%"))
  }
  l <- l + j
}

# preparing track for joining
mcp_95_5day <- milvus_track_mcp %>%
  select(event_id, mcp_area_95_5day)

# Joining area information to milvus data set
milvus <- left_join(milvus, mcp_95_5day, by = "event_id")



# 50% 5 days
milvus_track_mcp$mcp_area_50_5day <- NA
l <- 0
for (i in unique(milvus_track_mcp$year_id)) {
  milvus_track_individual <- milvus_track_mcp[milvus_track_mcp$year_id == i ,]
  for (j in 5:length(unique(milvus_track_individual$id))) {
    k <- unique(milvus_track_individual$id)[(j-4):j]
    milvus_track_subset <- milvus_track_individual[milvus_track_individual$id %in% k, ]
    mcp_area <- hr_mcp(milvus_track_subset, levels = c(0.50)) %>% hr_area()
    milvus_track_mcp[milvus_track_mcp$year_id == i &
                       milvus_track_mcp$id == unique(milvus_track_individual$id)[j]
                     ,]$mcp_area_50_5day <- mcp_area$area / 1000000
    # this last chunk is for progress report, since function takes a lot of time
    print(paste0("year_id: ", match(i, unique(milvus_track_mcp$year_id)),
                 " of ", length(unique(milvus_track_mcp$year_id)),
                 " - date_id: ", j, " of ",
                 length(unique(milvus_track_individual$id)),
                 " - total progress: ",
                 round((l+j)*100/length(unique(milvus_track_mcp$id)), 2), "%"))
  }
  l <- l + j
}

# preparing track for joining
mcp_50_5day <- milvus_track_mcp %>%
  select(event_id, mcp_area_50_5day)

# Joining area information to milvus data set
milvus <- left_join(milvus, mcp_50_5day, by = "event_id")



# 95% 3 days
milvus_track_mcp$mcp_area_95_3day <- NA
l <- 0
for (i in unique(milvus_track_mcp$year_id)) {
  milvus_track_individual <- milvus_track_mcp[milvus_track_mcp$year_id == i ,]
  for (j in 3:length(unique(milvus_track_individual$id))) {
    k <- unique(milvus_track_individual$id)[(j-2):j]
    milvus_track_subset <- milvus_track_individual[milvus_track_individual$id %in% k, ]
    mcp_area <- hr_mcp(milvus_track_subset, levels = c(0.95)) %>% hr_area()
    milvus_track_mcp[milvus_track_mcp$year_id == i &
                       milvus_track_mcp$id == unique(milvus_track_individual$id)[j]
                     ,]$mcp_area_95_3day <- mcp_area$area / 1000000
    # this last chunk is for progress report, since function takes a lot of time
    print(paste0("year_id: ", match(i, unique(milvus_track_mcp$year_id)),
                 " of ", length(unique(milvus_track_mcp$year_id)),
                 " - date_id: ", j, " of ",
                 length(unique(milvus_track_individual$id)),
                 " - total progress: ",
                 round((l+j)*100/length(unique(milvus_track_mcp$id)), 2), "%"))
  }
  l <- l + j
}

# preparing track for joining
mcp_95_3day <- milvus_track_mcp %>%
  select(event_id, mcp_area_95_3day)

# Joining area information to milvus data set
milvus <- left_join(milvus, mcp_95_3day, by = "event_id")



# 50% 3 days
milvus_track_mcp$mcp_area_50_3day <- NA
l <- 0
for (i in unique(milvus_track_mcp$year_id)) {
  milvus_track_individual <- milvus_track_mcp[milvus_track_mcp$year_id == i ,]
  for (j in 3:length(unique(milvus_track_individual$id))) {
    k <- unique(milvus_track_individual$id)[(j-2):j]
    milvus_track_subset <- milvus_track_individual[milvus_track_individual$id %in% k, ]
    mcp_area <- hr_mcp(milvus_track_subset, levels = c(0.50)) %>% hr_area()
    milvus_track_mcp[milvus_track_mcp$year_id == i &
                       milvus_track_mcp$id == unique(milvus_track_individual$id)[j]
                     ,]$mcp_area_50_3day <- mcp_area$area / 1000000
    # this last chunk is for progress report, since function takes a lot of time
    print(paste0("year_id: ", match(i, unique(milvus_track_mcp$year_id)),
                 " of ", length(unique(milvus_track_mcp$year_id)),
                 " - date_id: ", j, " of ",
                 length(unique(milvus_track_individual$id)),
                 " - total progress: ",
                 round((l+j)*100/length(unique(milvus_track_mcp$id)), 2), "%"))
  }
  l <- l + j
}

# preparing track for joining
mcp_50_3day <- milvus_track_mcp %>%
  select(event_id, mcp_area_50_3day)

# Joining area information to milvus data set
milvus <- left_join(milvus, mcp_50_3day, by = "event_id")



# 95% moving window (mean over seven days)
milvus$mcp_area_95_mw_7day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 7:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-6):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(mcp_area_95 = first(mcp_area_95))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$mcp_area_95_mw_7day <- 
      mean(l$mcp_area_95, na.rm = T)
  }
}



# 95% moving window (mean over five days)
milvus$mcp_area_95_mw_5day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 5:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-4):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(mcp_area_95 = first(mcp_area_95))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$mcp_area_95_mw_5day <- 
      mean(l$mcp_area_95, na.rm = T)
  }
}



# 95% moving window (mean over three days)
milvus$mcp_area_95_mw_3day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 3:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-2):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(mcp_area_95 = first(mcp_area_95))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$mcp_area_95_mw_3day <- 
      mean(l$mcp_area_95, na.rm = T)
  }
}



# 50% moving window (mean over seven days)
milvus$mcp_area_50_mw_7day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 7:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-6):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(mcp_area_50 = first(mcp_area_50))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$mcp_area_50_mw_7day <- 
      mean(l$mcp_area_50, na.rm = T)
  }
}



# 50% moving window (mean over five days)
milvus$mcp_area_50_mw_5day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 5:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-4):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(mcp_area_50 = first(mcp_area_50))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$mcp_area_50_mw_5day <- 
      mean(l$mcp_area_50, na.rm = T)
  }
}



# 50% moving window (mean over three days)
milvus$mcp_area_50_mw_3day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 3:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-2):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(mcp_area_50 = first(mcp_area_50))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$mcp_area_50_mw_3day <- 
      mean(l$mcp_area_50, na.rm = T)
  }
}



# remove data before 10th of March,
# they were only necessary for moving window calculations
milvus_temp <- milvus %>%
  filter(!(month == 3 & day < 10))

# remove NAs that occur in some MCP calculations
milvus_temp <- na.omit(milvus_temp)

# save file
# creating directory
if (!dir.exists(here("data/modified"))) {
  dir.create("data/modified")
}
if (!dir.exists(here("data/modified/03_multinomial"))) {
  dir.create("data/modified/03_multinomial")
}
write.csv(milvus_temp, here("data/modified/03_multinomial/01_milvus_parameters.csv"),
          row.names = F)



end_time_MCP <- Sys.time() # --------------------------------------------------------------------- DELETE!!!
end_time_MCP - start_time_MCP



# ----------------------------- RECURSIONS TO NEST ----------------------------- 

start_time_recurse <- Sys.time() # --------------------------------------------------------------------- DELETE!!!

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

# find the time/amount for each date_id
residence_time_nest <- c()
revisits_nest <- c()
for (i in 1:length(milvus_revisits_nest)) {
  residence_time_nest[i] <- as.integer(round(first(milvus_revisits_nest[[i]]$residenceTime)))
  revisits_nest[i] <- first(milvus_revisits_nest[[i]]$revisits)
}

# combine the IDs with the max time data from the recursion analysis
milvus_recurse_nest <- cbind(data.frame(date_id = names(milvus_track_recurse_nest_list),
                                        residence_time_nest, revisits_nest))

# join nest recurse information to milvus data set
milvus <- left_join(milvus, milvus_recurse_nest, by = "date_id")



# residence time nest (mean over seven days)
milvus$residence_time_nest_mw_7day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 7:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-6):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(residence_time_nest = first(residence_time_nest))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$residence_time_nest_mw_7day <- 
      mean(l$residence_time_nest, na.rm = T)
  }
}

# residence time nest (mean over five days)
milvus$residence_time_nest_mw_5day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 5:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-4):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(residence_time_nest = first(residence_time_nest))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$residence_time_nest_mw_5day <- 
      mean(l$residence_time_nest, na.rm = T)
  }
}

# residence time nest (mean over three days)
milvus$residence_time_nest_mw_3day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 3:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-2):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(residence_time_nest = first(residence_time_nest))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$residence_time_nest_mw_3day <- 
      mean(l$residence_time_nest, na.rm = T)
  }
}

# revisits nest (mean over seven days)
milvus$revisits_nest_mw_7day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 7:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-6):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(revisits_nest = first(revisits_nest))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$revisits_nest_mw_7day <- 
      mean(l$revisits_nest, na.rm = T)
  }
}

# revisits nest (mean over five days)
milvus$revisits_nest_mw_5day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 5:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-4):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(revisits_nest = first(revisits_nest))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$revisits_nest_mw_5day <- 
      mean(l$revisits_nest, na.rm = T)
  }
}

# revisits nest (mean over three days)
milvus$revisits_nest_mw_3day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 3:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-2):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(revisits_nest = first(revisits_nest))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$revisits_nest_mw_3day <- 
      mean(l$revisits_nest, na.rm = T)
  }
}



# remove data before 10th of March,
# they were only necessary for moving window calculations
milvus_temp <- milvus %>%
  filter(!(month == 3 & day < 10))

# remove NAs that occur in some recurse calculations
milvus_temp <- na.omit(milvus_temp)

# save file
write.csv(milvus_temp, here("data/modified/03_multinomial/02_milvus_parameters.csv"),
          row.names = F)


end_time_recurse <- Sys.time() # --------------------------------------------------------------------- DELETE!!!
end_time_recurse - start_time_recurse



# -------------------------------- STEP LENGTH ---------------------------------

start_time_sl <- Sys.time() # --------------------------------------------------------------------- DELETE!!!


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
    sl_mean = mean(sl_)
  )

# join step length information to milvus data set
milvus <- left_join(milvus, milvus_track_sl, by = c("date_id" = "burst_"))


# mean step length (mean over seven days)
milvus$sl_mean_mw_7day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 7:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-6):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(sl_mean = first(sl_mean))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$sl_mean_mw_7day <- 
      mean(l$sl_mean, na.rm = T)
  }
}

# mean step length (mean over five days)
milvus$sl_mean_mw_5day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 5:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-4):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(sl_mean = first(sl_mean))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$sl_mean_mw_5day <- 
      mean(l$sl_mean, na.rm = T)
  }
}

# mean step length (mean over three days)
milvus$sl_mean_mw_3day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 3:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-2):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(sl_mean = first(sl_mean))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$sl_mean_mw_3day <- 
      mean(l$sl_mean, na.rm = T)
  }
}


end_time_sl <- Sys.time() # --------------------------------------------------------------------- DELETE!!!
end_time_sl - start_time_sl








# -------------------------------- TEMPERATURE ---------------------------------

start_time_temp <- Sys.time() # --------------------------------------------------------------------- DELETE!!!


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

# mean temperature (mean over seven days)
milvus$t_mean_mw_7day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 7:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-6):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(t_mean = first(t_mean))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$t_mean_mw_7day <- 
      mean(l$t_mean, na.rm = T)
  }
}

# mean temperature (mean over five days)
milvus$t_mean_mw_5day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 5:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-4):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(t_mean = first(t_mean))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$t_mean_mw_5day <- 
      mean(l$t_mean, na.rm = T)
  }
}

# mean temperature (mean over three days)
milvus$t_mean_mw_3day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 3:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-2):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(t_mean = first(t_mean))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$t_mean_mw_3day <- 
      mean(l$t_mean, na.rm = T)
  }
}

# temperature variance (mean over seven days)
milvus$t_var_mw_7day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 7:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-6):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(t_var = first(t_var))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$t_var_mw_7day <- 
      mean(l$t_var, na.rm = T)
  }
}

# temperature variance (mean over five days)
milvus$t_var_mw_5day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 5:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-4):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(t_var = first(t_var))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$t_var_mw_5day <- 
      mean(l$t_var, na.rm = T)
  }
}

# temperature variance (mean over three days)
milvus$t_var_mw_3day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 3:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-2):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(t_var = first(t_var))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$t_var_mw_3day <- 
      mean(l$t_var, na.rm = T)
  }
}

end_time_temp <- Sys.time() # --------------------------------------------------------------------- DELETE!!!
end_time_temp - start_time_temp








# DISTANCE TO NEST -------------------------- 5 hours --------------------------

start_time_nest_dist <- Sys.time() # --------------------------------------------------------------------- DELETE!!!


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

# This loop takes about ---> 5 hours <---
for (i in 1:nrow(milvus_sf)) {
  milvus_sf[i ,]$nest_dist <- st_distance(milvus_sf[i ,]$geometry,
                                          nest_sf[nest_sf$nest_id == milvus_sf[i ,]$nest_id ,]$geometry)
  if (i%%50 == 0) {
    cat(paste0(i, " of ", nrow(milvus_sf), " --> ", round(i*100/nrow(milvus_sf),1), "%\n")) 
  }
}

# drop geometry to save file as in csv format
milvus_nest_dist <- milvus_sf %>%
  st_drop_geometry()

# save file
write.csv(milvus_nest_dist, here("data/modified/03_multinomial/03_milvus_nest_dist.csv"),
          row.names = F)

# calculate some statistical measures on a daily basis
milvus_nest_dist <- milvus_nest_dist %>%
  group_by(date_id) %>%
  summarise(
    nest_dist_max = max(nest_dist),
    nest_dist_min = min(nest_dist),
    nest_dist_mean = mean(nest_dist)
  ) %>%
  select(date_id, nest_dist_max, nest_dist_min, nest_dist_mean)

# join nest distance information to milvus data set
milvus <- left_join(milvus, milvus_nest_dist, by = "date_id")

# nest distance mean (mean over seven days)
milvus$nest_dist_mean_mw_7day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 7:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-6):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(nest_dist_mean = first(nest_dist_mean))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$nest_dist_mean_mw_7day <- 
      mean(l$nest_dist_mean, na.rm = T)
  }
}

# nest distance mean (mean over five days)
milvus$nest_dist_mean_mw_5day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 5:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-4):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(nest_dist_mean = first(nest_dist_mean))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$nest_dist_mean_mw_5day <- 
      mean(l$nest_dist_mean, na.rm = T)
  }
}

# nest distance mean (mean over three days)
milvus$nest_dist_mean_mw_3day <- NA
for (i in unique(milvus$year_id)) {
  milvus_individual <- milvus[milvus$year_id == i ,]
  for (j in 3:length(unique(milvus_individual$date_id))) {
    k <- unique(milvus_individual$date_id)[(j-2):j]
    l <- milvus_individual[milvus_individual$date_id %in% k ,] %>%
      group_by(date_id) %>%
      summarise(nest_dist_mean = first(nest_dist_mean))
    milvus[milvus$year_id == i &
             milvus$date_id == unique(milvus_individual$date_id)[j]
           ,]$nest_dist_mean_mw_3day <- 
      mean(l$nest_dist_mean, na.rm = T)
  }
}









# remove data before 10th of March,
# they were only necessary for moving window calculations
milvus <- milvus %>%
  filter(!(month == 3 & day < 10))

# remove NAs that occur in some recurse calculations
milvus <- na.omit(milvus)

# save file
write.csv(milvus, here("data/modified/03_multinomial/04_milvus_parameters.csv"),
          row.names = F)


end_time_nest_dist <- Sys.time() # --------------------------------------------------------------------- DELETE!!!
end_time_nest_dist - start_time_nest_dist











# ---------------------- FINAL ORGANISATION OF DATA FRAME ----------------------
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
    mcp_area_95 = first(mcp_area_95),
    mcp_area_50 = first(mcp_area_50),
    mcp_area_95_7day = first(mcp_area_95_7day),
    mcp_area_50_7day = first(mcp_area_50_7day),
    mcp_area_95_5day = first(mcp_area_95_5day),
    mcp_area_50_5day = first(mcp_area_50_5day),
    mcp_area_95_3day = first(mcp_area_95_3day),
    mcp_area_50_3day = first(mcp_area_50_3day),
    mcp_area_95_mw_7day = first(mcp_area_95_mw_7day),
    mcp_area_95_mw_5day = first(mcp_area_95_mw_5day),
    mcp_area_95_mw_3day = first(mcp_area_95_mw_3day),
    mcp_area_50_mw_7day = first(mcp_area_50_mw_7day),
    mcp_area_50_mw_5day = first(mcp_area_50_mw_5day),
    mcp_area_50_mw_3day = first(mcp_area_50_mw_3day),
    residence_time_nest = first(residence_time_nest),
    revisits_nest = first(revisits_nest),
    residence_time_nest_mw_7day = first(residence_time_nest_mw_7day),
    residence_time_nest_mw_5day = first(residence_time_nest_mw_5day),
    residence_time_nest_mw_3day = first(residence_time_nest_mw_3day),
    revisits_nest_mw_7day = first(revisits_nest_mw_7day),
    revisits_nest_mw_5day = first(revisits_nest_mw_5day),
    revisits_nest_mw_3day = first(revisits_nest_mw_3day),
    sl_max = first(sl_max),
    sl_min = first(sl_min),
    sl_mean = first(sl_mean),
    sl_mean_mw_7day = first(sl_mean_mw_7day),
    sl_mean_mw_5day = first(sl_mean_mw_5day),
    sl_mean_mw_3day = first(sl_mean_mw_3day),
    t_max = first(t_max),
    t_min = first(t_min),
    t_mean = first(t_mean),
    t_var = first(t_var),
    t_mean_mw_7day = first(t_mean_mw_7day),
    t_mean_mw_5day = first(t_mean_mw_5day),
    t_mean_mw_3day = first(t_mean_mw_3day),
    t_var_mw_7day = first(t_var_mw_7day),
    t_var_mw_5day = first(t_var_mw_5day),
    t_var_mw_3day = first(t_var_mw_3day),
    nest_dist_max = first(nest_dist_max),
    nest_dist_min = first(nest_dist_min),
    nest_dist_mean = first(nest_dist_mean),
    nest_dist_mean_mw_7day = first(nest_dist_mean_mw_7day),
    nest_dist_mean_mw_5day = first(nest_dist_mean_mw_5day),
    nest_dist_mean_mw_3day = first(nest_dist_mean_mw_3day),
    locations_per_day = n()
    )

# save file
write.csv(milvus_daily, here("data/modified/03_multinomial/05_milvus_parameters_daily.csv"),
          row.names = F)



# -------------------------- ADDING GROUND TRUTH DATA --------------------------
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
  milvus_daily[milvus_daily$year_id == ground_truth[i,]$year_id &
                 milvus_daily$date >= ground_truth[i,]$egg_laying_date &
                 milvus_daily$date < ground_truth[i,]$hatching_date
               ,]$breeding_status <- "incubating"
  milvus_daily[milvus_daily$year_id == ground_truth[i,]$year_id &
                 milvus_daily$date >= ground_truth[i,]$hatching_date &
                 milvus_daily$date <= ground_truth[i,]$empty_date
               ,]$breeding_status <- "feeding"
}

# make ground truth information ready to join
ground_truth <- ground_truth %>%
  select(year_id, sex, age_cy, nest_elevation)

# join ground truth information to milvus data set
milvus_daily <- left_join(milvus_daily, ground_truth, by = "year_id")

# save file
write.csv(milvus_daily, here("data/modified/03_multinomial/06_milvus_parameters_daily_ground_truth.csv"),
          row.names = F)












library(ggplot2)
library(ggpubr)
milvus_daily_new <- read.csv(here("data/modified/03_multinomial/06_milvus_parameters_daily_ground_truth.csv"))

milvus_daily_new$breeding_status <- factor(milvus_daily_new$breeding_status, levels = c("nonbreeding", "incubating", "feeding"))

for (i in unique(milvus_daily_new$year_id)) {
  assign(paste0("plot_", i),
         ggplot(data = milvus_daily_new[milvus_daily_new$year_id == i,],
                aes(x = as.Date(date))) +
           geom_rect(aes(ymin = 0, ymax = 1, xmin = as.Date(date) - .5, xmax = as.Date(date) + .5,
                         fill = breeding_status), alpha = .6) +
           scale_fill_manual(values = c("nonbreeding" = "blue",
                                        "incubating" = "red",
                                        "feeding" = "darkgreen")) +
           labs(x = element_blank(), title = paste0("Year ID: ", i), fill = "Breeding status") +
           theme(plot.title = element_text(hjust = 0.5),
                 panel.background = element_blank(),
                 panel.grid.major = element_line(colour = "gray70"),
                 panel.grid.minor = element_line(colour = "gray90"))
  )
}

# arranging plot
plot <- ggarrange(plot_2017_10, plot_2017_120, plot_2017_186, plot_2017_2,
                  plot_2018_140, plot_2018_443, plot_2018_454, plot_2018_456,
                  plot_2018_462, plot_2018_11, plot_2018_446, plot_2018_449,
                  plot_2018_463, plot_2018_440, plot_2018_448, plot_2018_452,
                  plot_2018_451, plot_2018_461, plot_2018_458, plot_2019_116,
                  plot_2019_161, plot_2019_476, plot_2019_480, plot_2019_526,
                  plot_2019_462, plot_2019_451, plot_2019_455, plot_2019_461,
                  plot_2020_448, plot_2020_456, plot_2020_524, plot_2020_451,
                  plot_2020_440, plot_2022_747, plot_2022_458,
                  ncol = 4, nrow = 9)

# adding main title and y axis title
annotate_figure(plot, top = text_grob(paste0("Ground Truth Data"), size = 36))

# save plot
ggsave(("plots/ground_truth/gt_all_birds.pdf"),
       width = 8000, height = 12000, units = "px", dpi = 320, limitsize = FALSE)



