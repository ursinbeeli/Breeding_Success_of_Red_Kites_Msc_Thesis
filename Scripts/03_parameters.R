# LOADING PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)
library(sf)
library(amt)
library(recurse)
library(ggplot2)



# Running this script took around 5 hours with a late 2016 MacBook Pro with 16GB Ram



# LOADING DATA -----------------------------------------------------------------
# Movement data
milvus <- read.csv(here("../Data/Output/02_preprocessed_data/milvus.csv"))
# Validation information
milvus_validation <-
  read.csv(here("../Data/Output/01_validation_data/milvus_home_range_and_nest.csv")) %>%
  select(year_id, sex, home_range_id, nest_id)



# DATA PREPARATION -------------------------------------------------------------
# Formatting timestamp and date
milvus <- milvus %>%
  mutate(timestamp = as.POSIXct(timestamp, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"),
         date = as.Date(date, tz = "UTC"))
# Removing entries with NAs in timestamp column (happens during formatting since some entries have only date in timestamp)
milvus <- milvus[!is.na(milvus$timestamp),]

# Creating a daily data frame to add calculated daily parameters later
milvus_daily <- milvus %>%
  group_by(date_id) %>%
  summarise(bird_id = first(bird_id),
            year_id = first(year_id),
            date = first(date),
            year = first(year),
            month = first(month),
            day = first(day),
            external_temperature = first(external_temperature),
            week = first(week),
            year_week = first(year_week),
            year_week_id = first(year_week_id)) %>%
  arrange(year_id, date)

# Adding validation information to daily movement data frame
milvus_daily <- left_join(milvus_daily, milvus_validation, by = "year_id")
milvus_daily$home_range <- as.character(milvus_daily$home_range_id)
milvus_daily[milvus_daily$home_range != "0" ,]$home_range <- "home range"
milvus_daily[milvus_daily$home_range == "0" ,]$home_range <- "no home range"
milvus_daily$nest <- as.character(milvus_daily$nest_id)
milvus_daily[milvus_daily$nest != "0" ,]$nest <- "nest"
milvus_daily[milvus_daily$nest == "0" ,]$nest <- "no nest"

# Creating a track (for calculations with amt package)
milvus_track <- milvus %>%
  mk_track(
    .x = long_eea,
    .y = lat_eea,
    .t = timestamp,
    id = year_id,
    date = date,
    date_id = date_id,
    crs = 3035
  )

# Ordering track information by id and timestamp
milvus_track <- milvus_track %>%
  arrange(id, t_)



# CALCULATING 7 DAY MCP AND ITS CENTROID ---------------------------------------
# Adding columns to fill with relevant information
milvus_daily <- milvus_daily %>%
  mutate(mcp_area_95_7day = NA,
         centroid_long = NA,
         centroid_lat = NA)
for (i in unique(milvus_track$id)) {
  # Creating an individual data frame for every year_id
  milvus_individual <- milvus_track[milvus_track$id == i ,]
  for (j in 1:length(unique(milvus_individual$date))) {
    # Creating a data frame for every 7-day sequence
    date_ind <- unique(milvus_individual$date)[j]
    date_seq <- seq(date_ind-6, date_ind, by = "days")
    milvus_ind_date_seq <- milvus_individual[milvus_individual$date %in% date_seq ,]
    # Calculating an MCP for every 7-day sequence
    mcp_95 <- hr_mcp(milvus_ind_date_seq, levels = c(0.95))
    # Calculating the centroid of the MCP
    mcp_95_centroid <- centroid(mcp_95$data) %>% t() %>% as.data.frame()
    # Assigning the MCP area to original data frame
    milvus_daily[milvus_daily$year_id == i &
                   milvus_daily$date == date_ind ,]$mcp_area_95_7day <-
      hr_area(mcp_95)$area / 1000000
    # Assigning the MCP centroid longitude to original data frame
    milvus_daily[milvus_daily$year_id == i &
                   milvus_daily$date == date_ind ,]$centroid_long <-
      mcp_95_centroid$x_
    # Assigning the MCP centroid latitude to original data frame
    milvus_daily[milvus_daily$year_id == i &
                   milvus_daily$date == date_ind ,]$centroid_lat <-
      mcp_95_centroid$y_
  }
}

# Removing the individuals with only one location per year
milvus_year_id_one_day_only <- milvus_daily %>%
  group_by(year_id) %>%
  summarise(n_days = n()) %>%
  filter(n_days == 1)
milvus_daily <- milvus_daily[!milvus_daily$year_id %in% milvus_year_id_one_day_only$year_id ,]



# SAVING DATA ------------------------------------------------------------------
# Creating directory
if (!dir.exists(here("../Data/Output/03_parameters"))) {
  if (!dir.exists(here("../Data/Output"))) {
    dir.create("../Data/Output")
  }
  dir.create("../Data/Output/03_parameters")
}

write.csv(milvus_daily,
          here("../Data/Output/03_parameters/milvus_daily_7day_mcp_centroid.csv"),
          row.names = F)


