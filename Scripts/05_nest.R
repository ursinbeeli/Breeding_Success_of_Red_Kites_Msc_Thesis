# LOADING PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)
library(sf)
library(amt)
library(recurse)
library(ggplot2)



# LOADING DATA -----------------------------------------------------------------
# Movement data
milvus <- read.csv(here("../Data/Output/02_preprocessed_data/milvus.csv"))
# Home range analysis results
milvus_hr <- read.csv(here("../Data/Output/04_home_range/milvus_home_range.csv"))
# Validation information
milvus_validation <- read.csv(here("../Data/Output/01_validation_data/milvus_home_range_and_nest.csv"))
# Nest list
nest_list <- read.csv(here("../Data/Basic_nest_list_2015_2022.csv"))



# DATA PREPARATION -------------------------------------------------------------
# Formatting timestamp and date
milvus <- milvus %>%
  mutate(timestamp = as.POSIXct(timestamp, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"),
         date = as.Date(date, tz = "UTC"))

# Removing entries with NAs in timestamp column (happens during formatting since some entries have only date in timestamp)
milvus <- milvus[!is.na(milvus$timestamp),]

# Keeping only birds which could have a home range regarding previous analysis
milvus <- milvus %>%
  filter(year_id %in% milvus_hr$year_id)

# Keeping only the information of relevant months
milvus <- milvus %>%
  filter(month %in% c(3,4,5,6))

# Keeping only the data from the first settling week onward
milvus_hr_join <- milvus_hr %>%
  select(year_id, first_settling_week, nest)
milvus <- left_join(milvus, milvus_hr_join, by = "year_id")
milvus <- milvus %>%
  nest(data = -year_id) %>%
  mutate(data = map(data, ~ filter(., week >= first_settling_week))
  ) %>%
  unnest(cols = data)

# Creating a track
milvus_track <- milvus %>%
  mk_track(
    .x = long_eea,
    .y = lat_eea,
    .t = timestamp,
    id = year_id,
    date_id = date_id,
    crs = 3035
  ) %>%
  time_of_day(include.crepuscule = T)

# Excluding night points (could affect determination of nest location due to possible different sleeping location in the beginning of nest building phase)
milvus_track <- milvus_track %>%
  filter(tod_ != "night") %>%
  select(-tod_) %>%
  arrange(id, t_)









# RECURSE ----------------------------------------------------------------------
# Splitting track into a list with each single id grouped to an element
milvus_track <- as.data.frame(milvus_track)
milvus_track_list <- split(milvus_track, milvus_track$id)

# Calculating recursions
milvus_recurse <- lapply(milvus_track_list, function(x)
  getRecursions(x = x[1:4], radius = 50, timeunits = "hours"))

# Adding recurse information to data frame
milvus_track$revisits <- NA
milvus_track$residence_time <- NA
for (i in 1:length(milvus_recurse)) {
  milvus_track[milvus_track$id == unique(milvus_track$id)[i] ,]$revisits <-
    milvus_recurse[[i]]$revisits
  milvus_track[milvus_track$id == unique(milvus_track$id)[i] ,]$residence_time <-
    milvus_recurse[[i]]$residenceTime
}

# Adding home range and nest information to track
milvus_hr_join <- milvus_hr %>%
  select(year_id, home_range, nest)
milvus_track <- left_join(milvus_track, milvus_hr_join, by = c("id" = "year_id"))
# Adding sex and nest_id to track
milvus_validation <- milvus_validation %>%
  select(year_id, sex, nest_id)
milvus_track <- left_join(milvus_track, milvus_validation, by = c("id" = "year_id"))

# Keeping only birds with at least 10 revisits at most revisited location
milvus_most_revisited <- milvus_track %>%
  mutate(date = as.Date(t_, tz ="UTC")) %>%
  group_by(id) %>%
  summarise(revisits = max(revisits),
            home_range = first(home_range),
            nest = first(nest),
            nest_id = first(nest_id),
            sex = first(sex),
            n_locations = n(),
            n_days = n_distinct(date)) %>%
  filter(revisits >= 10)

milvus_with_potential_nest <- milvus_track[milvus_track$id %in% milvus_most_revisited$id ,]

# Creating an sf object to calculate geometric stats on locations with highest recursions
milvus_track_sf <- st_as_sf(milvus_with_potential_nest,
                            coords = c("x_", "y_"),
                            crs = 3035)









# CHANGE POINT DETECTION OF N MOST VISITED LOCATIONS ---------------------------
# Creating directory for plots
if (!dir.exists(here("../Data/Output/Plots/05_nest"))) {
  if (!dir.exists(here("../Data/Output"))) {
    dir.create("../Data/Output")
  }
  if (!dir.exists(here("../Data/Output/Plots"))) {
    dir.create("../Data/Output/Plots")
  }
  dir.create("../Data/Output/Plots/05_nest")
}
for (i in c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) {
  n_most_visited <- i
  milvus_track_sf_top_n <- milvus_track_sf %>%
    arrange(id, desc(revisits)) %>%
    group_by(id) %>%
    slice_head(n = n_most_visited)
  
  # Calculating the area of the top n most visited places
  milvus_track_sf_top_n <- milvus_track_sf_top_n %>%
    group_by(id) %>%
    summarise(geometry = st_cast(st_combine(geometry), to = "POLYGON"),
              sex = first(sex),
              nest = first(nest),
              nest_id = first(nest_id)) %>%
    mutate(area = as.numeric(st_area(geometry))/1000000)
  
  assign(paste0("milvus_track_sf_top_", i), milvus_track_sf_top_n)
  
  birds_with_nest <- milvus_track_sf_top_n[milvus_track_sf_top_n$nest == "nest" ,]
  birds_without_nest <- milvus_track_sf_top_n[milvus_track_sf_top_n$nest == "no nest" ,]
  
  birds_with_nest <- birds_with_nest %>%
    arrange(desc(area)) %>%
    mutate(n_birds = 0:(nrow(birds_with_nest)-1)) %>%
    mutate(n_birds = n_birds/nrow(birds_with_nest)*100)
  
  birds_without_nest <- birds_without_nest %>%
    arrange(desc(area)) %>%
    mutate(n_birds = 0:(nrow(birds_without_nest)-1)) %>%
    mutate(n_birds = n_birds/nrow(birds_without_nest)*100)
  
  first_bird_with_nest <- max(birds_with_nest$area)
  
  x_lab <- expression(Area ~ paste("[") ~km^2 ~ paste("]"))
  line_lab <- bquote(Area ~ paste("=") ~ .(round(first_bird_with_nest, 3)) ~ km^2)
  ggplot() +
    geom_line(birds_with_nest, mapping = aes(x = area, y = n_birds), color = "darkblue") +
    geom_line(birds_without_nest, mapping = aes(x = area, y = n_birds), color = "lightblue") +
    geom_line(mapping = aes(x = first_bird_with_nest, y = 1:100), color = "red", linetype = "dashed") +
    annotate("text", x = first_bird_with_nest + .002, y = 50,
             label = line_lab, angle = 90, color = "red") +
    labs(x = x_lab, y = "Individuals [%]") +
    theme(panel.background = element_blank(),
          panel.grid = element_line(colour = "gray95"),
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(trans = "reverse") + 
    coord_cartesian(xlim=c(.1, 0))
  
  # Saving plots
  ggsave(here(paste0("../Data/Output/Plots/05_nest/01_area_", get("n_most_visited"), "_most_visited.pdf")),
         width = 3000, height = 1500, units = "px", dpi = 300)
}

# Resulting threshold value: 45 locations, 0.052 km^2
n_most_visited <- 45

# Keeping only birds with an area < 0.052 km^2 of the 45 most visited locations
milvus_small_area <- milvus_track_sf_top_45 %>%
  filter(area < 0.052)

milvus_with_potential_nest <- milvus_with_potential_nest[milvus_with_potential_nest$id %in% milvus_small_area$id ,]



# Sex analysis
y_lab = bquote(Area ~ of ~ the ~ .(n_most_visited) ~ most ~ visited ~ locations ~ paste("[") ~ km^2 ~ paste("]"))
milvus_track_sf_top_45 %>%
  filter(!is.na(sex)) %>%
  filter(nest == "nest") %>%
  ggplot(aes(x = sex, y = area, color = sex)) +
  geom_boxplot() +
  scale_color_manual(values = c("indianred", "darkslateblue")) +
  labs(x = element_blank(), y = y_lab) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray20", size = .1),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank(),
        legend.position = "none"
  )

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/05_nest/01_area_sex_diff_nest.pdf")),
       width = 1500, height = 2000, units = "px", dpi = 300)

milvus_track_sf_top_45 %>%
  filter(!is.na(sex)) %>%
  filter(nest == "no nest") %>%
  ggplot(aes(x = nest, y = area, color = sex)) +
  geom_boxplot() +
  scale_color_manual(values = c("indianred", "darkslateblue")) +
  labs(x = element_blank(), y = y_lab) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray20", size = .1),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank(),
        legend.position = "none"
  )

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/05_nest/01_area_sex_diff_no_nest.pdf")),
       width = 1500, height = 2000, units = "px", dpi = 300)



# Isolating the potential nest location per individual per year
# Most visited location (if multiple -> location with longest residence time)
milvus_nest_location <- milvus_with_potential_nest %>%
  group_by(year_id = id) %>%
  filter(revisits == max(revisits)) %>%
  filter(residence_time == max(residence_time)) %>%
  summarise(potential_nest_long = first(x_),
            potential_nest_lat = first(y_),
            date_at_nest_loc = as.Date(first(t_)),
            home_range = first(home_range),
            nest = first(nest),
            nest_id = first(nest_id),
            sex = first(sex)) %>%
  mutate(month = format(date_at_nest_loc, format = "%m"))









# RECURSIONS PER DAY AT NEST LOCATION ------------------------------------------
# Joining potential nest locations information as new columns to trajectory
milvus_track_nest <- milvus_with_potential_nest %>%
  select(x_, y_, t_,
         id = date_id,
         year_id = id,)
milvus_track_nest <- left_join(milvus_track_nest, milvus_nest_location, by = "year_id")

# Splitting the track into a list where each element is a unique ID-month-year identifier
milvus_track_nest_list <- split(milvus_track_nest, milvus_track_nest$id)

# Recursion
milvus_recurse_to_nest <- lapply(milvus_track_nest_list, function(x)
  getRecursionsAtLocations(x = x[1:4], locations = x[6:7],
                           radius = 50, timeunits = "mins"))

# Extracting the time/number for each date_id
residence_time_nest <- c()
revisits_nest <- c()
for (i in 1:length(milvus_recurse_to_nest)) {
  residence_time_nest[i] <- as.integer(round(first(milvus_recurse_to_nest[[i]]$residenceTime)))
  revisits_nest[i] <- first(milvus_recurse_to_nest[[i]]$revisits)
}

# Creating a data frame with daily values to join
milvus_recurse_to_nest_join <- cbind(data.frame(date_id = names(milvus_track_nest_list),
                                                residence_time_nest, revisits_nest))

# Joining recurse information back to daily data frame
milvus_daily <- milvus_track_nest %>%
  group_by(date_id = id) %>%
  summarise(year_id = first(year_id),
            potential_nest_long = first(potential_nest_long),
            potential_nest_lat = first(potential_nest_lat),
            home_range = first(home_range),
            nest = first(nest),
            nest_id = first(nest_id),
            sex = first(sex)) %>%
  arrange(year_id)

milvus_daily <- left_join(milvus_daily, milvus_recurse_to_nest_join, by = "date_id")









# COMPARISON TO ACTUAL NEST LOCATIONS ------------------------------------------
# Preparing data frames for comparison
nest_list <- nest_list %>%
  select(nest_id = ID,
         nest_long_wgs = longitude,
         nest_lat_wgs = latitude) %>%
  st_as_sf(coords = c("nest_long_wgs", "nest_lat_wgs"), crs = 4326) %>%
  dplyr::mutate(nest_long_wgs = st_coordinates(.)[,1],
                nest_lat_wgs = st_coordinates(.)[,2]) %>%
  st_transform(crs = 3035) %>%
  dplyr::mutate(nest_long_eea = st_coordinates(.)[,1],
                nest_lat_eea = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

nest_list_join <- nest_list %>%
  select(nest_id, nest_long_eea, nest_lat_eea)

nest_potential <- milvus_daily %>%
  group_by(year_id) %>%
  summarise(potential_nest_long = first(potential_nest_long),
            potential_nest_lat = first(potential_nest_lat),
            sex = first(sex))

nest_actual <- milvus_daily %>%
  group_by(year_id) %>%
  summarise(nest_id = first(nest_id),
            sex = first(sex))

nest_actual <- left_join(nest_actual, nest_list_join, by = "nest_id")

nest_actual <- nest_actual %>%
  filter(nest_id != 0)

nest_potential <- nest_potential %>%
  filter(year_id %in% nest_actual$year_id)

# Calculating distances between predicted and actual nest locations
nest_potential_sf <- nest_potential %>%
  st_as_sf(coords = c("potential_nest_long", "potential_nest_lat"),
           crs = 3035) %>%
  mutate(dist_to_real_nest = NA)

nest_actual_sf <- nest_actual %>%
  st_as_sf(coords = c("nest_long_eea", "nest_lat_eea"),
           crs = 3035)

nest_potential_sf$dist_to_real_nest <-
  st_distance(nest_potential_sf, nest_actual_sf, by_element = T)



# Visualisation of distance to real nest
# Distances
dist_to_nest <- nest_potential_sf %>%
  st_drop_geometry() %>%
  select(dist_to_real_nest) %>%
  mutate(dist_to_real_nest = sort(round(as.numeric(dist_to_real_nest))))

# Percentiles
n_99 <- round(0.99*nrow(dist_to_nest))
dist_to_nest_99_percent <- dist_to_nest %>%
  slice(1:n_99)
max_dist_99_percent <- max(dist_to_nest_99_percent)

n_95 <- round(0.95*nrow(dist_to_nest))
dist_to_nest_95_percent <- dist_to_nest %>%
  slice(1:n_95)
max_dist_95_percent <- max(dist_to_nest_95_percent)

n_50 <- round(0.50*nrow(dist_to_nest))
dist_to_nest_50_percent <- dist_to_nest %>%
  slice(1:n_50)
max_dist_50_percent <- max(dist_to_nest_50_percent)

# Plotting
dist_to_nest %>%
  ggplot(aes(x = round(dist_to_real_nest))) +
  geom_bar(width = 10, color = "black") +
  geom_vline(xintercept = max_dist_50_percent, linetype = "dashed", 
                  color = "orange", size = 0.5) +
  annotate("text", x = max_dist_50_percent - 100, y = 7, label = "50% Percentile (34m)",
           angle = 90, color = "orange") +
  geom_vline(xintercept = max_dist_95_percent, linetype = "dashed", 
             color = "red", size = 0.5) +
  annotate("text", x = max_dist_95_percent + 60, y = 7, label = "95% Percentile (119m)",
           angle = 90, color = "red") +
  geom_vline(xintercept = max_dist_99_percent, linetype = "dashed", 
             color = "darkred", size = 0.5) +
  annotate("text", x = max_dist_99_percent + 60, y = 7, label = "99% Percentile (784m)",
           angle = 90, color = "darkred") +
  labs(y = "Count", x = "Distance to actual nest location [m]",
       title = element_blank()) +
  scale_y_continuous(breaks = seq(0,14,2)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray20", size = .05),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank()
        )

# Saving plot
ggsave(here("../Data/Output/Plots/05_nest/02_nest_dist_bar.pdf"),
       width = 3000, height = 1500, units = "px", limitsize = F)



# Sex analysis
dist_to_nest_sex <- nest_potential_sf %>%
  st_drop_geometry() %>%
  mutate(dist_to_real_nest = round(as.numeric(dist_to_real_nest))) %>%
  filter(dist_to_real_nest < 1000)

dist_to_nest_sex %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = sex, y = dist_to_real_nest, color = sex)) +
  geom_boxplot() +
  scale_color_manual(values = c("indianred", "darkslateblue")) +
  labs(x = element_blank(), y = "Distance to real nest location [m]") +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray20", size = .1),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank(),
        #axis.text.x = element_blank(),
        legend.position = "none"
  )

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/05_nest/03_nest_dist_sex_diff.pdf")),
       width = 1500, height = 2000, units = "px", dpi = 300)



# T-Test
# Female birds
dist_to_nest_sex_f <- dist_to_nest_sex %>%
  filter(sex == "f")
# Male birds
dist_to_nest_sex_m <- dist_to_nest_sex %>%
  filter(sex == "m")
# Conducting test
t.test(dist_to_nest_sex_f$dist_to_real_nest,
       dist_to_nest_sex_m$dist_to_real_nest)









# MEAN DAILY RESIDENCE TIME AT POTENTIAL NEST LOCATION -------------------------
milvus_nest_stats <- milvus_daily %>%
  group_by(year_id) %>%
  summarise(residence_time_nest = sum(residence_time_nest),
            revisits_nest = sum(revisits_nest),
            sex = first(sex),
            nest = first(nest),
            nest_id = first(nest_id),
            potential_nest_long = first(potential_nest_long),
            potential_nest_lat = first(potential_nest_lat),
            number_of_days = n()) %>%
  mutate(residence_time_nest_relative = residence_time_nest/number_of_days,
         revisits_nest_relative = revisits_nest/number_of_days)



# Visual change point detection of average daily residence time
birds_with_nest <- milvus_nest_stats[milvus_nest_stats$nest == "nest" ,]
birds_without_nest <- milvus_nest_stats[milvus_nest_stats$nest == "no nest" ,]

birds_with_nest <- birds_with_nest %>%
  arrange(residence_time_nest_relative) %>%
  mutate(n_birds = 0:(nrow(birds_with_nest)-1)) %>%
  mutate(n_birds = n_birds/nrow(birds_with_nest)*100)

birds_without_nest <- birds_without_nest %>%
  arrange(residence_time_nest_relative) %>%
  mutate(n_birds = 0:(nrow(birds_without_nest)-1)) %>%
  mutate(n_birds = n_birds/nrow(birds_without_nest)*100)

first_bird_with_nest <- min(birds_with_nest$residence_time_nest_relative)

ggplot() +
  geom_line(birds_with_nest, mapping = aes(x = residence_time_nest_relative, y = n_birds), color = "darkblue") +
  geom_line(birds_without_nest, mapping = aes(x = residence_time_nest_relative, y = n_birds), color = "lightblue") +
  # geom_line(mapping = aes(x = 5, y = 1:100), color = "red", linetype = "dashed") +
  # geom_line(mapping = aes(x = 1, y = 1:100), color = "orange", linetype = "dashed") +
  geom_line(mapping = aes(x = first_bird_with_nest, y = 1:100), color = "red", linetype = "dashed") +
  annotate("text", x = first_bird_with_nest + 8, y = 50,
           label= "Residence time = 31 min", angle = 90, color = "red") +
  labs(x = "Mean daily residence time [min]", y = "Individuals [%]") +
  theme(panel.background = element_blank(),
        panel.grid = element_line(colour = "gray95"),
        plot.title = element_text(hjust = 0.5))

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/05_nest/04_residence_time.pdf")),
       width = 3000, height = 1500, units = "px", dpi = 300)



# Sex analysis
milvus_nest_stats %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = nest, y = residence_time_nest_relative, color = sex)) +
  geom_boxplot() +
  scale_color_manual(values = c("indianred", "darkslateblue")) +
  labs(x = element_blank(), y = "Mean daily residence time [min]") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray20", size = .1),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank(),
        legend.position = "none"
  )

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/05_nest/05_residence_time_sex_diff.pdf")),
       width = 1500, height = 2000, units = "px", dpi = 300)



# T-Test
# Nest
milvus_nest_stats_nest <- milvus_nest_stats %>%
  filter(nest == "nest")
# No nest
milvus_nest_stats_no_nest <- milvus_nest_stats %>%
  filter(nest == "no nest")
# Conducting test
t.test(milvus_nest_stats_nest$residence_time_nest_relative,
       milvus_nest_stats_no_nest$residence_time_nest_relative)
# Female birds
milvus_nest_stats_nest_f <- milvus_nest_stats_nest %>%
  filter(sex == "f")
# Male birds
milvus_nest_stats_nest_m <- milvus_nest_stats_nest %>%
  filter(sex == "m")
# Conducting test
t.test(milvus_nest_stats_nest_f$residence_time_nest_relative,
       milvus_nest_stats_nest_m$residence_time_nest_relative)









# AVERAGE DAILY REVISITS AT POTENTIAL NEST LOCATION ----------------------------
birds_with_nest <- milvus_nest_stats[milvus_nest_stats$nest == "nest" ,]
birds_without_nest <- milvus_nest_stats[milvus_nest_stats$nest == "no nest" ,]

birds_with_nest <- birds_with_nest %>%
  arrange(revisits_nest_relative) %>%
  mutate(n_birds = 0:(nrow(birds_with_nest)-1)) %>%
  mutate(n_birds = n_birds/nrow(birds_with_nest)*100)

birds_without_nest <- birds_without_nest %>%
  arrange(revisits_nest_relative) %>%
  mutate(n_birds = 0:(nrow(birds_without_nest)-1)) %>%
  mutate(n_birds = n_birds/nrow(birds_without_nest)*100)

first_bird_with_nest <- min(birds_with_nest$revisits_nest_relative)

# Visual change point detection of average daily residence time
ggplot() +
  geom_line(birds_with_nest, mapping = aes(x = revisits_nest_relative, y = n_birds), color = "darkblue") +
  geom_line(birds_without_nest, mapping = aes(x = revisits_nest_relative, y = n_birds), color = "lightblue") +
  # geom_line(mapping = aes(x = 5, y = 1:100), color = "red", linetype = "dashed") +
  # geom_line(mapping = aes(x = 1, y = 1:100), color = "orange", linetype = "dashed") +
  geom_line(mapping = aes(x = first_bird_with_nest, y = 1:100), color = "red", linetype = "dashed") +
  annotate("text", x = first_bird_with_nest + 0.06, y = 50,
           label= "Revisitations = 0.54", angle = 90, color = "red") +
  labs(x = "Mean daily revisitations", y = "Individuals [%]") +
  theme(panel.background = element_blank(),
        panel.grid = element_line(colour = "gray95"),
        plot.title = element_text(hjust = 0.5))

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/05_nest/07_revisits.pdf")),
       width = 3000, height = 1500, units = "px", dpi = 300)



# Sex analysis
milvus_nest_stats %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = nest, y = revisits_nest_relative, color = sex)) +
  geom_boxplot() +
  scale_color_manual(values = c("indianred", "darkslateblue")) +
  labs(x = element_blank(), y = "Mean daily revisitations") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray20", size = .1),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank(),
        legend.key = element_rect("transparent")
        # legend.position = "none"
  )

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/05_nest/07_revisits_sex_diff.pdf")),
       width = 1500, height = 2000, units = "px", dpi = 300)



# T-Test
t.test(milvus_nest_stats_nest$revisits_nest_relative,
       milvus_nest_stats_no_nest$revisits_nest_relative)
t.test(milvus_nest_stats_nest_f$residence_time_nest_relative,
       milvus_nest_stats_nest_m$residence_time_nest_relative)









# SAVING DATA ------------------------------------------------------------------
# Preparing data frame to save relevant information only
milvus_nest_location <- milvus_nest_location %>%
  filter(year_id %in% milvus_daily$year_id)

# Creating directory
if (!dir.exists(here("../Data/Output/05_nest"))) {
  if (!dir.exists(here("../Data/Output"))) {
    dir.create("../Data/Output")
  }
  dir.create("../Data/Output/05_nest")
}

write.csv(milvus_daily,
          here("../Data/Output/05_nest/milvus_daily.csv"),
          row.names = F)

write.csv(milvus_nest_location,
          here("../Data/Output/05_nest/milvus_nest_location.csv"),
          row.names = F)


