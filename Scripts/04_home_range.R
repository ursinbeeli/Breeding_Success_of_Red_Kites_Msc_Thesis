# LOADING PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)
library(sf)
library(ggplot2)



# LOADING DATA -----------------------------------------------------------------
# Movement data
milvus_daily <- read.csv(here("../Data/Output/03_parameters/milvus_daily_7day_mcp_centroid.csv"))



# DATA PREPARATION--------------------------------------------------------------
# Formatting date to date format
milvus_daily <- milvus_daily %>%
  mutate(date = as.Date(date, tz = "UTC"))

# Keeping only the information of relevant months for settling period
milvus_daily <- milvus_daily %>%
  filter(month %in% c(2,3,4,5))

# Counting number of available days per bird
milvus_n_days <- milvus_daily %>%
  group_by(year_id) %>%
  summarise(n_days = n()) %>%
  filter(n_days >= 14)

# Deleting birds with less than a week of data
milvus_daily <- milvus_daily[milvus_daily$year_id %in% milvus_n_days$year_id ,]

# Deleting birds with a nest but an unpredictable movement behaviour (this is due to the fact that the settlement period happens after the timespan for which data is available (concluded from individual trajectory analysis))
insufficient_birds <- c("2019_297", "2020_306", "2021_142", "2021_37", "2021_402", "2021_517")
milvus_daily <- milvus_daily %>%
  filter(!(year_id %in% insufficient_birds))



# COUNTING THE CONTINUOUS WEEKS WITH AN AREA < X -------------------------------
# Adding column to check if home range is < 10/20/30/40/50/60/70/80/90/100 km^2
for (i in c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) {
  x <- i
  milvus_daily$n_days_hr_less_x <- 0
  milvus_daily[milvus_daily$mcp_area_95_7day < x ,]$n_days_hr_less_x <- 1
  milvus_daily_weeks <- milvus_daily %>%
    group_by(year_week_id) %>%
    summarise(year_id = first(year_id),
              week = first(week),
              n_days_hr_less_x = sum(n_days_hr_less_x),
              n_days_total = n(),
              home_range = first(home_range),
              nest = first(nest),
              sex = first(sex)) %>%
    mutate(percentage_of_days_hr_less_x = n_days_hr_less_x/n_days_total) %>%
    mutate(full_week_hr_less_x = 0)
  
  milvus_daily_weeks[milvus_daily_weeks$percentage_of_days_hr_less_x == 1 ,]$full_week_hr_less_x <- 1
  
  milvus_daily_weeks$consecutive_week <- 0
  for (i in unique(milvus_daily_weeks$year_id)) {
    milvus_individual <- milvus_daily_weeks[milvus_daily_weeks$year_id == i ,]
    for (j in 2:nrow(milvus_individual)) {
      if (milvus_individual[j ,]$week - milvus_individual[(j-1) ,]$week == 1 &
          milvus_individual[j ,]$full_week_hr_less_x +
          milvus_individual[(j-1) ,]$full_week_hr_less_x == 2) {
        milvus_daily_weeks[milvus_daily_weeks$year_week_id ==
                             milvus_individual[j,]$year_week_id
                           ,]$consecutive_week <-
          milvus_daily_weeks[milvus_daily_weeks$year_week_id ==
                               milvus_individual[(j-1),]$year_week_id
                             ,]$consecutive_week + 1
      }
    }
  }
  
  milvus_daily_weeks[milvus_daily_weeks$consecutive_week > 0 ,]$consecutive_week <- 
    milvus_daily_weeks[milvus_daily_weeks$consecutive_week > 0 ,]$consecutive_week + 1
  
  assign(paste0("milvus_", x, "_km2_consecutive"), milvus_daily_weeks)
  
  milvus_daily_weeks_grouped <- milvus_daily_weeks %>%
    group_by(year_id) %>%
    summarise(n_consecutive_weeks = max(consecutive_week),
              n_days_total = sum(n_days_total),
              home_range = first(home_range),
              nest = first(nest),
              sex = first(sex))
  
  assign(paste0("milvus_", x, "_km2_consecutive_grouped"), milvus_daily_weeks_grouped)
  
  # Analysing data visually for a fitting threshold value (creating plots)
  # Creating directory for plots
  if (!dir.exists(here("../Data/Output/Plots/04_home_range"))) {
    if (!dir.exists(here("../Data/Output"))) {
      dir.create("../Data/Output")
    }
    if (!dir.exists(here("../Data/Output/Plots"))) {
      dir.create("../Data/Output/Plots")
    }
    dir.create("../Data/Output/Plots/04_home_range")
  }
  
  birds_with_hr <- milvus_daily_weeks_grouped[milvus_daily_weeks_grouped$home_range == "home range" ,]
  birds_without_hr <- milvus_daily_weeks_grouped[milvus_daily_weeks_grouped$home_range == "no home range" ,]
  
  birds_with_hr <- birds_with_hr %>%
    arrange(n_consecutive_weeks) %>%
    mutate(n_birds = 1:nrow(birds_with_hr)) %>%
    mutate(n_birds = n_birds/nrow(birds_with_hr)*100)
  
  birds_without_hr <- birds_without_hr %>%
    arrange(n_consecutive_weeks) %>%
    mutate(n_birds = 1:nrow(birds_without_hr)) %>%
    mutate(n_birds = n_birds/nrow(birds_without_hr)*100)
  
  first_bird_with_nest <- birds_with_hr %>%
    filter(nest == "nest")
  first_bird_with_nest <- min(first_bird_with_nest$n_consecutive_weeks)
  
  x_lab <- bquote(Number ~ of ~ consecutive ~ weeks ~ with ~ an ~ area ~ lower ~ than ~ .(x) ~ km^2)
  ggplot() +
    geom_line(birds_with_hr, mapping = aes(x = n_consecutive_weeks, y = n_birds), color = "darkblue") +
    geom_line(birds_without_hr, mapping = aes(x = n_consecutive_weeks, y = n_birds), color = "lightblue") +
    # geom_line(mapping = aes(x = 5, y = 1:100), color = "red", linetype = "dashed") +
    # geom_line(mapping = aes(x = 1, y = 1:100), color = "orange", linetype = "dashed") +
    geom_line(mapping = aes(x = first_bird_with_nest, y = 1:100), color = "red", linetype = "dashed") +
    scale_x_continuous(breaks = seq(0,20,2)) +
    annotate("text", x = first_bird_with_nest + .3, y = 50,
             label= "First individual with a nest", angle = 90, color = "red") +
    labs(x = x_lab, y = "Individuals [%]") +
    theme(panel.background = element_blank(),
          panel.grid = element_line(colour = "gray95"))
  
  # Saving plot
  ggsave(here(paste0("../Data/Output/Plots/04_home_range/01_size_", get("x"), "consecutive.pdf")),
         width = 3000, height = 1500, units = "px", dpi = 300)
}

# Resulting threshold value: 60 km^2
milvus_daily_weeks_grouped_60 <- milvus_60_km2_consecutive_grouped %>%
  filter(n_consecutive_weeks >= 3)

# Sex Analysis
# Plotting the different behaviour according to the sex of the bird
y_lab <- bquote(Number ~ of ~ consecutive ~ weeks ~ with ~ an ~ area ~ lower ~ than ~ 60 ~ km^2)
milvus_60_km2_consecutive_grouped %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = home_range, y = n_consecutive_weeks, color = sex)) +
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
ggsave(here(paste0("../Data/Output/Plots/04_home_range/02_hr_area_sex_diff_no_legend.pdf")),
       width = 1500, height = 2000, units = "px", dpi = 300)



# T-Test
# Home range
milvus_60_km2_consecutive_grouped_home_range <- milvus_60_km2_consecutive_grouped %>%
  filter(home_range == "home range")
# No home range
milvus_60_km2_consecutive_grouped_no_home_range <- milvus_60_km2_consecutive_grouped %>%
  filter(home_range == "no home range")
# Conducting test
t.test(milvus_60_km2_consecutive_grouped_home_range$n_consecutive_weeks,
       milvus_60_km2_consecutive_grouped_no_home_range$n_consecutive_weeks)
# Female birds
milvus_60_km2_consecutive_grouped_home_range_f <- milvus_60_km2_consecutive_grouped_home_range %>%
  filter(sex == "f")
# Male birds
milvus_60_km2_consecutive_grouped_home_range_m <- milvus_60_km2_consecutive_grouped_home_range %>%
  filter(sex == "m")
# Conducting test
t.test(milvus_60_km2_consecutive_grouped_home_range_f$n_consecutive_weeks,
       milvus_60_km2_consecutive_grouped_home_range_m$n_consecutive_weeks)



# Keeping only the settlement period in the data for further analysis
# Selecting only year_id's with more than 3 continuous weeks
milvus_60_km2_consecutive_grouped_3_weeks <- milvus_60_km2_consecutive_grouped %>%
  filter(n_consecutive_weeks >= 3)

# Selecting only these year_id's also out of the weekly data set
milvus_60_km2_consecutive_3_weeks <- milvus_60_km2_consecutive %>%
  filter(year_id %in% milvus_60_km2_consecutive_grouped_3_weeks$year_id)

# Finding the third week
milvus_60_km2_3rd_week <- milvus_60_km2_consecutive_3_weeks %>%
  filter(consecutive_week == 3)

# Selecting only the last 3rd week (there might be two consecutive periods, one in wintering are, one in breeding area)
milvus_60_km2_3rd_week_position <- milvus_60_km2_3rd_week %>%
  group_by(year_id) %>%
  summarise(week = max(week),
            year_week_id = last(year_week_id))
milvus_60_km2_3rd_week <- milvus_60_km2_3rd_week %>%
  filter(year_week_id %in% milvus_60_km2_3rd_week_position$year_week_id)

# Finding the consecutive weeks before the third localised 3rd week
# Creating a sequence identifier
milvus_60_km2_consecutive_3_weeks$in_sequence <- F
# Setting identifier to T at localised 3rd week
milvus_60_km2_consecutive_3_weeks[milvus_60_km2_consecutive_3_weeks$year_week_id %in%
                                    milvus_60_km2_3rd_week$year_week_id ,]$in_sequence <- T
# Finding the weeks
for (i in unique(milvus_60_km2_consecutive_3_weeks$year_id)) {
  milvus_individual <- milvus_60_km2_consecutive_3_weeks[milvus_60_km2_consecutive_3_weeks$year_id == i ,]
  for (j in 2:nrow(milvus_individual)) {
    if (milvus_individual[j ,]$consecutive_week == 3 & milvus_individual[j ,]$in_sequence == T) {
      milvus_60_km2_consecutive_3_weeks[milvus_60_km2_consecutive_3_weeks$year_week_id ==
                                     milvus_individual[j ,]$year_week_id
                                   ,]$in_sequence <- T
      milvus_60_km2_consecutive_3_weeks[milvus_60_km2_consecutive_3_weeks$year_week_id ==
                                     milvus_individual[(j-1) ,]$year_week_id
                                   ,]$in_sequence <- T
      milvus_60_km2_consecutive_3_weeks[milvus_60_km2_consecutive_3_weeks$year_week_id ==
                                     milvus_individual[(j-2) ,]$year_week_id
                                   ,]$in_sequence <- T
    }
  }
}

# Finding the weeks in the sequence after the 3rd week
for (i in unique(milvus_60_km2_consecutive_3_weeks$year_id)) {
  milvus_individual <- milvus_60_km2_consecutive_3_weeks[milvus_60_km2_consecutive_3_weeks$year_id == i ,]
  milvus_individual_position <- milvus_individual[milvus_individual$consecutive_week == 3 & milvus_individual$in_sequence == T ,]
  milvus_individual <- milvus_individual[milvus_individual$week >= milvus_individual_position$week &
                                           milvus_individual$consecutive_week >= 3 ,]
  milvus_60_km2_consecutive_3_weeks[milvus_60_km2_consecutive_3_weeks$year_week_id %in%
                                      milvus_individual$year_week_id
                                    ,]$in_sequence <- T
}

# Deleting all weeks that are not part of the consecutive week sequence
milvus_60_km2_sequence <- milvus_60_km2_consecutive_3_weeks %>%
  filter(in_sequence == T)
# Setting 0th week to 1st week
milvus_60_km2_sequence[milvus_60_km2_sequence$consecutive_week == 0 ,]$consecutive_week <- 1









# ANALYSING THE CENTROID DISPLACEMENT BETWEEN SETTLEMENT WEEKS -----------------
# Choosing the days of the weeks where a home range settlement could have happened
milvus_60_km2_3_weeks <- milvus_daily %>%
  filter(milvus_daily$year_week_id %in% milvus_60_km2_sequence$year_week_id)

# Creating an sf object
milvus_weekly_centroids <- milvus_60_km2_3_weeks %>%
  st_as_sf(coords = c("centroid_long", "centroid_lat"), crs = 3035)
# Calculating the weekly centroid from the daily centroids
milvus_weekly_centroids <- milvus_weekly_centroids %>%
  group_by(year_week_id) %>%
  summarise(year_id = first(year_id),
            week = first(week),
            home_range = first(home_range),
            nest = first(nest),
            sex = first(sex),
            geometry = st_union(geometry)) %>%
  mutate(geometry = st_centroid(geometry))
# Sorting data
milvus_weekly_centroids <- milvus_weekly_centroids %>%
  arrange(year_id, week)

# Calculating the displacement of the weekly centroids
milvus_weekly_centroids$centroid_displacement <- NA
for (i in unique(milvus_weekly_centroids$year_id)) {
  milvus_individual <- milvus_weekly_centroids[milvus_weekly_centroids$year_id == i ,]
  for (j in 2:nrow(milvus_individual)) {
    milvus_weekly_centroids[milvus_weekly_centroids$year_week_id ==
                              milvus_individual[j ,]$year_week_id
                            ,]$centroid_displacement <-
      as.numeric(st_distance(milvus_individual[j ,]$geometry,
                             milvus_individual[(j-1) ,]$geometry))
  }
}

# Calculating statistics of centroid displacement for the whole period per bird
milvus_weekly_centroids_grouped <- milvus_weekly_centroids %>%
  st_drop_geometry() %>%
  group_by(year_id) %>%
  summarise(max_centroid_displacement = max(centroid_displacement, na.rm = T),
            mean_centroid_displacement = mean(centroid_displacement, na.rm = T),
            min_centroid_displacement = min(centroid_displacement, na.rm = T),
            home_range = first(home_range),
            nest = first(nest),
            sex = first(sex))

# Analysing data visually for a fitting threshold value (creating a plot)
birds_with_hr <- milvus_weekly_centroids_grouped[milvus_weekly_centroids_grouped$home_range == "home range" ,]
birds_without_hr <- milvus_weekly_centroids_grouped[milvus_weekly_centroids_grouped$home_range == "no home range" ,]

birds_with_hr <- birds_with_hr %>%
  arrange(desc(min_centroid_displacement)) %>%
  mutate(n_birds = 1:nrow(birds_with_hr)) %>%
  mutate(n_birds = n_birds/nrow(birds_with_hr)*100)

birds_without_hr <- birds_without_hr %>%
  arrange(desc(min_centroid_displacement)) %>%
  mutate(n_birds = 1:nrow(birds_without_hr)) %>%
  mutate(n_birds = n_birds/nrow(birds_without_hr)*100)

first_bird_with_nest <- birds_with_hr %>%
  filter(home_range == "home range")
first_bird_with_nest <- max(first_bird_with_nest$min_centroid_displacement)

x <- 60
x_lab <- bquote(Minimum ~ centroid ~ displacement ~ "[m]" ~ between ~ weeks ~ with ~ an ~ area ~ lower ~ than ~ .(x) ~ km^2)
ggplot() +
  geom_line(birds_with_hr, mapping = aes(x = min_centroid_displacement, y = n_birds), color = "darkblue") +
  geom_line(birds_without_hr, mapping = aes(x = min_centroid_displacement, y = n_birds), color = "lightblue") +
  # geom_line(mapping = aes(x = 5, y = 1:100), color = "red", linetype = "dashed") +
  # geom_line(mapping = aes(x = 1, y = 1:100), color = "orange", linetype = "dashed") +
  geom_line(mapping = aes(x = first_bird_with_nest, y = 1:100), color = "red", linetype = "dashed") +
  annotate("text", x = first_bird_with_nest + 60, y = 50,
           label= "Centroid displacement = 760m", angle = 90, color = "red") +
  labs(x = "Minimum centroid displacement [m]", y = "Individuals [%]") +
  scale_x_continuous(trans = "reverse") +
  theme(panel.background = element_blank(),
        panel.grid = element_line(colour = "gray95"))

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/04_home_range/03_centroid_displacement.pdf")),
       width = 3000, height = 1500, units = "px", dpi = 300)

# Cutting data at the threshold
milvus_weekly_centroids_grouped_filtered <- milvus_weekly_centroids_grouped %>%
  filter(min_centroid_displacement < 760)

milvus_weekly_centroids_grouped_join <- milvus_weekly_centroids_grouped_filtered %>%
  select(year_id, min_centroid_displacement)

milvus_weekly_centroids_grouped_final <- milvus_60_km2_consecutive_grouped_3_weeks %>%
  filter(year_id %in% milvus_weekly_centroids_grouped_filtered$year_id)

milvus_weekly_centroids_grouped_final <- left_join(milvus_weekly_centroids_grouped_final,
                                                   milvus_weekly_centroids_grouped_join,
                                                   by = "year_id")

# Sex analysis
y_lab <- bquote(Number ~ of ~ consecutive ~ weeks ~ with ~ an ~ area ~ lower ~ than ~ 60 ~ km^2)
milvus_weekly_centroids_grouped %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = home_range, y = min_centroid_displacement, color = sex)) +
  geom_boxplot() +
  scale_color_manual(values = c("indianred", "darkslateblue")) +
  labs(x = element_blank(), y = "Minimum centroid displacement [m]") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray20", size = .1),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank(),
        legend.key = element_rect("transparent")
        #legend.position = "none",
  )

# Saving plot
ggsave(here(paste0("../Data/Output/Plots/04_home_range/04_centroid_displacement_sex_diff.pdf")),
       width = 1500, height = 2000, units = "px", dpi = 300)



# T-Test
# Home range
milvus_weekly_centroids_grouped_home_range <- milvus_weekly_centroids_grouped %>%
  filter(home_range == "home range")
# No home range
milvus_weekly_centroids_grouped_no_home_range <- milvus_weekly_centroids_grouped %>%
  filter(home_range == "no home range")
# Conducting test
t.test(milvus_weekly_centroids_grouped_home_range$min_centroid_displacement,
       milvus_weekly_centroids_grouped_no_home_range$min_centroid_displacement)
# Female birds
milvus_weekly_centroids_grouped_home_range_f <- milvus_weekly_centroids_grouped_home_range %>%
  filter(sex == "f")
# Male birds
milvus_weekly_centroids_grouped_home_range_m <- milvus_weekly_centroids_grouped_home_range %>%
  filter(sex == "m")
# Conducting Test
t.test(milvus_weekly_centroids_grouped_home_range_f$min_centroid_displacement,
       milvus_weekly_centroids_grouped_home_range_m$min_centroid_displacement)









# SAVING DATA ------------------------------------------------------------------
# Preparing data frame to save relevant information only
milvus_daily_weeks_position <- milvus_60_km2_sequence %>%
  group_by(year_id) %>%
  summarise(year_week_id = first(year_week_id),
            first_settling_week = min(week))

milvus_daily_weeks_position <- milvus_daily_weeks_position %>%
  filter(year_id %in% milvus_weekly_centroids_grouped_final$year_id)

milvus_filtered <- left_join(milvus_daily_weeks_position, milvus_weekly_centroids_grouped_final,
                             by = "year_id")

# Creating directory
if (!dir.exists(here("../Data/Output/04_home_range"))) {
  if (!dir.exists(here("../Data/Output"))) {
    dir.create("../Data/Output")
  }
  dir.create("../Data/Output/04_home_range")
}

write.csv(milvus_filtered,
          here("../Data/Output/04_home_range/milvus_home_range.csv"),
          row.names = F)


