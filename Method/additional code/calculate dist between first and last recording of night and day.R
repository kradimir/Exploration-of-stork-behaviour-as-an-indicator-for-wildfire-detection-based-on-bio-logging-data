#code to Check the distance between the first and last recorded point of each day
  #observe if there's movement during the night
  #observe if they migated during the day

#plotting speed over time for each day


#upload data:

#this data transformation seems good
load_and_transform_data <- function(file_path) {
  # Read the CSV file
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Convert timestamp columns to POSIXct
  data$timestamp <- as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M:%S")
  data$fire_time <- as.POSIXct(data$fire_time, format = "%Y-%m-%d %H:%M:%S")
  
  # Convert Dist2Fire to numeric and add units
  data$Dist2Fire <- as.numeric(data$Dist2Fire)
  data$Dist2Fire <- set_units(data$Dist2Fire, "m")
  
  # Ensure TimeDiff2Fire is numeric
  data$TimeDiff2Fire <- as.numeric(data$TimeDiff2Fire)
  
  # Convert logical columns
  data$interesting_event <- as.logical(data$interesting_event)
  data$in_range <- as.logical(data$in_range)
  data$Within_1km <- as.logical(data$Within_1km)
  
  # Retain original coordinate columns
  data$location.long <- as.numeric(data$location.long)
  data$location.lat <- as.numeric(data$location.lat)
  
  # Recreate the geometry column for sf
  data <- st_as_sf(data, coords = c("location.long", "location.lat"), crs = 4326, remove = FALSE)
  
  return(data)
}


#csv upload from narrowed down overlap analysis
data <- load_and_transform_data("D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event\\Leon_HL452__3064_2.csv.csv")

data <- data %>% mutate(
  interesting_event = ifelse(is.na(interesting_event), FALSE, interesting_event),
  in_range = ifelse(is.na(in_range), FALSE, in_range),
  Within_1km = ifelse(is.na(Within_1km), FALSE, Within_1km)
)

#manually selected interesting date we want to focus on
data[data$timestamp %in% as.POSIXct(c("2020-01-03 15:15:06", "2020-01-05 13:20:06", "2020-01-06 14:40:06", "2020-01-07 07:50:08", "2020-01-08 11:00:06", "2020-01-10 13:10:06", "2020-01-11 10:55:07", "2020-01-12 13:15:07", "2020-01-13 07:15:07"), format="%Y-%m-%d %H:%M:%S"), "interesting_event"] <- TRUE
table(data$interesting_event)


library(dplyr)
library(lubridate)
library(sf)
library(units)

# Define the timeframe
start_date <- as.Date("2020-01-08") - 50
end_date <- as.Date("2020-01-08") + 50

# Filter data for the 100-day window
filtered_data <- data %>%
  filter(timestamp >= start_date & timestamp <= end_date)

# Group data by date
daily_data <- filtered_data %>%
  group_by(Date = as.Date(timestamp)) %>%
  summarize(
    first_record = min(timestamp),
    last_record = max(timestamp),
    first_geom = geometry[which.min(timestamp)],
    last_geom = geometry[which.max(timestamp)],
    .groups = 'drop'
  )

# Calculate daily distances and overnight distances
daily_data <- daily_data %>%
  mutate(
    daily_dist_traveled = st_distance(first_geom, last_geom, by_element = TRUE),
    next_first_record = lead(first_record),
    next_first_geom = lead(first_geom),
    overnight_dist_traveled = st_distance(last_geom, next_first_geom, by_element = TRUE),
    overnight_time_diff = as.numeric(difftime(next_first_record, last_record, units = "hours"))
  )

# Create the final data frame
final_data <- daily_data %>%
  select(
    Date,
    overnight_dist_traveled,
    daily_dist_traveled,
    time_prev_last = last_record,
    time_first = next_first_record,
    time_last = last_record,
    first_geom,
    last_geom
  )

# Format distances for display, rounding to the nearest meter
final_data <- final_data %>%
  mutate(
    overnight_dist_traveled = set_units(overnight_dist_traveled, "m") %>% round(0),
    daily_dist_traveled = set_units(daily_dist_traveled, "m") %>% round(0)
  )

# Display the final data
view(final_data)


#ploting the result in a timeserie

# Plot for daily distance traveled
daily_plot <- ggplot(final_data, aes(x = Date, y = daily_dist_traveled)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Daily Distance Traveled",
    x = "Date",
    y = "Distance (meters)"
  ) +
  theme_minimal()

print(daily_plot)

# Plot for overnight distance traveled
overnight_plot <- ggplot(final_data, aes(x = Date, y = overnight_dist_traveled)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(
    title = "Overnight Distance Traveled",
    x = "Date",
    y = "Distance (meters)"
  ) +
  theme_minimal()

print(overnight_plot)


# Define the filename for the ggplot2 plot
plot_filename_ggplot <- paste0("D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event/Leon dist travel over nightday/", "Leon overday_overnight travel distance", ".jpg")

# Create ggplot2 plots
p1 <- ggplot(final_data, aes(x = Date, y = daily_dist_traveled)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Daily Distance Traveled", x = "Date", y = "Distance (meters)") +
  theme_minimal()

p2 <- ggplot(final_data, aes(x = Date, y = overnight_dist_traveled)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Overnight Distance Traveled", x = "Date", y = "Distance (meters)") +
  theme_minimal()

# Save the ggplot2 plots to file
ggsave(plot_filename_ggplot, plot = grid.arrange(p1, p2, ncol = 1), width = 16, height = 12)
