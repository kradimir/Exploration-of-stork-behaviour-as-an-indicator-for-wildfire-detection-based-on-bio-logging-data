
# incorporating googlemap API in the longitude latitude plot

# List of all required packages
packages <- c("tidyverse", "data.table", "dplyr", "rlang", "lubridate", "sf", "spatialrisk", "readr", "readxl", "move2", 
              "suntools", "tidyr", "ggplot2", "Rfast", "parallel")

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

# Specific check for 'rlang' version 1.1.2 or higher
if (!("rlang" %in% installed.packages()[,"Package"]) || packageVersion("rlang") < "1.1.2") {
  install.packages("rlang")
}

# Load all the required packages
invisible(lapply(packages, library, character.only = TRUE))

# Confirm the version of 'rlang' to ensure it meets the requirements
#rlang_version <- packageVersion("rlang")
#print(paste("rlang version:", rlang_version))


# Register Google API key
register_google(key = "AIzaSyBxGRRuCewkdNGvl9_0jagofvQxq0GWeTo")

#adjust zoom
calculate_zoom_level <- function(lon_range, lat_range) {
  # Determine the maximum range difference
  max_range_diff <- max(diff(lon_range), diff(lat_range))
  
  print(paste("Maximum range difference:", max_range_diff))
  
  # Adjust the zoom level dynamically based on the data spread
  #  zoom <- if (max_range_diff < 0.01) {14} else if (max_range_diff <= 0.1) {13} else if (max_range_diff <= 0.2) {12} else if (max_range_diff <= 0.5) {11} else if (max_range_diff <= 1) {10} else {9}  return(zoom)}
  
  #this seems like the good zoom option
  zoom <- if (max_range_diff < 0.01) {
    14
  } else if (max_range_diff <= 0.09) {
    13
  } else if (max_range_diff <= 0.18) {
    12
  } else if (max_range_diff <= 0.5) {
    11
  } else if (max_range_diff <= 1) {
    10
  } else if (max_range_diff <= 2) {
    9
  } else if (max_range_diff <= 4) {
    8
  } else if (max_range_diff <= 5) {
    7
  } else {
    6
  }
  return(zoom)
}

# Define the function to generate the plot
# Load required libraries
library(ggplot2)
library(dplyr)
library(ggmap)
library(patchwork)
library(sf)
library(ggmap)

#START HERE ##################################################

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


# Extract a bird ID and an event timestamp
bird_id <- unique(data$individual.local.identifier)[1]
#define timestamp
event_timestamps <- as.POSIXct(c("2020-01-03 15:15:06", "2020-01-05 13:20:06", "2020-01-06 14:40:06", 
                           "2020-01-07 07:50:08", "2020-01-08 11:00:06", "2020-01-10 13:10:06", 
                           "2020-01-11 10:55:07", "2020-01-12 13:15:07", "2020-01-13 07:15:07"))
event_timestamps <- ("2020-01-03 15:15:06")


#WORKING LOOP FOR PLOT GENERATION
# Load required packages
library(ggmap)
library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)
library(sf)

combined_plot <- function(data, bird_id, event_timestamp, file_path) {

  # Define the start and end dates
  start_date <- event_timestamp - hours(16)
  end_date <- event_timestamp + hours(16)
  
  # Filter the data within the specified time range and for the specific bird
  filtered_data <- data %>%
    filter(individual.local.identifier == bird_id, timestamp >= start_date & timestamp <= end_date) %>%
    arrange(timestamp) %>%
    mutate(track_id = as.numeric(difftime(timestamp, min(timestamp), units = "hours")) - 16,
           track_id_factor = cut(track_id, breaks = seq(-16, 16, by = 4), include.lowest = TRUE))
  
  # Extract coordinates from the geometry column
  coords <- st_coordinates(filtered_data)
  
  # Calculate the range for longitudes and latitudes
  lon_range <- range(coords[, "X"], na.rm = TRUE)
  lat_range <- range(coords[, "Y"], na.rm = TRUE)
  
  # Where the fire is when overlap occurs
  fire_points <- filtered_data %>%
    filter(!is.na(fire_latitude) & !is.na(fire_longitude) & Within_1km == TRUE) %>%
    distinct(Nearest_fire_ID, .keep_all = TRUE) %>%
    mutate(Dist2Fire = as.numeric(Dist2Fire),
           start_time = as.numeric(difftime(fire_time - hours(4), min(filtered_data$timestamp), units = "hours")) - 16,
           end_time = as.numeric(difftime(fire_time + hours(4), min(filtered_data$timestamp), units = "hours")) - 16,
           track_id = as.numeric(difftime(fire_time, min(filtered_data$timestamp), units = "hours")) - 16)
  
  # Combine and expand ranges
  combined_lon_range <- range(c(lon_range, range(fire_points$fire_longitude, na.rm = TRUE)))
  combined_lat_range <- range(c(lat_range, range(fire_points$fire_latitude, na.rm = TRUE)))
  
  lon_margin <- diff(combined_lon_range) * 0.1
  lat_margin <- diff(combined_lat_range) * 0.1
  
  expanded_lon_range <- combined_lon_range + c(-lon_margin, lon_margin)
  expanded_lat_range <- combined_lat_range + c(-lat_margin, lat_margin)
  
  mean_lat <- mean(expanded_lat_range)
  mean_long <- mean(expanded_lon_range)
  
  zoom <- calculate_zoom_level(expanded_lon_range, expanded_lat_range)
  
  # Get and plot the map
  map <- get_map(location = c(lon = mean_long, lat = mean_lat), maptype = "satellite", zoom = zoom)
  svap <- ggmap(map)
  
  custom_colors <- c(
    "#4575b4",  # Interval 1
    "#74add1",  # Interval 2
    "#abd9e9",  # Interval 3
    "#e0f3f8",  # Interval 4
    "#fee090",  # Interval 5
    "#fdae61",  # Interval 6
    "#f46d43",  # Interval 7
    "#d73027"   # Interval 8
  )
  
  # Where the bird is when overlap occurs
  bird_fire_overlap_points <- filtered_data %>%
    filter(Within_1km == TRUE) %>%
    select(location.long, location.lat, timestamp) %>%  # Ensure timestamp is included
    mutate(track_id = as.numeric(difftime(timestamp, min(filtered_data$timestamp), units = "hours")) - 16,
           shape_label = "fire within last hour detection")
  
  # Create the plot
  spatial_plot <- svap +
    geom_path(data = filtered_data, aes(x = location.long, y = location.lat, color = track_id_factor), linewidth = 1.2) +
    scale_color_manual(values = custom_colors, name = "hours from Event") +
    geom_point(data = fire_points, aes(x = fire_longitude, y = fire_latitude, size = Dist2Fire), color = "yellow", shape = 21, fill = NA, stroke = 1.5) +
    scale_size_continuous(name = "Fire and bird distance from it [m]") +
    geom_point(data = bird_fire_overlap_points, aes(x = location.long, y = location.lat), shape = 4, color = "green", size = 1, stroke = 0.5) +
    labs(title = paste("Movement Track of", bird_id),
         x = "Longitude",
         y = "Latitude",
         color = "hours from Event") +
    theme_minimal() +
    theme(legend.position = "right")
  
  # Create the latitude plot with vertical areas for fire overlap periods
  latitude_plot <- ggplot(filtered_data, aes(x = track_id, y = location.lat, color = track_id_factor)) +
    geom_line(size = 1) +  # Adjust the size of the line as needed
    scale_color_manual(values = custom_colors, name = "hours from Event") +
    scale_x_continuous(breaks = seq(-12, 12, by = 6), limits = c(-18, 18)) +
    scale_y_continuous(limits = expanded_lat_range) +
    labs(title = "Latitude over Time",
         x = "hours from Event",
         y = "Latitude",
         color = "hours from Event") +
    geom_rect(data = fire_points, aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = 0.01) +
    geom_point(data = bird_fire_overlap_points, aes(x = track_id, y = location.lat), shape = 4, color = "green", size = 1.2, stroke = 0.6) +  
    geom_point(data = fire_points, aes(x = track_id, y = fire_latitude), color = "forestgreen", shape = 21, fill = NA, size = 5, stroke = 1.5) +
    theme_minimal() +
    theme(aspect.ratio = 1,
          plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8))
  
  combined_plot <- spatial_plot + latitude_plot +
    plot_annotation(
      title = paste(bird_id, "overlap with wildfires"),
      subtitle = paste("Bird movement from hours before and after overlap on", event_timestamp),
      theme = theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 10)
      )
    ) & theme(plot.margin = margin(0, 0, 0, 0))
  
  # Save the plot as a JPG file
  ggsave(file_path, combined_plot, width = 20, height = 16, units = "in", dpi = 400)
}


# Filter data where interesting_event is TRUE
selected_events <- data %>%
  filter(interesting_event == TRUE)

# Ensure the directory exists
output_dir <- "D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event/Leon track plot"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Loop over each row in the selected events data
for (i in 1:nrow(selected_events)) {
  # Extract the bird_id and selected_event_timestamp for the current row
  bird_id <- selected_events$individual.local.identifier[i]
  selected_event_timestamp <- selected_events$timestamp[i]
  
  # Print the current event timestamp to debug
  print(paste("Processing timestamp:", selected_event_timestamp))
  
  # Generate the file name based on the current event timestamp
  file_name <- paste("plot_", format(selected_event_timestamp, "%Y-%m-%d_%H-%M-%S"), ".jpg", sep = "")
  
  # Generate the full file path
  file_path <- file.path(output_dir, file_name)
  
  # Print the file path to debug
  print(paste("Saving plot to:", file_path))
  
  # Try to generate the plot and catch any errors
  tryCatch({
    # Call the combined_plot function with the current event timestamp
    combined_plot(data, bird_id, selected_event_timestamp, file_path)
    
    # Print a success message
    print(paste("Successfully processed:", selected_event_timestamp))
  }, error = function(e) {
    # Print an error message with details
    print(paste("Error with timestamp:", selected_event_timestamp))
    print(e)
  })
}



#

















#Same code but with hours instead of days for time frame
combined_plot <- function(data, bird_id, event_timestamp) {
  
  # Define the start and end dates
  start_date <- event_timestamp - hours(16)
  end_date <- event_timestamp + hours(16)
  
  # Filter the data within the specified time range and for the specific bird
  filtered_data <- data %>%
    filter(individual.local.identifier == bird_id, timestamp >= start_date & timestamp <= end_date) %>%
    arrange(timestamp) %>%
    mutate(track_id = as.numeric(difftime(timestamp, min(timestamp), units = "hours")) - 16,
           track_id_factor = cut(track_id, breaks = seq(-16, 16, by = 4), include.lowest = TRUE))
  
  # Extract coordinates from the geometry column
  coords <- st_coordinates(filtered_data)
  
  # Calculate the range for longitudes and latitudes
  lon_range <- range(coords[, "X"], na.rm = TRUE)
  lat_range <- range(coords[, "Y"], na.rm = TRUE)
  
  # Where the fire is when overlap occurs
  fire_points <- filtered_data %>%
    filter(!is.na(fire_latitude) & !is.na(fire_longitude) & Within_1km == TRUE) %>%
    distinct(Nearest_fire_ID, .keep_all = TRUE) %>%
    mutate(Dist2Fire = as.numeric(Dist2Fire),
           start_time = as.numeric(difftime(fire_time - hours(4), min(filtered_data$timestamp), units = "hours")) - 16,
           end_time = as.numeric(difftime(fire_time + hours(4), min(filtered_data$timestamp), units = "hours")) - 16,
           track_id = as.numeric(difftime(fire_time, min(filtered_data$timestamp), units = "hours")) - 16)
  
  # Combine and expand ranges
  combined_lon_range <- range(c(lon_range, range(fire_points$fire_longitude, na.rm = TRUE)))
  combined_lat_range <- range(c(lat_range, range(fire_points$fire_latitude, na.rm = TRUE)))
  
  lon_margin <- diff(combined_lon_range) * 0.1
  lat_margin <- diff(combined_lat_range) * 0.1
  
  expanded_lon_range <- combined_lon_range + c(-lon_margin, lon_margin)
  expanded_lat_range <- combined_lat_range + c(-lat_margin, lat_margin)
  
  mean_lat <- mean(expanded_lat_range)
  mean_long <- mean(expanded_lon_range)
  
  zoom <- calculate_zoom_level(expanded_lon_range, expanded_lat_range)
  
  # Get and plot the map
  map <- get_map(location = c(lon = mean_long, lat = mean_lat), maptype = "satellite", zoom = zoom)
  svap <- ggmap(map)
  
  custom_colors <- c(
    "#4575b4",  # Interval 1
    "#74add1",  # Interval 2
    "#abd9e9",  # Interval 3
    "#e0f3f8",  # Interval 4
    "#fee090",  # Interval 5
    "#fdae61",  # Interval 6
    "#f46d43",  # Interval 7
    "#d73027"   # Interval 8
  )
  
  
  # Where the bird is when overlap occurs
  bird_fire_overlap_points <- filtered_data %>%
    filter(Within_1km == TRUE) %>%
    select(location.long, location.lat, timestamp) %>%  # Ensure timestamp is included
    mutate(track_id = as.numeric(difftime(timestamp, min(filtered_data$timestamp), units = "hours")) - 16,
           shape_label = "fire within last hour detection")
  
  # Create the plot
  spatial_plot <- svap +
    geom_path(data = filtered_data, aes(x = location.long, y = location.lat, color = track_id_factor), linewidth = 1.2) +
    scale_color_manual(values = custom_colors, name = "hours from Event") +
    geom_point(data = fire_points, aes(x = fire_longitude, y = fire_latitude, size = Dist2Fire), color = "yellow", shape = 21, fill = NA, stroke = 1.5) +
    scale_size_continuous(name = "Fire and bird distance from it [m]") +
    geom_point(data = bird_fire_overlap_points, aes(x = location.long, y = location.lat), shape = 4, color = "green", size = 1, stroke = 1.2) +
    labs(title = paste("Movement Track of", bird_id),
         x = "Longitude",
         y = "Latitude",
         color = "hours from Event") +
    theme_minimal() +
    theme(legend.position = "right")
  
  # Create the latitude plot with vertical areas for fire overlap periods
  latitude_plot <- ggplot(filtered_data, aes(x = track_id, y = location.lat, color = track_id_factor)) +
    geom_line(size = 1) +  # Adjust the size of the line as needed
    scale_color_manual(values = custom_colors, name = "hours from Event") +
    scale_x_continuous(breaks = seq(-12, 12, by = 6), limits = c(-18, 18)) +
    scale_y_continuous(limits = expanded_lat_range) +
    labs(title = "Latitude over Time",
         x = "hours from Event",
         y = "Latitude",
         color = "hours from Event") +
    geom_rect(data = fire_points, aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = 0.01) +
    geom_point(data = bird_fire_overlap_points, aes(x = track_id, y = location.lat), shape = 4, color = "green", size = 2, stroke = 1.5) +  
    geom_point(data = fire_points, aes(x = track_id, y = fire_latitude), color = "forestgreen", shape = 21, fill = NA, size = 5, stroke = 1.5) +
    theme_minimal() +
    theme(aspect.ratio = 1,
          plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8))
  
  combined_plot <- spatial_plot + latitude_plot +
    plot_annotation(
      title = paste(bird_id, "overlap with wildfires"),
      subtitle = paste("Bird movement from hours before and after overlap on", event_timestamp),
      theme = theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 10)
      )
    ) & theme(plot.margin = margin(0, 0, 0, 0))
  
  print(combined_plot)
}


#event_timestamp <- as.POSIXct(c("2020-01-03 15:15:06", "2020-01-05 13:20:06", "2020-01-06 14:40:06", "2020-01-07 07:50:08", "2020-01-08 11:00:06", 
#"2020-01-10 13:10:06", "2020-01-11 10:55:07", "2020-01-12 13:15:07", "2020-01-13 07:15:07"))

event_timestamp <- as.POSIXct("2020-01-10 13:10:06")

combined_plot(data, bird_id, event_timestamp)

#















combined_plot <- function(data, bird_id, event_timestamp) {
  
  # Define the start and end dates
  start_date <- event_timestamp - days(1)
  end_date <- event_timestamp + days(1)
  
  # Filter the data within the specified time range and for the specific bird
  filtered_data <- data %>%
    filter(individual.local.identifier == bird_id, timestamp >= start_date & timestamp <= end_date) %>%
    arrange(timestamp) %>%
    mutate(track_id = as.numeric(difftime(timestamp, min(timestamp), units = "days")) - 1,
           track_id_factor = cut(track_id, breaks = seq(-1, 1, by = 1), include.lowest = TRUE))
  
  # Extract coordinates from the geometry column
  coords <- st_coordinates(filtered_data)
  
  # Calculate the range for longitudes and latitudes
  lon_range <- range(coords[, "X"], na.rm = TRUE)
  lat_range <- range(coords[, "Y"], na.rm = TRUE)
  
  # Where the fire is when overlap occurs
  fire_points <- filtered_data %>%
    filter(!is.na(fire_latitude) & !is.na(fire_longitude) & Within_1km == TRUE) %>%
    distinct(Nearest_fire_ID, .keep_all = TRUE) %>%
    mutate(Dist2Fire = as.numeric(Dist2Fire),
           start_time = as.numeric(difftime(fire_time - hours(6), min(filtered_data$timestamp), units = "days")) - 1,
           end_time = as.numeric(difftime(fire_time + hours(6), min(filtered_data$timestamp), units = "days")) - 1,
           track_id = as.numeric(difftime(fire_time, min(filtered_data$timestamp), units = "days")) - 1)
  
  # Combine and expand ranges
  combined_lon_range <- range(c(lon_range, range(fire_points$fire_longitude, na.rm = TRUE)))
  combined_lat_range <- range(c(lat_range, range(fire_points$fire_latitude, na.rm = TRUE)))
  
  lon_margin <- diff(combined_lon_range) * 0.1
  lat_margin <- diff(combined_lat_range) * 0.1
  
  expanded_lon_range <- combined_lon_range + c(-lon_margin, lon_margin)
  expanded_lat_range <- combined_lat_range + c(-lat_margin, lat_margin)
  
  mean_lat <- mean(expanded_lat_range)
  mean_long <- mean(expanded_lon_range)
  
  zoom <- calculate_zoom_level(expanded_lon_range, expanded_lat_range)
  
  # Get and plot the map
  map <- get_map(location = c(lon = mean_long, lat = mean_lat), maptype = "satellite", zoom = zoom)
  svap <- ggmap(map)
  
  custom_colors <- c(
    "#d1e5f0",  # Day -3 -2
    "#67a9cf",  # Day -2 -1
    "#d73027",  # Day -1 -0
    "#a50026",  # Day 0 +1
    "#fdae61",  # Day 1 +2
    "#2166ac"  # Day 2 +3
  )
  
  
  
  # Where the bird is when overlap occurs
  bird_fire_overlap_points <- filtered_data %>%
    filter(Within_1km == TRUE) %>%
    select(location.long, location.lat, timestamp) %>%  # Ensure timestamp is included
    mutate(track_id = as.numeric(difftime(timestamp, min(filtered_data$timestamp), units = "days")) - 1,
           shape_label = "fire within last hour detection")
  
  # Create the plot
  spatial_plot <- svap +
    geom_path(data = filtered_data, aes(x = location.long, y = location.lat, color = track_id_factor), linewidth = 1) +
    scale_color_manual(values = custom_colors, name = "Days from Event") +
    geom_point(data = fire_points, aes(x = fire_longitude, y = fire_latitude, size = Dist2Fire), color = "forestgreen", shape = 21, fill = NA, stroke = 2) +
    scale_size_continuous(name = "Fire and bird distance from it [m]") +
    geom_point(data = bird_fire_overlap_points, aes(x = location.long, y = location.lat), shape = 4, color = "green", size = 1, stroke = 1) +
    labs(title = paste("Movement Track of", bird_id),
         x = "Longitude",
         y = "Latitude",
         color = "Days from Event") +
    theme_minimal() +
    theme(legend.position = "right")
  
  # Create the latitude plot with vertical areas for fire overlap periods
  latitude_plot <- ggplot(filtered_data, aes(x = track_id, y = location.lat, color = track_id_factor)) +
    geom_line(size = 1) +  # Adjust the size of the line as needed
    scale_color_manual(values = custom_colors, name = "Days from Event") +
    scale_x_continuous(breaks = seq(-1, 1, by = 1), limits = c(-1.5, 1.5)) +
    scale_y_continuous(limits = expanded_lat_range) +
    labs(title = "Latitude over Time",
         x = "Days from Event",
         y = "Latitude",
         color = "Days from Event") +
    geom_rect(data = fire_points, aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = 0.01) +
    geom_point(data = bird_fire_overlap_points, aes(x = track_id, y = location.lat), shape = 4, color = "green", size = 2, stroke = 1.5) +  
    geom_point(data = fire_points, aes(x = track_id, y = fire_latitude), color = "forestgreen", shape = 21, fill = NA, size = 5, stroke = 1.5) +
    theme_minimal() +
    theme(aspect.ratio = 1,
          plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8))
  
  combined_plot <- spatial_plot + latitude_plot +
    plot_annotation(
      title = paste(bird_id, "overlap with wildfires"),
      subtitle = paste("Bird movement from days before and after overlap on", event_timestamp),
      theme = theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 10)
      )
    ) & theme(plot.margin = margin(0, 0, 0, 0))
  
  print(combined_plot)
}

combined_plot(data, bird_id, event_timestamp)



#










































# Define the start and end dates
start_date <- event_timestamp - days(3)
end_date <- event_timestamp + days(3)

# Filter the data within the specified time range and for the specific bird
filtered_data <- data %>%
  filter(individual.local.identifier == bird_id, timestamp >= start_date & timestamp <= end_date) %>%
  arrange(timestamp) %>%
  mutate(track_id = as.numeric(difftime(timestamp, min(timestamp), units = "days")) - 3,
         track_id_factor = cut(track_id, breaks = seq(-3, 3, by = 1), include.lowest = TRUE))

# Extract coordinates from the geometry column
coords <- st_coordinates(filtered_data)

# Calculate the range for longitudes and latitudes
lon_range <- range(coords[, "X"], na.rm = TRUE)
lat_range <- range(coords[, "Y"], na.rm = TRUE)

# Combine and expand ranges
combined_lon_range <- range(c(lon_range, range(fire_points$fire_longitude, na.rm = TRUE)))
combined_lat_range <- range(c(lat_range, range(fire_points$fire_latitude, na.rm = TRUE)))

lon_margin <- diff(combined_lon_range) * 0.1
lat_margin <- diff(combined_lat_range) * 0.1

expanded_lon_range <- combined_lon_range + c(-lon_margin, lon_margin)
expanded_lat_range <- combined_lat_range + c(-lat_margin, lat_margin)

mean_lat <- mean(expanded_lat_range)
mean_long <- mean(expanded_lon_range)

zoom <- calculate_zoom_level(expanded_lon_range, expanded_lat_range)

# Get and plot the map
map <- get_map(location = c(lon = mean_long, lat = mean_lat), maptype = "satellite", zoom = zoom)
svap <- ggmap(map)


custom_colors <- c(
  "#d1e5f0",  # Day -3 -2
  "#67a9cf",  # Day -2 -1
  "#d73027",  # Day -1 -0
  "#a50026",  # Day 0 +1
  "#fdae61",  # Day 1 +2
  "#2166ac"  # Day 2 +3
)



# Where the fire is when overlap occurs
fire_points <- filtered_data %>%
  filter(!is.na(fire_latitude) & !is.na(fire_longitude) & Within_1km == TRUE) %>%
  distinct(Nearest_fire_ID, .keep_all = TRUE) %>%
  mutate(Dist2Fire = as.numeric(Dist2Fire),
         start_time = as.numeric(difftime(fire_time - hours(6), min(filtered_data$timestamp), units = "days")) - 3,
         end_time = as.numeric(difftime(fire_time + hours(6), min(filtered_data$timestamp), units = "days")) - 3,
         track_id = as.numeric(difftime(fire_time, min(filtered_data$timestamp), units = "days")) - 3)

# Where the bird is when overlap occurs
bird_fire_overlap_points <- filtered_data %>%
  filter(Within_1km == TRUE) %>%
  select(location.long, location.lat, timestamp) %>%  # Ensure timestamp is included
  mutate(track_id = as.numeric(difftime(timestamp, min(filtered_data$timestamp), units = "days")) - 3,
         shape_label = "fire within last hour detection")

# Create the plot
spatial_plot <- svap +
  geom_path(data = filtered_data, aes(x = location.long, y = location.lat, color = track_id_factor), linewidth = 1) +
  scale_color_manual(values = custom_colors, name = "Days from Event") +
  geom_point(data = fire_points, aes(x = fire_longitude, y = fire_latitude, size = Dist2Fire), color = "forestgreen", shape = 21, fill = NA, stroke = 2) +
  scale_size_continuous(name = "Fire and bird distance from it [m]") +
  geom_point(data = bird_fire_overlap_points, aes(x = location.long, y = location.lat), shape = 4, color = "green", size = 1, stroke = 1) +
  labs(title = paste("Movement Track of", bird_id),
       x = "Longitude",
       y = "Latitude",
       color = "Days from Event") +
  theme_minimal() +
  theme(legend.position = "right")

# Print the plot
#print(spatial_plot)


# Create the latitude plot with vertical areas for fire overlap periods
latitude_plot <- ggplot(filtered_data, aes(x = track_id, y = location.lat, color = track_id_factor)) +
  geom_line(size = 1) +  # Adjust the size of the line as needed
  scale_color_manual(values = custom_colors, name = "Days from Event") +
  scale_x_continuous(breaks = seq(-3, 3, by = 1), limits = c(-3.5, 3.5)) +
  scale_y_continuous(limits = expanded_lat_range) +
  labs(title = "Latitude over Time",
       x = "Days from Event",
       y = "Latitude",
       color = "Days from Event") +
  geom_rect(data = fire_points, aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.01) +
  geom_point(data = bird_fire_overlap_points, aes(x = track_id, y = location.lat), shape = 4, color = "green", size = 2, stroke = 1.5) +  
  geom_point(data = fire_points, aes(x = track_id, y = fire_latitude), color = "forestgreen", shape = 21, fill = NA, size = 5, stroke = 1.5) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8))

combined_plot <- spatial_plot + latitude_plot +
  plot_annotation(
    title = paste(bird_id, "overlap with wildfires"),
    subtitle = paste("Bird movement from days before and after overlap on", event_timestamp),
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 10)
    )
  ) & theme(plot.margin = margin(0, 0, 0, 0))

print(combined_plot)










# Define the input and output directories
input_dir <- "D:/Vladimir/temporally_upscaled_overlap_data"
output_dir <- "D:/Vladimir/temporally_upscaled_overlap_data_10d_plus"

# Ensure the output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cat("Created output directory:", output_dir, "\n")
} else {
  cat("Output directory already exists:", output_dir, "\n")
}

# Run the processing function for all CSV files in the input directory
process_all_csv_files(input_dir, output_dir)
