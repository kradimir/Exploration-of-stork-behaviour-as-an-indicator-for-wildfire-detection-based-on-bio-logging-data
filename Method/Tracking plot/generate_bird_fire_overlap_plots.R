# full rundown
# incorporating googlemap API in the longitude latitude plot

# List of all required packages
packages <- c("tidyverse", "data.table", "dplyr", "rlang", "lubridate", "sf", "spatialrisk", "readr", "readxl", "move2", "ggmap", 
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
register_google(key = "xxx")

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


#function to read data from csv

# Define the function to read and transform a CSV file
read_and_transform_csv <- function(file_path) {
  cat("Reading file:", file_path, "\n")
  
  # Step 1: Read the CSV file
  data <- read.csv(file_path, stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)
  
  # Convert date-time fields
  data$timestamp <- as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  data$fire_time <- as.POSIXct(data$fire_time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  
  # Convert Dist2Fire to numeric (assuming data is in meters)
  data$Dist2Fire <- as.numeric(data$Dist2Fire)
  
  # Convert TimeDiff2Fire to integer
  data$TimeDiff2Fire <- as.integer(data$TimeDiff2Fire)
  
  # Convert Nearest_fire_ID to integer
  data$Nearest_fire_ID <- as.integer(data$Nearest_fire_ID)
  
  # Convert location.long and location.lat to numeric
  data$location.long <- as.numeric(data$location.long)
  data$location.lat <- as.numeric(data$location.lat)
  
  # Convert Within_10km, Fire_within_last_hour, and Dist_to_fire_within_last_hour to logical
  data$Within_10km <- ifelse(is.na(data$Within_10km), FALSE, data$Within_10km)
  data$Fire_within_last_hour <- ifelse(is.na(data$Fire_within_last_hour), FALSE, data$Fire_within_last_hour)
  data$Dist_to_fire_within_last_hour <- ifelse(is.na(data$Dist_to_fire_within_last_hour), FALSE, data$Dist_to_fire_within_last_hour)
  
  # Remove rows with NA values in coordinates
  data <- data %>% filter(!is.na(location.long) & !is.na(location.lat))
  
  # Convert to sf object
  data <- st_as_sf(data, coords = c("location.long", "location.lat"), crs = 4326, remove = FALSE)
  
  return(data)
}


#AAAA loop which creatw a function to upload csv, then create a function which will run track_fire_plot function and csv function to print jpg in  a folder


# Define the function to generate the plot
# Load required libraries
library(ggplot2)
library(dplyr)
library(ggmap)
library(patchwork)
library(sf)


# Define the function to generate the plot
track_fire_plot <- function(event_timestamp, identifier, bird_id, plot_index, output_dir, filtered_data) {
  start_date <- event_timestamp - days(10)
  end_date <- event_timestamp + days(5)
  
  filtered_data <- filtered_data %>%
    filter(timestamp >= start_date & timestamp <= end_date) %>%
    arrange(timestamp) %>%
    mutate(track_id = as.numeric(difftime(timestamp, min(timestamp), units = "days")))
  
  if (nrow(filtered_data) == 0) {
    stop("No data available for the specified time range.")
  }
  
  lon_range <- range(filtered_data$location.long, na.rm = TRUE)
  lat_range <- range(filtered_data$location.lat, na.rm = TRUE)
  
  # Check if the range difference is within the acceptable limit
  max_range_diff <- max(diff(lon_range), diff(lat_range))
  if (max_range_diff > 1.5) {
    cat("Skipping event with large movement:", event_timestamp, "\n")
    return(NULL)
  }
  
  # Now calculate fire ranges and proceed
  fire_points <- filtered_data %>%
    filter(!is.na(fire_latitude) & !is.na(fire_longitude) & Fire_within_last_hour == TRUE)
  
  if (nrow(fire_points) == 0) {
    cat("No fire points available for the specified time range:", event_timestamp, "\n")
    return(NULL)
  }
  
  fire_lon_range <- range(fire_points$fire_longitude, na.rm = TRUE)
  fire_lat_range <- range(fire_points$fire_latitude, na.rm = TRUE)
  
  combined_lon_range <- range(c(lon_range, fire_lon_range))
  combined_lat_range <- range(c(lat_range, fire_lat_range))
  
  lon_margin <- (combined_lon_range[2] - combined_lon_range[1]) * 0.1
  lat_margin <- (combined_lat_range[2] - combined_lat_range[1]) * 0.1
  
  expanded_lon_range <- c(combined_lon_range[1] - lon_margin, combined_lon_range[2] + lon_margin)
  expanded_lat_range <- c(combined_lat_range[1] - lat_margin, combined_lat_range[2] + lat_margin)
  
  mean_lat <- mean(expanded_lat_range)
  mean_long <- mean(expanded_lon_range)
  
  zoom <- calculate_zoom_level(expanded_lon_range, expanded_lat_range)
  
  map <- get_map(location = c(lon = mean_long, lat = mean_lat), maptype = "satellite", zoom = zoom)
  svap <- ggmap(map)
  
  fire_event_timestamps <- filtered_data %>%
    filter(Fire_within_last_hour == TRUE) %>%
    pull(timestamp) %>%
    unique()
  
  # Extract the bird locations at the time of fire overlap and add a shape label
  bird_fire_overlap_points <- filtered_data %>%
    filter(Fire_within_last_hour == TRUE) %>%
    select(location.long, location.lat) %>%
    mutate(shape_label = "fire within last hour detection")
  
  spatial_plot <- svap +
    geom_path(data = filtered_data, aes(x = location.long, y = location.lat, color = track_id), size = 0.7) +
    scale_color_gradient(low = "blue", high = "red", name = "Days from Start",
                         labels = scales::number_format(scale = 1)) +
    geom_point(data = fire_points, 
               aes(x = fire_longitude, y = fire_latitude, size = Dist2Fire), 
               color = "green", shape = 21, fill = NA, stroke = 1) +
    scale_size_continuous(name = "Fire and bird distance from it [m]") +
    geom_point(data = bird_fire_overlap_points,
               aes(x = location.long, y = location.lat, shape = shape_label),
               color = "green", size = 2, stroke = 1) +  # Add black "X" markers
    scale_shape_manual(name = "Legend", values = c("fire within last hour detection" = 4)) +
    theme_minimal() +
    theme(legend.position = "left",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 6),
          legend.key.size = unit(0.4, "cm")) +
    coord_fixed(xlim = expanded_lon_range, ylim = expanded_lat_range)
  
  latitude_plot <- ggplot(filtered_data, aes(x = track_id, y = location.lat)) +
    geom_line(color = "blue") +
    geom_point(color = "black") +
    scale_x_continuous(breaks = seq(0, 6, by = 1), limits = c(0, 6)) + # Change this depending on the timeframe chosen for start/end date at the beginning
    scale_y_continuous(limits = expanded_lat_range) +
    labs(x = "Days from Start", y = "Latitude") +
    geom_vline(data = tibble(track_id = as.numeric(difftime(fire_event_timestamps, min(filtered_data$timestamp), units = "days"))), 
               aes(xintercept = track_id), color = "red", linetype = "solid") +
    theme_minimal() +
    theme(aspect.ratio = 1,
          plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8))
  
  combined_plot <- spatial_plot + latitude_plot +
    plot_annotation(
      title = paste(bird_id, "overlap with wildfire"),
      subtitle = paste("Bird movement from 3 days before to 3 days after overlap on", event_timestamp),
      theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
                    plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 10))
    ) & theme(plot.margin = margin(0, 0, 0, 0))
  
  # Include bird identifier in the filename and replace any invalid characters with underscores
  sanitized_bird_id <- gsub("[^[:alnum:]_-]", "_", bird_id)
  filename <- paste0(output_dir, "/", sanitized_bird_id, "_", identifier, "_plot_", plot_index, ".jpg")
  
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  ggsave(filename = filename, plot = combined_plot, width = 16, height = 8)
}

# Define the main loop to process each CSV file in the directory
process_all_csv_files <- function(input_dir, output_dir) {
  # List all CSV files in the input directory
  csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
  cat("Found", length(csv_files), "CSV files in directory", input_dir, "\n")
  
  for (file_path in csv_files) {
    # Read and transform the CSV file
    data <- read_and_transform_csv(file_path)
    
    # Check if data was successfully loaded
    if (nrow(data) == 0) {
      cat("No data found in file:", file_path, "\n")
      next
    }
    
    cat("Processing file:", file_path, "\n")
    
    # Identify unique timestamps where 'Fire_within_last_hour' is TRUE
    specific_timestamps <- data %>%
      filter(Fire_within_last_hour == TRUE) %>%
      arrange(timestamp) %>%
      pull(timestamp) %>%
      unique()
    
    # Check if there are any specific timestamps to process
    if (length(specific_timestamps) == 0) {
      cat("No specific timestamps found in file:", file_path, "\n")
      next
    }
    
    # Extract the bird identifier (assuming it is consistent within each file)
    bird_id <- unique(data$individual.local.identifier)
    if (length(bird_id) != 1) {
      cat("Error: More than one bird identifier found in file:", file_path, "\n")
      next
    }
    
    # Helper function to sanitize filenames
    sanitize_filename <- function(filename) {
      gsub("[^[:alnum:]_-]", "_", filename)
    }
    
    # Generate and save plots for each unique specific event without overlapping
    last_start_date <- as.POSIXct(0)  # Initialize with an early date
    
    for (i in seq_along(specific_timestamps)) {
      event_timestamp <- specific_timestamps[i]
      start_date <- event_timestamp - days(7)
      
      # Check if the current event_timestamp is at least 12 hours after the last start_date
      if (event_timestamp >= (last_start_date + hours(4))) {
        identifier <- sanitize_filename(as.character(event_timestamp))
        
        cat("Generating plot for timestamp:", event_timestamp, "\n")
        
        # Try-catch block to handle errors in the plot generation
        tryCatch({
          track_fire_plot(event_timestamp, identifier, bird_id, i, output_dir, data)
          cat("Successfully generated plot for timestamp:", event_timestamp, "\n")
        }, error = function(e) {
          cat("Error generating plot for timestamp:", event_timestamp, "\n", e$message, "\n")
        })
        
        last_start_date <- event_timestamp  # Update the last start_date
      } else {
        cat("Skipping timestamp too close to previous:", event_timestamp, "\n")
      }
    }
  }
}

# Define the input and output directories
input_dir <- "D:/Vladimir/foldertotrystuff"
output_dir <- "D:/Vladimir/foldertotrystuff"

# Ensure the output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cat("Created output directory:", output_dir, "\n")
} else {
  cat("Output directory already exists:", output_dir, "\n")
}

# Run the processing function for all CSV files in the input directory
process_all_csv_files(input_dir, output_dir)

###test not working, need to open the loop to test further
track_fire_plot <- function(event_timestamp, identifier, bird_id, plot_index, output_dir, filtered_data) {
  start_date <- event_timestamp - days(9)
  end_date <- event_timestamp + days(6)
  
  cat("Event timestamp:", event_timestamp, "\n")
  
  filtered_data <- filtered_data %>%
    filter(timestamp >= start_date & timestamp <= end_date) %>%
    arrange(timestamp) %>%
    mutate(day_relative_to_fire = as.numeric(difftime(timestamp, event_timestamp, units = "days")))
  
  cat("Filtered data size:", nrow(filtered_data), "\n")
  if (nrow(filtered_data) == 0) {
    stop("No data available for the specified time range.")
  }
  
  # Define custom color palette
  custom_colors <- c(
    "#d73027", "#d73027", "#d73027",  # D1-3
    "#f46d43", "#f46d43", "#f46d43",  # D4-6
    "#fdae61", "#fdae61", "#fdae61",  # D7-9
    "#fee090",                        # D10
    "#e0f3f8", "#e0f3f8", "#e0f3f8",  # D11-13
    "#74add1", "#74add1", "#74add1"   # D14-16
  )
  
  filtered_data <- filtered_data %>%
    mutate(color_category = case_when(
      day_relative_to_fire <= -7 ~ custom_colors[1],
      day_relative_to_fire > -7 & day_relative_to_fire <= -4 ~ custom_colors[2],
      day_relative_to_fire > -4 & day_relative_to_fire < 0 ~ custom_colors[3],
      day_relative_to_fire == 0 ~ custom_colors[4],
      day_relative_to_fire > 0 & day_relative_to_fire <= 3 ~ custom_colors[5],
      day_relative_to_fire > 3 & day_relative_to_fire <= 6 ~ custom_colors[6],
      day_relative_to_fire > 6 & day_relative_to_fire <= 9 ~ custom_colors[7],
      day_relative_to_fire > 9 & day_relative_to_fire <= 12 ~ custom_colors[8],
      day_relative_to_fire > 12 & day_relative_to_fire <= 15 ~ custom_colors[9],
      TRUE ~ "grey"  # Default color if outside the defined range
    ))
  
  cat("Color categories assigned\n")
  
  lon_range <- range(filtered_data$location.long, na.rm = TRUE)
  lat_range <- range(filtered_data$location.lat, na.rm = TRUE)
  
  cat("Longitude range:", lon_range, "\n")
  cat("Latitude range:", lat_range, "\n")
  
  max_range_diff <- max(diff(lon_range), diff(lat_range))
  if (max_range_diff > 2) {
    cat("Skipping event with large movement:", event_timestamp, "\n")
    return(NULL)
  }
  
  fire_points <- filtered_data %>%
    filter(!is.na(fire_latitude) & !is.na(fire_longitude) & Fire_within_last_hour == TRUE)
  
  cat("Fire points size:", nrow(fire_points), "\n")
  if (nrow(fire_points) == 0) {
    cat("No fire points available for the specified time range:", event_timestamp, "\n")
    return(NULL)
  }
  
  fire_lon_range <- range(fire_points$fire_longitude, na.rm = TRUE)
  fire_lat_range <- range(fire_points$fire_latitude, na.rm = TRUE)
  
  combined_lon_range <- range(c(lon_range, fire_lon_range))
  combined_lat_range <- range(c(lat_range, fire_lat_range))
  
  lon_margin <- (combined_lon_range[2] - combined_lon_range[1]) * 0.1
  lat_margin <- (combined_lat_range[2] - combined_lat_range[1]) * 0.1
  
  expanded_lon_range <- c(combined_lon_range[1] - lon_margin, combined_lon_range[2] + lon_margin)
  expanded_lat_range <- c(combined_lat_range[1] - lat_margin, combined_lat_range[2] + lat_margin)
  
  mean_lat <- mean(expanded_lat_range)
  mean_long <- mean(expanded_lon_range)
  
  cat("Zoom level calculation\n")
  zoom <- calculate_zoom_level(expanded_lon_range, expanded_lat_range)
  
  map <- get_map(location = c(lon = mean_long, lat = mean_lat), maptype = "satellite", zoom = zoom)
  svap <- ggmap(map)
  
  cat("Map retrieved\n")
  
  fire_event_timestamps <- filtered_data %>%
    filter(Fire_within_last_hour == TRUE) %>%
    pull(timestamp) %>%
    unique()
  
  bird_fire_overlap_points <- filtered_data %>%
    filter(Fire_within_last_hour == TRUE) %>%
    select(location.long, location.lat) %>%
    mutate(shape_label = "fire within last hour detection")
  
  spatial_plot <- svap +
    geom_path(data = filtered_data, aes(x = location.long, y = location.lat, color = color_category), size = 0.7) +
    scale_color_identity() +
    geom_point(data = fire_points, 
               aes(x = fire_longitude, y = fire_latitude, size = Dist2Fire), 
               color = "green", shape = 21, fill = NA, stroke = 1) +
    scale_size_continuous(name = "Fire and bird distance from it [m]") +
    geom_point(data = bird_fire_overlap_points,
               aes(x = location.long, y = location.lat, shape = shape_label),
               color = "green", size = 2, stroke = 1) +  # Add black "X" markers
    scale_shape_manual(name = "Legend", values = c("fire within last hour detection" = 4)) +
    theme_minimal() +
    theme(legend.position = "left",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 6),
          legend.key.size = unit(0.4, "cm")) +
    coord_fixed(xlim = expanded_lon_range, ylim = expanded_lat_range)
  
  cat("Spatial plot created\n")
  
  latitude_plot <- ggplot(filtered_data, aes(x = track_id, y = location.lat)) +
    geom_line(color = "blue") +
    geom_point(color = "black") +
    geom_point(data = bird_fire_overlap_points, aes(x = as.numeric(difftime(timestamp, min(filtered_data$timestamp), units = "days")), y = location.lat), color = "red", shape = 4, size = 3) +
    scale_x_continuous(breaks = seq(0, 6, by = 1), limits = c(0, 6)) + 
    scale_y_continuous(limits = expanded_lat_range) +
    labs(x = "Days from Start", y = "Latitude") +
    geom_vline(data = tibble(track_id = as.numeric(difftime(fire_event_timestamps, min(filtered_data$timestamp), units = "days"))), 
               aes(xintercept = track_id), color = "red", linetype = "solid") +
    theme_minimal() +
    theme(aspect.ratio = 1,
          plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8))
  
  cat("Latitude plot created\n")
  
  combined_plot <- spatial_plot + latitude_plot +
    plot_annotation(
      title = paste(bird_id, "overlap with wildfire"),
      subtitle = paste("Bird movement from 3 days before to 3 days after overlap on", event_timestamp),
      theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
                    plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 10))
    ) & theme(plot.margin = margin(0, 0, 0, 0))
  
  sanitized_bird_id <- gsub("[^[:alnum:]_-]", "_", bird_id)
  filename <- paste0(output_dir, "/", sanitized_bird_id, "_", identifier, "_plot_", plot_index, ".jpg")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  cat("Saving plot to file:", filename, "\n")
  ggsave(filename = filename, plot = combined_plot, width = 16, height = 8)
  cat("Plot saved\n")
}
