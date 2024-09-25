library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(ggmap)


# Define the function to create and save plots for 1-day chunks
generate_plots_for_chunks <- function(data, zoom, output_dir) {
  
  # Filter and arrange data where in_range is TRUE
  filtered_data <- data %>%
    filter(in_range == TRUE) %>%
    arrange(timestamp)
  
  # Check if there are valid timestamps
  if (nrow(filtered_data) == 0) {
    stop("No data available where in_range is TRUE.")
  }
  
  # Determine the start and end dates
  start_date <- min(filtered_data$timestamp)
  end_date <- max(filtered_data$timestamp)
  
  # Generate 1-day chunks
  current_start <- start_date
  while (current_start < end_date) {
    current_end <- current_start + days(1)
    
    # Filter data for the current 1-day chunk
    chunk_data <- filtered_data %>%
      filter(timestamp >= current_start & timestamp < current_end)
    
    if (nrow(chunk_data) > 0) {
      # Extract coordinates from the geometry column
      coords <- st_coordinates(chunk_data)
      
      # Extract fire points
      fire_points <- chunk_data %>%
        filter(!is.na(fire_latitude) & !is.na(fire_longitude)) %>%
        arrange(Nearest_fire_ID, desc(interesting_event)) %>%
        distinct(Nearest_fire_ID, .keep_all = TRUE)
      
      # Convert Dist2Fire to numeric for comparison
      fire_points <- fire_points %>%
        mutate(Dist2Fire_numeric = as.numeric(Dist2Fire))
      
      fire_points_light_red <- fire_points %>% filter(Dist2Fire_numeric > 250)
      fire_points_dark_red <- fire_points %>% filter(Dist2Fire_numeric <= 250)
      
      # Calculate the range for longitudes and latitudes
      lon_range <- range(coords[, "X"], na.rm = TRUE)
      lat_range <- range(coords[, "Y"], na.rm = TRUE)
      
      # Combine and expand ranges with fire points
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
      
      # Create the spatial plot
      spatial_plot <- svap +
        geom_path(data = chunk_data, aes(x = coords[, "X"], y = coords[, "Y"], color = as.POSIXct(timestamp, origin = "1970-01-01")), linewidth = 1.5) +
        scale_color_gradientn(colors = c("#4575b4", "#fee090", "#d73027"), 
                              name = "Timestamp",
                              breaks = c(min(chunk_data$timestamp), 
                                         min(chunk_data$timestamp) + (max(chunk_data$timestamp) - min(chunk_data$timestamp)) / 2, 
                                         max(chunk_data$timestamp)),
                              labels = c(format(min(chunk_data$timestamp), "%H:%M"), 
                                         format(min(chunk_data$timestamp) + (max(chunk_data$timestamp) - min(chunk_data$timestamp)) / 2, "%H:%M"), 
                                         format(max(chunk_data$timestamp), "%H:%M"))) +
        scale_y_continuous(limits = expanded_lat_range) + # Ensure y-axis limits are the same
        scale_x_continuous(limits = expanded_lon_range) +
        geom_point(data = fire_points_light_red, aes(x = fire_longitude, y = fire_latitude), color = "green", shape = 4, size = 2, stroke = 1.4) +
        geom_point(data = fire_points_dark_red, aes(x = fire_longitude, y = fire_latitude), color = "red", shape = 4, size = 3, stroke = 1.6) +
        labs(title = paste("Leon track", format(current_start, "%Y-%m-%d")),
             x = "Longitude",
             y = "Latitude") +
        theme_minimal(base_size = 15) +
        theme(legend.position = "right",
              panel.background = element_rect(fill = "white", color = NA),
              plot.background = element_rect(fill = "white", color = NA),
              legend.key = element_rect(fill = "white", color = NA))
      
      # Set a fixed width and calculate height based on aspect ratio, with a maximum height
      plot_width <- 20
      aspect_ratio <- diff(expanded_lat_range) / diff(expanded_lon_range)
      plot_height <- plot_width * aspect_ratio
      max_height <- 40  # Maximum height to avoid excessive dimensions
      plot_height <- min(plot_height, max_height)
      
      # Save the plot
      file_name <- paste0("leon_track_", format(current_start, "%Y-%m-%d"), ".png")
      file_path <- file.path(output_dir, file_name)
      ggsave(file_path, spatial_plot, width = plot_width, height = plot_height, units = "in", dpi = 150, limitsize = FALSE)
    }
    
    # Move to the next 1-day chunk
    current_start <- current_end
  }
}

# Example usage
output_directory <- "D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event/Leon track plot/1 day plots_2"
generate_plots_for_chunks(data, zoom, output_directory)















# Define the function to create and save plots for 1-day chunks
generate_plots_for_chunks <- function(data, zoom, output_dir) {
  
  # Filter and arrange data where in_range is TRUE
  filtered_data <- data %>%
    filter(in_range == TRUE) %>%
    arrange(timestamp)
  
  # Check if there are valid timestamps
  if (nrow(filtered_data) == 0) {
    stop("No data available where in_range is TRUE.")
  }
  
  # Determine the start and end dates
  start_date <- as.Date(min(filtered_data$timestamp))
  end_date <- as.Date(max(filtered_data$timestamp))
  
  # Generate 1-day chunks
  current_date <- start_date
  while (current_date <= end_date) {
    # Define start and end of the current day
    day_start <- as.POSIXct(paste0(current_date, " 00:00:00"))
    day_end <- as.POSIXct(paste0(current_date, " 23:59:59"))
    
    # Filter data for the current day
    chunk_data <- filtered_data %>%
      filter(timestamp >= day_start & timestamp <= day_end)
    
    if (nrow(chunk_data) > 0) {
      # Extract coordinates from the geometry column
      coords <- st_coordinates(chunk_data)
      
      # Extract fire points
      fire_points <- chunk_data %>%
        filter(!is.na(fire_latitude) & !is.na(fire_longitude)) %>%
        arrange(Nearest_fire_ID, desc(interesting_event)) %>%
        distinct(Nearest_fire_ID, .keep_all = TRUE)
      
      # Convert Dist2Fire to numeric for comparison
      fire_points <- fire_points %>%
        mutate(Dist2Fire_numeric = as.numeric(Dist2Fire))
      
      fire_points_light_red <- fire_points %>% filter(Dist2Fire_numeric > 250)
      fire_points_dark_red <- fire_points %>% filter(Dist2Fire_numeric <= 250)
      
      # Calculate the range for longitudes and latitudes
      lon_range <- range(coords[, "X"], na.rm = TRUE)
      lat_range <- range(coords[, "Y"], na.rm = TRUE)
      
      # Combine and expand ranges with fire points
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
      
      # Create the spatial plot
      spatial_plot <- svap +
        geom_path(data = chunk_data, aes(x = coords[, "X"], y = coords[, "Y"], color = as.POSIXct(timestamp, origin = "1970-01-01")), linewidth = 1.5) +
        scale_color_gradientn(colors = c("#4575b4", "#fee090", "#d73027"), 
                              name = "Timestamp",
                              breaks = c(min(chunk_data$timestamp), 
                                         min(chunk_data$timestamp) + (max(chunk_data$timestamp) - min(chunk_data$timestamp)) / 2, 
                                         max(chunk_data$timestamp)),
                              labels = c(format(min(chunk_data$timestamp), "%H:%M"), 
                                         format(min(chunk_data$timestamp) + (max(chunk_data$timestamp) - min(chunk_data$timestamp)) / 2, "%H:%M"), 
                                         format(max(chunk_data$timestamp), "%H:%M"))) +
        scale_y_continuous(limits = expanded_lat_range) + # Ensure y-axis limits are the same
        scale_x_continuous(limits = expanded_lon_range) +
        geom_point(data = fire_points_light_red, aes(x = fire_longitude, y = fire_latitude), color = "green", shape = 4, size = 2, stroke = 1.4) +
        geom_point(data = fire_points_dark_red, aes(x = fire_longitude, y = fire_latitude), color = "red", shape = 4, size = 3, stroke = 1.6) +
        labs(title = paste("Leon track", format(current_date, "%Y-%m-%d")),
             x = "Longitude",
             y = "Latitude") +
        theme_minimal(base_size = 15) +
        theme(legend.position = "right",
              panel.background = element_rect(fill = "white", color = NA),
              plot.background = element_rect(fill = "white", color = NA),
              legend.key = element_rect(fill = "white", color = NA))
      
      # Set a fixed width and calculate height based on aspect ratio, with a maximum height
      plot_width <- 20
      aspect_ratio <- diff(expanded_lat_range) / diff(expanded_lon_range)
      plot_height <- plot_width * aspect_ratio
      max_height <- 40  # Maximum height to avoid excessive dimensions
      plot_height <- min(plot_height, max_height)
      
      # Save the plot
      file_name <- paste0("leon_track_", format(current_date, "%Y-%m-%d"), ".png")
      file_path <- file.path(output_dir, file_name)
      ggsave(file_path, spatial_plot, width = plot_width, height = plot_height, units = "in", dpi = 150, limitsize = FALSE)
    }
    
    # Move to the next day
    current_date <- current_date + 1
  }
}

# Example usage
output_directory <- "D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event/Leon track plot/1 day plots_2"
generate_plots_for_chunks(data, zoom, output_directory)












# Define the function to create and save plots for 1-day chunks
generate_plots_for_chunks <- function(data, zoom, output_dir) {
  
  # Filter and arrange data where in_range is TRUE
  filtered_data <- data %>%
    filter(in_range == TRUE) %>%
    arrange(timestamp)
  
  # Check if there are valid timestamps
  if (nrow(filtered_data) == 0) {
    stop("No data available where in_range is TRUE.")
  }
  
  # Determine the start and end dates
  start_date <- as.Date(min(filtered_data$timestamp))
  end_date <- as.Date(max(filtered_data$timestamp))
  
  # Define fixed start and end times for the color scale
  fixed_start_time <- as.POSIXct("04:30:00", format = "%H:%M:%S", tz = "UTC")
  fixed_end_time <- as.POSIXct("19:00:00", format = "%H:%M:%S", tz = "UTC")
  
  # Generate 1-day chunks
  current_date <- start_date
  while (current_date <= end_date) {
    # Define start and end of the current day
    day_start <- as.POSIXct(paste0(current_date, " 00:00:00"))
    day_end <- as.POSIXct(paste0(current_date, " 23:59:59"))
    
    # Filter data for the current day
    chunk_data <- filtered_data %>%
      filter(timestamp >= day_start & timestamp <= day_end)
    
    if (nrow(chunk_data) > 0) {
      # Extract coordinates from the geometry column
      coords <- st_coordinates(chunk_data)
      
      # Extract fire points
      fire_points <- chunk_data %>%
        filter(!is.na(fire_latitude) & !is.na(fire_longitude)) %>%
        arrange(Nearest_fire_ID, desc(interesting_event)) %>%
        distinct(Nearest_fire_ID, .keep_all = TRUE)
      
      # Convert Dist2Fire to numeric for comparison
      fire_points <- fire_points %>%
        mutate(Dist2Fire_numeric = as.numeric(Dist2Fire))
      
      fire_points_light_red <- fire_points %>% filter(Dist2Fire_numeric > 250)
      fire_points_dark_red <- fire_points %>% filter(Dist2Fire_numeric <= 250)
      
      # Calculate the range for longitudes and latitudes
      lon_range <- range(coords[, "X"], na.rm = TRUE)
      lat_range <- range(coords[, "Y"], na.rm = TRUE)
      
      # Combine and expand ranges with fire points
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
      
      # Create a new normalized time column within the desired fixed time range
      chunk_data <- chunk_data %>%
        mutate(time_of_day = format(timestamp, "%H:%M:%S"),
               time_of_day = as.POSIXct(time_of_day, format = "%H:%M:%S", tz = "UTC"))
      
      # Ensure the times are within the fixed scale, clipping outside times to the nearest limit
      chunk_data$time_of_day <- pmin(pmax(chunk_data$time_of_day, fixed_start_time), fixed_end_time)
      
      # Create the spatial plot with fixed color scale
      spatial_plot <- svap +
        geom_path(data = chunk_data, aes(x = coords[, "X"], y = coords[, "Y"], color = time_of_day), linewidth = 1.5) +
        
        # Adjust the color scale to have fixed time limits
        scale_color_gradientn(colors = c("#4575b4", "#fee090", "#d73027"), 
                              name = "Timestamp",
                              limits = c(fixed_start_time, fixed_end_time),  # Set fixed time range
                              breaks = c(fixed_start_time, 
                                         fixed_start_time + (fixed_end_time - fixed_start_time) / 2, 
                                         fixed_end_time),
                              labels = c(format(fixed_start_time, "%H:%M"), 
                                         format(fixed_start_time + (fixed_end_time - fixed_start_time) / 2, "%H:%M"), 
                                         format(fixed_end_time, "%H:%M"))) +
        scale_y_continuous(limits = expanded_lat_range) +  # Ensure y-axis limits are the same
        scale_x_continuous(limits = expanded_lon_range) +
        geom_point(data = fire_points_light_red, aes(x = fire_longitude, y = fire_latitude), color = "green", shape = 4, size = 2, stroke = 1.4) +
        geom_point(data = fire_points_dark_red, aes(x = fire_longitude, y = fire_latitude), color = "red", shape = 4, size = 3, stroke = 1.6) +
        labs(title = paste("Leon track", format(current_date, "%Y-%m-%d")),
             x = "Longitude",
             y = "Latitude") +
        theme_minimal(base_size = 15) +
        theme(legend.position = "right",
              panel.background = element_rect(fill = "white", color = NA),
              plot.background = element_rect(fill = "white", color = NA),
              legend.key = element_rect(fill = "white", color = NA))
      
      # Set a fixed width and calculate height based on aspect ratio, with a maximum height
      plot_width <- 20
      aspect_ratio <- diff(expanded_lat_range) / diff(expanded_lon_range)
      plot_height <- plot_width * aspect_ratio
      max_height <- 40  # Maximum height to avoid excessive dimensions
      plot_height <- min(plot_height, max_height)
      
      # Save the plot
      file_name <- paste0("leon_track_", format(current_date, "%Y-%m-%d"), ".png")
      file_path <- file.path(output_dir, file_name)
      ggsave(file_path, spatial_plot, width = plot_width, height = plot_height, units = "in", dpi = 150, limitsize = FALSE)
    }
    
    # Move to the next day
    current_date <- current_date + 1
  }
}

# Example usage
output_directory <- "D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event/Leon track plot/1 day plots_2"
generate_plots_for_chunks(data, zoom, output_directory)




# Define the function to create and save plots for 1-day chunks
generate_plots_for_chunks <- function(data, zoom, output_dir) {
  
  # Filter and arrange data where in_range is TRUE
  filtered_data <- data %>%
    filter(in_range == TRUE) %>%
    arrange(timestamp)
  
  # Check if there are valid timestamps
  if (nrow(filtered_data) == 0) {
    stop("No data available where in_range is TRUE.")
  }
  
  # Determine the start and end dates
  start_date <- as.Date(min(filtered_data$timestamp))
  end_date <- as.Date(max(filtered_data$timestamp))
  
  # Define fixed start and end times for the color scale
  fixed_start_time <- as.POSIXct("04:30:00", format = "%H:%M:%S", tz = "UTC")
  fixed_end_time <- as.POSIXct("19:00:00", format = "%H:%M:%S", tz = "UTC")
  
  # Generate 1-day chunks
  current_date <- start_date
  while (current_date <= end_date) {
    # Define start and end of the current day
    day_start <- as.POSIXct(paste0(current_date, " 00:00:00"))
    day_end <- as.POSIXct(paste0(current_date, " 23:59:59"))
    
    # Filter data for the current day
    chunk_data <- filtered_data %>%
      filter(timestamp >= day_start & timestamp <= day_end)
    
    if (nrow(chunk_data) > 0) {
      # Extract coordinates from the geometry column
      coords <- st_coordinates(chunk_data)
      
      # Extract fire points
      fire_points <- chunk_data %>%
        filter(!is.na(fire_latitude) & !is.na(fire_longitude)) %>%
        arrange(Nearest_fire_ID, desc(interesting_event)) %>%
        distinct(Nearest_fire_ID, .keep_all = TRUE)
      
      # Convert Dist2Fire to numeric for comparison
      fire_points <- fire_points %>%
        mutate(Dist2Fire_numeric = as.numeric(Dist2Fire))
      
      fire_points_light_red <- fire_points %>% filter(Dist2Fire_numeric > 250)
      fire_points_dark_red <- fire_points %>% filter(Dist2Fire_numeric <= 250)
      
      # Calculate the range for longitudes and latitudes
      lon_range <- range(coords[, "X"], na.rm = TRUE)
      lat_range <- range(coords[, "Y"], na.rm = TRUE)
      
      # Combine and expand ranges with fire points
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
      
      # Create a new normalized time column within the desired fixed time range
      chunk_data <- chunk_data %>%
        mutate(time_of_day = format(timestamp, "%H:%M:%S"),
               time_of_day = as.POSIXct(time_of_day, format = "%H:%M:%S", tz = "UTC"))
      
      # Ensure the times are within the fixed scale, clipping outside times to the nearest limit
      chunk_data$time_of_day <- pmin(pmax(chunk_data$time_of_day, fixed_start_time), fixed_end_time)
      
      # Create the spatial plot with fixed color scale and horizontal legend
      spatial_plot <- svap +
        geom_path(data = chunk_data, aes(x = coords[, "X"], y = coords[, "Y"], color = time_of_day), linewidth = 1.5) +
        
        # Adjust the color scale to have fixed time limits and additional breaks
        scale_color_gradientn(colors = c("#4575b4", "#91bfdb", "#fee090", "#d73027", "#543005"),  # Add intermediary colors
                              name = "Timestamp",
                              limits = c(fixed_start_time, fixed_end_time),  # Set fixed time range
                              breaks = c(fixed_start_time, 
                                         fixed_start_time + (fixed_end_time - fixed_start_time) / 4, 
                                         fixed_start_time + (fixed_end_time - fixed_start_time) / 2, 
                                         fixed_start_time + 3 * (fixed_end_time - fixed_start_time) / 4, 
                                         fixed_end_time),
                              labels = c(format(fixed_start_time, "%H:%M"), 
                                         format(fixed_start_time + (fixed_end_time - fixed_start_time) / 4, "%H:%M"), 
                                         format(fixed_start_time + (fixed_end_time - fixed_start_time) / 2, "%H:%M"), 
                                         format(fixed_start_time + 3 * (fixed_end_time - fixed_start_time) / 4, "%H:%M"), 
                                         format(fixed_end_time, "%H:%M")),
                              guide = guide_colorbar(direction = "horizontal", 
                                                     title.position = "top",
                                                     title.hjust = 0.5,
                                                     barwidth = 20, 
                                                     barheight = 1)) +  # Make the legend horizontal
        scale_y_continuous(limits = expanded_lat_range) +  # Ensure y-axis limits are the same
        scale_x_continuous(limits = expanded_lon_range) +
        geom_point(data = fire_points_light_red, aes(x = fire_longitude, y = fire_latitude), color = "green", shape = 4, size = 2, stroke = 1.4) +
        geom_point(data = fire_points_dark_red, aes(x = fire_longitude, y = fire_latitude), color = "red", shape = 4, size = 3, stroke = 1.6) +
        labs(title = paste("Leon track", format(current_date, "%Y-%m-%d")),
             x = "Longitude",
             y = "Latitude") +
        theme_minimal(base_size = 15) +
        theme(legend.position = "bottom",  # Position the legend at the bottom
              panel.background = element_rect(fill = "white", color = NA),
              plot.background = element_rect(fill = "white", color = NA),
              legend.key = element_rect(fill = "white", color = NA))
      
      # Set a fixed width and calculate height based on aspect ratio, with a maximum height
      plot_width <- 20
      aspect_ratio <- diff(expanded_lat_range) / diff(expanded_lon_range)
      plot_height <- plot_width * aspect_ratio
      max_height <- 40  # Maximum height to avoid excessive dimensions
      plot_height <- min(plot_height, max_height)
      
      # Save the plot
      file_name <- paste0("leon_track_", format(current_date, "%Y-%m-%d"), ".png")
      file_path <- file.path(output_dir, file_name)
      ggsave(file_path, spatial_plot, width = plot_width, height = plot_height, units = "in", dpi = 150, limitsize = FALSE)
    }
    
    # Move to the next day
    current_date <- current_date + 1
  }
}

# Example usage
output_directory <- "D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event/Leon track plot/1 day plots_2"
generate_plots_for_chunks(data, zoom, output_directory)



