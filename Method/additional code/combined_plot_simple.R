library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(ggmap)
library(patchwork)

combined_plot_simple <- function(data, zoom) {
  
  # Filter and arrange data where in_range is TRUE
  filtered_data <- data %>%
    filter(in_range == TRUE) %>%
    arrange(timestamp)
  
  # Check if there are valid timestamps
  if(nrow(filtered_data) == 0) {
    stop("No data available where in_range is TRUE.")
  }
  
  # Extract the first and last dates from the filtered data
  first_date <- format(min(filtered_data$timestamp), "%d-%m-%Y")
  last_date <- format(max(filtered_data$timestamp), "%d-%m-%Y")
  
  # Extract coordinates from the geometry column
  coords <- st_coordinates(filtered_data)
  
  # Calculate the range for longitudes and latitudes
  lon_range <- range(coords[, "X"], na.rm = TRUE)
  lat_range <- range(coords[, "Y"], na.rm = TRUE)
  
  lon_margin <- diff(lon_range) * 0.1
  lat_margin <- diff(lat_range) * 0.1
  
  expanded_lon_range <- lon_range + c(-lon_margin, lon_margin)
  expanded_lat_range <- lat_range + c(-lat_margin, lat_margin)
  
  mean_lat <- mean(expanded_lat_range)
  mean_long <- mean(expanded_lon_range)
  
  # Get and plot the map
  map <- get_map(location = c(lon = mean_long, lat = mean_lat), maptype = "satellite", zoom = zoom)
  svap <- ggmap(map)
  
  # Extract fire points
  fire_points <- filtered_data %>%
    filter(!is.na(fire_latitude) & !is.na(fire_longitude)) %>%
    distinct(Nearest_fire_ID, .keep_all = TRUE)
  
  # Convert Dist2Fire to numeric for comparison
  fire_points <- fire_points %>%
    mutate(Dist2Fire_numeric = as.numeric(Dist2Fire))
  
  fire_points_light_red <- fire_points %>% filter(Dist2Fire_numeric > 250)
  fire_points_dark_red <- fire_points %>% filter(Dist2Fire_numeric <= 250)
  
  # Create the spatial plot
  spatial_plot <- svap +
    geom_path(data = filtered_data, aes(x = coords[, "X"], y = coords[, "Y"], color = as.POSIXct(timestamp, origin = "1970-01-01")), linewidth = 1.5) +
    scale_color_gradientn(colors = c("#4575b4", "#fee090", "#d73027"), name = "Timestamp") +
    scale_y_continuous(limits = expanded_lat_range) + # Ensure y-axis limits are the same
    geom_point(data = fire_points_light_red, aes(x = fire_longitude, y = fire_latitude), color = "green", shape = 4, size = 2, stroke = 1.4) +
    geom_point(data = fire_points_dark_red, aes(x = fire_longitude, y = fire_latitude), color = "red", shape = 4, size = 3, stroke = 1.6) +
    labs(title = "Movement Track",
         x = "Longitude",
         y = "Latitude") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Create the latitude plot
  latitude_plot <- ggplot(filtered_data, aes(x = as.POSIXct(timestamp, origin = "1970-01-01"), y = coords[, "Y"], color = as.POSIXct(timestamp, origin = "1970-01-01"))) +
    geom_line(size = 2) +  # Adjust the size of the line to be thicker
    scale_color_gradientn(colors = c("#4575b4", "#fee090", "#d73027"), name = "Timestamp") +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d") +
    scale_y_continuous(limits = expanded_lat_range) + # Ensure y-axis limits are the same
    geom_vline(xintercept = as.numeric(seq(min(filtered_data$timestamp), max(filtered_data$timestamp), by = "1 day")), linetype = "dashed", color = "grey") +
    geom_point(data = fire_points_light_red, aes(x = as.POSIXct(timestamp, origin = "1970-01-01"), y = fire_latitude), color = "green", shape = 4, size = 3, stroke = 1.2) +
    geom_point(data = fire_points_dark_red, aes(x = as.POSIXct(timestamp, origin = "1970-01-01"), y = fire_latitude), color = "red", shape = 4, size = 5, stroke = 1.3) +
    labs(title = "Latitude over Time",
         x = "Timestamp",
         y = "") +  # Remove y-axis label
    theme_minimal() +
    theme(aspect.ratio = 1,
          plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8),
          legend.position = "none",
          axis.title.y = element_blank(),  # Remove y-axis title
          axis.text.y = element_blank(),  # Remove y-axis text
          axis.ticks.y = element_blank())  # Remove y-axis ticks
  
  # Create the combined plot with dynamic title
  combined_plot <- spatial_plot + latitude_plot +
    plot_annotation(
      title = paste("Leon track between", first_date, "and", last_date),
      theme = theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 10)
      )
    ) & theme(plot.margin = margin(0, 0, 0, 0))
  
  print(combined_plot)
}

# Example usage
combined_plot_simple(data, zoom)
