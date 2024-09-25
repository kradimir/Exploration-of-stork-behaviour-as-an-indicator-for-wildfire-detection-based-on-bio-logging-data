#plotting speed over time for each day
#for the days in Leon actogram from when interesting event 


#we need to use the df "All_locs_no_gaps_good_birddates", which is calculated when doing an actogram, here we need the speed
# in All_locs_no_gaps_good_birddates we will use "Speed" column
#but before that we have the select the data we want to use
# I need data from 2020-01-01 to 2020-01-16 
#within this timeframe I want for each day to have a distribution of the bird Speed (Y axis) in respect to the time (X axis)
# I also want all the plot on the same image

#this is to define the speed of the bird (extracted from atrogram code)

All_locs_no_gaps <- All_locs_no_gaps %>% distinct(individual.local.identifier, timestamp, .keep_all = TRUE)


# Calculate time differences between consecutive records to identify movement intervals.
All_locs_no_gaps <- All_locs_no_gaps %>%
  arrange(individual.local.identifier, timestamp) %>%  # Sort data by animal ID and timestamp.
  group_by(individual.local.identifier) %>%  # Group data by each animal.
  mutate(Time_diff = c(diff(as.numeric(timestamp)), NA))  # Calculate time differences between consecutive points.

# Exclude records that are less than 2.5 minutes apart to reduce noise and focus on significant movements.
All_locs_no_gaps <- All_locs_no_gaps %>%
  filter(if_else(row_number() == 1, TRUE, Time_diff/60 >= 2.5))  # Keep the first record and those spaced by at least 2.5 minutes.

# Enhance data with date information, next point IDs, and decision flags for keeping records based on day boundaries and unique IDs.
All_locs_no_gaps$date <- format(All_locs_no_gaps$timestamp, format='%Y-%m-%d')  # Add formatted date column.
All_locs_no_gaps <- All_locs_no_gaps %>%
  mutate(
    Next_ID = lead(individual.local.identifier),  # Identify the next record's ID to check for day boundaries.
    Next_Date = lead(date),  # Identify the next record's date.
    Keep = if_else(individual.local.identifier == Next_ID & date == Next_Date, TRUE, FALSE)  # Flag records to keep based on same day and ID.
  )


# Convert sf object to regular dataframe for distance calculation can be reverted lower
All_locs_no_gaps_df <- as.data.frame(All_locs_no_gaps)

# This snippet transforms the spatial dataset into a regular dataframe to calculate distances between consecutive points and the speed of movement for each record flagged for inclusion. 
# It specifically addresses records marked 'Keep', calculating the direct distance to the next point, the time difference between records, and deriving the speed of movement. 
# This conversion from a spatial to a regular dataframe facilitates the use of non-spatial distance calculation methods, 
# allowing for the handling of spatial data in scenarios where spatial functionality is not required for immediate calculations.

All_locs_no_gaps_df$Dist_to_next <- if_else(
  All_locs_no_gaps_df$Keep,
  c(sp::spDists(as.matrix(All_locs_no_gaps_df[c("location.long", "location.lat")]), longlat = TRUE, segments = TRUE), NA),
  NA
)

All_locs_no_gaps_df$Time_diff <- if_else(
  All_locs_no_gaps_df$Keep,
  c(diff(as.numeric(All_locs_no_gaps_df$timestamp)), NA),
  NA
)

All_locs_no_gaps_df$Speed <- if_else(
  !is.na(All_locs_no_gaps_df$Time_diff) & All_locs_no_gaps_df$Time_diff > 0,
  All_locs_no_gaps_df$Dist_to_next / All_locs_no_gaps_df$Time_diff,
  NA
)


tmp_5 <- All_locs_no_gaps_df %>%
  group_by(date) %>%
  summarize(
    n = n(),
    p_less_than_5 = sum(Time_diff < 7.5 * 300, na.rm = TRUE) / n
  )


All_locs_no_gaps_good_birddates <- All_locs_no_gaps_df %>%
  inner_join(tmp_5, by = c("date")) %>%
  filter(p_less_than_5 > 0.85) %>%                            ####this is the thing to change 
  select(-p_less_than_5, -n)


# Identify indices where there is a significant movement (distance greater than 30 meters and not NA)
Moved_Index<-which(!is.na(All_locs_no_gaps_good_birddates$Dist_to_next) & All_locs_no_gaps_good_birddates$Dist_to_next>0.03) # over 30 meters.. 
length(Moved_Index) # 3312601 movements

# Create a logical vector indicating where significant movements occur
Movement_logical<-!is.na(All_locs_no_gaps_good_birddates$Dist_to_next) &All_locs_no_gaps_good_birddates$Dist_to_next>0.03

# Convert the logical vector to numeric for use with coldiffs in Rfast
Movement_logical_num<-as.numeric(Movement_logical)

#install.packages('Rfast')
library(Rfast)

# Identify the start of a movement
All_starts<-which(coldiffs(cbind(c(0,Movement_logical_num),c(Movement_logical_num, 0)))==1)

# Identify the end of a movement
#All_starts<-which(apply(cbind(c(0,Movement_logical_num),c(Movement_logical_num, 0)), 1, diff)==1)
All_ends<-which(coldiffs(cbind(c(0,Movement_logical_num),c(Movement_logical_num, 0)))==-1)

# Create a dataframe summarizing each movement with start/end time, location, and IDs
Movements<-data.frame(
  Individual=All_locs_no_gaps_good_birddates$individual.local.identifier[All_starts],
  Start.time=All_locs_no_gaps_good_birddates$timestamp[All_starts],
  Start.id=All_starts,
  End.time=All_locs_no_gaps_good_birddates$timestamp[All_ends],
  End.id=All_ends,
  Duration=All_locs_no_gaps_good_birddates$timestamp[All_ends]-All_locs_no_gaps_good_birddates$timestamp[All_starts],
  Start_long=All_locs_no_gaps_good_birddates$location.long[All_starts],
  Start_lat=All_locs_no_gaps_good_birddates$location.lat[All_starts]
)

# Initialize distance for each movement to NAhttp://127.0.0.1:43529/graphics/plot_zoom_png?width=2560&height=1377
Movements$Distance=NA
# Calculate total distance for each movement period (start to end)
Movements$Distance<-apply(Movements, 1, FUN=function(x) sum(All_locs_no_gaps_good_birddates$Dist_to_next[x[3]:(as.numeric(x[5])-1)]))

# Convert start and end times to Julian dates for easier handling
Movements$Start.jdate<-as.Date(Movements$Start.time)
Movements$End.jdate<-as.Date(Movements$End.time)
Movements_all<- Movements

# Save the detailed movements data for further analysis
str(Movements_all) 

# Visualize start locations of movements to understand spatial patterns
plot(Movements_all$Start_lat~Movements_all$Start_long)

# Calculate the speed of each movement in meters per minute
Movements_all$Speed<-Movements_all$Distance/as.numeric(Movements_all$Duration)*60


# Load necessary libraries
library(ggplot2)
library(dplyr)


#try another method
# Function to expand the data
expand_data <- function(df) {
  expanded_df <- df %>%
    rowwise() %>%
    do(data.frame(
      Time = seq.POSIXt(from = .$Start.time, to = .$End.time, by = "min"),
      Speed = .$Speed,
      date = as.Date(.$Start.time),
      Individual = .$Individual
    ))
  return(expanded_df)
}

# Expand the Movements_all data
expanded_movements <- expand_data(Movements_all)

# Filter data for the specified date range
filtered_movements_speed <- expanded_movements %>%
  filter(Time >= as.POSIXct("2020-01-01") & Time <= as.POSIXct("2020-01-16"))

# Plot the speed distribution for each day
ggplot(filtered_movements_speed, aes(x = Time, y = Speed, group = date)) +
  geom_col(alpha = 0.7) + 
  facet_wrap(~ date, scales = "free_x") +
  labs(title = "Bird Speed Distribution from 2020-01-01 to 2020-01-16",
       x = "Time",
       y = "Speed (m/s)") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hours", minor_breaks = "1 hour") +
  coord_cartesian(ylim = c(0, 12)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#to save the plot
# Create the plot
plot <- ggplot(filtered_movements_speed, aes(x = Time, y = Speed, group = date)) +
  geom_col(alpha = 0.7) + 
  facet_wrap(~ date, scales = "free_x") +
  labs(title = "Bird Speed Distribution from 2020-01-01 to 2020-01-16",
       x = "Time",
       y = "Speed (m/s)") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hours", minor_breaks = "1 hour") +
  coord_cartesian(ylim = c(0, 12)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "white", color = NA),  # Set plot background to white
    panel.background = element_rect(fill = "white", color = NA), # Set panel background to white
    strip.background = element_rect(fill = "white", color = NA)  # Set facet label background to white
  )

# Specify the file path
file_path <- "D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event/Leon speed over time/Leon speed over time.png"

# Save the plot with high DPI
ggsave(filename = file_path, plot = plot, dpi = 200, width = 24, height = 14, bg = "white")



