#Full actogram rundown
#insert data from
#D:\Vladimir\Narrowed down overlap analysis on few events\overlap analysis result_2

# 2 selected events
#Bruno
#file_path <- "D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event/Bruno_9705_E0661.csv.csv"

#use this one below for Bruno
#file_path <- "D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event/Bruno_9705_E0661_2.csv.csv"


#file_path <- "D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event/Leon_HL452__3064_.csv.csv"
#use this one below for Leon
file_path <- "D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event/Leon_HL452__3064_2.csv.csv"


# Define the function to read and transform a CSV file
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
  
  # Recreate the geometry column for sf
  #data <- st_as_sf(data, coords = c("location.long", "location.lat"), crs = 4326)
  
  return(data)
}

data <- load_and_transform_data(file_path)
data <- data %>% mutate(
  interesting_event = ifelse(is.na(interesting_event), FALSE, interesting_event),
  in_range = ifelse(is.na(in_range), FALSE, in_range),
  Within_1km = ifelse(is.na(Within_1km), FALSE, Within_1km)
)

#delete all interesting event to then add the one we want to focus on
#data$interesting_event <- FALSE


#for large timeframe (100 to 20 days) we want less events (less messy), see "interval" within the loop 
data[data$timestamp == as.POSIXct("2020-01-08 11:00:06", format="%Y-%m-%d %H:%M:%S") & 
       data$event.id == 15467545217 & row.names(data) == 920, "interesting_event"] <- TRUE

#these dates where manually added after looking at larger timeframe actrogram to then focus on smaller timeframes. 
#Per interesting day  (days where many overlap happened) I filtered for: within_1km = TRUE, dist2fire is the smallest
#this is for when plot with smaller timeframe to keep different days in the center of the actrogram
# Set the specific row with the given timestamp and event.id to TRUE
#data[data$timestamp %in% as.POSIXct(c("2020-01-03 15:15:06", "2020-01-05 13:20:06", "2020-01-06 14:40:06", "2020-01-07 07:50:08", "2020-01-08 11:00:06", "2020-01-10 13:10:06", "2020-01-11 10:55:07", "2020-01-12 13:15:07", "2020-01-13 07:15:07"), format="%Y-%m-%d %H:%M:%S"), "interesting_event"] <- TRUE
table(data$interesting_event)




# Assuming 'data' contains data with 'individual.local.identifier' and 'timestamp'
unique_animals <- unique(data$individual.local.identifier)

# Initialize an empty dataframe to store resampled data without gaps
All_locs_no_gaps <- data.frame()

# Set up a parallel cluster to utilize multiple CPU cores for faster processing
# Detect the number of cores available and reserve one core for system processes to avoid overloading the system
num_cores <- detectCores() - 1 # Leave one core free for system processes
cl <- makeCluster(num_cores)


# Use tryCatch to handle errors during the parallel processing
tryCatch({
  # Loop through each unique animal identifier
  for (animal in unique_animals) {
    # Print the current animal being processed for tracking progress
    cat(animal, '....\n')
    
    # Filter data for the current animal
    Cur_locs <- filter(data, individual.local.identifier == animal)
    
    # Adjust the timestamp of interesting events to the nearest 5-minute interval
    Cur_locs$timestamp[Cur_locs$interesting_event] <- round_date(Cur_locs$timestamp[Cur_locs$interesting_event], unit = "5 minutes")
    
    # Remove rows with NA timestamps
    Cur_locs <- Cur_locs %>% filter(!is.na(timestamp))
    
    # Store the total number of rows before resampling for comparison
    rows_total <- nrow(Cur_locs)
    
    # Generate a sequence of 5-minute intervals between the first and last timestamp of the current animal's data
    All_5_minute_intervals <- seq(from=min(Cur_locs$timestamp), to=max(Cur_locs$timestamp), by='5 min')
    
    # Convert timestamps to numeric format for easier interval calculation
    all_timestamps <- as.numeric(Cur_locs$timestamp)
    
    # Calculate new rounded intervals for each timestamp to the nearest 5 minutes
    all_intervals <- (all_timestamps + 150) %/% 300 * 300
    # Remove duplicate intervals to avoid processing the same interval multiple times
    all_intervals_unique <- unique(all_intervals)
    
    # Export the necessary variables to each worker in the cluster for independent processing
    clusterExport(cl, varlist=c("all_intervals", "all_intervals_unique", "all_timestamps", "Cur_locs"), envir=environment())
    
    # Use parallel processing to find indices of the closest timestamps to each unique interval
    Indices_to_use <- parSapply(cl, all_intervals_unique, function(x) {
      # Identify timestamps closest to the current interval
      Ind <- which(all_intervals == x)
      # If multiple timestamps are equally close, choose the one with the smallest absolute difference
      if (length(Ind) > 1) Ind[which.min(abs(all_timestamps[Ind] - x))]
      else Ind
    })
    
    # Check for and prevent duplicates after resampling
    if (any(duplicated(Indices_to_use))) stop("Duplicates detected after resampling.")
    
    # Append the resampled data for the current animal to the main dataframe
    All_locs_no_gaps <- rbind(All_locs_no_gaps, Cur_locs[Indices_to_use, ])
    
    # Debugging: Check the structure of Cur_locs and All_locs_no_gaps
    cat('Structure of Cur_locs:\n')
    str(Cur_locs)
    cat('Structure of All_locs_no_gaps after rbind:\n')
    str(All_locs_no_gaps)
    
    # Report how many rows were removed during the resampling process
    cat('Deleted ', rows_total - length(Indices_to_use), ' from ', rows_total, '; ')
  }
}, error=function(e) {
  # If an error occurs, print the error message
  cat("Error during processing:", e$message, "\n")
})

# After processing all animals, stop the cluster to free up system resources
stopCluster(cl)

# Check the structure of All_locs_no_gaps before distinct operation
cat('Final structure of All_locs_no_gaps:\n')
str(All_locs_no_gaps)



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
  filter(p_less_than_5 > 0.85) %>%
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

# Initialize distance for each movement to NA
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

individuals <- unique(All_locs_no_gaps_good_birddates$individual.local.identifier)

library(ggplot2)
library(gridExtra)

for (cur_animal in individuals) {
  cat("Processing: ", cur_animal, '\n')
  
  # Clean the cur_animal string
  safe_animal_name <- gsub("[^[:alnum:] ]", "_", cur_animal)
  
  Dataset_bird_cur <- All_locs_no_gaps_good_birddates %>%
    filter(individual.local.identifier == cur_animal) %>%
    select(location.long, location.lat, timestamp, Within_1km, interesting_event, Dist2Fire) %>%
    drop_na(timestamp)
  
  Dataset_bird_cur$timestamp <- as.POSIXct(Dataset_bird_cur$timestamp, tz='UTC')
  
  # Calculate sunrise and sunset times
  sunrise_results <- sunriset(
    crds = cbind(Dataset_bird_cur$location.long, Dataset_bird_cur$location.lat),
    dateTime = Dataset_bird_cur$timestamp,
    direction = "sunrise",
    POSIXct.out = TRUE
  )
  Dataset_bird_cur$sunrise_time <- sunrise_results$time
  
  sunset_results <- sunriset(
    crds = cbind(Dataset_bird_cur$location.long, Dataset_bird_cur$location.lat),
    dateTime = Dataset_bird_cur$timestamp,
    direction = "sunset",
    POSIXct.out = TRUE
  )
  Dataset_bird_cur$sunset_time <- sunset_results$time
  
  Dataset_bird_cur$year_day <- paste0(format(Dataset_bird_cur$timestamp, format='%Y'), '.', format(Dataset_bird_cur$timestamp, format='%j'))
  
  # Identify interesting events and create x-day windows
  interesting_events <- Dataset_bird_cur %>%
    filter(interesting_event == TRUE)
  
  for (i in 1:nrow(interesting_events)) {
    
    #change this to change the scale temporal of the actogram
    #define interval
    interval <- 50
    
    event_time <- interesting_events$timestamp[i]
    start_time <- event_time - days(interval)
    end_time <- event_time + days(interval)
    
    Dataset_bird_event <- Dataset_bird_cur %>%
      filter(timestamp >= start_time & timestamp <= end_time)
    
    # Calculate closest sunrises and sunsets
    closest_sunrises <- Dataset_bird_event %>%
      group_by(year_day) %>%
      mutate(time_diff = abs(as.numeric(sunrise_time - timestamp))) %>%
      filter(row_number() == which.min(time_diff)) %>%
      pull(sunrise_time)
    
    closest_sunsets <- Dataset_bird_event %>%
      group_by(year_day) %>%
      mutate(time_diff = abs(as.numeric(sunset_time - timestamp))) %>%
      filter(row_number() == which.min(time_diff)) %>%
      pull(sunset_time)
    
    cur_time <- Dataset_bird_event$timestamp
    cur_time_start <- as.POSIXct(Movements_all$Start.time[Movements_all$Individual == cur_animal], tz='UTC')
    cur_time_end <- as.POSIXct(Movements_all$End.time[Movements_all$Individual == cur_animal], tz='UTC')
    cur_speeds <- Movements_all$Speed[Movements_all$Individual == cur_animal]
    
    # Check if cur_time is empty
    if (length(cur_time) == 0) {
      cat("cur_time is empty for animal:", cur_animal, "\n")
      next
    }
    
    # Check if cur_time_start and cur_time_end are valid
    if (length(cur_time_start) == 0 || length(cur_time_end) == 0) {
      cat("cur_time_start or cur_time_end is empty for animal:", cur_animal, "\n")
      next
    }
    
    # Define the filename for the base R plot
    plot_filename <- paste0("D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event/actrogram_test_leon_", interval, "days_", safe_animal_name, "_", format(event_time, "%Y-%m-%d_%H-%M-%S"), "test", ".jpg")
    
    # Open the JPG device with higher resolution for base R plot
    jpeg(filename = plot_filename, width = 3200, height = 2400, res = 300)
    
    par(mfrow = c(2, 1), mar = c(0, 4, 1, 1))
    
    plot(c(min(as.numeric(format(cur_time_start, format='%H'))) - 1, max(as.numeric(format(cur_time_start, format='%H'))) + 2) ~ range(cur_time), pch = '.', cex = 2, col = 'blue', 
         main = paste(cur_animal, " Event at ", event_time, " with timeframe = ", 2 * interval, " days"), type = 'n', ylab = 'day hour', xlab = 'Time')
    
    abline(v = seq(as.POSIXct('2012/01/01'), as.POSIXct('2020/01/01'), by = 'month'), col = grey(0.9), lwd = 1)
    abline(h = c(0:24), col = grey(0.9), lwd = 1)
    
    Speed_Colors <- colorRampPalette(rev(c('#d53e4f', '#fc8d59', '#ffff73', '#abdda4', '#2b83ba')), bias = 1)
    
    Breaks <- c(min(Movements_all$Speed, na.rm = TRUE), quantile(Movements_all$Speed, seq(0.1, 0.9, by = 0.1), na.rm = TRUE), max(Movements_all$Speed, na.rm = TRUE))
    
    segments(y0 = as.numeric(format(cur_time_start, format='%H')) + as.numeric(format(cur_time_start, format='%M')) / 60, y1 = as.numeric(format(cur_time_end, format='%H')) + as.numeric(format(cur_time_end, format='%M')) / 60, x0 = cur_time_start, x1 = cur_time_end, 
             pch = '2', lwd = 9, col = Speed_Colors(length(Breaks) - 1)[findInterval(cur_speeds, Breaks)])
    
    Dataset_bird_event$Dist2Fire_numeric <- as.numeric(Dataset_bird_event$Dist2Fire)
    fire_point_size <- 1 / Dataset_bird_event$Dist2Fire_numeric * 80
    
    points(as.numeric(format(Dataset_bird_event$timestamp[Dataset_bird_event$Within_1km == TRUE], format='%H')) + 
             as.numeric(format(Dataset_bird_event$timestamp[Dataset_bird_event$Within_1km == TRUE], format='%M')) / 60 ~ 
             Dataset_bird_event$timestamp[Dataset_bird_event$Within_1km == TRUE], 
           pch = 20, 
           col = 'black', 
           cex = 0.1)
    
    points(as.numeric(format(interesting_events$timestamp, format='%H')) + as.numeric(format(interesting_events$timestamp, format='%M')) / 60 ~ 
             interesting_events$timestamp, 
           pch = 4, 
           col = 'red', 
           cex = 0.5, 
           lwd = 1)
    
    lines(as.numeric(format(closest_sunrises, format='%H')) + as.numeric(format(closest_sunrises, format='%M')) / 60 ~ closest_sunrises, 
          col = 'brown', lwd = 2)
    lines(as.numeric(format(closest_sunsets, format='%H')) + as.numeric(format(closest_sunsets, format='%M')) / 60 ~ closest_sunsets, 
          col = 'brown', lwd = 2)
    
    dev.off()
    
    # Define the filename for the ggplot2 plot
    plot_filename_ggplot <- paste0("D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event/", interval, "days_", safe_animal_name, "_", format(event_time, "%Y-%m-%d_%H-%M-%S"), ".jpg")
    
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
  }
}
