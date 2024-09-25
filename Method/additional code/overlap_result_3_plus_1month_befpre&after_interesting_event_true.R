#to extand on the actograms I need to take the narrow data of: 
# "Bruno 9705 E0661" on 2022-11-02 14:00:22 
#and:
# "Leon HL452 (3064)" on 2020-01-08 11:00:06
#and extand to months before and after the event. once we have more data we could run a Lévy walks code.

#usual package list:
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

# ### 
#1st: upload big dataset:
#We need to get combined stork data from the folder into the environment because this is where the upscaled data is
load("D:/Vladimir/Vladimir/FF_STK_dataset_fusion_ONLY_combined_stork_Data_post_2020_environment.RData")

#load bird in question want are interesting in

file_path <- "D:/Vladimir/Narrowed down overlap analysis on few events/overlap analysis result_3/Overlap_Bruno_9705_E0661.csv"

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

# then first for Bruno we need to get the data and make sure the interesting_event is only the one we want "2022-11-02 14:00:22" :
# this is manually hard coded to only select the exact row we want to be the center of the data we will later extract around

# Set the entire interesting_event column to FALSE
data$interesting_event <- FALSE

# Set the specific row with the given timestamp and event.id to TRUE
data[data$timestamp == as.POSIXct("2022-11-02 14:00:22", format="%Y-%m-%d %H:%M:%S") & 
       data$event.id == 24229643461 & row.names(data) == 169, "interesting_event"] <- TRUE


#check
# Check the specific row to ensure the change was made
data[169, ]

# Optionally, check the entire column to ensure all other values are FALSE
table(data$interesting_event)

# now we merge both dataset
# uisng interesting_event from data as a temporal center we want to add data from combined_stork_data.
# some rows in combined_stork_data already exist in data (in data there's more columns and we want to keep these)
# we add the new rows up to "X" month before interesting event

# Filter combined_stork_data for the relevant individual and time range
# Filter for the same individual.local.identifier
filtered_combined_stork_data <- combined_stork_data %>%
  filter(individual.local.identifier %in% data$individual.local.identifier)

# Define the time range
interesting_event_time <- data %>% filter(interesting_event == TRUE) %>% select(timestamp)
start_time <- min(interesting_event_time$timestamp) - months(6)
end_time <- max(interesting_event_time$timestamp) + months(6)

# Filter the combined_stork_data for the time range and drop geometry
filtered_combined_stork_data <- filtered_combined_stork_data %>%
  filter(timestamp >= start_time & timestamp <= end_time) %>%
  st_drop_geometry()

#Merge the datasets while avoiding duplicates:
# Remove the geometry column from filtered_combined_stork_data
filtered_combined_stork_data <- filtered_combined_stork_data %>%
  select(event.id, timestamp, location.long, location.lat, individual.local.identifier)

# Merge the datasets
merged_data <- bind_rows(data, filtered_combined_stork_data)

# Remove duplicates based on event.id, keeping the rows from `data`
merged_data <- merged_data %>%
  group_by(event.id) %>%
  filter(row_number() == 1) %>%
  ungroup()

merged_data <- as.data.frame(merged_data)

# Get the unique individual.local.identifier for the filename
identifier <- unique(merged_data$individual.local.identifier)

# Replace any illegal characters in the filename
identifier <- gsub("[^A-Za-z0-9]", "_", identifier)

# Define the output directory
output_directory <- "D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event"

# Define the output file path
output_file_path <- paste0(output_directory, "/", identifier, ".csv")

# Save the data frame to a CSV file
write.csv(merged_data, file = output_file_path, row.names = FALSE)



#now make sure to also do this for:

file_path <- "D:/Vladimir/Narrowed down overlap analysis on few events/overlap analysis result_3/Overlap_Leon_HL452__3064_.csv"

data <- load_and_transform_data(file_path)
#timestamp: "2020-01-08 11:00:06"

# Set the entire interesting_event column to FALSE
data$interesting_event <- FALSE

# Set the specific row with the given timestamp and event.id to TRUE
data[data$timestamp == as.POSIXct("2020-01-08 11:00:06", format="%Y-%m-%d %H:%M:%S") & 
       data$event.id == 15467545217 & row.names(data) == 920, "interesting_event"] <- TRUE

table(data$interesting_event)

# then repeat the rest as above:

# Filter combined_stork_data for the relevant individual and time range
# Filter for the same individual.local.identifier
filtered_combined_stork_data <- combined_stork_data %>%
  filter(individual.local.identifier %in% data$individual.local.identifier)

# Define the time range
interesting_event_time <- data %>% filter(interesting_event == TRUE) %>% select(timestamp)
start_time <- min(interesting_event_time$timestamp) - months(6)
end_time <- max(interesting_event_time$timestamp) + months(6)

# Filter the combined_stork_data for the time range and drop geometry
filtered_combined_stork_data <- filtered_combined_stork_data %>%
  filter(timestamp >= start_time & timestamp <= end_time) %>%
  st_drop_geometry()

#Merge the datasets while avoiding duplicates:
# Remove the geometry column from filtered_combined_stork_data
filtered_combined_stork_data <- filtered_combined_stork_data %>%
  select(event.id, timestamp, location.long, location.lat, individual.local.identifier)

# Merge the datasets
merged_data <- bind_rows(data, filtered_combined_stork_data)

# Remove duplicates based on event.id, keeping the rows from `data`
merged_data <- merged_data %>%
  group_by(event.id) %>%
  filter(row_number() == 1) %>%
  ungroup()

merged_data <- as.data.frame(merged_data)

# Get the unique individual.local.identifier for the filename
identifier <- unique(merged_data$individual.local.identifier)

# Replace any illegal characters in the filename
identifier <- gsub("[^A-Za-z0-9]", "_", identifier)

# Define the output directory
output_directory <- "D:/Vladimir/Narrowed down overlap analysis on few events/interesting_event_TRUE_csv_using_narrow_data_wit_extanded_time_around_event"

# Define the output file path
output_file_path <- paste0(output_directory, "/", identifier, ".csv")

# Save the data frame to a CSV file
write.csv(merged_data, file = output_file_path, row.names = FALSE)

