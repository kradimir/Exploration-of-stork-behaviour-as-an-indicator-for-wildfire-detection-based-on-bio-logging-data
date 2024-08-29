library(dplyr)
library(sf)


##### HERE FOR A SINGLE CSV
##### FOR WHOLE FOLDER SEE BELOW


###if there's an error while running any dataset it means it's empty and should be deleted from folder before running to code
#We need to get combined stork data from the folder into the environment because this is where the upscaled data is
load("D:/Vladimir/Vladimir/FF_STK_dataset_fusion_ONLY_combined_stork_Data_post_2020_environment.RData")

#now we need to get the data with more information (within10_km, etc..) from all_in_one_plus
# Function to load and transform CSV data from All_in_one_plus
load_and_transform_data <- function(file_path) {
  # Read the CSV file
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Convert date-time fields
  data$timestamp <- as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  data$fire_time <- as.POSIXct(data$fire_time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  
  # Convert Dist2Fire to numeric (assuming data is in meters)
  data$Dist2Fire <- as.numeric(data$Dist2Fire)
  
  # Convert TimeDiff2Fire to difftime in seconds
  data$TimeDiff2Fire <- as.difftime(data$TimeDiff2Fire, units = "secs")
  
  # Convert Within_10km and Fire_within_last_hour to logical
  data$Within_10km <- as.logical(data$Within_10km)
  data$Fire_within_last_hour <- as.logical(data$Fire_within_last_hour)
  
  # Convert Dist_to_fire_within_last_hour to numeric
  data$Dist_to_fire_within_last_hour <- as.numeric(data$Dist_to_fire_within_last_hour)
  
  # Return the transformed data
  return(data)
}

# upload the "data"
file_path <- "D:\\Vladimir\\csv_output_All_in_one_plus\\Hourly_overlap_Lusa__eobs_3606_.csv"
data <- load_and_transform_data(file_path)

# Filter combined_stork_data to retain only rows with individual.local.identifier in data
filtered_stork_data <- combined_stork_data %>%
  filter(individual.local.identifier %in% data$individual.local.identifier)

# Select only the relevant columns from data
data_selected_columns <- data %>%
  select(event.id, Dist2Fire, Nearest_fire_ID, fire_longitude, fire_latitude, fire_time, 
         fire_confidence, TimeDiff2Fire, Within_10km, Fire_within_last_hour, Dist_to_fire_within_last_hour)

# Perform the left join to merge data frames on event.id
merged_data <- filtered_stork_data %>%
  left_join(data_selected_columns, by = "event.id") %>%
  mutate(
    Within_10km = ifelse(is.na(Within_10km), FALSE, Within_10km),
    Fire_within_last_hour = ifelse(is.na(Fire_within_last_hour), FALSE, Fire_within_last_hour)
  )

#need to remove geometry (from sf to df) to save into csv properly
# Drop geometry to convert sf object to a regular data frame
merged_data <- st_drop_geometry(merged_data)

# Get the unique individual.local.identifier for the filename
identifier <- unique(merged_data$individual.local.identifier)

# Replace any illegal characters in the filename
identifier <- gsub("[^A-Za-z0-9]", "_", identifier)

# Define the output file path
output_file_path <- paste0("D:/Vladimir/temporally_upscaled_overlap_data/", identifier, ".csv")

# Save the data frame to a CSV file
write.csv(merged_data, output_file_path, row.names = FALSE)


#to upload generated csv (will not be an sf object):
# Path to the new CSV file
new_file_path <- "D:/Vladimir/temporally_upscaled_overlap_data/Olli___DER_A1P74__e_obs_6382_.csv"

# Load and transform the new CSV data
transformed_data <- load_and_transform_data(new_file_path)

######LOOP FOR WHOLE FOLDER
# assuming
#load("D:/Vladimir/Vladimir/FF_STK_dataset_fusion_ONLY_combined_stork_Data_post_2020_environment.RData")
# is done

library(dplyr)
library(sf)

# Function to load and transform CSV data from All_in_one_plus
load_and_transform_data <- function(file_path) {
  # Read the CSV file
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Convert date-time fields
  data$timestamp <- as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  data$fire_time <- as.POSIXct(data$fire_time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  
  # Convert Dist2Fire to numeric (assuming data is in meters)
  data$Dist2Fire <- as.numeric(data$Dist2Fire)
  
  # Convert TimeDiff2Fire to difftime in seconds
  data$TimeDiff2Fire <- as.difftime(data$TimeDiff2Fire, units = "secs")
  
  # Convert Within_10km and Fire_within_last_hour to logical
  data$Within_10km <- as.logical(data$Within_10km)
  data$Fire_within_last_hour <- as.logical(data$Fire_within_last_hour)
  
  # Convert Dist_to_fire_within_last_hour to numeric
  data$Dist_to_fire_within_last_hour <- as.numeric(data$Dist_to_fire_within_last_hour)
  
  # Return the transformed data
  return(data)
}

# Path to the directories
input_directory <- "D:/Vladimir/continuation"
output_directory <- "D:/Vladimir/temporally_upscaled_overlap_data"

# List all CSV files in the input directory
csv_files <- list.files(input_directory, pattern = "\\.csv$", full.names = TRUE)

# Function to process each CSV file
process_csv <- function(file_path) {
  # Load and transform the data
  data <- load_and_transform_data(file_path)
  
  # Filter combined_stork_data to retain only rows with individual.local.identifier in data
  filtered_stork_data <- combined_stork_data %>%
    filter(individual.local.identifier %in% data$individual.local.identifier)
  
  # Select only the relevant columns from data
  data_selected_columns <- data %>%
    select(event.id, Dist2Fire, Nearest_fire_ID, fire_longitude, fire_latitude, fire_time, 
           fire_confidence, TimeDiff2Fire, Within_10km, Fire_within_last_hour, Dist_to_fire_within_last_hour)
  
  # Perform the left join to merge data frames on event.id
  merged_data <- filtered_stork_data %>%
    left_join(data_selected_columns, by = "event.id") %>%
    mutate(
      Within_10km = ifelse(is.na(Within_10km), FALSE, Within_10km),
      Fire_within_last_hour = ifelse(is.na(Fire_within_last_hour), FALSE, Fire_within_last_hour)
    )
  
  # Drop geometry to convert sf object to a regular data frame
  merged_data <- st_drop_geometry(merged_data)
  
  # Get the unique individual.local.identifier for the filename
  identifier <- unique(merged_data$individual.local.identifier)
  
  # Replace any illegal characters in the filename
  identifier <- gsub("[^A-Za-z0-9]", "_", identifier)
  
  # Define the output file path
  output_file_path <- paste0(output_directory, "/", identifier, ".csv")
  
  # Save the data frame to a CSV file
  write.csv(merged_data, output_file_path, row.names = FALSE)
}

# Loop through each CSV file and process it
for (file in csv_files) {
  process_csv(file)
}

# Optional: Verify the structure of the last processed file
str(merged_data)



