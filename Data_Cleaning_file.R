#Installing necessary library
library(dplyr)

# Loading data
Accident_last_five <- read.csv('Accident_the_last_five_years.csv')
Casualties_last_five <- read.csv('Causualties_for_the_five_years.csv')
Vehicle_last_five <- read.csv('Vehicle_for_the_five_years.csv')


# Identifying duplicates
duplicates_casualties <- Casualties_last_five[duplicated(Casualties_last_five$accident_index) | duplicated(Casualties_last_five$accident_index, fromLast = TRUE), ]
duplicates_Vehicles <- Vehicle_last_five[duplicated(Vehicle_last_five$accident_index) | duplicated(Vehicle_last_five$accident_index, fromLast = TRUE), ]


# Data cleaning for Accident_last_five 
Accident_last_five <- Accident_last_five %>% select( - c('accident_reference', 'location_easting_osgr', 'location_northing_osgr', 
                                                         'longitude', 'latitude', 'local_authority_ons_district', 'local_authority_highway',
                                                         'lsoa_of_accident_location', 'police_force', 'accident_year'))

# Filter for Single Vehicle Collision.
if ("number_of_vehicles" %in% colnames(Accident_last_five)) {
  Accident_last_five <- Accident_last_five %>% filter(number_of_vehicles == 1)
} else {
  print("Error: number_of_vehicles column not found.")
}

# Joining Accident and Vehicle data sets
final_join <- Accident_last_five %>% left_join(Vehicle_last_five, by ="accident_index")


# Sorting and removing duplicates for Casualties data set
# Selecting the Maximum casualty for each accident reference and selecting only the maximum casualty in the
# duplicates
Casualties_last_five <- Casualties_last_five %>% arrange(desc(casualty_severity)) %>% distinct(accident_index, .keep_all = TRUE)

# Joining Casualty severity to the Accident Data set
final_join <- final_join %>% left_join(Casualties_last_five, by = "accident_index")

# Function to check for duplicate rows
check_for_duplicates <- function(dataset) {
  if(any(duplicated(dataset))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Check for duplicates
check_for_duplicates(final_join$accident_index)

# More data cleaning
final_join <- final_join %>% select( -c('accident_reference.x', 'accident_reference.y','age_band_of_driver', 'propulsion_code', 'generic_make_model',
                                        'driver_imd_decile',  'driver_home_area_type', 'lsoa_of_driver', 
                                        'date', 'day_of_week', 'did_police_officer_attend_scene_of_accident','accident_year.x',
                                        'accident_index', 'number_of_vehicles', 'time', 'accident_year.y', 'casualty_reference',
                                        'vehicle_reference.y', 'vehicle_reference.x'))

# Removing negative values from the dataset
final_join <- final_join[rowSums(final_join < 0) == 0, ]

# More column exclusion
final_join <- final_join %>% select( - c('trunk_road_flag', 'local_authority_district','first_road_class', 'first_road_number', 'second_road_class',
                                         'lsoa_of_casualty', 'casualty_home_area_type', 'casualty_imd_decile', 'casualty_type', 'second_road_number'))

# Excluding junctions to remove bias and complexities in the model
final_join <- final_join %>% select(-c( 'junction_control', 'junction_detail','junction_location'))

# Excluding pedestrian related collisions to remove bias from the model
final_join <- final_join %>% select( -c('pedestrian_crossing_human_control', 'pedestrian_crossing_physical_facilities', 
                                        'pedestrian_location', 'pedestrian_movement', 'pedestrian_road_maintenance_worker'))

# Excluding variables Not Relevant to the research Question
final_join <- final_join %>% select( -c('vehicle_left_hand_drive', 'engine_capacity_cc',
                                        'vehicle_direction_to', 'vehicle_direction_from', 
                                        'number_of_casualties','towing_and_articulation' ))

# Checking for multi collinearity
correlation_matrix <- cor(final_join, use = "pairwise.complete.obs")
diag(correlation_matrix) <- 0
high_correlation_matrix <- abs(correlation_matrix) > 0.7
high_correlation_matrix[lower.tri(high_correlation_matrix)] <- FALSE
high_correlation_pairs <- which(high_correlation_matrix, arr.ind = TRUE)


# Print correlated variables
if(nrow(high_correlation_pairs) > 0) {
  print(paste("Variables with absolute correlation"))
  for(i in 1:nrow(high_correlation_pairs)) {
    print(paste0(
      names(final_join)[high_correlation_pairs[i, 1]], 
      " and ", 
      names(final_join)[high_correlation_pairs[i, 2]], 
      " with correlation of ", 
      round(correlation_matrix[high_correlation_pairs[i, 1], high_correlation_pairs[i, 2]], 2) * 100, 
      "%"
    ))
  }
} else {
  print("No pairs of variables with absolute correlation over 70% were found.")
}

# Removing highly correlated items
final_join <- final_join %>% select(-c('hit_object_off_carriageway', 'vehicle_leaving_carriageway',
                                       'accident_severity', 'age_band_of_casualty', 'vehicle_location_restricted_lane',
                                        'hit_object_in_carriageway', 'vehicle_manoeuvre'))


# Further column exclusion
final_join <- final_join %>% select( -c('casualty_class'))


# Data cleaning function
# Removing Encoding which does not exist on the stats19 data set
delete_specific_rows <- function(df) {
  df <- subset(df, !(  road_surface_conditions == 9 |
                         special_conditions_at_site == 9|
                         carriageway_hazards == 9 |
                         urban_or_rural_area == 3 |
                         skidding_and_overturning == 9 |
                         first_point_of_impact == 9 |
                         car_passenger == 9))
  return(df)
}

# Applying function
final_join <- delete_specific_rows(final_join)

# Function to delete non-meaningful encoding
delete_non_meaningful_encoding <- function(data){
  data <- subset(data, !( weather_conditions == 9 | # Removed Unknown weather conditions
                           sex_of_driver == 3 |     # Removed Unknown sex type 
                           road_type == 1  |        # Removed roundabout to avoid bias and complexities in the model
                           road_type == 9))         # Removed unknown road types
  return(data)
}

# Applying function
final_join <- delete_non_meaningful_encoding(final_join)


# Removing non-conventional Vehicle types
values_to_remove <- c(20, 21, 98, 02, 03, 04, 05, 97, 23, 01, 17, 16, 22, 18, 90, 11)
final_join <- final_join[!(final_join$vehicle_type %in% values_to_remove), ]

# Selecting only WET and DRY for Road Surface Conditions
final_join <- final_join[!(final_join$road_surface_conditions %in% c(3, 4, 5)), ]

# Restricting Age of Vehicle to a Maximum of 24 years due to vehicle safety technology
final_join <- final_join[final_join$age_of_vehicle <= 24, ]

# Dropping the 'bus_or_coach_passenger' variable due to small values of passengers in the dataset
final_join <- final_join %>% select(-('bus_or_coach_passenger'))

# Creating a new column 'recoded_severity' for recoded severity values
final_join$recoded_severity <- final_join$casualty_severity

# Recoding the values in the 'recoded_severity' column
final_join <- final_join %>%
  mutate(recoded_severity = case_when(
    casualty_severity == 1 ~ 2,  # Fatal accidents
    casualty_severity == 2 ~ 1,  # Serious accidents
    casualty_severity == 3 ~ 0   # Minor accidents
  ))

# Dropping the original 'casualty_severity' column
final_join <- final_join %>% select(-('casualty_severity'))


# Changing 'speed_limit' values below 40 to 0 and above 40 to 1
final_join$speed_limit[final_join$speed_limit < 40] <- 0  # Named as "Build up < 40mph"
final_join$speed_limit[final_join$speed_limit >= 40] <- 1 # Named as "Non-Build up > 40mph"

# Encoding 'sex_of_driver': 0 for Male Drivers, 1 for Female Drivers
final_join$sex_of_driver[final_join$sex_of_driver == 1] <- 0
final_join$sex_of_driver[final_join$sex_of_driver == 2] <- 1  # Female driver 1, Other wise 0


# Grouping 'car_passenger' (Front and Rear seat) to compensate for the small size of the variable in the data set
final_join$car_passenger[final_join$car_passenger > 0] <- 1  # Car passenger 1 Otherwise 0


# Binary Encoding 'urban_or_rural_area': 1 for Urban Areas, 0 for Rural Areas
final_join$urban_or_rural_area[final_join$urban_or_rural_area == 1] <- 0 
final_join$urban_or_rural_area[final_join$urban_or_rural_area == 2] <- 1 # Grouped as 1 rural, Otherwise 0


# Binary Encoding 'sex_of_casualty': 0 for Male casualties, 1 for Female casualties
final_join$sex_of_casualty[final_join$sex_of_casualty == 1] <- 0
final_join$sex_of_casualty[final_join$sex_of_casualty == 2] <- 1 # Grouped as 1 female, Otherwise Male


# Binary Encoding 'road_surface_conditions': 1 for Wet Road surface conditions, 0 for Dry Road surface conditions
final_join$road_surface_conditions[final_join$road_surface_conditions == 1] <- 0
final_join$road_surface_conditions[final_join$road_surface_conditions == 2] <- 1  # Grouped 1 wet Otherwise Dry


# Changing 'carriageway_hazards' values above 0 to 1
final_join$carriageway_hazards[final_join$carriageway_hazards > 0] <- 1  # All Hazards present grouped as 1, 0 otherwise


# Modifying 'age_of_vehicle' values: 0 for 0 to 10 years old, 1 for above 10 years old
final_join$age_of_vehicle <- case_when(
  final_join$age_of_vehicle <= 10 ~ 0,
  TRUE ~ 1
)

# Changing 'special_conditions_at_site' values: 1 for Debris/Defective road/works present, 0 for others
final_join$special_conditions_at_site <- case_when(
  final_join$special_conditions_at_site %in% c(1, 2, 3, 5, 4, 6, 7) ~ 1,
  TRUE ~ final_join$special_conditions_at_site
)


# Changing 'journey_purpose_of_driver' values: 0 for Others, 1 for Work-related
final_join$journey_purpose_of_driver <- case_when(
  final_join$journey_purpose_of_driver %in% c(3, 4, 5, 6) ~ 0,
  final_join$journey_purpose_of_driver %in% c(1, 2) ~ 1,
  TRUE ~ final_join$journey_purpose_of_driver
)


# Creating Indicators for Point of impact 1 if impacted Other wise Zero.
final_join$first_point_of_impact <- case_when(
  final_join$first_point_of_impact %in% c(1,2, 3, 4) ~ 1,
  TRUE ~ 0
)


# Creating Indicators for Road Type: 0 for Others (Slip road and one-way street), 1 for Carriageway
final_join$road_type <- case_when(
  final_join$road_type %in% c(7, 2) ~ 0,
  final_join$road_type %in% c(3, 6) ~ 1,
  TRUE ~ final_join$road_type
)

# Creating Indicators for Vehicle Type: 0 for Car/Taxis, 1 for Vans and Buses
final_join$vehicle_type <- case_when(
  final_join$vehicle_type %in% c(8, 9) ~ 0,
  final_join$vehicle_type %in% c(10, 19) ~ 1,
  TRUE ~ final_join$vehicle_type
)


# Creating indicator variable for Weather: 1 for Fine Weather, 0 for Others
final_join$weather_conditions <- case_when(
  final_join$weather_conditions %in% c(2, 3, 5, 6, 7, 8) ~ 0,
  final_join$weather_conditions %in% c(1, 4) ~ 1,
  TRUE ~ final_join$weather_conditions
)


# Creating indicator variable for Skidding and Overturning: 1 for Skidded/Overturned/Jackknifed, 0 for Others
final_join$skidding_and_overturning <- case_when(
  final_join$skidding_and_overturning %in% c(1, 2, 3, 4, 5) ~ 1,
  TRUE ~ final_join$skidding_and_overturning
)


# Create indicator variables for age of driver
final_join$age_of_driver <- case_when(
  final_join$age_of_driver <= 25 ~ 1, # Grouped as less than 25 1, Otherwise 0
  TRUE ~ 0
)


final_join$age_of_casualty <- case_when(
  final_join$age_of_casualty <= 30 ~ 1,   # Grouped as less than 30 1, Otherwise 0
  TRUE ~ 0
)


# Checking for multi-Collinearity again
correlation_matrix <- cor(final_join, use = "pairwise.complete.obs")
diag(correlation_matrix) <- 0
high_correlation_matrix <- abs(correlation_matrix) > 0.7
high_correlation_matrix[lower.tri(high_correlation_matrix)] <- FALSE
high_correlation_pairs <- which(high_correlation_matrix, arr.ind = TRUE)

# Test for potential correlation after creating indicator variables
if(nrow(high_correlation_pairs) > 0) {
  print(paste("Variables with absolute correlation"))
  for(i in 1:nrow(high_correlation_pairs)) {
    print(paste0(
      names(final_join)[high_correlation_pairs[i, 1]], 
      " and ", 
      names(final_join)[high_correlation_pairs[i, 2]], 
      " with correlation of ", 
      round(correlation_matrix[high_correlation_pairs[i, 1], high_correlation_pairs[i, 2]], 2) * 100, 
      "%"
    ))
  }
} else {
  print("No pairs of variables with absolute correlation over 70% were found.")
}

# Group the data set into Daylight, Darkness lit and Darkness
# This is done to measure the effect of lighting conditions on injury severity.
final_join$light_conditions <- case_when(
  final_join$light_conditions == 1 ~ 0,            # Grouped as Day Light
  final_join$light_conditions == 4 ~ 1,            # Grouped as Darkness lit
  final_join$light_conditions %in% c(5,6,7) ~ 2,   # Grouped as Darkness
  TRUE ~ final_join$light_conditions
)


# creating a copy of the data set for tables and visualization
final_copy <- final_join
write.csv(final_copy, 'dataset_for_visualization.csv', row.names = FALSE) 


# To capture possible variations in the determinants of injury severity of single-vehicle accidents
# 3 combination of lighting conditions was considered.
# Daylight, Darkness but lit and Darkness no street light
dataset_day_light <- final_join %>% filter(light_conditions == 0)
dataset_darkness_lit <- final_join %>% filter(light_conditions == 1)
dataset_darkness_no_street_light <- final_join %>% filter(light_conditions == 2)


# Saving the variations of the files.
write.csv(dataset_day_light, 'final_dataset_day_light.csv', row.names = FALSE)
write.csv(dataset_darkness_lit, 'final_dataset_darkness_lit.csv', row.names = FALSE)
write.csv(dataset_darkness_no_street_light, 'final_dataset_darkness.csv', row.names = FALSE)