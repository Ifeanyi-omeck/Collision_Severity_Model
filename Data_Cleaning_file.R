#Installing the necessary components
library(dplyr)

#Data Exploration and Analysis
#Conducting Data Exploration on the data sets
Accident_last_five <- read.csv('Accident_the_last_five_years.csv')

#Loading Casualties Data
Casualties_last_five <- read.csv('Causualties_for_the_five_years.csv')

#loading Vehicle Data
Vehicle_last_five <- read.csv('Vehicle_for_the_five_years.csv')


# All instances of accidents have unique variables so accidents do not have duplicates
# Find the duplicates in the Casualties Data sets
duplicates_casaulties <- Casualties_last_five[duplicated(Casualties_last_five$accident_index)
                                               | duplicated(Casualties_last_five$accident_index, fromLast = TRUE), ]

# Find duplicates in the Vehicle last five Data sets
duplicates_Vehicles <- Vehicle_last_five[duplicated(Vehicle_last_five$accident_index)
                                         | duplicated(Vehicle_last_five$accident_index, fromLast = TRUE), ]


# Removing unwanted columns from the Accident Data set
# These column variable do not relate to the research questions.
Accident_last_five <- Accident_last_five %>% select( - c('accident_reference', 'location_easting_osgr', 'location_northing_osgr', 
                                                          'longitude', 'latitude', 'local_authority_ons_district', 'local_authority_highway',
                                                          'lsoa_of_accident_location', 'police_force', 'accident_year'))

# Selecting Single Vehicle Collision only
if ("number_of_vehicles" %in% colnames(Accident_last_five)) {
  Accident_last_five <- Accident_last_five %>% filter(number_of_vehicles == 1)
} else {
  # Handle the case when number_of_vehicles column is not present
  print("Error: number_of_vehicles column not found.")
}


# Joining the Vehicle Data set to the Accident Data set
final_join <- Accident_last_five %>% left_join(Vehicle_last_five, by ="accident_index")


# Sort the Casualties severity in descending order
Casualties_last_five <- Casualties_last_five %>% arrange(desc(casualty_severity))


# Then, remove duplicates based on 'accident_index', keeping only the first row (which is the max 'casualty severity
#due to previous sorting)
Casualties_last_five <- Casualties_last_five %>% distinct(accident_index, .keep_all = TRUE)


# Joining the Casualty severity to the Accident Data set
final_join <- final_join %>% left_join(Casualties_last_five, by = "accident_index")


# Function to check for duplicate rows in a data set
check_for_duplicates <- function(dataset) {
  # Check for duplicates
  duplicates <- duplicated(dataset)
  
  # If there are any TRUE values in duplicates, return TRUE. Else, return FALSE.
  if(any(duplicates)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#Checking for duplicates in the data frame
check_for_duplicates(final_join$accident_index)


# cleaning the data set for Vehicle and Dropping some columns
# These columns are irrelevant to the research questions
final_join <- final_join %>% select( -c('accident_reference.x', 'accident_reference.y','age_band_of_driver', 'propulsion_code', 'generic_make_model',
                                         'driver_imd_decile',  'driver_home_area_type', 'lsoa_of_driver', 
                                        'date', 'day_of_week', 'did_police_officer_attend_scene_of_accident','accident_year.x',
                                         'accident_index', 'number_of_vehicles', 'time', 'accident_year.y', 'casualty_reference',
                                        'vehicle_reference.y', 'vehicle_reference.x'))



# write a function that loops through the entire data set and removes negative values(we don't need negative values since
# all of the encoding are positive)
final_join <- final_join[rowSums(final_join < 0) == 0, ]


# Drop columns not listed on the STATS19 Document
# Drop Vehicle reference cause it counts the Number of vehicle in the Collision 
final_join <- final_join %>% select( - c('trunk_road_flag', 'local_authority_district','first_road_class', 'first_road_number', 'second_road_class', 'second_road_number',
                                           'lsoa_of_casualty', 'casualty_home_area_type', 'casualty_imd_decile', 'casualty_type'))



# Removing intersection related columns due to the complexities of Intersection relating Accidents
final_join <- final_join %>% select(-c( 'junction_control', 'junction_detail','junction_location'))



# Exclude Pedestrian related Collision as the injury severity differs
final_join <- final_join[final_join$pedestrian_crossing_human_control == 0 & 
                           final_join$pedestrian_crossing_physical_facilities == 0, ]

# Since we excluded pedestrian related Collision Severity we drop the columns
# Excluding pedestrain related columns in the casualty data set
final_join <- final_join %>% select( -c('pedestrian_crossing_human_control', 'pedestrian_crossing_physical_facilities', 
                                         'pedestrian_location', 'pedestrian_movement', 'pedestrian_road_maintenance_worker'))


# Set Towing and Articulation = 0, to Remove Large Vehicle due the complexities relating to size
final_join <- final_join %>% filter(final_join$towing_and_articulation == 0)

# Drop Column Engine size due to Missing Data regards Vehicle registration, Also Drop Direction To and FRO as
# This related to Compass direction has little relevance to the research Questions
# Dropping Towing and articulation as this does not fit the specifics of the vehicle class in the Research questions
final_join <- final_join %>% select( -c('vehicle_left_hand_drive', 'engine_capacity_cc',
                                        'vehicle_direction_to', 'vehicle_direction_from', 
                                        'number_of_casualties','towing_and_articulation' ))




# Using the pearson Coefficient to check for Multi-collinearity between variables in the data set.
correlation_matrix <- cor(final_join, use = "pairwise.complete.obs")

# Set diagonal to 0 to avoid self-pairing
diag(correlation_matrix) <- 0

# Create a matrix of the same shape as 'correlation_matrix' with 'TRUE' values above 70% absolute correlation
high_correlation_matrix <- abs(correlation_matrix) > 0.65

# As correlation matrix is symmetrical, remove duplicate pairs
high_correlation_matrix[lower.tri(high_correlation_matrix)] <- FALSE

# Extract variables with high correlation
high_correlation_pairs <- which(high_correlation_matrix, arr.ind = TRUE)


# print correlated Variables
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
  print("No pairs of variables with absolute correlation over 65% were found.")
}


# Removing highly correlated items from the data set so we don't have bias estimates in the model
#set the following columns to 0 and drop the from the data set since they are slightly not relevant
final_join <- final_join[final_join$hit_object_off_carriageway == 0 & 
                           final_join$vehicle_leaving_carriageway == 0, ]

#Drop correlated columns
final_join <- final_join %>% select(-c('hit_object_off_carriageway', 'vehicle_leaving_carriageway',
                                        'accident_severity', 'age_band_of_casualty'))


# creating a copy of the dataset
final_copy <- final_join
write.csv(final_copy, 'dataset_for_visualization.csv', row.names = FALSE)

#Drop casualty class from the data frame as this represent casualties associated with pedestrian,
#Escooters, Electric Bicycles etc
final_join <- final_join %>% select( -c('casualty_class'))

# Data Cleaning(Removing wrong encoding from the data set 
# i.e number which do not match the description  in STATS19 AND STATS20)
delete_specific_rows <- function(df) {
  df <- subset(df, !(  road_surface_conditions == 9 |
                       special_conditions_at_site == 9|
                       carriageway_hazards == 9 |
                       urban_or_rural_area == 3 |
                       vehicle_manoeuvre == 99 |
                       vehicle_location_restricted_lane == 99 |
                       skidding_and_overturning == 9 |
                       first_point_of_impact == 9 |
                       hit_object_in_carriageway == 99|
                       car_passenger == 9))
  return(df)
}


# Deleting encoding which do not appear on the stats19 and stats 20 data set.
final_join <- delete_specific_rows(final_join)


# Excluding variables that are not essential for the final data modeling.
# Variables such as 'Unknown Weather' and 'Unknown Light Conditions' are excluded
# because they do not contribute meaningful information for the model.
delete_non_meaningful_encoding <- function(data){
  data <- subset(data, !(hit_object_in_carriageway == 9 |  # Excluding  roundabouts related crash.
                         weather_conditions == 9 |         #Unknown weather conditions.
                         sex_of_driver == 3 |             # Excluding value which represents unknown sex.
                         road_type == 1  |                #Excluding roundabout related roads.
                         road_type == 9                 #Excluding accidents on unclassified road.
                           ))
}


final_join <- delete_non_meaningful_encoding(final_join)


# Removing non conventional Vehicle types
# Define a vector of the vehicle values to be removed
values_to_remove <- c(20, 21, 98, 02, 03, 04, 05, 97, 23, 01, 17, 16, 22, 18, 90)

# Subset the data frame to only include rows where 'vehicle_type' is not in 'values_to_remove'
final_join <- final_join[!(final_join$vehicle_type %in% values_to_remove), ]


# Selecting only WET and Dry for Road Surface Conditions
final_join <- final_join[!(final_join$road_surface_conditions %in% c(3,4,5)), ]


#Restrict Age of Vehicle to be Maximum 24 years because of vehicle safety technology
final_join <- final_join[final_join$age_of_vehicle <= 24, ]

# Dropped this variable due small values of passangers in the dataset
final_join <- final_join %>% select( -('bus_or_coach_passenger'))


# Create a new column for the re coded severity values
final_join$recoded_severity <- final_join$casualty_severity

# Recode the values in the new column
final_join$recoded_severity[final_join$casualty_severity == 1] <- 2 # Fatal accidents
final_join$recoded_severity[final_join$casualty_severity == 2] <- 1 # Serious accidents
final_join$recoded_severity[final_join$casualty_severity == 3] <- 0 # Minor accident which are zero inflated.


#Drop vehicle severity Original column
final_join <- final_join %>% select(- ('casualty_severity'))

# Modify Vehicle Manoeuvre encodings
final_join$vehicle_manoeuvre <- case_when(
  final_join$vehicle_manoeuvre %in% c(7, 8, 9, 10) ~ 1,         # Grouped as Turning
  final_join$vehicle_manoeuvre %in% c(16, 17) ~ 2,              # Grouped as  Bending left or right     
  final_join$vehicle_manoeuvre == 18 ~ 3,                       # Going ahead 
  final_join$vehicle_manoeuvre %in% c(11, 12, 13, 14, 15) ~ 4,  # Grouped as Overtaking/Changing lanes
  final_join$vehicle_manoeuvre %in% c(3, 4, 5) ~ 5,             # Grouped as Moving slowly
  final_join$vehicle_manoeuvre %in% c(1, 2, 6) ~ 6,             # Grouped as Others
  TRUE ~ final_join$vehicle_manoeuvre
)


# Change speed_limit values below 40 to 0 and above 40 to 1
final_join$speed_limit[final_join$speed_limit < 40] <- 0        # Named as  Build up < 40mph
final_join$speed_limit[final_join$speed_limit >= 40] <- 1       # Named as non-Build up > 40mph

#light_conditions == 7 | Darkness Unknown to be grouped into light conditions
# Change values of light_conditions 5, 6, 7 to 3
final_join$light_conditions[final_join$light_conditions == 4] <- 2            # Grouped as Darkness with light
final_join$light_conditions[final_join$light_conditions %in% c(5, 6, 7)] <- 3 # Grouped as Total Darkness 


# Change values of weather_conditions
final_join$weather_conditions <- case_when(
  final_join$weather_conditions %in% c(2,3) ~ 2,      # Grouped as rain/snow without high winds     
  final_join$weather_conditions == 4 ~ 3,             # Name fine with winds
  final_join$weather_conditions %in% c(5, 6) ~ 4,     # Grouped as Rain/Snow with winds
  final_join$weather_conditions %in% c(7, 8, 9) ~ 5,  # Grouped as Adverse weather conditions
  TRUE ~ final_join$weather_conditions
)


# Change special_conditions_at_site values 
final_join$special_conditions_at_site <- case_when(
  final_join$special_conditions_at_site == 4 ~ 1,           # Named Road works present
  final_join$special_conditions_at_site %in% c(1, 2,3,5, 6, 7) ~ 2, #  Debris/Defective road.
  TRUE ~ final_join$special_conditions_at_site
)


# Change carriageway_hazards values above 0 to 1
# carriage hazard is split into two categories Present or None
final_join$carriageway_hazards[final_join$carriageway_hazards > 0] <- 1 #Present


# Change vehicle_location_restricted_lane values above 0 to 1
# Split into Main carriage and not on main carriage
final_join$vehicle_location_restricted_lane[final_join$vehicle_location_restricted_lane > 0] <- 1 

# Change skidding_and_overturning values 3 to 1, 5, 4 to 2
final_join$skidding_and_overturning[final_join$skidding_and_overturning %in% c(1,3)] <- 1    #Grouped as Skidded
final_join$skidding_and_overturning[final_join$skidding_and_overturning %in% c(2,5, 4)] <- 2 #Grouped as Overturned

# Change hit_object_in_carriageway values above 0 to 1
# Objects in carriageway are split in none and present
final_join$hit_object_in_carriageway[final_join$hit_object_in_carriageway > 0] <- 1  #Present     

# Change journey_purpose_of_driver values 5, 6 to 0, 1, 2 to 1, 3, 4 to 3
final_join$journey_purpose_of_driver <- case_when(
  final_join$journey_purpose_of_driver %in% c(5, 6) ~ 0,  # Grouped as Others
  final_join$journey_purpose_of_driver %in% c(1, 2) ~ 1,  # Grouped as worked related
  final_join$journey_purpose_of_driver %in% c(3, 4) ~ 2,  # Grouped as School related
  TRUE ~ final_join$journey_purpose_of_driver
)


# Change age_of_driver values < 17 to 0, 17-24 to 2, 25-40 to 3, 41-65 to 4, 65 and above to 5
final_join$age_of_driver <- case_when(
  final_join$age_of_driver < 17 ~ 0,                                   # Grouped as less than 17 years old
  final_join$age_of_driver >= 17 & final_join$age_of_driver <= 24 ~ 1, # Grouped as  17 - 24 years
  final_join$age_of_driver >= 25 & final_join$age_of_driver <= 40 ~ 2, # Grouped as 25 - 40 years
  final_join$age_of_driver >= 41 & final_join$age_of_driver <= 65 ~ 3, # Grouped as 41 - 65 years
  final_join$age_of_driver > 65 ~ 4,                                   # Grouped as 65 years and above
  TRUE ~ final_join$age_of_driver
)

# Modify Values for age of Vehicle
final_join$age_of_vehicle <- case_when(
  final_join$age_of_vehicle < 3 ~ 0,                                   # Grouped as less than 3 years old
  final_join$age_of_vehicle >= 3 & final_join$age_of_vehicle <= 5 ~ 1, # Grouped as 3-5 years old
  final_join$age_of_vehicle >= 6 & final_join$age_of_vehicle <= 10 ~ 2,# Grouped as 6 - 10 years old
  final_join$age_of_vehicle >= 11 & final_join$age_of_vehicle <= 15 ~ 3,# Grouped as 11 - 15 years old
  final_join$age_of_vehicle > 15 ~ 4,                                   # 16 years and above
  TRUE ~ final_join$age_of_vehicle
)


# Change age_of_casualty values < 17 to 0, 17-24 to 2, 25-40 to 3, 41-65 to 4, 65 and above to 5
final_join$age_of_casualty <- case_when(
  final_join$age_of_casualty < 17 ~ 0,                                   # Grouped as less than 17 years old
  final_join$age_of_casualty >= 17 & final_join$age_of_casualty <= 24 ~ 1, # Grouped as  17 - 24 years
  final_join$age_of_casualty >= 25 & final_join$age_of_casualty<= 40 ~ 2, # Grouped as 25 - 40 years
  final_join$age_of_casualty >= 41 & final_join$age_of_casualty <= 65 ~ 3, # Grouped as 41 - 65 years
  final_join$age_of_casualty > 65 ~ 4,                                   # Grouped as 65 years and above
  TRUE ~ final_join$age_of_casualty
)


# modifying  the vehicle_type column
final_join$vehicle_type <- case_when(
  final_join$vehicle_type %in% c(8,9) ~ 0, # Grouped as passenger car(Car / Taxi)
  final_join$vehicle_type  %in% c(10, 11) ~ 1,  # Named as  Bus (Mini Bus/Coach bus)
  final_join$vehicle_type == 19 ~ 2,   # Van under 3.5 tonnes
  TRUE ~ final_join$vehicle_type
)


#Save and Export final Data set
write.csv(final_join, 'final_dataset.csv', row.names = FALSE)