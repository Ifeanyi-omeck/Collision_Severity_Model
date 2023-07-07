#Installing the necessary components
library(dplyr)
library(corrplot)

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
Accident_last_five <- Accident_last_five %>% select( - c('accident_reference', 'location_easting_osgr', 'location_northing_osgr', 
                                                          'longitude', 'latitude', 'local_authority_ons_district', 'local_authority_highway',
                                                          'lsoa_of_accident_location', 'police_force', 'accident_year'))

# Selecting Single Vehicle Collision
if ("number_of_vehicles" %in% colnames(Accident_last_five)) {
  Accident_last_five <- Accident_last_five %>% filter(number_of_vehicles == 1)
} else {
  # Handle the case when number_of_vehicles column is not present
  print("Error: number_of_vehicles column not found.")
}

# Joining the Vehicle Data set to the Accident Data set
final_join <- Accident_last_five %>% left_join(Vehicle_last_five, by ="accident_index")

# cleaning the data set for Vehicle and Dropping some columns
final_join <- final_join %>% select( -c('accident_reference', 'age_band_of_driver', 'propulsion_code', 'generic_make_model',
                                         'driver_imd_decile', 'driver_imd_decile', 'driver_home_area_type', 'lsoa_of_driver',
                                        'date', 'day_of_week', 'did_police_officer_attend_scene_of_accident', 'accident_year',
                                         'accident_index', 'number_of_vehicles', 'time'))



# write a function that loops through the entire data set and removes negative values(we don't need negative values since
# all of the encoding are positive)
final_join <- final_join[rowSums(final_join < 0) == 0, ]

# Drop columns not listed on the STATS19 Document
# Drop Vehicle reference cause it counts the Number of vehicle in the Collision
final_join <- final_join %>% select( - c('trunk_road_flag', 'vehicle_reference', 'local_authority_district',
                                          'first_road_class', 'first_road_number', 'second_road_class', 'second_road_number'))



# Data Cleaning(Removing wrong encoding from the data set i.e number which do not match the description  in STATS19 AND STATS20)
delete_specific_rows <- function(df) {
  df <- subset(df, !(  junction_detail == 99 |
                       junction_control == 9 |
                       pedestrian_crossing_human_control == 9 |
                       pedestrian_crossing_physical_facilities == 9 |
                       light_conditions %in% c(2,3,8,9) |
                       road_surface_conditions == 9 |
                       carriageway_hazards == 9 |
                       urban_or_rural_area == 3 |
                       vehicle_type %in% c(23, 97, 98) |
                       towing_and_articulation == 9 |
                       vehicle_manoeuvre == 99 |
                       vehicle_location_restricted_lane == 99 |
                       junction_location == 9 |
                       skidding_and_overturning == 9 |
                       hit_object_in_carriageway == 9 |
                       first_point_of_impact == 9 |
                       sex_of_driver == 3 |
                       light_conditions == 7 |
                       weather_conditions == 9 |
                       special_conditions_at_site == 9 |
                       hit_object_in_carriageway == 99 |
                       vehicle_left_hand_drive == 9))
  return(df)
}

final_join <- delete_specific_rows(final_join)

# Removing intersection relation columns due to the complexities of Intersection relating Accidents
final_join <- final_join %>% select(-c( 'junction_control', 'junction_detail','junction_location'))

# Removing accidents at Roundabouts( Intersections and Unclassified Roads) Due to complexities
final_join <- final_join[!(final_join$road_type %in% c(1, 9)), ]


# Exclude Pedestrian related Collision as the injury severity differs
final_join <- final_join[final_join$pedestrian_crossing_human_control == 0 & 
                               final_join$pedestrian_crossing_physical_facilities == 0, ]

# Since we excluded pedestrian related Collision Severity we drop the columns
final_join <- final_join %>% select( -c('pedestrian_crossing_human_control', 'pedestrian_crossing_physical_facilities'))


# Removing non conventional Vehicle types
# Define a vector of the vehicle values to be removed
values_to_remove <- c(20, 21, 98, 02, 03, 04, 05, 97, 23, 01, 17, 16, 22, 18, 90, 11)

# Subset the data frame to only include rows where 'vehicle_type' is not in 'values_to_remove'
final_join <- final_join[!(final_join$vehicle_type %in% values_to_remove), ]


# Remove hit_object_in_carriageway Involving  animals as collision severity differs and form 
# Minor part of the data
final_join <- final_join[!(final_join$carriageway_hazards %in% c(6,7)), ]

# Remove Carriage Hazard Involving Pedestrian and animal as this only forms a minor part of the data
final_join <- final_join[!(final_join$carriageway_hazards %in% c(6,7)), ]

# Set Towing and Articulation = 0, to Remove Large Vehicle due the complexities relating to size
final_join <- final_join %>% filter(final_join$towing_and_articulation == 0)

# Drop Column Engine size due to Missing Data regards Vehicle registration, Also Drop Direction To and FRO as
# This related to Compass direction has little relevance to the research Questions
final_join <- final_join %>% select( -c('vehicle_left_hand_drive', 'engine_capacity_cc',
                                        'vehicle_direction_to', 'vehicle_direction_from', 
                                        'number_of_casualties','towing_and_articulation' ))


# set the following columns to 0 and drop them due to irrelevance to the research Question
final_join <- final_join[final_join$hit_object_off_carriageway == 0 & 
                           final_join$vehicle_leaving_carriageway == 0, ]

final_join <- final_join %>% select( -c('hit_object_off_carriageway', 'vehicle_leaving_carriageway'))


#Selecting only WET and Dry for Road Surface Conditions
final_join <- final_join[!(final_join$road_surface_conditions %in% c(3,4,5)), ]


#Restrict Age of Vehicle to be Maximum 24 years and 
final_join <- final_join[final_join$age_of_vehicle <= 24, ]


# Using the pearson Coefficient to check for Multi-collinearity between variables in the data set.
correlation_matrix <- cor(final_join, use = "pairwise.complete.obs")

# Open a new window for the plot with specified width and height
dev.new(width = 10, height = 10)

# Set diagonal to 0 to avoid self-pairing
diag(correlation_matrix) <- 0

# Create a matrix of the same shape as 'correlation_matrix' with 'TRUE' values above 70% absolute correlation
high_correlation_matrix <- abs(correlation_matrix) > 0.5

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
  print("No pairs of variables with absolute correlation over 50% were found.")
}

# categorizing and Grouping Variables for Modelling in the Zero inflated Ordered probit Model
# Change Value 3 in accident_severity column to 0
final_join$accident_severity[final_join$accident_severity == 3] <- 0

# Modify Vehicle Manoeuvre encodings
final_join$vehicle_manoeuvre <- case_when(
  final_join$vehicle_manoeuvre %in% c(7, 8, 9, 10) ~ 1,
  final_join$vehicle_manoeuvre %in% c(16, 17) ~ 2,
  final_join$vehicle_manoeuvre == 18 ~ 3,
  final_join$vehicle_manoeuvre %in% c(11, 12, 13, 14, 15) ~ 4,
  final_join$vehicle_manoeuvre %in% c(3, 4) ~ 5,
  final_join$vehicle_manoeuvre %in% c(1, 2) ~ 6,
  TRUE ~ final_join$vehicle_manoeuvre
)


# Change speed_limit values below 40 to 0 and above 40 to 1
final_join$speed_limit[final_join$speed_limit < 40] <- 0
final_join$speed_limit[final_join$speed_limit >= 40] <- 1

# Change values of light_conditions 5, 6, 7 to 3
final_join$light_conditions[final_join$light_conditions %in% c(5, 6, 7)] <- 3

# Change values of weather_conditions
final_join$weather_conditions <- case_when(
  final_join$weather_conditions == 3 ~ 2,
  final_join$weather_conditions == 4 ~ 3,
  final_join$weather_conditions %in% c(5, 6) ~ 4,
  final_join$weather_conditions %in% c(7, 8, 9) ~ 5,
  TRUE ~ final_join$weather_conditions
)


# Change special_conditions_at_site values 
final_join$special_conditions_at_site <- case_when(
  final_join$special_conditions_at_site %in% c(2, 3) ~ 1,
  final_join$special_conditions_at_site == 4 ~ 2,
  final_join$special_conditions_at_site == 5 ~ 3,
  final_join$special_conditions_at_site %in% c(6, 7) ~ 4,
  TRUE ~ final_join$special_conditions_at_site
)


# Change carriageway_hazards values above 0 to 1
final_join$carriageway_hazards[final_join$carriageway_hazards > 0] <- 1


# Change vehicle_location_restricted_lane values above 0 to 1
final_join$vehicle_location_restricted_lane[final_join$vehicle_location_restricted_lane > 0] <- 1

# Change skidding_and_overturning values 3 to 1, 5, 4 to 2
final_join$skidding_and_overturning[final_join$skidding_and_overturning == 3] <- 1
final_join$skidding_and_overturning[final_join$skidding_and_overturning %in% c(5, 4)] <- 2

# Change hit_object_in_carriageway values above 0 to 1
final_join$hit_object_in_carriageway[final_join$hit_object_in_carriageway > 0] <- 1

# Change journey_purpose_of_driver values 5, 6 to 0, 1, 2 to 1, 3, 4 to 3
final_join$journey_purpose_of_driver <- case_when(
  final_join$journey_purpose_of_driver %in% c(5, 6) ~ 0,
  final_join$journey_purpose_of_driver %in% c(1, 2) ~ 1,
  final_join$journey_purpose_of_driver %in% c(3, 4) ~ 2,
  TRUE ~ final_join$journey_purpose_of_driver
)


# Change age_of_driver values < 17 to 0, 17-24 to 2, 25-40 to 3, 41-65 to 4, 65 and above to 5
final_join$age_of_driver <- case_when(
  final_join$age_of_driver < 17 ~ 0,
  final_join$age_of_driver >= 17 & final_join$age_of_driver <= 24 ~ 1,
  final_join$age_of_driver >= 25 & final_join$age_of_driver <= 40 ~ 2,
  final_join$age_of_driver >= 41 & final_join$age_of_driver <= 65 ~ 3,
  final_join$age_of_driver > 65 ~ 4,
  TRUE ~ final_join$age_of_driver
)

# Modify Values for Age of Driver



#Save and Export final Data set
write.csv(final_join, 'final_dataset.csv', row.names = FALSE)