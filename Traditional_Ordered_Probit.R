# Fitting a traditional Ordered Probit model
if(!require(ordinal)) {
  install.packages("ordinal")
  library(ordinal)
}


#Reading data into the ZIOP model
data <- read.csv("final_dataset.csv")

#Convert items to factor Variables
convert_to_factors <- function(dataframe) {
  dataframe[] <- lapply(dataframe, as.factor)
  return(dataframe)
}


# Convert data set to factor variables
factor_data <- convert_to_factors(data)


# Fitting Traditional Ordered Probit Model to select significant variable for the ZIOP model 
first_ordered_probit <- clm(factor(recoded_severity) ~ road_type + speed_limit + light_conditions + weather_conditions + 
                              road_surface_conditions + special_conditions_at_site + carriageway_hazards + 
                              urban_or_rural_area + vehicle_type + vehicle_manoeuvre + 
                              vehicle_location_restricted_lane + skidding_and_overturning + 
                              hit_object_in_carriageway + first_point_of_impact + journey_purpose_of_driver + 
                              sex_of_driver + age_of_driver + age_of_vehicle + sex_of_casualty + age_of_casualty + 
                              car_passenger, data = factor_data, link = "probit")


summary(first_ordered_probit)

# Fitting Traditional Ordered Probit model with interaction between sex of driver and age of driver
second_ordered_probit <- clm(recoded_severity ~ road_type + speed_limit + light_conditions + 
                                         weather_conditions + road_surface_conditions + special_conditions_at_site + 
                                         carriageway_hazards + urban_or_rural_area + vehicle_type +vehicle_manoeuvre + 
                                         vehicle_location_restricted_lane + skidding_and_overturning + 
                                         hit_object_in_carriageway + first_point_of_impact + journey_purpose_of_driver + 
                                         sex_of_driver*age_of_driver + age_of_vehicle + sex_of_casualty +
                                         age_of_casualty + car_passenger, data = factor_data, link = "probit")


summary(second_ordered_probit)




# Fitting Traditional Ordered Probit model with interaction between speed limit and Rural Urban area
Third_ordered_probit <- clm(recoded_severity ~ road_type + speed_limit*urban_or_rural_area + light_conditions + 
                     weather_conditions + road_surface_conditions + special_conditions_at_site + 
                     carriageway_hazards + vehicle_type + vehicle_manoeuvre +
                     vehicle_location_restricted_lane + skidding_and_overturning + 
                     hit_object_in_carriageway + first_point_of_impact + journey_purpose_of_driver + 
                     sex_of_driver + age_of_driver + age_of_vehicle + sex_of_casualty +
                     age_of_casualty + car_passenger, data = factor_data, link = "probit")


summary(Third_ordered_probit)



# Function to extract variables significant at 90% confidence level
get_significant_vars <- function(model) {
  # Get the summary of the model
  model_summary <- summary(model)
  
  # Get the coefficients table
  coef_table <- model_summary$coefficients
  
  # Add row names to the coefficients table
  coef_table <- cbind(Variable = rownames(coef_table), coef_table)
  
  # Reset the row names
  rownames(coef_table) <- NULL
  
  # Filter out the variables with p-value < 0.10 (90% confidence level)
  significant_vars <- coef_table[coef_table[, "Pr(>|z|)"] < 0.05, ]
  
  # Return the significant variables
  return(significant_vars)
}

# Grabbing the significant Variables for the zero inflated models
significant_vars <- get_significant_vars(first_ordered_probit)
print(significant_vars)


# Fitting Traditional Ordered Model with variables significant at the 90% confidence level
significant_model <- clm(recoded_severity ~ road_type + speed_limit + light_conditions + vehicle_type +
                           vehicle_manoeuvre +  skidding_and_overturning + sex_of_driver + age_of_casualty + 
                           car_passenger + road_surface_conditions, data = factor_data, link = 'probit')


summary(significant_model)


# Calculating AIC, BIC and Log likelihood of the model with significant variables for comparison
# Again interaction between the various variables test where non significant.
compute_AIC_BIC_LL <- function(model){
  
  aic <- AIC(model)
  print(paste("AIC:", aic))
  
  bic <- BIC(model)
  print(paste("BIC:", bic))
  
  logLik_value <- logLik(model)
  print(paste("Log-Likelihood:", logLik_value))
  

}



# Grabbing variables for comparison with the zero inflated models
ordered_model_evaluation <- compute_AIC_BIC_LL(significant_model)