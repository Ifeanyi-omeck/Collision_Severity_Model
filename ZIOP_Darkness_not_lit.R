# Install the "devtools" package if not already installed.
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install the "Ordinal" Package if not already installed.
if(!require(ordinal)) {
  install.packages("ordinal")
  library(ordinal)
}

# Load the required packages for Zero inflated Ordered Probit model and the Ordinal Model.
library(zmiop)
library(ordinal)

# Set a seed for reproducibility
seed_value <- 123
set.seed(seed_value)


# Fitting model for the first iteration of the zero inflated ordered probit model for Day light.
# Load the prepared data set for modelling.
prep_data <- read.csv('final_dataset_darkness.csv')


# Fitting the first Traditional ordered Probit model using the CLM function in the ordinal package.
Op_one_darkness <- clm(factor(recoded_severity) ~ road_type +  speed_limit + weather_conditions + road_surface_conditions +
                              special_conditions_at_site + carriageway_hazards + urban_or_rural_area + vehicle_type +
                              skidding_and_overturning + first_point_of_impact + journey_purpose_of_driver + sex_of_driver +
                              age_of_driver + age_of_vehicle + sex_of_casualty + age_of_casualty + car_passenger,
                              data = prep_data, link = 'probit')

summary(Op_one_darkness)




# Fitting the Zero inflated Ordered Probit model with significant estimates from the Ordered Probit Model
ziop_one_darkness <- iop(recoded_severity ~  road_type + speed_limit + weather_conditions + road_surface_conditions + 
                           special_conditions_at_site + carriageway_hazards + urban_or_rural_area +
                           vehicle_type + skidding_and_overturning + first_point_of_impact +
                           journey_purpose_of_driver + sex_of_driver + age_of_driver + age_of_vehicle + 
                           sex_of_casualty + age_of_casualty + car_passenger | road_type + speed_limit + 
                           weather_conditions + road_surface_conditions + special_conditions_at_site + 
                           carriageway_hazards + urban_or_rural_area + vehicle_type + skidding_and_overturning + 
                           first_point_of_impact + journey_purpose_of_driver + sex_of_driver + age_of_driver + 
                           age_of_vehicle +  sex_of_casualty + age_of_casualty + car_passenger,
                           data = prep_data, type = c('ziop'))


summary(ziop_one_darkness)


# Compare statistically significant variables in the OP model and ZIOP model, pick statistically significant variables
# in either model and common to both models
# fitting the model again with statistically significant variables 
ziop_two_darkness <- iop(recoded_severity ~  weather_conditions + urban_or_rural_area + skidding_and_overturning + 
                                             age_of_driver + road_type + vehicle_type + journey_purpose_of_driver + 
                                             sex_of_driver + age_of_vehicle + car_passenger | weather_conditions + 
                                             urban_or_rural_area + skidding_and_overturning + age_of_driver + road_type +
                                             vehicle_type + journey_purpose_of_driver + sex_of_driver + age_of_vehicle +
                                             car_passenger, data = prep_data, type = c('ziop'))

summary(ziop_two_darkness)




# Fit a standard ordered probit model with the same variables for comparison
final_op_darkness <- clm(factor(recoded_severity) ~  weather_conditions + urban_or_rural_area + skidding_and_overturning + 
                                                     age_of_driver + road_type + vehicle_type + journey_purpose_of_driver +
                                                     sex_of_driver + age_of_casualty + car_passenger,
                                                     data = prep_data, link = 'probit')


summary(final_op_darkness)


# Saving the results of the Zero inflated Ordered Probit to a csv file
ziop_model <- summary(ziop_two_darkness)$coefficients
ziop_model <- as.data.frame(ziop_model)
ziop_model

# Navigate to the 'Results' folder within the current working directory
setwd("Results_Darkness")
write.csv(ziop_model, 'Estimates_ZIOP_darkness.csv', row.names = FALSE)


# Saving the results of the Ordered Probit model to a csv file.
Op_final_results <-  final_op_darkness$coefficients
Op_final_results <- as.data.frame(Op_final_results)
write.csv(Op_final_results, 'Estimates_OP_darkness.csv', row.names = FALSE)

###################################################################################################################################################
# Calculate AIC BIC and Log likelihood for the Ordered Probit model and them to csv
compute_AIC_BIC_LL <- function(model, file_suffix) {
  aic <- AIC(model)
  print(paste("AIC:", aic))
  
  bic <- BIC(model)
  print(paste("BIC:", bic))
  
  logLik_value <- logLik(model)
  print(paste("Log-Likelihood:", logLik_value))
  
  # Create data frames for AIC, BIC, and log-likelihood values
  aic_df <- data.frame(AIC = aic)
  bic_df <- data.frame(BIC = bic)
  log_likelihood_df <- data.frame(LogLikelihood = logLik_value)
  
  # Save the data frames to separate CSV files with the specified file_suffix
  write.csv(aic_df, file = paste0("AIC_", file_suffix, ".csv"), row.names = FALSE)
  write.csv(bic_df, file = paste0("BIC_", file_suffix, ".csv"), row.names = FALSE)
  write.csv(log_likelihood_df, file = paste0("LogLikelihood_", file_suffix, ".csv"), row.names = FALSE)
}

# Return values for the ordered Probit model for comparison
compute_AIC_BIC_LL(final_op_darkness, "Op_model_darkness")


# Calculate AIC, BIC and Log Likelihood for the zero inflated Ordered Probit Model(ZIOP Model)
calculate_AIC_BIC_LL_ZIOP <- function(model, n ){
  
  
  # Run the function for calculating AIC and BIC first before running this function
  # estimates <- calls the calculate_AIC_BIC function
  estimates <- calculate_AIC_BIC(model = model , n = n)  # Custom code that calculates AIC and BIC for the ZIOP MODEL
  ZIOP_AIC <- estimates$AIC
  print(paste("AIC:",  ZIOP_AIC))
  
  ZIOP_BIC <- estimates$BIC
  ZIOP_BIC
  print(paste("BIC:",  ZIOP_BIC))
  
  
  ZIOP_Log_likelihood <- model$ll
  ZIOP_Log_likelihood 
  print(paste("Likelihood:",  ZIOP_Log_likelihood))
  
  
  # Create data frames for AIC, BIC, and log-likelihood values
  aic_df_ziop <- data.frame(AIC = ZIOP_AIC)
  bic_df_ziop <- data.frame(BIC = ZIOP_BIC)
  log_likelihood_df_ziop <- data.frame(LogLikelihood = ZIOP_Log_likelihood)
  
  # Save the data frames to separate CSV files with the specified file_suffix
  write.csv(aic_df_ziop, file = paste0("AIC_", "Ziop_model_darkness", ".csv"), row.names = FALSE)
  write.csv(bic_df_ziop, file = paste0("BIC_", "Ziop_model_darkness", ".csv"), row.names = FALSE)
  write.csv(log_likelihood_df_ziop, file = paste0("LogLikelihood_darkness_", "Ziop_model", ".csv"), row.names = FALSE)
}


# Return values for Zero inflated Ordered Probit model for Comparison
calculate_AIC_BIC_LL_ZIOP(ziop_two_darkness, 1094)

####################################################################################################################################################
# Calculating the Vuong test and Marginal effect for the Zero inflated Ordered Probit Model.
# The Vuong test score between the Ordered Probit model and the Zero Inflated Ordered Probit Model.
# Storing the results in a csv file 
voung_result <- vuong_test(final_op_darkness, ziop_two_darkness, prep_data) # A custom code that calculates the Vuong test.
voung_result_df <- data.frame(voung_result)
write.csv(voung_result_df, 'vuong_test_result_daylight.csv', row.names = FALSE)
voung_result


# Marginal effect for the Zero inflated Ordered Probit Model
ziop_marginal_result <- marginal_effect_ordered(ziop_two_darkness, prep_data, rho = 0) # A custom code for Calculating Marginal effect.
ziop_marginal_result_df <- data.frame(ziop_marginal_result$Ordered_Probit)
write.csv(ziop_marginal_result_df, 'ziop_marginal_result_daylight.csv', row.names = FALSE)
ziop_marginal_result_df
