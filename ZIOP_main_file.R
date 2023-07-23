devtools::install_github("hknd23/zmiop")
library(zmiop)


data <- read.csv("final_dataset.csv")

#Convert items to factor Variables
convert_to_factors <- function(dataframe) {
  dataframe[] <- lapply(dataframe, as.factor)
  return(dataframe)
}

# Converting variables to Factor variables for modelling.
dataset <- convert_to_factors(data)
colnames(dataset)



# Fitting initial Zero inflated model for collision severity data set
first_ZIOP_model_test <- iop(recoded_severity ~  speed_limit + light_conditions + road_type + vehicle_type + vehicle_manoeuvre +
                                           skidding_and_overturning + sex_of_driver + age_of_casualty + car_passenger + 
                                           road_surface_conditions | speed_limit + light_conditions + vehicle_type +
                                           vehicle_manoeuvre + skidding_and_overturning + sex_of_driver + age_of_casualty + 
                                           car_passenger + road_surface_conditions, data = dataset, type = c('ziop'))


# Viewing the summary of the Zero inflated model data set.
summary(first_ZIOP_model_test)


# Marginal effect of the zero inflated Ordered Probit model(ZIOP)
marginal_effect <- ziop_marginal_effects(first_ZIOP_model_test)
marginal_effect


# calculating Model estimates(AIC, BIC and log likelihood)
estimates <- calculate_AIC_BIC(first_ZIOP_model_test, n = 8376)
estimates$AIC
estimates$BIC

# Grabbing the log likelihood for ZIOP model
first_ZIOP_model$ll