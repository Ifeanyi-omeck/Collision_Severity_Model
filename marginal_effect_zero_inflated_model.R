ziop_marginal_effects <- function(ZIOP_model) {
  
  # dnorm is a reference to the standard normal probability density function. 
  # This function is used to calculate the difference in the density of the ordered categories 
  # (i.e., the difference in the density of the probability of being in one category versus the next).
  
  # The operations uj[1:len_response_variable] - c(xb) and  uj[2:(len_response_variable + 1)] - c(xb)
  # represent the calculation of the threshold values  for each category. 
  # Here, len_response_variable refers to the number of categories in the response variable, 
  # and xb represents the predicted values on the linear scale (the X?? part of the model).
  
  # After calculating the differences in densities, 
  # these are multiplied by the models coefficient estimates (stored in model_estimates)
  # to obtain the marginal effects. This operation represents the change in the predicted probability 
  # of being in each category for a one-unit increase in the respective predictor, assuming other predictors are held constant.
  
  # The assumption that all other predictors are held constant is 
  # implicit in the way the linear predictor xb is calculated. 
  # It is computed at the mean of the predictor variables (x_lin_bar), 
  # which in effect assumes that all other predictors are held constant at their means when calculating the marginal effects.
  # This is a common approach in calculating marginal effects.
  
  # Extract Estimates from the model
  response_variable <- unique(ZIOP_model$y)  # Grabbing the unique values in response variable from the ZIOP model.
  
  len_response_variable  <- length(response_variable)  # Determine the length of Unique.
  
  column_names <- colnames(ZIOP_model$x)  # Get the column names of the independent variables.
  
  x_variables <- ZIOP_model$x  # Assign the independent variables to a new variable.
  
  
  # Create the formula for the model
  form <- paste("~ 1", paste("+", column_names, collapse = " "), collapse = " ") 
  
  # Generate the design matrix for the model. Excluding the intercept.
  x_lin <- model.matrix(as.formula(form), data = x_variables)[, -1] 
  
  # Calculate the mean of the design matrix.
  x_lin_bar <- as.matrix(colMeans(x_lin)) 
  
  # Extract the model estimates from the Ordered components.
  model_estimates <- ZIOP_model$coefficients$ordered  
  
  # Remove the names from the model estimates
  model_estimates_new <- unname(model_estimates)  
  
  
  # Calculate the linear predictors.
  xb <- t(x_lin_bar) %*% model_estimates  
  
  
  # Setting lower Bound Threshold for the cut-points.
  # Extract the cut-point estimates.
  uj <-  summary(ZIOP_model)$coefficients[grep("^cut", rownames(summary(ZIOP_model)$coefficients)), "Estimate"]  
  uj <- c(-10^6, uj, 10^6)  # Set lower and upper bounds for the cut-points
  
  
  # Calculating the Marginal Effects
  effects.xb <- dnorm(uj[1:len_response_variable] - c(xb)) - dnorm(uj[2:(len_response_variable + 1)] - c(xb))
  
  # Multiply the model estimates by the marginal effects
  marg_effects <- model_estimates %*% matrix(data =  effects.xb, nrow = 1)  
  
  # Passing variables to a data frame.
  marg_effects_df <- as.data.frame(marg_effects)
  
  # Assign column names as row names.
  # Remove "X" from the column names.
  column_names <- gsub("^X", "", column_names) 
  
  # Assign the column names as row names.
  row.names(marg_effects_df) <- column_names  
  
  # Renaming the Marginal effect Column data frame.
  col_names <- paste("marginal_effect", 1:ncol(marg_effects_df))
  colnames(marg_effects_df) <- col_names  # Assign the column names to the data frame
  
  return(marg_effects_df)  # Return the data frame of marginal effects
}


# Calculating Marginal effects for the ZIOP model.
ziop_marginal_effects(first_ZIOP_model)