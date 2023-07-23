ziop_marginal_effects <- function(first_mod) {
  
  # dist_function is a reference to the standard normal probability density function, dnorm. 
  # This function is used to calculate the difference in the density of the ordered categories 
  # (i.e., the difference in the density of the probability of being in one category versus the next).
  
  # The operations z[1:lens] - c(xb) and  z[2:(lens + 1)] - c(xb)
  # represent the calculation of the z-scores for each category. 
  # Here, lens refers to the number of categories in the response variable, 
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
  cat_y_variable <- unique(first_mod$y)  # Extract unique values of the dependent variable
  lens <- length(cat_y_variable)  # Determine the number of unique values
  column_names <- colnames(first_mod$x)  # Get the column names of the independent variables
  x_var <- first_mod$x  # Assign the independent variables to a new variable
  form <- paste("~ 1", paste("+", column_names, collapse = " "), collapse = " ")  # Create the formula for the model
  x_lin <- model.matrix(as.formula(form), data = x_var)[, -1]  # Generate the design matrix for the model
  x_lin_bar <- as.matrix(colMeans(x_lin))  # Calculate the column means of the design matrix
  model_estimates <- first_mod$coefficients$ordered  # Extract the model estimates
  model_estimates_new <- unname(model_estimates)  # Remove the names from the model estimates
  len_model_estimates <- nrow(summary(first_mod)$coefficients[-c(1,2), ])  # Determine the number of model estimates
  xb <- t(x_lin_bar) %*% model_estimates  # Calculate the linear predictors
  
  # Setting lower Bound Threshold for the cut-points
  z <-  summary(first_mod)$coefficients[grep("^cut", rownames(summary(first_mod)$coefficients)), "Estimate"]  # Extract the cut-point estimates
  z <- c(-10^6, z, 10^6)  # Set lower and upper bounds for the cut-points
  
  # Instantiating Function
  dist_function <- dnorm  # Specify the distribution function to use (in this case, the standard normal distribution)
  
  # Calculating the Marginal Effects
  effects.xb <- dist_function(z[1:lens] - c(xb)) - dist_function(z[2:(lens + 1)] - c(xb))  # Calculate the marginal effects
  marg_effects <- model_estimates %*% matrix(data =  effects.xb, nrow = 1)  # Multiply the model estimates by the marginal effects
  
  # Passing variables to a data frame
  marg_effects_df <- as.data.frame(marg_effects)  # Convert the marginal effects to a data frame
  
  # Assign column names as row names
  column_names <- gsub("^X", "", column_names)  # Remove "X" from the column names
  row.names(marg_effects_df) <- column_names  # Assign the column names as row names
  
  # Renaming the Marginal effect Column data frame
  col_names <- paste("marginal_effect", 1:ncol(marg_effects_df))  # Generate column names for the data frame
  colnames(marg_effects_df) <- col_names  # Assign the column names to the data frame
  
  return(marg_effects_df)  # Return the data frame of marginal effects
}
