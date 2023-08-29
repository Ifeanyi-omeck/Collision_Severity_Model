marginal_effect_ordered <- function(ZIOP_MODEL, data, rho) {
  
  response_variable <- length(unique(ZIOP_MODEL$y))
  
  # Extract the ordered Probit coefficients from the ZIOP model
  ordered_estimates <- unname(ZIOP_MODEL$coefficients$ordered)
  
  # Extract the inflation coefficients from the ZIOP model, excluding the intercept
  infl_estimates <- unname(ZIOP_MODEL$coefficients$inflation[-1])
  
  intercept_term <- unname(ZIOP_MODEL$coefficients$inflation[1])
  
  # Extracting coefficient common to both part of the model
  #unique_val <- c(Gamma, Beta)
  
  # xi represents the variables used in the ordered component of the ZIOP model
  x_variables <- as.matrix(data[, colnames(ZIOP_MODEL$x)])
  
  col_names_x <- colnames(ZIOP_MODEL$x)
  
  # Get column names from the ZIOP model's inflation variables
  col_names_z <- colnames(ZIOP_MODEL$z)
  
  # Clean the column names by removing the 'inflation.' prefix
  col_names_z <- gsub("inflation.", "", col_names_z)
  
  # Remove the first element from the column names vector, which is the intercept term
  col_names_z <- col_names_z[-1]
  
  # Extract inflation variables from the data using the cleaned column names and convert to a matrix
  z_variables <- as.matrix(data[, col_names_z])
  
  # Extracting the levels of severity in the in the model
  J_levels_ordered <- unique(ZIOP_MODEL$y) 
  
  # Create the formula for the model ordered section of the model
  form_x  <- paste("~ 1", paste("+", col_names_x, collapse = " "), collapse = " ") 
  
  # Create the formula for the Binary Probit section of the model
  form_z  <- paste("~ 1", paste("+", col_names_z, collapse = " "), collapse = " ") 
  
  # Generate the design matrix for the model. Excluding the intercept.
  x_lin <- model.matrix(as.formula(form_x), data = data)[, -1]
  
  z_lin <- model.matrix(as.formula(form_z), data = data)[, -1]
  
  # since Marginal effects could be  calculated at the mean of the independent variables(joint mean of the covariates)
  # The mean of independent variables for both the Binary and Ordered probit are calculated.
  # The Author chose to calculate the Marginal effect at the join mean of the covariates below.
  x_mean <- as.matrix(colMeans(x_lin))
  Y_mean <- as.matrix(colMeans(z_lin))
  
  # Calculate XB and zY by multiplying the mean and model estimates
  xb <- t(x_mean) %*% ordered_estimates
  zY <- t(Y_mean) %*% infl_estimates
  
  # assign  X* and Y* to a variable. They include coefficients associated not only with the variables included 
  # in either Binary or Ordered probit but also the common variables that appear in both equations.
  # Extracting coefficients
  ordered_coefs <- ZIOP_MODEL$coefficients$ordered
  inflation_coefs <- ZIOP_MODEL$coefficients$inflation[-1]
  
  # Finding unique and common variable names
  common_vars_names <- intersect(names(ordered_coefs), names(inflation_coefs))
  unique_ordered_vars_names <- setdiff(names(ordered_coefs), common_vars_names)
  unique_inflation_vars_names <- setdiff(names(inflation_coefs), common_vars_names)
  
  # Populate x_asteric and Y_asteric lists based on the unique and common variables
  x_asteric <- list()
  Y_asteric <- list()
  
  # Looping through the variables to find unique or common variables for x* AND  Y*
  if (length(unique_ordered_vars_names) > 0 || length(unique_inflation_vars_names) > 0) {
    # For ordered_coefs
    x_asteric[common_vars_names] <- ordered_coefs[common_vars_names]
    x_asteric[unique_ordered_vars_names] <- ordered_coefs[unique_ordered_vars_names]
    
    # For inflation_coefs
    Y_asteric[common_vars_names] <- inflation_coefs[common_vars_names]
    Y_asteric[unique_inflation_vars_names] <- inflation_coefs[unique_inflation_vars_names]
  } else {
    x_asteric[common_vars_names] <- ordered_coefs[common_vars_names]
    Y_asteric[common_vars_names] <- inflation_coefs[common_vars_names]
  }
  
  # Removing the inflation.name(ie. inflation.age hence making it just age)
  names(Y_asteric) <- gsub("inflation.", "", names(Y_asteric))
  
  #Converting list to Matix for allow for multiplication and transposing them
  x_asteric_mat <- as.matrix(unlist(x_asteric))
  x_asteric_mat <- t(x_asteric_mat)
  
  Y_asteric_mat <- as.matrix(unlist(Y_asteric))
  Y_asteric_mat  <- t(Y_asteric_mat)
  
  # Extract the cut-points (threshold parameters) from the ZIOP model
  alpha <-  summary(ZIOP_MODEL)$coefficients[grep("^cut", rownames(summary(ZIOP_MODEL)$coefficients)), "Estimate"]  
  alpha <- c(-10^6, alpha, 10^6)  # Set lower and upper bounds for the cut-points
  
  # Calculating the Marginal effects for the Zero inflated Ordered Probit Model
  binary_prob <- (pnorm((alpha[2:(response_variable + 1)] - c(xb) + c(rho * zY)) / sqrt(1 - rho^2)) - 
                    pnorm((alpha[1:response_variable] - c(xb) + c(rho * zY)) / sqrt(1 - rho^2))) * dnorm(c(zY))
  
  ordered_prob <- pnorm((c(zY + rho) * (alpha[1:response_variable] -c(xb))) / sqrt(1 - rho^2)) * dnorm(alpha[1:response_variable] - c(xb)) - 
    pnorm((c(zY + rho) * (alpha[2:(response_variable + 1)] - c(xb))) / sqrt(1 - rho^2))  * dnorm(alpha[2:(response_variable + 1)] - c(xb))
  
  # Multiply by X* and Y* which represents coefficients associated not only with the variables included 
  # in either Binary or Ordered probit but also the common variables that appear in both equations
  binary_probit_effect <- as.vector(Y_asteric_mat) %*% matrix(data =  binary_prob, nrow = 1) 
  ordered_probit_effect <- as.vector(x_asteric_mat) %*% matrix(data = ordered_prob, nrow = 1)
  
  # Add the Marginal effect for both term one and term two
  overall_marginal_effect <- binary_probit_effect + ordered_probit_effect
  
  # Assign the row names and column to the binary and Ordered probit section of the returned data.,
  # Passing variables to a data frame.
  binary_probit_effect <- as.data.frame(binary_probit_effect)
  ordered_probit_effect<- as.data.frame(ordered_probit_effect)
  overall_marginal_effect <- as.data.frame(overall_marginal_effect)
  
  row.names(binary_probit_effect) <- col_names_z
  row.names(ordered_probit_effect) <- col_names_x
  
  # renaming the columns for the Ordered probit section.
  colnames(ordered_probit_effect)[colnames(ordered_probit_effect) %in% c("V1", "V2", "V3")] <- c("Slight Injury", 
                                                                                                 "Serious Injury", "Fatal Injury")
  
  # Return various Marginal effect component
  return(list(Binary_Probit = binary_probit_effect, Ordered_Probit = ordered_probit_effect, 
              Overall_effect = overall_marginal_effect))
}



# Example usage : Here Rho is set to zero since the ZIOP model assumes independence among the latent processes.
# For correlated disturbance include the Rho value which is normally within (-1 and 1).
effects <- marginal_effect_ordered(ziop_two, prep_data, rho = 0)
effects$Ordered_Probit

summary(ziop_two)