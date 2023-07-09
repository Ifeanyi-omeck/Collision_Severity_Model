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
  cat_y_variable <- unique(first_mod$y)
  lens <- length(cat_y_variable)
  column_names <- colnames(first_mod$x)
  x_var <- first_mod$x
  form <- paste("~ 1", paste("+", column_names, collapse = " "), collapse = " ")
  x_lin <- model.matrix(as.formula(form), data = x_var)[, -1]
  x_lin_bar <- as.matrix(colMeans(x_lin))
  model_estimates <- first_mod$coefficients$ordered
  model_estimates_new <- unname(model_estimates)
  len_model_estimates <- nrow(summary(first_mod)$coefficients[-c(1,2), ])
  xb <- t(x_lin_bar) %*% model_estimates
  
  # Setting lower Bound Threshold for the cut-points
  z <-  summary(first_mod)$coefficients[grep("^cut", rownames(summary(first_mod)$coefficients)), "Estimate"]
  z <- c(-10^6, z, 10^6)
  
  # Instantiating Probit Function
  dist_function <- dnorm
  prob_function <- pnorm
  
  # Obtaining the covariance matrix of the model
  first_cov <- vcov(first_mod)
  second_cov <- rbind(cbind(first_cov, 0, 0), 0, 0)
  ind <- c(1:len_model_estimates, nrow(second_cov)-1, (len_model_estimates+1):(len_model_estimates+lens-1), nrow(second_cov))
  third_cov <- second_cov[ind,]
  third_cov <- second_cov[, ind]
  
  # Calculating the Marginal Effects
  effects.xb <- dist_function(z[1:lens] - c(xb)) -dist_function(z[2:(lens + 1)] -c(xb))
  marg_effects <- model_estimates %*% matrix(data =  effects.xb, nrow = 1)
  
  # Passing variables to a data frame
  marg_effects_df <- as.data.frame(marg_effects)
  
  # Assign column names as row names
  column_names <- gsub("^X", "", column_names)  # Remove "X" from row names
  row.names(marg_effects_df) <- column_names
  
  # Renaming the Marginal effect Column data frame
  col_names <- paste("marginal_effect", 1:ncol(marg_effects_df))
  colnames(marg_effects_df) <- col_names
  
  return(marg_effects_df)
}


ziop_marginal_effects(model_ziop)