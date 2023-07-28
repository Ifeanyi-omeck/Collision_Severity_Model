library(MASS) 

# Calculate the Probabilites to used in the Vuong Test
calculate_probabilities <- function(model, data){
  
  # Extract the explanatory variables for the ordered component from the data
  ordered_var <- data[, colnames(model$x)]
  
  # Get the column names from the inflation component of the model
  col_names <- colnames(model$z)
  
  # Clean the column names by removing the 'inflation.' prefix
  formatted_col_names <- gsub("inflation.", "", col_names)
  
  # Exclude 'Intercept' from the column names
  formatted_col_names <- setdiff(formatted_col_names, "Intercept")
  
  # Extract the intercept from the inflation component of the model
  intercept <- unique(model$z[1])
  
  # Construct the 'inflated_data' dataframe using selected columns
  # Add a new column 'intercept' with repeated values from the model's intercept
  inflated_data <- data[, formatted_col_names, drop = FALSE]
  inflated_data$intercept <- rep(intercept,  nrow(inflated_data))
  
  # Reorder columns in 'inflated_data' to place 'intercept' first
  inflated_data <- inflated_data %>% select(intercept, everything())
  
  # Convert the 'intercept' column values to integer
  inflated_data$intercept <- unlist(inflated_data$intercept)
  inflated_data$intercept <- as.integer(inflated_data$intercept)
  
  # Calculate the product of the 'inflated_data' and model's inflation coefficients
  # This is the 'infla' part of the model's probability calculations
  inflation_result <- as.matrix(inflated_data) %*% model$coefficients$inflation
  
  # Calculate the product of the 'ordered_var' and model's ordered coefficients
  # This is the 'ordered' part of the model's probability calculations
  ordered_result <- as.matrix(ordered_var) %*% model$coefficients$ordered
  
  # Prepare cut-point probabilities matrix(i,e thresholds which determines the position of 
  #the ordered variable)
  cutpoint_probs <- matrix(nrow=model$y.categories-1,ncol=1)
  
  # Get number of rows in inflated_data
  num_rows <- nrow(inflated_data)
  
  # Prepare probabilities matrix
  probabilities <- matrix(nrow=num_rows,ncol=model$y.categories)
  
  cutpoint_probs[1,1] <- model$coefficients$cutpoints[1]
  
  # Calculate cutpoint probabilities
  for(j in 2:(model$y.categories-1)){
    cutpoint_probs[j,1] <- cutpoint_probs[j-1,1] + exp(model$coefficients$cutpoints[j])
  }
  
  # Calculate the last and first categories
  probabilities[,model$y.categories] <- (pnorm(inflation_result)) * (1 - pnorm(cutpoint_probs[model$y.categories-1,1] - ordered_result))
  probabilities[,1] <- (1 - pnorm(inflation_result)) + (pnorm(inflation_result)) * pnorm(cutpoint_probs[1,1] - ordered_result)
  
  # Calculate intermediate categories
  for(j in 2:(model$y.categories-1)){
    probabilities[,j] <- (pnorm(inflation_result)) * ((pnorm(cutpoint_probs[j,1] - ordered_result)) - (pnorm(cutpoint_probs[j-1,1] - ordered_result)))
  }
  
  # Set column names of the probabilities matrix
  colnames(probabilities) <- paste("Pr(Y=", sort(unique(model$y)), ")", sep="")
  return(probabilities)
}




# This function applies the Voung Test (Voung 1989) to assess two non-nested models. 
# Specifically, the test is designed for evaluating the Zero-Inflated Ordered Probit (ZIOP) model and the Traditional Ordered Probit model within the CLM package.
# The goal of this function is to evaluate the model fit between the ZIOP model and the Traditional Ordered Probit model.

vuong_test <- function(trad_ordinal_model,  ZIOP_model, data) {
  
  # Voung test for comparing the Traditional Ordered Probit model and the Zero-Inflated Ordered Probit model.
  # trad_ordinal_model: Traditional Ordered Probit model.
  # ZIOP_model: Zero-Inflated Ordered Probit model.

  # Extracting the length of the data.
  len1 <- trad_ordinal_model$n
  
  # Extracting response variables and explanatory variables from the data.
  y <- data$recoded_severity  #Manually input this into this function
  x <- data[, colnames(ZIOP_model$x)] # 
  
  # Initializing a new floating points array for the thresholds of the Traditional Ordered Probit model.
  float_threshold <- rep(0, length(unname(trad_ordinal_model$alpha)))
  
  # Grab the coefficients of the model excluding the cut-points in the Traditional Ordered Probit model.
  trad_coefs <- unname(trad_ordinal_model$beta)
  
  # Removing names from the threshold arrays
  float_threshold <- unname(trad_ordinal_model$alpha)
  
  # Instantiate a model matrix for the explanatory variables.
  x_variable <- matrix(nrow = len1, ncol = ncol(x))
  
  # Populate the model matrix using element-wise multiplication between 
  # explanatory variables in the zero-inflated model and estimates from the Traditional Ordered Probit model.
  for (j in 1:ncol(x)) {
    x_variable[, j] <- trad_coefs[j] * x[, j]
  }
  
  # Calculate the row sums of the model matrix.
  x_variable_sum <- rowSums(x_variable)
  
  # Extracting predicted probabilities from the zero-inflated model
  # This a function for calculating probabilities(Written above this one)
  predict_prob <- calculate_probabilities(model = ZIOP_model, data = data) 
  
  # Converting response variable to factor and extracting unique categories.
  y_factor <- as.factor(y)
  unique_categories <- unique(y_factor)
  num_categories <- length(unique_categories)
  sorted_categories <- sort(unique_categories)
  
  # Creating a matrix to store indicator variables for each category.
  # v: Matrix of indicator variables to represent each category of the response variable 'y'.
  # Each column of 'v' corresponds to a distinct category, and each row represents an observation.
  # If an observation belongs to a certain category, the corresponding element in that column is set to TRUE (1); otherwise, it is set to FALSE (0).
  v <- matrix(0, nrow = len1, ncol = num_categories)
  
  for (j in 1:num_categories) {
    v[, j] <- y == sorted_categories[j]
  }
  
  
  # m: A numeric vector initialized with zeros to store the calculated probabilities for each observation.
  m <- rep(0, len1)
  
  # probs: A matrix to store the probabilities of each observation belonging to each category.
  # The first column (probs[, 1]) represents the probabilities of being in the first category, 
  # and the last column (probs[, num_categories]) represents the probabilities of being in the last category.
  probs <- matrix(0, nrow = len1, ncol = num_categories)
  
  # Calculating the probability of being in the first category.
  probs[, 1] <- pnorm(float_threshold[[1]] - x_variable_sum) / predict_prob[, 1]
  
  # Calculating the probability of being in the last category.
  probs[, num_categories] <- (1 - pnorm(float_threshold[num_categories - 2] - x_variable_sum)) / predict_prob[, (num_categories - 1)]
  
  # Calculating the probabilities of being in the intermediate categories.
  for (i in 2:(num_categories - 1)) {
    probs[, i] <- (pnorm(float_threshold[i] - x_variable_sum) - pnorm(float_threshold[i - 1] - x_variable_sum)) / predict_prob[, i]
  }
  
  # Calculating the weighted probabilities for each observation and category.
  m <- matrix(0, nrow = len1, ncol = num_categories)
  
  for (k in 1:len1) {
    for (j in 1:num_categories) {
      m[k, j] <- v[k, j] * probs[k, j]
    }
  }
  
  # Removing no zero values
  m[m[,1] == 0, 1] <- apply(m[m[,1] == 0,], 1, function(x) x[x!=0][1])
  
  # Now keep only the first column
  m2 <- m[ , 1, drop = FALSE]
  
  
  # Calculating the log-likelihood for each observation and category.
  mlog <- log(m2)
  
  
  diffmsq <- (mlog - mean(mlog)) **2
  sumdms <- sum(diffmsq)
  
  # Calculating the Vuong test statistic.
  numerator = sqrt(len1) * ((1/len1) * sum(mlog))
  denominator =  sqrt((1 / len1) * sumdms)
  
  vuong_test <- numerator / denominator 
  
  #Returning the Vuong test
  return(vuong_test)
}

# Voung test between Ordinal Model and Zero inflated Ordered Probit model
vuong_test(first_ordinal_model, first_ziop_model, data_without_levels)