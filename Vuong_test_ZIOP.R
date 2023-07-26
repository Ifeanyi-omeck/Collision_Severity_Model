# library(MASS)  # For the norm functions

# This function applies the Voung Test (Voung 1989) to assess two non-nested models. 
# Specifically, the test is designed for evaluating the Zero-Inflated Ordered Probit (ZIOP) model and the Traditional Ordered Probit model within the CLM package.
# The goal of this function is to evaluate the model fit between the ZIOP model and the Traditional Ordered Probit model.

vuong_test <- function(trad_ordinal_model, ZIOP_model) {
  
  # Voung test for comparing the Traditional Ordered Probit model and the Zero-Inflated Ordered Probit model.
  # trad_ordinal_model: Traditional Ordered Probit model.
  # ZIOP_model: Zero-Inflated Ordered Probit model.
  
  # Extracting the length of the data.
  len1 <- trad_ordinal_model$n  
  
  # Extracting response variables and explanatory variables from the ZIOP model.
  y <- ZIOP_model$y
  x <- ZIOP_model$x
  
  # Initializing a new floating points array for the thresholds of the Traditional Ordered Probit model.
  float_threshold <- rep(0, length(unname(trad_ordinal_model$alpha)))
  
  # Grab the coefficients of the model excluding the cutpoints in the Traditional Ordered Probit model.
  trad_coefs <- unname(trad_ordinal_model$beta)
  float_threshold[1] <- unname(trad_ordinal_model$alpha[1])
  
  # Updating the float threshold parameter based on the cumulative exp(alpha) values.
  for (i in 2:length(trad_ordinal_model$alpha)) {
    float_threshold[i] <- float_threshold[i - 1] + exp(trad_ordinal_model$alpha[i])
  }
  
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
  predict_prob <- predict(ZIOP_model, type = c("prob.full"))
  predict_prob <- unname(predict_prob)
  
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
  # The first column (probs[, 1]) represents the probabilities of being in the first category, and the last column (probs[, num_categories]) represents the probabilities of being in the last category.
  probs <- matrix(0, nrow = len1, ncol = num_categories)
  
  # Calculating the probability of being in the first category.
  probs[, 1] <- pnorm(float_threshold[[1]] - x_variable_sum) / predict_prob[, 1]
  
  # Calculating the probability of being in the last category.
  probs[, num_categories] <- (1 - pnorm(float_threshold[num_categories - 2] - x_variable_sum)) / predict_prob[, num_categories - 1]
  
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
  
  # Calculating the log-likelihood for each observation and category.
  m2 <- m[m != 0]
  mlog <- log(m2)
  diffmsq <- (mlog - mean(mlog))^2
  sumdms <- sum(diffmsq)
  
  # Calculating the Voung test statistic.
  vuongopiopc <- (sqrt(len1) * (1 / len1) * sum(mlog)) / sqrt((1 / len1) * sumdms)
  
  return(vuongopiopc)
}