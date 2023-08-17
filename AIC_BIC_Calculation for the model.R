# Function to calculate AIC and BIC for a custom zero-inflated model
calculate_AIC_BIC <- function(model, n) {
  
  # Model: Pass Custom Zero-inflated model component
  # n: Total number of observations in the model
  
  nll <- model$ll  #  log-likelihood
  num_parameters <- sum(sapply(model$coefficients, length))  # Number of estimated parameters
  
  AIC <- -2 * nll + 2 * num_parameters 
  BIC <- -2 * nll + log(n) * num_parameters 
  
  return(list(AIC = AIC, BIC = BIC))
}

# Usage example:
estimates <- calculate_AIC_BIC(model_ziop, n = n(data))
estimates$AIC
estimates$BIC

