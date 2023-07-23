library(ggplot2)
library(corrplot)
library(RColorBrewer)


visual_data <- read.csv('final_dataset.csv')
# correlation plot of the correlated variables in the data set with correlation threshold set at  65% 
# Using the pearson Coefficient to check for Multi-collinearity between variables in the data set.
correlation_matrix <- cor(final_join, use = "pairwise.complete.obs")

# Set diagonal to 0 to avoid self-pairing
diag(correlation_matrix) <- 0

# Create a matrix of the same shape as 'correlation_matrix' with 'TRUE' values above 65% absolute correlation
high_correlation_matrix <- abs(correlation_matrix) > 0.65

# As correlation matrix is symmetrical, remove duplicate pairs
high_correlation_matrix[lower.tri(high_correlation_matrix)] <- FALSE

# Extract variables with high correlation
high_correlation_pairs <- which(high_correlation_matrix, arr.ind = TRUE)


# print correlated Variables
if(nrow(high_correlation_pairs) > 0) {
  print(paste("Variables with absolute correlation"))
  for(i in 1:nrow(high_correlation_pairs)) {
    print(paste0(
      names(final_join)[high_correlation_pairs[i, 1]], 
      " and ", 
      names(final_join)[high_correlation_pairs[i, 2]], 
      " with correlation of ", 
      round(correlation_matrix[high_correlation_pairs[i, 1], high_correlation_pairs[i, 2]], 2) * 100, 
      "%"
    ))
  }
} else {
  print("No pairs of variables with absolute correlation over 65% were found.")
}


#These are correlated values returned from the code above with over 65% threshold
correlated_values = c('hit_object_off_carriageway', 'vehicle_manoeuvre', 'vehicle_location_restricted_lane',
                      'hit_object_in_carriageway', 'age_of_casualty', 'age_band_of_casualty', 'accident_severity',
                       'casualty_severity', 'skidding_and_overturning')


#Visualizing the correlated pairs with over 65 percent threshold
correlation_matrix <- cor(final_join[, correlated_values], use = "pairwise.complete.obs")

#Visualizing correlated variables
correlation_plot <- corrplot(correlation_matrix, 
                             type = 'upper', 
                             order = 'hclust', 
                             tl.col = "black", # color of text labels
                             tl.srt = 45, # rotation of text labels
                             addCoef.col = "black", # add correlation coefficient to plot
                             method="color", # fill color
                             col=brewer.pal(n=8, name="RdYlBu"), # color palette
                             title="Correlation plot of the injury  dataset", # plot title
                             mar=c(0,0,1,0)) # margins around plot


