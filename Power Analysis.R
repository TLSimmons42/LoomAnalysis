# Install and load the 'pwr' package if not already installed
#install.packages("pwr")
library(pwr)

# Define parameters
effect_size <- 0.2   # Adjust this based on your expectations
alpha <- 0.05        # Significance level
power <- 0.80        # Desired power
groups <- 2          # Number of groups
n_per_group <- NULL  # Sample size per group (to be calculated)

# Perform power analysis
pwr.anova.test(k = groups, n = n_per_group, f = effect_size, sig.level = alpha, power = power)

# If you want to calculate the required total sample size
total_sample_size <- pwr.anova.test(k = groups, n = n_per_group, f = effect_size, sig.level = alpha, power = power)$n
n_per_group <- total_sample_size / groups