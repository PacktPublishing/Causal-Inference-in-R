# Install the grf package
install.packages("grf")

# Load the grf package
library(grf)

# Set seed for reproducibility
set.seed(42)

# Number of students
n <- 1000

# Simulate student characteristics (covariates)
X <- matrix(rnorm(n * 5), nrow = n, ncol = 5)
colnames(X) <- c("PriorGrades", "Attendance", "SocioeconomicStatus", "InterestInSubject", "ParentalSupport")

# Simulate treatment assignment (1 if treated, 0 if control)
T <- rbinom(n, 1, 0.5)

# Simulate outcomes with a heterogeneous treatment effect
Y <- 5 + X[,1] + 2*X[,2] + 0.5*X[,3] + rnorm(n) + T * (2 + 0.5*X[,1])

# Combine into a data frame for convenience
data <- data.frame(Y, T, X)

# Train a causal forest
causal_forest <- causal_forest(X, Y, T)

# Print a summary of the causal forest
summary(causal_forest)

# Estimate treatment effects
tau_hat <- predict(causal_forest)$predictions

# Add the estimated treatment effects to the data frame
data$tau_hat <- tau_hat

# View the first few rows of the data frame
head(data)

# Split the data into training and estimation sets
train_indices <- sample(1:n, size = n/2)
est_indices <- setdiff(1:n, train_indices)

# Train a causal forest on the training set
causal_forest_train <- causal_forest(X[train_indices, ], Y[train_indices], T[train_indices])

# Estimate treatment effects on the estimation set
tau_hat_est <- predict(causal_forest_train, newdata = X[est_indices, ])$predictions

# Combine the estimation set with the estimated treatment effects
data_est <- data[est_indices, ]
data_est$tau_hat_est <- tau_hat_est

# View the first few rows of the estimation set with the estimated treatment effects
head(data_est)

# Calculate the average treatment effect (ATE) bias
true_ate <- mean((Y[est_indices][T[est_indices] == 1]) - (Y[est_indices][T[est_indices] == 0]))
est_ate <- mean(tau_hat_est)
ate_bias <- est_ate - true_ate
print(paste("Average Treatment Effect (ATE) Bias:", ate_bias))

# Calculate the CATE accuracy using RMSE
true_cate <- 2 + 0.5 * X[est_indices, 1]  # True CATE based on the data generation process
cate_rmse <- sqrt(mean((tau_hat_est - true_cate)^2))
print(paste("Conditional Average Treatment Effect (CATE) RMSE:", cate_rmse))

# Construct confidence intervals and evaluate coverage
alpha <- 0.05
ci <- predict(causal_forest_train, newdata = X[est_indices, ], estimate.variance = TRUE)
z <- qnorm(1 - alpha / 2)
se <- sqrt(ci$variance.estimates)
ci_low <- ci$predictions - z * se
ci_high <- ci$predictions + z * se
coverage <- mean((true_cate >= ci_low) & (true_cate <= ci_high))
print(paste("Coverage of", (1 - alpha) * 100, "% confidence intervals:", coverage))

# Visualize the estimated treatment effects across covariate values
plot(X[est_indices, 1], tau_hat_est, xlab = "PriorGrades", ylab = "Estimated Treatment Effect")
abline(h = 0, col = "red", lty = 2)  # Add a reference line at y = 0

# Extract the leaf indices
leaf_indices <- predict(causal_forest, type = "leaf.index")$leaf.index

# Add the leaf indices to the data frame
data$leaf <- leaf_indices

# Check balance within each leaf
balance_check <- function(leaf) {
  leaf_data <- data[data$leaf == leaf, ]
  treated <- leaf_data[leaf_data$T == 1, ]
  control <- leaf_data[leaf_data$T == 0, ]
  if (nrow(treated) > 1 & nrow(control) > 1) {
    balance <- sapply(leaf_data[, 3:7], function(col) {
      t.test(col ~ leaf_data$T)$p.value
    })
    return(balance)
  } else {
    return(rep(NA, 5)) # Return NA if not enough data for t-test
  }
}

# Apply balance check to each leaf
balance_results <- lapply(unique(data$leaf), balance_check)

# Print the balance results for the first few leaves
balance_results[1:5]

# Debugging: Print the first few leaves and their data
for (i in 1:5) {
  cat("Leaf:", unique(data$leaf)[i], "\n")
  print(data[data$leaf == unique(data$leaf)[i], ])
}