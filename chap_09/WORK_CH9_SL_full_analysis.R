# Install necessary packages if not already installed
packages_needed <- c("tmle", "SuperLearner", "randomForest", "gbm")
new_packages <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load required libraries
library(tmle)
library(SuperLearner)
library(randomForest)
library(gbm)

# GENERATE DATA
set.seed(123) # For reproducibility
n <- 500 # Number of observations
packaging <- rbinom(n, 1, 0.5) # Treatment assignment
weather <- runif(n, 5, 30) # Weather conditions
festival_proximity <- rbinom(n, 1, 0.5) # Whether close to a festival or not
store_capacity <- rnorm(n, 100, 20) # Store capacity
competitors_pricing <- runif(n, 50, 150) # Competitors' pricing strategies

# Outcome model (True effect + noise)
sales <- 200 + 50 * packaging + 10 * weather - 15 * festival_proximity +
  0.5 * store_capacity - 0.25 * competitors_pricing + rnorm(n, 0, 50)

data <- data.frame(packaging, weather, 
                   festival_proximity, store_capacity, 
                   competitors_pricing, sales)

# ESTIMATE PROPENSITY SCORES
# Logistic regression for propensity scores
propensity_model <- glm(packaging ~ weather + festival_proximity + store_capacity + competitors_pricing,
                        family = binomial(), data = data)
data$propensity_score <- predict(propensity_model, type = "response")

# OUTCOME REGRESSION MODEL
outcome_model <- lm(sales ~ packaging + weather + festival_proximity + store_capacity + competitors_pricing, data = data)

# DOUBLY ROBUST ESTIMATION
# Inverse Probability of Treatment Weighting (IPTW)
weights <- ifelse(data$packaging == 1, 1 / data$propensity_score, 1 / (1 - data$propensity_score))

# Predicted sales under treatment and control, using the outcome model
predicted_sales_treated <- predict(outcome_model, newdata = transform(data, packaging = 1))
predicted_sales_control <- predict(outcome_model, newdata = transform(data, packaging = 0))

# DR Estimation (augmented IPW): the outcome-model contrast, plus the
# weighted residuals of each arm
residual_treated <- data$packaging * (data$sales - predicted_sales_treated)
residual_control <- (1 - data$packaging) * (data$sales - predicted_sales_control)
dr_estimate <- mean(predicted_sales_treated - predicted_sales_control +
                      weights * (residual_treated - residual_control))

print(paste("Doubly Robust Estimate:", dr_estimate))

# ROBUST STANDARD ERRORS
library(sandwich)

# Calculate robust standard errors
robust_se <- sqrt(diag(vcovHC(outcome_model, type = "HC1")))

# Update the summary of the outcome model with robust standard errors
coefs <- summary(outcome_model)$coefficients
robust_summary <- cbind(coefs[,1:2], Robust.Std.Error = robust_se, 
                        t = coefs[,1] / robust_se, 
                        `Pr(>|t|)` = 2 * (1 - pt(abs(coefs[,1] / robust_se), df = outcome_model$df.residual)))

# Print the updated summary table with robust standard errors
print(robust_summary)

# ADVANCED DOUBLY ROBUST ESTIMATION USING TMLE
# Define the list of candidate learners

# Define the list of candidate learners for SuperLearner
#SL.library <- c("SL.glm", "SL.randomForest", "SL.gbm") # slower
SL.library <- c("SL.glm", "SL.randomForest") # Removed SL.gbm for speed

# Applying TMLE for causal effect estimation
tmle_result <- tmle(Y = data$sales, 
                    A = data$packaging, 
                    W = data[, c("weather", "festival_proximity", "store_capacity", "competitors_pricing")],
                    Q.SL.library = SL.library,
                    cvQinit = 2,  # Reduce cross-validation folds
                    g.SL.library = SL.library)

# Print TMLE results
print(tmle_result)

# Extracting and printing the estimated Average Treatment Effect (ATE)
print(paste("TMLE Estimated ATE:", tmle_result$estimates$ATE$psi))
print(paste("TMLE ATE 95% CI:", 
            tmle_result$estimates$ATE$CI[1], "to", tmle_result$estimates$ATE$CI[2]))