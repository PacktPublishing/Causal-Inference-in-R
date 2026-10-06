
# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 500

# Generate synthetic data
data <- data.frame(
  comic_freq = sample(1:5, n, replace = TRUE), # 1: Rarely, 5: Very Frequently
  parents_edu = sample(c('Low', 'Medium', 'High'), n, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
  homework_time = rnorm(n, mean = 2, sd = 0.5), # Hours per day
  higher_studies = sample(c(0, 1), n, replace = TRUE) # 0: No, 1: Yes
)

# Fit a logistic regression model
model <- glm(higher_studies ~ comic_freq + parents_edu + homework_time, 
             data = data, family = binomial)

# Print model summary
summary(model)

# Extract odds ratio and confidence interval for comic_freq
or <- exp(coef(model)["comic_freq"])
ci <- exp(confint(model)["comic_freq",])

# Print odds ratio and confidence interval
cat("Odds Ratio for comic_freq:", or, "\n")
cat("95% Confidence Interval:", ci[1], "-", ci[2], "\n")

# Calculate the prevalence of the outcome
outcome_prevalence <- mean(data$higher_studies)

# Function to calculate E-value
calculate_evalue <- function(estimate) {
  if (estimate < 1) estimate <- 1 / estimate
  return(estimate + sqrt(estimate * (estimate - 1)))
}

# Calculate E-values
evalue_point <- calculate_evalue(or)
# The E-value of the interval uses the limit closer to the null, and is 1
# when the interval contains 1: no confounding is needed to explain it away
ci_limit <- if (or >= 1) ci[1] else ci[2]
evalue_lower <- if (ci[1] <= 1 && ci[2] >= 1) 1 else calculate_evalue(ci_limit)

# Print E-values
cat("E-value (point estimate):", evalue_point, "\n")
cat("E-value (lower confidence limit):", evalue_lower, "\n")

# Create data for plotting
conf_strengths <- seq(1, max(evalue_point, evalue_lower) + 0.5, by = 0.1)
bias_factors <- sapply(conf_strengths, function(x) x + sqrt(x * (x-1)))

# Plot
plot(conf_strengths, bias_factors, type = "l", 
     xlab = "Confounder-Outcome Relative Risk", 
     ylab = "E-value", 
     main = "Sensitivity Analysis Plot")

# Calculate a suitable x-position for text
x_pos <- min(conf_strengths) + (max(conf_strengths) - min(conf_strengths)) * 0.02

# Calculate y-axis offset (adjust this value as needed)
y_offset <- (max(bias_factors) - min(bias_factors)) * 0.02

abline(h = or, col = "red", lty = 2)
text(x_pos, or + y_offset, "Observed OR", pos = 4, col = "red", adj = 0)

abline(h = evalue_point, col = "blue", lty = 2)
text(x_pos, evalue_point + y_offset, "E-value (point estimate)", pos = 4, col = "blue", adj = 0)

abline(h = evalue_lower, col = "green", lty = 2)
text(x_pos, evalue_lower + y_offset, "E-value (lower CI)", pos = 4, col = "green", adj = 0)