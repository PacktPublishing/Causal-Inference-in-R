library(bnlearn)
library(Rgraphviz)

# Create a complex dataset
set.seed(400)
n <- 1000

data <- data.frame(
  age = sample(18:60, n, replace = TRUE),
  education = sample(0:5, n, replace = TRUE),  # 0: No education, 1: Primary, 2: Secondary, 3: High school, 4: University, 5: Postgraduate
  employed = sample(c(0,1), n, replace = TRUE, prob = c(0.3, 0.7)),  # 0: Unemployed, 1: Employed
  income = round(rlnorm(n, meanlog = 7, sdlog = 1)),  # Monthly income in thousands of Colombian Pesos
  dependents = sample(0:5, n, replace = TRUE, prob = c(0.2, 0.3, 0.2, 0.15, 0.1, 0.05)),
  domestic_violence = sample(c(0,1), n, replace = TRUE, prob = c(0.7, 0.3)),  # 0: No history, 1: History of domestic violence
  region = sample(c("Urban", "Rural"), n, replace = TRUE, prob = c(0.6, 0.4)),
  organized_crime = sample(1:5, n, replace = TRUE),  # Scale of 1-5, 5 being highest presence
  local_unemployment = round(runif(n, 5, 25), 1),  # Percentage
  social_services = sample(1:5, n, replace = TRUE)  # Scale of 1-5, 5 being highest access
)

# Calculate probability of involvement based on the factors
prob_involvement <- with(data, 
                         (age - 30)^2 / 1000 - 
                           0.1 * education + 
                           0.2 * (1 - employed) + 
                           (7000 - income) / 10000 + 
                           0.05 * dependents + 
                           0.2 * domestic_violence + 
                           0.1 * (region == "Rural") + 
                           0.1 * organized_crime + 
                           0.01 * local_unemployment - 
                           0.1 * social_services
)

# Normalize probabilities
prob_involvement <- (prob_involvement - min(prob_involvement)) / (max(prob_involvement) - min(prob_involvement))

# Assign involvement status
data$involved <- rbinom(n, 1, prob_involvement)

# Convert all variables to factors (categorical) for bnlearn
data[] <- lapply(data, factor)




# Plot Bayesian Network with improved styling
plot_bn <- function(bn) {
  # Convert bnlearn object to igraph object
  ig <- bnlearn::as.igraph(bn)

  
  # Create a named vector for node labels (using the node names from the Bayesian Network)
  node_labels <- c("Age", "Education", "Employed", "Income", "Dependents", 
                   "Domestic Violence", "Region", "Organized Crime",
                   "Local Unemployment", "Social Services", "Involved")
  names(node_labels) <- V(ig)$name
  
  # Set layout
  layout <- layout_with_fr(ig)
  
  # Set node attributes for better visibility and consistency
  V(ig)$color <- "lightblue"  # Soft background color for nodes
  V(ig)$frame.color <- "darkblue"  # Darker border around nodes for contrast
  V(ig)$size <- 20  # Set node size for better visibility
  V(ig)$label.color <- "darkblue"  # Use dark color for text to improve contrast
  V(ig)$label.cex <- 1  # Adjust label font size
  V(ig)$label.font <- 2  # Make labels bold
  V(ig)$label.dist <- 2.5  # Move labels below the nodes
  
  # Set edge attributes for better visibility
  E(ig)$color <- "gray30"  # Neutral color for edges
  E(ig)$arrow.size <- 0.5  # Arrow size for clarity
  E(ig)$width <- 2  # Thicker edges for visibility
  
  # Plot the Bayesian Network with improved aesthetics
  plot(ig, 
       layout = layout, 
       vertex.label = node_labels[V(ig)$name], 
       vertex.label.dist = 2.5,  # Distance of labels from nodes
       vertex.label.degree = pi / 2,  # Position labels below nodes
       edge.arrow.mode = 1,  # Use arrows for directed edges
       main = "Bayesian Network: Women's Involvement in Drug Trafficking",
       sub = "Learned using Hill-Climbing Algorithm",
       margin = c(0.1, 0.1, 0.1, 0.1))  # Add margins for better spacing
}

# Learn the structure of the Bayesian Network
bn <- hc(data)

# Call the function to plot the Bayesian Network
plot_bn(bn)

# Print summary of the Bayesian Network
print("Summary of the Bayesian Network:")
print(bn)

# Perform some basic inference
cpdag <- cpdag(bn)
print("\nMarkov Blanket of 'involved':")
print(mb(cpdag, "involved"))

print("\nParents of 'involved':")
print(parents(cpdag, "involved"))

print("\nChildren of 'involved':")
print(children(cpdag, "involved"))

# Fit the parameters of the Bayesian Network
fitted_bn <- bn.fit(bn, data)

# Print some conditional probability tables
print("\nConditional Probability Table for 'involved':")
print(fitted_bn$involved)

# Perform some example queries
print("\nExample Query: Probability of involvement given high education and employment")
cpquery(fitted_bn, event = (involved == 1), evidence = (education == 5 & employed == 1))

print("\nExample Query: Probability of involvement given low education and unemployment")
cpquery(fitted_bn, event = (involved == 1), evidence = (education == 1 & employed == 0))

# Assess the network score
print("\nNetwork Score (BIC):")
print(score(bn, data, type = "bic"))

# Cross-validate the network
cv_score <- bn.cv(data, bn, k = 10, loss = "logl")
print("\nCross-validated log-likelihood loss:")
print(mean(cv_score))