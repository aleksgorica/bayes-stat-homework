library(R2BayesX)

ns <- 200
N <- 100
beta <- c(10, 0.5, 0, 2)




simulate_data <- function(N = 100, beta = c(1, 0.5, -0.5, 0, 0), intercept = 2, variance = 1, distribution = "normal") {
  X <- matrix(rnorm(N * length(beta)), N, length(beta))

  if (distribution == "normal") {
    epsilon <- rnorm(N, 0, sqrt(variance))
  } else if (distribution == "chi-squared") {
    epsilon <- rchisq(N, df = 1) - 1 # Centriramo, da ima povprečje 0
    epsilon <- epsilon * sqrt(variance / var(epsilon)) # Skaliramo na izbrano varianco
  }
  
  Y <- X %*% matrix(beta) + intercept + epsilon
  list(X = X, Y = Y)
}


library(R2BayesX)

# Assuming simulate_data function is defined as above

run_simulation_bayesx <- function(reps = 100, N = 100, beta = c(1, 0.5, -0.5, 0, 0), intercept = 2, variance = 1, distribution = "normal") {
  results <- list(frequentist = list(), bayesian = list())
  
  for (i in 1:reps) {
    simulated_data <- simulate_data(N, beta, intercept, variance, distribution)
    Y <- simulated_data$Y
    X <- simulated_data$X
    data <- data.frame(Y = Y, X1 = X[,1], X2 = X[,2], X3 = X[,3], X4 = X[,4], X5 = X[,5])
    
    # Frequentist approach using lm()
    lm_model <- lm(Y ~ ., data = data)
    results$frequentist[[i]] <- summary(lm_model)$coefficients
    
    # Bayesian approach using BayesX
    # Note: BayesX syntax for specifying models is different, and it requires data frames or formulae
    # Here, we fit a similar model as with lm(), but using BayesX
    bayesx_model <- bayesx(Y ~ X1 + X2 + X3 + X4 + X5, data = data, family = "gaussian", method = "MCMC")
    
    # Assuming we want to extract estimated coefficients or similar summary stats
    # Note: Adjust the following line according to the specific output you're interested in from BayesX
    results$bayesian[[i]] <- bayesx_model$fixed.effects[, c("Mean", "Sd")]
    # Note: BayesX provides a wide range of outputs and diagnostics, the extraction of which 
    # may vary depending on the specifics of the model and desired results.
    }
  
  results
}

# Running the simulation
beta = c(1, 0.5, -0.5, 0, 0)
simulation_results <- run_simulation_bayesx(reps=100)

# Example dimensions
n_variables <- nrow(simulation_results$bayesian[[1]]) # Assuming all simulations return the same number of variables
n_statistics <- ncol(simulation_results$bayesian[[1]]) # Assuming the same statistics are calculated for each
n_simulations <- length(simulation_results$bayesian)

# Initialize the 3D matrix
results_3D <- array(NA, dim = c(n_variables, n_statistics, n_simulations))

for (i in 1:n_simulations) {
  results_3D[, , i] <- simulation_results$bayesian[[i]]
}

mean_over_draws <- apply(results_3D, c(1, 2), mean)



# Convert the Bayesian results to a more easily analyzable structure, such as a data frame
#bayesian_estimates <- do.call(rbind, simulation_results$bayesian)

#means <- c(0,0,0,0,0)
#means[1] <-mean(bayesian_estimates[which(rownames(bayesian_estimates) == "X1"),"Mean"])
#means[2] <-mean(bayesian_estimates[which(rownames(bayesian_estimates) == "X2"),"Mean"])
#means[3] <-mean(bayesian_estimates[which(rownames(bayesian_estimates) == "X3"),"Mean"])
#means[4] <-mean(bayesian_estimates[which(rownames(bayesian_estimates) == "X4"),"Mean"])
#means[5] <-mean(bayesian_estimates[which(rownames(bayesian_estimates) == "X5"),"Mean"])
#sds <- c(0,0,0,0,0)
#sds[1] <-mean(bayesian_estimates[which(rownames(bayesian_estimates) == "X1"),"Sd"])
#sds[2] <-mean(bayesian_estimates[which(rownames(bayesian_estimates) == "X2"),"Sd"])
#sds[3] <-mean(bayesian_estimates[which(rownames(bayesian_estimates) == "X3"),"Sd"])
#sds[4] <-mean(bayesian_estimates[which(rownames(bayesian_estimates) == "X4"),"Sd"])
#sds[5] <-mean(bayesian_estimates[which(rownames(bayesian_estimates) == "X5"),"Sd"])


sds
bias <- means - beta

# Print the overall means and SDs
print(overall_means)
print(overall_sds)

