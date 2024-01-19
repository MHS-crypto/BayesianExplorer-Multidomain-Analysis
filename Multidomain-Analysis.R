# Load required libraries
library(rjags)
library(coda)



# Read data from the file
data <- read.table("Olympicsdata.txt", header = TRUE)

# Combine data across all years for the host country in the host year
X1 <- sum(data$hostM)
N1 <- sum(data$hostP)

# Combine data across all years for the host country in the previous year
X0 <- sum(data$preM)
N0 <- sum(data$preP)

# Specify the JAGS model
model_code <- "
model {
  # Likelihood
  X1 ~ dpois(N1 * lambda1)
  X0 ~ dpois(N0 * lambda0)

  # Priors
  lambda1 ~ dgamma(0.1, 0.1)
  lambda0 ~ dgamma(0.1, 0.1)
}
"

# Combine data in a list
jags_data <- list(X1 = X1, N1 = N1, X0 = X0, N0 = N0)

# Compile the model
model <- jags.model(textConnection(model_code), data = jags_data, n.chains = 3)

# Burn-in and sampling
update(model, 1000)
samples <- coda.samples(model, variable.names = c("lambda1", "lambda0"), n.iter = 5000)

# Plot posterior distributions
plot(samples)


# Part B

perform_bayesian_test <- function(alpha, beta, scale1, scale0, iterations = 100000) {
  model_code <- "
  model {
    # Likelihood
    X1 ~ dpois(N1 * lambda1)
    X0 ~ dpois(N0 * lambda0)

    # Priors with hyper-prior parameters
    lambda1 ~ dgamma(alpha, beta)
    lambda0 ~ dgamma(alpha, beta)
  }
  "
  
  # Combine data and hyper-prior parameters in a list
  jags_data <- list(X1 = X1, N1 = N1, X0 = X0, N0 = N0, alpha = alpha, beta = beta)
  
  model <- jags.model(textConnection(model_code), data = jags_data, n.chains = 3)
  
  update(model, 1000)
  samples <- coda.samples(model, variable.names = c("lambda1", "lambda0"), n.iter = iterations)
  
  # Extract posterior samples
  posterior_samples <- as.matrix(samples)
  
  # Compute the posterior probability of λ1 > λ0
  posterior_prob <- mean(posterior_samples[, "lambda1"] > posterior_samples[, "lambda0"])
  
  return(posterior_prob)
}

# Specify multiple sets of hyper-prior parameters
hyperparameters <- list(
  c(alpha = 0.1, beta = 0.1),
  c(alpha = 1, beta = 1),
  c(alpha = 0.5, beta = 0.5),
  c(alpha = 2, beta = 2)
)

results <- sapply(hyperparameters, function(params) {
  perform_bayesian_test(params[1], params[2], scale1 = 1, scale0 = 1)
})

cat("Posterior Probability of λ1 > λ0 for different hyper-prior parameters:\n")
print(data.frame(hyperparameters, Posterior_Probability = results))



# Part C
perform_country_analysis <- function(country_data, alpha, beta, iterations = 100000) {
  # Extract data for the country
  X1 <- sum(country_data$hostM)
  N1 <- sum(country_data$hostP)
  X0 <- sum(country_data$preM)
  N0 <- sum(country_data$preP)
  
  # Specify the JAGS model with a single set of hyper-parameters
  model_code <- "
  model {
    # Likelihood
    X1 ~ dpois(N1 * lambda1)
    X0 ~ dpois(N0 * lambda0)

    # Prior with a single set of hyper-parameters
    lambda1 ~ dgamma(alpha, beta)
    lambda0 ~ dgamma(alpha, beta)
  }
  "
  
  # Combine data and hyper-parameters in a list
  jags_data <- list(X1 = X1, N1 = N1, X0 = X0, N0 = N0, alpha = alpha, beta = beta)
  
  # Compile the model
  model <- jags.model(textConnection(model_code), data = jags_data, n.chains = 3)
  
  # Burn-in and sampling
  update(model, 1000)
  samples <- coda.samples(model, variable.names = c("lambda1", "lambda0"), n.iter = iterations)
  
  # Extract posterior samples
  posterior_samples <- as.matrix(samples)
  
  # Compute the posterior distribution of the ratio r = lambda1 / lambda0
  r_posterior <- posterior_samples[, "lambda1"] / posterior_samples[, "lambda0"]
  
  return(r_posterior)
}

# Choose a country (e.g., Finland)
country_data <- subset(data, hostC == "Japan")

# Specify hyper-parameters
alpha <- 0.1
beta <- 0.1

# Perform country-specific Bayesian analysis for the chosen hyper-parameters
result <- perform_country_analysis(country_data, alpha, beta)

# Compute summary statistics
mean_r <- mean(result)
median_r <- median(result)
cred_interval <- quantile(result, c(0.025, 0.975))
prob_positive_effect <- mean(result > 1)

# Display summary statistics
cat("Mean:", mean_r, "\n")
cat("Median:", median_r, "\n")
cat("95% Credible Interval:", cred_interval, "\n")
cat("Probability of Positive Effect:", prob_positive_effect, "\n")



# Part d


# Data for 2021 Olympics
participants_2021 <- 398
medals_2021 <- 33

# Participants for 2024
participants_2024 <- 500

# Specify the JAGS model with hyper-prior parameters
model_code <- "
model {
  # Likelihood
  medals_2021 ~ dpois(rate_2021)
  
  # Prior
  rate_2021 ~ dgamma(alpha, beta)
  
  # Prediction for 2024
  medals_2024 ~ dpois(rate_2024)
  rate_2024 <- rate_2021 * (participants_2024 / participants_2021)
}
"


jags_data <- list(medals_2021 = medals_2021, participants_2021 = participants_2021, medals_2024 = NA, participants_2024 = participants_2024, alpha = 0.1, beta = 0.1)

# Compile the model
model <- jags.model(textConnection(model_code), data = jags_data, n.chains = 3)

update(model, 1000)
samples <- coda.samples(model, variable.names = c("medals_2024"), n.iter = 10000)

# Extract posterior samples for each chain
posterior_samples_chain1 <- samples[[1]]
posterior_samples_chain2 <- samples[[2]]
posterior_samples_chain3 <- samples[[3]]

# Combine samples into a single mcmc object
posterior_samples <- mcmc.list(posterior_samples_chain1, posterior_samples_chain2, posterior_samples_chain3)

# Calculate summary statistics
summary_stats <- summary(posterior_samples)

print(summary_stats)



# Q2
# Part A


# Data
operator1_data <- c(4.3, 4.3, 2.7, 3.6, 3.5, 4.5)
operator2_data <- c(3.9, 4.0, 4.5, 2.9, 5.2, 4.8)

# Combine data
data <- list(
  operator1_data = operator1_data,
  operator2_data = operator2_data,
  N1 = length(operator1_data),
  N2 = length(operator2_data),
  nu = 0.2,
  mu_prior_mean = 4.7,
  mu_prior_precision = 1,
  tau1_alpha = 4,
  tau1_beta = 5,
  tau2_alpha = 4,
  tau2_beta = 5
)

# Model
model_code <- "
model {
  # Likelihood
  for (i in 1:N1) {
    operator1_data[i] ~ dnorm(mu, tau1)
  }

  for (i in 1:N2) {
    operator2_data[i] ~ dnorm(mu, tau2)
  }

  # Priors
  mu ~ dnorm(mu_prior_mean, mu_prior_precision)
  tau1 ~ dgamma(tau1_alpha, tau1_beta)
  tau2 ~ dgamma(tau2_alpha, tau2_beta)

  # Derived parameter
  phi <- tau2 / tau1
}"

# Compile the model
model <- jags.model(textConnection(model_code), data = data, n.chains = 3)

# Burn-in and sampling
update(model, 1000)
samples <- coda.samples(model, variable.names = c("mu", "phi"), n.iter = 10000)

# Extract posterior samples
posterior_samples <- as.matrix(samples)

# Summary statistics
summary_stats <- summary(posterior_samples)

# Display results
print(summary_stats)


# Part B

model_code <- "
model {
  # Likelihood
  for (i in 1:N1) {
    y1[i] ~ dnorm(mu, tau1)
  }
  for (i in 1:N2) {
    y2[i] ~ dnorm(mu, tau2)
  }

  # Priors
  mu ~ dnorm(mu_prior_mean, mu_prior_precision)
  tau1 ~ dgamma(tau1_alpha, tau1_beta)
  tau2 ~ dgamma(tau2_alpha, tau2_beta)
}
"

# Compile the model
model <- jags.model(textConnection(model_code), data = data, n.chains = 3)

# Burn-in and sampling
update(model, 1000)
samples <- coda.samples(model, variable.names = c("mu", "tau1", "tau2"), n.iter = 10000)

# Extract posterior samples
posterior_samples <- as.matrix(samples)

# Calculate summary statistics or perform further analysis as needed
summary_stats <- summary(posterior_samples)
print(summary_stats)



# Q3

# Data
y <- c(190.5, 172.5, 167.0, 169.5, 175.0, 177.5, 179.5, 179.5, 173.5, 162.5, 178.5, 171.5, 180.5, 183.0, 169.5, 172.0, 170.0)
z <- c(182.5, 179.5, 191.0, 184.5, 181.0, 173.5, 188.5, 175.0, 196.0, 200.0, 185.0, 174.5, 195.5, 197.0, 182.5)

# Combine data
data <- list(
  y = y,
  z = z,
  Ny = length(y),
  Nz = length(z),
  mu1_prior_mean = 100,
  mu2_prior_mean = 100,
  tau_prior_alpha = 0.1,
  tau_prior_beta = 0.1
)

# JAGS model
model_code <- "
model {
  # Priors
  mu1 ~ dnorm(mu1_prior_mean, 1 / 10000)
  mu2 ~ dnorm(mu2_prior_mean, 1 / 10000)
  tau ~ dgamma(tau_prior_alpha, tau_prior_beta)

  # Likelihood
  for (i in 1:Ny) {
    y[i] ~ dnorm(mu1, tau)
  }

  for (j in 1:Nz) {
    z[j] ~ dnorm(mu2, tau)
  }

  # Difference
  diff_mu <- mu1 - mu2
}"

# Compile the model
model <- jags.model(textConnection(model_code), data = data, n.chains = 3)

# Burn-in and sampling
update(model, 1000)
samples <- coda.samples(model, variable.names = "diff_mu", n.iter = 10000)

# Extract posterior samples
posterior_samples <- as.matrix(samples)

# Calculate summary statistics
summary_stats <- summary(posterior_samples[, "diff_mu"])

print(summary_stats)




