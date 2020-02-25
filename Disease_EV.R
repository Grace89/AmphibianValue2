# This code is written by: G.V. DiRenzo, A. D. Wright, and R. F. Bernard
# If you have any questions, please email: XXX@XXX
# The objective of this code 
  # To perform a cost avoidance analysis for how well amphibians serve as a mosquito control 

################################
###### Table of Contents #######
################################

# 0. Set working directory
# 1. Load packages
# 2. Write model
# 3. MCMC settings
# 4. Bundle data & initial values
# 5. Run model
# 6. Check output
# 7. Figures

# 0. Set working directory -------------------------------------------------


# 1. Load packages --------------------------------------------------------

library(jagsUI)

# 2. Write model ----------------------------------------------------------
sink("model.txt")
cat("
model {
# Cost-avoidance analysis

# 1. Estimate the value of medical expenses that would have been spent in the absence of the amphibians

# Unit of analysis: individual amphibian 

# Because the most effective way of controlling mosquito populations is via reductions in reproduction, our goal is to estimate the number of larvae prevented from reaching maturity by the presence of a single amphibian.

# The overall impact on human society is estimated by scaling up our population estimates of a single amphibian to amphibian population in the study area.

# Weight of an average amphibian
weight ~ dnorm(2.5, 0.01)

# Consumption of larvea per day per amphibian
consumption ~ dnorm(mu, 0.01)

# Abundance of amphibians
abundance ~ dnorm(10, 0.001)

# Any seasonality changes to amphibian diet? i.e., during the summer they eat more?
mu <- 5
# mu <- alpha + beta * month[i]

# The proportion of mosquitos that would have caused disease
# Seroprevalence
prop_disease ~ dnorm(0.5, 0.01)T(0,1)


# 2. Estimate the reduced cost of pesticide use - private and social- attributed to the presence of amphibians


# 3. Estimate the numbser of human lives saved by amphibianpresence


# Derived quantities

}
",fill = TRUE)
sink()

# 3. MCMC settings --------------------------------------------------------

ni <- 50000
nb <- 10000
nt <- 50
nc <- 3
na <- 10000

# 4. Bundle data & initial values --------------------------------------------

# Bundle data
jags.data <- list(
)

# List parameters to moniter
params <- c()

# Initial values
inits <- function(){list(
)} 
# 5. Run model ------------------------------------------------------------

out <- jags(data = jags.data, inits = inits, parameters.to.save = params,
            model.file = "model.txt", 
            n.chains = nc, n.thin = nt, 
            n.iter = ni, n.burnin = nb, n.adapt = na,
            parallel = TRUE)

# 6. Check output ---------------------------------------------------------

print(out)

which(unlist(out$Rhat) > 1.1)

plot(out)

# 7. Figures --------------------------------------------------------------


