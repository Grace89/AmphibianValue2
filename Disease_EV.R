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
  #Is already set in the R project file when you link the Github repo to your computer

# 1. Load packages --------------------------------------------------------

library(jagsUI)

# 2. Write model ----------------------------------------------------------
sink("model.txt")
cat("
model {
# Cost-avoidance analysis

# 1. Estimate the value of medical expenses that would have been spent in the absence of the amphibians

# Need to calculate the total number of human infections avoided with the presence of amphibians- then we can multiple this by the average medical expense per case (etc.)

    # Unit of analysis: individual amphibian 
    
    # Spatial scale:
    
    # Temporal scale: An entire summer?
    
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

#Infection rate per person relative to mosquito abundance
  #infection_rate <- ?????

# Medical costs accumulated per mosquito
medical ~ dnorm(100, 0.01)
    
# The average consumption of mosquito larvae that cause disease by amphibian larvae = (the size of 1 amphibian * how many mosquitos the amphibian eats) * the proportion of mosquitors that cause disease * the total abundance of amphibians in the area
mosquitosEaten = ((weight * consumption) * prop_disease) * abundance

# Number of human cases per mosquito?? 1 mosquito = 1 human infection?

  
# Cost avoided = total number of mosquitos eaten that would have caused disease * medical expenses that those diseases would have caused
Expenses = mosquitosEaten * medical
    

# 2. Estimate the reduced cost of pesticide use - private and social- attributed to the presence of amphibians


# 3. Estimate the numbser of human lives saved by amphibian presence

#The cost and probabilty of death per case
cost_per_case ~ dnorm(cost_per_case_MEAN, 1/(cost_per_case_SD^2))  ## HAVE DATA FOR MEAN/SD
prob_death_per_case ~ dnorm(prob_death_per_case_MEAN, 1/(death_per_case_SD^2))T(0,1)  ## HAVE DATA FOR MEAN/SD

#The number of cases per year 
annual_cases ~ dpois(human_pop*infection_rate) ## HAVE DATA TO DESCRIBE ANNUAL CASES
  #annual_cases ~ dnorm(cases_per_year_MEAN, 1/(cases_per_year_SD^2)) ## HAVE DATA for MEAN/SD 

#Calculating cost and deaths per year given the number of annual cases
cost_per_year <- annual_cases*cost_per_case ## Derived Parameter
death_per_year <- annual_cases*prob_death_per_case ## Derived parameter



# Derived quantities

}
",fill = TRUE)
sink()

# 3. MCMC settings --------------------------------------------------------

ni <- 500
nb <- 10
nt <- 5
nc <- 3
na <- 100

# 4. Bundle data & initial values --------------------------------------------

# Read in data
dat <- read.csv("DiseaseValues.csv")
str(dat)

# Bundle data
jags.data <- list(weight_mean = dat[dat$Variable == "Amph_weight", ]$Mean,
                  weight_prec = 1/(dat[dat$Variable == "Amph_weight", ]$SD)^2
)

# List parameters to moniter
params <- c("Expenses")

# Initial values
inits <- function(){list(
)} 
# 5. Run model ------------------------------------------------------------

out <- jags(data = jags.data, inits = inits, parameters.to.save = params,
            model.file = "model.txt", 
            n.chains = nc, n.thin = nt, 
            n.iter = ni, n.burnin = nb, n.adapt = na,
            parallel = TRUE,
            DIC = FALSE)

# 6. Check output ---------------------------------------------------------

print(out)

which(unlist(out$Rhat) > 1.1)

plot(out)

# 7. Figures --------------------------------------------------------------

