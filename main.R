library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source("util/util.R")

# Prepare the long format csv data consisting of:
# Column 1 = Index of Examinees
# Column 2 = Index of Raters
# Column 3 = Index of Time Point
# Column 4 = Corresponding Score
# * All indexes and scores must be started from 1.
# * Set the header in the CSV file as (ExamineesID, RaterID, TimeID, Score)

# Set the numbers of examinees, raters, time points, and score categories as follows:
setting <- list(n_person = 134, # number of examinees
                n_rater = 15, # number of raters
                n_time = 4, # number of time points
                K = 5, # number of score categories 
                hyperprior_beta_rt = c(-2, 1) # hyper-parameters in LN(mu, delta)
                )

# Read the data CSV file
data <- read_data(setting, paste("data_full.csv", sep=""))

# Compile the STAN code for the proposed model.
stan <- stan_model(file="util/proposed.stan")

# Run the MCMC to estimate model parameters.
fit <- sampling(stan, data=data, iter=3000, warmup=2000, chains=3)

# Get the EAP estimates for all the parameters.
param <- get_estimates(fit, setting)

# Print rater parameters
param$rater_parameters

# Print examinee abilities
param$examinee_ability

