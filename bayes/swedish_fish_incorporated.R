# Swedish Fish Incorporated is the largest Swedish company delivering fish by mail 
# order. They are now trying to get into the lucrative Danish market by selling 
# one year Salmon subscriptions. The marketing department have done a pilot study 
# and tried the following marketing method:
# A: Sending a mail with a colorful brochure that invites people to sign up for 
# a one year salmon subscription.
# The marketing department sent out 16 mails of type A. Six Danes that received 
# a mail signed up for one year of salmon and marketing now wants to know, how 
# good is method A?
# Source: https://goo.gl/cxfnYK
################################################################################
# Number of random draws from the prior
n_draws <- 10000

prior <- runif(n_draws, min = 0, max = 1) # Here you sample n_draws draws from the prior  
hist(prior) # It's always good to eyeball the prior to make sure it looks ok.

# Here you define the generative model
generative_model <- function(parameters) {
  subscribers <- rbinom(1, size = 16, prob = rate)
  subscribers
}

# Here you simulate data using the parameters from the prior and the 
# generative model
sim_data <- rep(NA, n_draws)
for(i in 1:n_draws) {
  sim_data[i] <- generative_model(prior[i])
}

# Here you filter off all draws that do not match the data.
posterior <- prior[sim_data == observed_data] 

hist(posterior) # Eyeball the posterior
length(posterior) # See that we got enought draws left after the filtering.
# There are no rules here, but you probably want to aim
# for >1000 draws.

# Now you can summarize the posterior, where a common summary is to take the mean
# or the median posterior, and perhaps a 95% quantile interval.
median(posterior)
quantile(posterior, c(0.025, 0.975))