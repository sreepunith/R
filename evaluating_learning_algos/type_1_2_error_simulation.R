# PURPOSE: This is an experiment of simulating Type I & II error
################################################################################
# Type I error simulation
numTests <- 1000  #number of t-test
alphaSet = c(0.001, 0.01, 0.05, 0.1, 0.2)  #set of alpha values to test
pop <- rnorm(10000, mean = 100, sd = 20)

#A data.frame with three columns "err", "p-value", "alpha" full of NAs
sigTests <- data.frame(err = NA, pValue = NA, alpha = NA)

counter <- 1 #keeps track of row numbers in the output table
for (i in 1:numTests) {
  for (alpha in alphaSet) {
    # take two samples from the same population
    samp1 <- sample(pop, 100, repl = F)
    samp2 <- sample(pop, 100, repl = F)
    # test sample means
    test.result <- t.test(samp1, samp2)
    # recored results of test
    if (test.result$p.value < alpha) {
      sigTests[counter, 1] <- 1
      sigTests[counter, 2] <- test.result$p.value
      sigTests[counter, 3] <- alpha
    } else {
      sigTests[counter, 1] <- 0
      sigTests[counter, 2] <- test.result$p.value
      sigTests[counter, 3] <- alpha
    }
    counter <- counter + 1 #move to the next row
  }
} 
aggregate(sigTests$err ~ sigTests$alpha, FUN = sum)  #produce results of experiment

################################################################################
# Type II error simulation
numTests <- 1000  #number of t-test
alphaSet = c(0.001, 0.01, 0.05, 0.1, 0.2)  #set of alpha values to test
difference <- 0.01
pop1 <- rnorm(10000, mean = 100, sd = 20)
pop2 <- rnorm(10000, mean = 100 + difference, sd = 20)

#A data.frame with three columns "err", "p-value", "alpha" full of NAs
sigTests <- data.frame(err = NA, pValue = NA, alpha = NA)

counter <- 1 #keeps track of row numbers in the output table
for (i in 1:numTests) {
  for (alpha in alphaSet) {
    # take two samples from DIFFERENT populations
    samp1 <- sample(pop1, 100, replace = F)
    samp2 <- sample(pop2, 100, replace = F)
    # test sample means
    test.result <- t.test(samp1, samp2)
    # recored results of test
    if (test.result$p.value > alpha) { # accept null hypothesis
      sigTests[counter, 1] <- 1
      sigTests[counter, 2] <- test.result$p.value
      sigTests[counter, 3] <- alpha
    } else { # reject
      sigTests[counter, 1] <- 0
      sigTests[counter, 2] <- test.result$p.value
      sigTests[counter, 3] <- alpha
    }
    counter <- counter + 1 #move to the next row
  }
}
aggregate(sigTests$err ~ sigTests$alpha, FUN = sum)  #produce results of experiment
