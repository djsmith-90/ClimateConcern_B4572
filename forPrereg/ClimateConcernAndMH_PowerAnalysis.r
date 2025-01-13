#### Simulation-based power analysis script for whether climate concern causes subsequent mental health
#### Created by Dan Major-Smith
#### R version 4.3.1


###############################################################################################################
#### Clear workspace and install/load packages

rm(list = ls())

#install.packages("daggity")
library(dagitty)

library(nnet)


###########################################################################################
### Make a simple DAG for data-generating mechanism for this simulated dataset
dag <- dagitty('dag {
               Confounders [pos = "0,0"]
               Prior_MH [pos = "1,1"]
               Climate_concern [pos = "2,0"]
               Later_MH [pos = "3,1"]
               
               Confounders -> Prior_MH
               Confounders -> Climate_concern
               Confounders -> Later_MH
               Prior_MH -> Climate_concern
               Prior_MH -> Later_MH
               Climate_concern -> Later_MH
               }
               ')
plot(dag)


## Set-up the simulations using a massive sample size (1 million) to remove unwanted variability and set seed
n <- 1000000
set.seed(4655428)


## Simulate 'Confounders' - Continuous variable caused by nothing
confounders <- rnorm(n = n, mean = 0, sd = 1)

## Simulate prior mental health - Continuous variable caused by confounders (could represent, e.g., higher rates of mental health problems among women, lower SEP, etc.)
prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
summary(prior_mh)

# Check model and results are sensible
#plot(prior_mh ~ confounders)
summary(lm(prior_mh ~ confounders))


## Next, simulate exposure 'climate concern' - Three-level categorical variable (with levels 'not at all/not very concerned' [10%], 'somewhat concerned' [50%] and 'very concerned' [40%])

# Calculate the denominator of the multinomial regression model, with 'no' as the baseline
denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
  exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
head(denom)

# Calculate predicted probabilities for each outcome
prob_none <- 1 / denom
prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
head(cbind(prob_none, prob_some, prob_very))

# construct a random uniform variable and generate multinomial outcomes using this random unform variable and the predicted probabilities
rand <- runif(n = n, min = 0, max = 1)

climate_concern <- ifelse(rand < prob_none, 1,
                        ifelse(rand < prob_none + prob_some, 2, 3))
table(climate_concern)
table(climate_concern) / sum(table(climate_concern)) * 100

# Check model and results are sensible
by(prior_mh, climate_concern, summary)
by(confounders, climate_concern, summary)
summary(multinom(climate_concern ~ confounders + prior_mh))

# Also convert these to dummy variables, as need these to simulated when constructing the outcome
climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
table(climate_concern_somewhat); table(climate_concern_very)

# Remove some of the temporary variables
rm(rand)
rm(denom)
rm(prob_none)
rm(prob_some)
rm(prob_very)


## Finally, simulate the 'later MH' outcome - Continuous variable caused by confounders, prior MH and climate concern - Want this to be standardised, so mean = 0 and SD = 1 (interpretation of effect sizes is therefore broadly comparable to Cohen's d) - Starting with value of 0.1 (very small effect size)
later_mh <- -2.1 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.1) +
  (climate_concern_very * 0.1) + rnorm(n = n, mean = 0, sd = 0.8)
summary(later_mh); sd(later_mh)
hist(later_mh)

# Check model and that results are sensible
by(later_mh, climate_concern, summary)
#plot(later_mh ~ prior_mh)
summary(lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh))



#####################################################################################
### Okay, now plug these values into a loop with the expected complete-case sample size of 1,000, and estimate the power with effect sizes of 0.1

## Set up objects to store power analysis results
i <- 1000
res <- as.data.frame(cbind(iteration = 1:i, b_somewhat = rep(NA, i), p_somewhat = rep(NA, i),
                       b_very = rep(NA, i), p_very = rep(NA, i)))
res

## Loop over 1,000 iterations, simulate data of n = 1,000, and store effect sizes and p-values
set.seed(32123)
n <- 1000

for (j in 1:i) {
  print(paste0("On iteration: ", j))
  print("")
  
  # Simulate the variables (as above)
  
  ## Confounders and prior MH
  confounders <- rnorm(n = n, mean = 0, sd = 1)
  prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)

  ## Climate concern exposure
  # Calculate the denominator of the multinomial regression model, with 'no' as the baseline
  denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
    exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
  
  # Calculate predicted probabilities for each outcome
  prob_none <- 1 / denom
  prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
  prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
  
  # construct a random uniform variable and generate multinomial outcomes using this random uniform variable and the predicted probabilities
  rand <- runif(n = n, min = 0, max = 1)
  climate_concern <- ifelse(rand < prob_none, 1,
                        ifelse(rand < prob_none + prob_some, 2, 3))

  # Also convert these to dummy variables
  climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
  climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
  table(climate_concern_somewhat); table(climate_concern_very)
  
  ## Later MH outcome
  later_mh <- -2.1 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.1) +
    (climate_concern_very * 0.1) + rnorm(n = n, mean = 0, sd = 0.8)
  
  ### Run the model and extract relevant statistics
  mod <- lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh)
  
  res[j, "b_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat", "Estimate"]
  res[j, "p_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat", "Pr(>|t|)"]
  res[j, "b_very"] <- summary(mod)$coefficients["climate_concern_very", "Estimate"]
  res[j, "p_very"] <- summary(mod)$coefficients["climate_concern_very", "Pr(>|t|)"]
  
}

## Check the results table
res

## How many p-values are < 0.05

# 'Somewhat concerned' - Only ~19% power to detect an effect of this magnitude
sum(res$p_somewhat < 0.05)
sum(res$p_somewhat < 0.05) / j

# 'Very concerned' - Only ~18% power to detect an effect of this magnitude
sum(res$p_very < 0.05)
sum(res$p_very < 0.05) / j



###################################################################################
### Repeat above, but now with a larger effect size of interest (0.2, rather than 0.1)

## Set-up the simulations using a massive sample size (1 million) to remove unwanted variability and set seed
n <- 1000000
set.seed(4655428)


## Simulate 'Confounders' - Continuous variable caused by nothing
confounders <- rnorm(n = n, mean = 0, sd = 1)

## Simulate prior mental health - Continuous variable caused by confounders (could represent, e.g., higher rates of mental health problems among women, lower SEP, etc.)
prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
summary(prior_mh)

# Check model and results are sensible
#plot(prior_mh ~ confounders)
summary(lm(prior_mh ~ confounders))


## Next, simulate exposure 'climate concern' - Three-level categorical variable (with levels 'not at all/not very concerned' [10%], 'somewhat concerned' [50%] and 'very concerned' [40%])

# Calculate the denominator of the multinomial regression model, with 'no' as the baseline
denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
  exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
head(denom)

# Calculate predicted probabilities for each outcome
prob_none <- 1 / denom
prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
head(cbind(prob_none, prob_some, prob_very))

# construct a random uniform variable and generate multinomial outcomes using this random unform variable and the predicted probabilities
rand <- runif(n = n, min = 0, max = 1)

climate_concern <- ifelse(rand < prob_none, 1,
                      ifelse(rand < prob_none + prob_some, 2, 3))
table(climate_concern)
table(climate_concern) / sum(table(climate_concern)) * 100

# Check model and results are sensible
by(prior_mh, climate_concern, summary)
by(confounders, climate_concern, summary)
summary(multinom(climate_concern ~ confounders + prior_mh))

# Also convert these to dummy variables, as need these to simulated when constructing the outcome
climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
table(climate_concern_somewhat); table(climate_concern_very)

# Remove some of the temporary variables
rm(rand)
rm(denom)
rm(prob_none)
rm(prob_some)
rm(prob_very)


## Finally, simulate the 'later MH' outcome - Continuous variable caused by confounders, prior MH and climate concern - Want this to be standardised, so mean = 0 and SD = 1
later_mh <- -2.2 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.2) +
  (climate_concern_very * 0.2) + rnorm(n = n, mean = 0, sd = 0.79)
summary(later_mh); sd(later_mh)
hist(later_mh)

# Check model and that results are sensible
by(later_mh, climate_concern, summary)
#plot(later_mh ~ prior_mh)
summary(lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh))



#####################################################################################
### Okay, now plug these values into a loop with the expected complete-case sample size of 1,000, and estimate the power (with effect size of 0.2)

## Set up objects to store power analysis results
i <- 1000
res <- as.data.frame(cbind(iteration = 1:i, b_somewhat = rep(NA, i), p_somewhat = rep(NA, i),
                           b_very = rep(NA, i), p_very = rep(NA, i)))
res

## Loop over 1,000 iterations, simulate data of n = 1,000, and store effect sizes and p-values
set.seed(32123)
n <- 1000

for (j in 1:i) {
  print(paste0("On iteration: ", j))
  print("")
  
  # Simulate the variables (as above)
  
  ## Confounders and prior MH
  confounders <- rnorm(n = n, mean = 0, sd = 1)
  prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
  
  ## Climate concern exposure
  # Calculate the denominator of the multinomial regression model, with 'no' as the baseline
  denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
    exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
  
  # Calculate predicted probabilities for each outcome
  prob_none <- 1 / denom
  prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
  prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
  
  # construct a random uniform variable and generate multinomial outcomes using this random uniform variable and the predicted probabilities
  rand <- runif(n = n, min = 0, max = 1)
  climate_concern <- ifelse(rand < prob_none, 1,
                        ifelse(rand < prob_none + prob_some, 2, 3))
  
  # Also convert these to dummy variables
  climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
  climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
  table(climate_concern_somewhat); table(climate_concern_very)
  
  ## Later MH outcome
  later_mh <- -2.2 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.2) +
    (climate_concern_very * 0.2) + rnorm(n = n, mean = 0, sd = 0.79)
  
  ### Run the model and extract relevant statistics
  mod <- lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh)
  
  res[j, "b_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat", "Estimate"]
  res[j, "p_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat", "Pr(>|t|)"]
  res[j, "b_very"] <- summary(mod)$coefficients["climate_concern_very", "Estimate"]
  res[j, "p_very"] <- summary(mod)$coefficients["climate_concern_very", "Pr(>|t|)"]
  
}

## Check the results table
res

## How many p-values are < 0.05

# 'Somewhat concerned' - Now ~60% power to detect an effect of this magnitude
sum(res$p_somewhat < 0.05)
sum(res$p_somewhat < 0.05) / j

# 'Very concerned' - Now ~55% power to detect an effect of this magnitude
sum(res$p_very < 0.05)
sum(res$p_very < 0.05) / j



###################################################################################
### Repeat above, but now with a larger effect size of interest (0.3, rather than 0.1 or 0.2)

## Set-up the simulations using a massive sample size (1 million) to remove unwanted variability and set seed
n <- 1000000
set.seed(4655428)


## Simulate 'Confounders' - Continuous variable caused by nothing
confounders <- rnorm(n = n, mean = 0, sd = 1)

## Simulate prior mental health - Continuous variable caused by confounders (could represent, e.g., higher rates of mental health problems among women, lower SEP, etc.)
prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
summary(prior_mh)

# Check model and results are sensible
#plot(prior_mh ~ confounders)
summary(lm(prior_mh ~ confounders))


## Next, simulate exposure 'climate concern' - Three-level categorical variable (with levels 'not at all/not very concerned' [10%], 'somewhat concerned' [50%] and 'very concerned' [40%])

# Calculate the denominator of the multinomial regression model, with 'no' as the baseline
denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
  exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
head(denom)

# Calculate predicted probabilities for each outcome
prob_none <- 1 / denom
prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
head(cbind(prob_none, prob_some, prob_very))

# construct a random uniform variable and generate multinomial outcomes using this random unform variable and the predicted probabilities
rand <- runif(n = n, min = 0, max = 1)

climate_concern <- ifelse(rand < prob_none, 1,
                      ifelse(rand < prob_none + prob_some, 2, 3))
table(climate_concern)
table(climate_concern) / sum(table(climate_concern)) * 100

# Check model and results are sensible
by(prior_mh, climate_concern, summary)
by(confounders, climate_concern, summary)
summary(multinom(climate_concern ~ confounders + prior_mh))

# Also convert these to dummy variables, as need these to simulated when constructing the outcome
climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
table(climate_concern_somewhat); table(climate_concern_very)

# Remove some of the temporary variables
rm(rand)
rm(denom)
rm(prob_none)
rm(prob_some)
rm(prob_very)


## Finally, simulate the 'later MH' outcome - Continuous variable caused by confounders, prior MH and climate concern - Want this to be standardised, so mean = 0 and SD = 1
later_mh <- -2.3 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.3) +
  (climate_concern_very * 0.3) + rnorm(n = n, mean = 0, sd = 0.78)
summary(later_mh); sd(later_mh)
hist(later_mh)

# Check model and that results are sensible
by(later_mh, climate_concern, summary)
#plot(later_mh ~ prior_mh)
summary(lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh))



#####################################################################################
### Okay, now plug these values into a loop with the expected complete-case sample size of 1,000, and estimate the power (with effect size of 0.3)

## Set up objects to store power analysis results
i <- 1000
res <- as.data.frame(cbind(iteration = 1:i, b_somewhat = rep(NA, i), p_somewhat = rep(NA, i),
                           b_very = rep(NA, i), p_very = rep(NA, i)))
res

## Loop over 1,000 iterations, simulate data of n = 1,000, and store effect sizes and p-values
set.seed(32123)
n <- 1000

for (j in 1:i) {
  print(paste0("On iteration: ", j))
  print("")
  
  # Simulate the variables (as above)
  
  ## Confounders and prior MH
  confounders <- rnorm(n = n, mean = 0, sd = 1)
  prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
  
  ## Climate concern exposure
  # Calculate the denominator of the multinomial regression model, with 'no' as the baseline
  denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
    exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
  
  # Calculate predicted probabilities for each outcome
  prob_none <- 1 / denom
  prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
  prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
  
  # construct a random uniform variable and generate multinomial outcomes using this random uniform variable and the predicted probabilities
  rand <- runif(n = n, min = 0, max = 1)
  climate_concern <- ifelse(rand < prob_none, 1,
                        ifelse(rand < prob_none + prob_some, 2, 3))
  
  # Also convert these to dummy variables
  climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
  climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
  table(climate_concern_somewhat); table(climate_concern_very)
  
  ## Later MH outcome
  later_mh <- -2.3 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.3) +
    (climate_concern_very * 0.3) + rnorm(n = n, mean = 0, sd = 0.78)
  
  ### Run the model and extract relevant statistics
  mod <- lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh)
  
  res[j, "b_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat", "Estimate"]
  res[j, "p_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat", "Pr(>|t|)"]
  res[j, "b_very"] <- summary(mod)$coefficients["climate_concern_very", "Estimate"]
  res[j, "p_very"] <- summary(mod)$coefficients["climate_concern_very", "Pr(>|t|)"]
  
}

## Check the results table
res

## How many p-values are < 0.05

# 'Somewhat concerned' - Now ~93% power to detect an effect of this magnitude
sum(res$p_somewhat < 0.05)
sum(res$p_somewhat < 0.05) / j

# 'Very concerned' - Now ~90% power to detect an effect of this magnitude
sum(res$p_very < 0.05)
sum(res$p_very < 0.05) / j



###################################################################################
### Finally, repeat above again, but now with an intermediate effect size of interest (0.25, rather than 0.2 or 0.3)

## Set-up the simulations using a massive sample size (1 million) to remove unwanted variability and set seed
n <- 1000000
set.seed(4655428)


## Simulate 'Confounders' - Continuous variable caused by nothing
confounders <- rnorm(n = n, mean = 0, sd = 1)

## Simulate prior mental health - Continuous variable caused by confounders (could represent, e.g., higher rates of mental health problems among women, lower SEP, etc.)
prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
summary(prior_mh)

# Check model and results are sensible
#plot(prior_mh ~ confounders)
summary(lm(prior_mh ~ confounders))


## Next, simulate exposure 'climate concern' - Three-level categorical variable (with levels 'not at all/not very concerned' [10%], 'somewhat concerned' [50%] and 'very concerned' [40%])

# Calculate the denominator of the multinomial regression model, with 'no' as the baseline
denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
  exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
head(denom)

# Calculate predicted probabilities for each outcome
prob_none <- 1 / denom
prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
head(cbind(prob_none, prob_some, prob_very))

# construct a random uniform variable and generate multinomial outcomes using this random unform variable and the predicted probabilities
rand <- runif(n = n, min = 0, max = 1)

climate_concern <- ifelse(rand < prob_none, 1,
                      ifelse(rand < prob_none + prob_some, 2, 3))
table(climate_concern)
table(climate_concern) / sum(table(climate_concern)) * 100

# Check model and results are sensible
by(prior_mh, climate_concern, summary)
by(confounders, climate_concern, summary)
summary(multinom(climate_concern ~ confounders + prior_mh))

# Also convert these to dummy variables, as need these to simulated when constructing the outcome
climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
table(climate_concern_somewhat); table(climate_concern_very)

# Remove some of the temporary variables
rm(rand)
rm(denom)
rm(prob_none)
rm(prob_some)
rm(prob_very)


## Finally, simulate the 'later MH' outcome - Continuous variable caused by confounders, prior MH and climate concern - Want this to be standardised, so mean = 0 and SD = 1
later_mh <- -2.25 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.25) +
  (climate_concern_very * 0.25) + rnorm(n = n, mean = 0, sd = 0.785)
summary(later_mh); sd(later_mh)
hist(later_mh)

# Check model and that results are sensible
by(later_mh, climate_concern, summary)
#plot(later_mh ~ prior_mh)
summary(lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh))



#####################################################################################
### Okay, now plug these values into a loop with the expected complete-case sample size of 1,000, and estimate the power (with effect size of 0.25)

## Set up objects to store power analysis results
i <- 1000
res <- as.data.frame(cbind(iteration = 1:i, b_somewhat = rep(NA, i), p_somewhat = rep(NA, i),
                           b_very = rep(NA, i), p_very = rep(NA, i)))
res

## Loop over 1,000 iterations, simulate data of n = 1,000, and store effect sizes and p-values
set.seed(32123)
n <- 1000

for (j in 1:i) {
  print(paste0("On iteration: ", j))
  print("")
  
  # Simulate the variables (as above)
  
  ## Confounders and prior MH
  confounders <- rnorm(n = n, mean = 0, sd = 1)
  prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
  
  ## Climate concern exposure
  # Calculate the denominator of the multinomial regression model, with 'no' as the baseline
  denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
    exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
  
  # Calculate predicted probabilities for each outcome
  prob_none <- 1 / denom
  prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
  prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
  
  # construct a random uniform variable and generate multinomial outcomes using this random uniform variable and the predicted probabilities
  rand <- runif(n = n, min = 0, max = 1)
  climate_concern <- ifelse(rand < prob_none, 1,
                        ifelse(rand < prob_none + prob_some, 2, 3))
  
  # Also convert these to dummy variables
  climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
  climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
  table(climate_concern_somewhat); table(climate_concern_very)
  
  ## Later MH outcome
  later_mh <- -2.25 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.25) +
    (climate_concern_very * 0.25) + rnorm(n = n, mean = 0, sd = 0.785)
  
  ### Run the model and extract relevant statistics
  mod <- lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh)
  
  res[j, "b_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat", "Estimate"]
  res[j, "p_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat", "Pr(>|t|)"]
  res[j, "b_very"] <- summary(mod)$coefficients["climate_concern_very", "Estimate"]
  res[j, "p_very"] <- summary(mod)$coefficients["climate_concern_very", "Pr(>|t|)"]
  
}

## Check the results table
res

## How many p-values are < 0.05

# 'Somewhat concerned' - Now ~80% power to detect an effect of this magnitude
sum(res$p_somewhat < 0.05)
sum(res$p_somewhat < 0.05) / j

# 'Very concerned' - Now ~75% power to detect an effect of this magnitude
sum(res$p_very < 0.05)
sum(res$p_very < 0.05) / j




################################################################################
################################################################################
#### Next, we will conduct power analyses for our secondary research questions regarding factors which may modify the association between climate concern/concern and subsequent mental health

### The main simulations set-up will be the same as above, but now we assume that engagement in climate action (continuous variable) and belief that individual actions can impact climate change (binary variable) modify the relationship between climate concern and mental health. For simplicity, we assume that nothing causes these effect modifiers.

## We start first with climate action as an effect modifier

## Set-up the simulations using a massive sample size (1 million) to remove unwanted variability and set seed
n <- 1000000
set.seed(4655428)


## Simulate 'Confounders' - Continuous variable caused by nothing
confounders <- rnorm(n = n, mean = 0, sd = 1)

## Simulate prior mental health - Continuous variable caused by confounders (could represent, e.g., higher rates of mental health problems among women, lower SEP, etc.)
prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
summary(prior_mh)

# Check model and results are sensible
#plot(prior_mh ~ confounders)
summary(lm(prior_mh ~ confounders))


## Next, simulate exposure 'climate concern' - Three-level categorical variable (with levels 'not at all/not very concerned' [10%], 'somewhat concerned' [50%] and 'very concerned' [40%])

# Calculate the denominator of the multinomial regression model, with 'no' as the baseline
denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
  exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
head(denom)

# Calculate predicted probabilities for each outcome
prob_none <- 1 / denom
prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
head(cbind(prob_none, prob_some, prob_very))

# construct a random uniform variable and generate multinomial outcomes using this random unform variable and the predicted probabilities
rand <- runif(n = n, min = 0, max = 1)

climate_concern <- ifelse(rand < prob_none, 1,
                      ifelse(rand < prob_none + prob_some, 2, 3))
table(climate_concern)
table(climate_concern) / sum(table(climate_concern)) * 100

# Check model and results are sensible
by(prior_mh, climate_concern, summary)
by(confounders, climate_concern, summary)
summary(multinom(climate_concern ~ confounders + prior_mh))

# Also convert these to dummy variables, as need these to simulated when constructing the outcome
climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
table(climate_concern_somewhat); table(climate_concern_very)

# Remove some of the temporary variables
rm(rand)
rm(denom)
rm(prob_none)
rm(prob_some)
rm(prob_very)


## Simulate the 'climate action' effect modifier
climate_action <- rnorm(n = n, mean = 8, sd = 2)
summary(climate_action)


## Finally, simulate the 'later MH' outcome - Continuous variable caused by confounders, prior MH and climate concern, and the interaction between climate concern and climate action (with better mental health among those engaged in climate action) - Want this to be standardised, so mean = 0 and SD = 1

# Here, let's say that, when climate action = 0, those with climate concern (either somewhat or very) have 0.5 SD worse MH scores; but, for each one-unit increase in climate action, concern reduces by 0.05, so that a 4-unit increase in climate action (approx 2 SDs) reduces MH scores by 0.2 SD units.
later_mh <- -2.1 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.5) -
  (climate_concern_somewhat * climate_action * 0.05) + (climate_concern_very * 0.5) - 
  (climate_concern_very * climate_action * 0.05) + rnorm(n = n, mean = 0, sd = 0.79)
summary(later_mh); sd(later_mh)
hist(later_mh)

# Check model and that results are sensible
by(later_mh, climate_concern, summary)
#plot(later_mh ~ prior_mh)
summary(lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh +
             climate_concern_somewhat:climate_action + climate_concern_very:climate_action))



#####################################################################################
### Okay, now plug these values into a loop with the expected complete-case sample size of 1,000, and estimate the power (with effect size of 0.05 for interaction term)

## Set up objects to store power analysis results
i <- 1000
res <- as.data.frame(cbind(iteration = 1:i, b_somewhat = rep(NA, i), p_somewhat = rep(NA, i),
                           b_very = rep(NA, i), p_very = rep(NA, i)))
res

## Loop over 1,000 iterations, simulate data of n = 1,000, and store effect sizes and p-values
set.seed(32123)
n <- 1000

for (j in 1:i) {
  print(paste0("On iteration: ", j))
  print("")
  
  # Simulate the variables (as above)
  
  ## Confounders and prior MH
  confounders <- rnorm(n = n, mean = 0, sd = 1)
  prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
  
  ## Climate concern exposure
  # Calculate the denominator of the multinomial regression model, with 'no' as the baseline
  denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
    exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
  
  # Calculate predicted probabilities for each outcome
  prob_none <- 1 / denom
  prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
  prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
  
  # construct a random uniform variable and generate multinomial outcomes using this random uniform variable and the predicted probabilities
  rand <- runif(n = n, min = 0, max = 1)
  climate_concern <- ifelse(rand < prob_none, 1,
                        ifelse(rand < prob_none + prob_some, 2, 3))
  
  # Also convert these to dummy variables
  climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
  climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
  table(climate_concern_somewhat); table(climate_concern_very)
  
  ## Climate action effect modifier
  climate_action <- rnorm(n = n, mean = 8, sd = 2)
  
  ## Later MH outcome
  later_mh <- -2.1 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.5) -
    (climate_concern_somewhat * climate_action * 0.05) + (climate_concern_very * 0.5) - 
    (climate_concern_very * climate_action * 0.05) + rnorm(n = n, mean = 0, sd = 0.79)
  
  ### Run the model and extract relevant statistics
  mod <- lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh +
              climate_concern_somewhat:climate_action + climate_concern_very:climate_action)
  
  res[j, "b_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat:climate_action", "Estimate"]
  res[j, "p_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat:climate_action", "Pr(>|t|)"]
  res[j, "b_very"] <- summary(mod)$coefficients["climate_concern_very:climate_action", "Estimate"]
  res[j, "p_very"] <- summary(mod)$coefficients["climate_concern_very:climate_action", "Pr(>|t|)"]
  
}

## Check the results table
res

## How many p-values are < 0.05

# 'Somewhat concerned' by 'climate action' interaction - ~79% power to detect an effect of this magnitude
sum(res$p_somewhat < 0.05)
sum(res$p_somewhat < 0.05) / j

# 'Very concerned' by 'climate action' interaction - ~69% power to detect an effect of this magnitude
sum(res$p_very < 0.05)
sum(res$p_very < 0.05) / j



############################################################################################
### Repeat above, reducing effect size to 0.025 for the interaction terms


## Set-up the simulations using a massive sample size (1 million) to remove unwanted variability and set seed
n <- 1000000
set.seed(4655428)


## Simulate 'Confounders' - Continuous variable caused by nothing
confounders <- rnorm(n = n, mean = 0, sd = 1)

## Simulate prior mental health - Continuous variable caused by confounders (could represent, e.g., higher rates of mental health problems among women, lower SEP, etc.)
prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
summary(prior_mh)

# Check model and results are sensible
#plot(prior_mh ~ confounders)
summary(lm(prior_mh ~ confounders))


## Next, simulate exposure 'climate concern' - Three-level categorical variable (with levels 'not at all/not very concerned' [10%], 'somewhat concerned' [50%] and 'very concerned' [40%])

# Calculate the denominator of the multinomial regression model, with 'no' as the baseline
denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
  exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
head(denom)

# Calculate predicted probabilities for each outcome
prob_none <- 1 / denom
prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
head(cbind(prob_none, prob_some, prob_very))

# construct a random uniform variable and generate multinomial outcomes using this random unform variable and the predicted probabilities
rand <- runif(n = n, min = 0, max = 1)

climate_concern <- ifelse(rand < prob_none, 1,
                      ifelse(rand < prob_none + prob_some, 2, 3))
table(climate_concern)
table(climate_concern) / sum(table(climate_concern)) * 100

# Check model and results are sensible
by(prior_mh, climate_concern, summary)
by(confounders, climate_concern, summary)
summary(multinom(climate_concern ~ confounders + prior_mh))

# Also convert these to dummy variables, as need these to simulated when constructing the outcome
climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
table(climate_concern_somewhat); table(climate_concern_very)

# Remove some of the temporary variables
rm(rand)
rm(denom)
rm(prob_none)
rm(prob_some)
rm(prob_very)


## Simulate the 'climate action' effect modifier
climate_action <- rnorm(n = n, mean = 8, sd = 2)
summary(climate_action)


## Finally, simulate the 'later MH' outcome - Continuous variable caused by confounders, prior MH and climate concern, and the interaction between climate concern and climate action (with better mental health among those engaged in climate action) - Want this to be standardised, so mean = 0 and SD = 1

# Here, let's say that, when climate action = 0, those with climate concern (either somewhat or very) have 0.5 SD worse MH scores; but, for each one-unit increase in climate action, concern reduces by 0.025, so that a 4-unit increase in climate action (approx 2 SDs) reduces MH scores by 0.1 SD units.
later_mh <- -2.25 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.5) -
  (climate_concern_somewhat * climate_action * 0.025) + (climate_concern_very * 0.5) - 
  (climate_concern_very * climate_action * 0.025) + rnorm(n = n, mean = 0, sd = 0.78)
summary(later_mh); sd(later_mh)
hist(later_mh)

# Check model and that results are sensible
by(later_mh, climate_concern, summary)
#plot(later_mh ~ prior_mh)
summary(lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh +
             climate_concern_somewhat:climate_action + climate_concern_very:climate_action))



#####################################################################################
### Okay, now plug these values into a loop with the expected complete-case sample size of 1,000, and estimate the power (with effect size of 0.025 for interaction term)

## Set up objects to store power analysis results
i <- 1000
res <- as.data.frame(cbind(iteration = 1:i, b_somewhat = rep(NA, i), p_somewhat = rep(NA, i),
                           b_very = rep(NA, i), p_very = rep(NA, i)))
res

## Loop over 1,000 iterations, simulate data of n = 1,000, and store effect sizes and p-values
set.seed(32123)
n <- 1000

for (j in 1:i) {
  print(paste0("On iteration: ", j))
  print("")
  
  # Simulate the variables (as above)
  
  ## Confounders and prior MH
  confounders <- rnorm(n = n, mean = 0, sd = 1)
  prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
  
  ## Climate concern exposure
  # Calculate the denominator of the multinomial regression model, with 'no' as the baseline
  denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
    exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
  
  # Calculate predicted probabilities for each outcome
  prob_none <- 1 / denom
  prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
  prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
  
  # construct a random uniform variable and generate multinomial outcomes using this random uniform variable and the predicted probabilities
  rand <- runif(n = n, min = 0, max = 1)
  climate_concern <- ifelse(rand < prob_none, 1,
                        ifelse(rand < prob_none + prob_some, 2, 3))
  
  # Also convert these to dummy variables
  climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
  climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
  table(climate_concern_somewhat); table(climate_concern_very)
  
  ## Climate action effect modifier
  climate_action <- rnorm(n = n, mean = 8, sd = 2)
  
  ## Later MH outcome
  later_mh <- -2.25 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.5) -
    (climate_concern_somewhat * climate_action * 0.025) + (climate_concern_very * 0.5) - 
    (climate_concern_very * climate_action * 0.025) + rnorm(n = n, mean = 0, sd = 0.78)
  
  ### Run the model and extract relevant statistics
  mod <- lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh +
              climate_concern_somewhat:climate_action + climate_concern_very:climate_action)
  
  res[j, "b_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat:climate_action", "Estimate"]
  res[j, "p_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat:climate_action", "Pr(>|t|)"]
  res[j, "b_very"] <- summary(mod)$coefficients["climate_concern_very:climate_action", "Estimate"]
  res[j, "p_very"] <- summary(mod)$coefficients["climate_concern_very:climate_action", "Pr(>|t|)"]
  
}

## Check the results table
res

## How many p-values are < 0.05

# 'Somewhat concerned' by 'climate action' interaction - ~29% power to detect an effect of this magnitude
sum(res$p_somewhat < 0.05)
sum(res$p_somewhat < 0.05) / j

# 'Very concerned' by 'climate action' interaction - ~24% power to detect an effect of this magnitude
sum(res$p_very < 0.05)
sum(res$p_very < 0.05) / j



################################################################################
#### Now repeat with the binary variable 'belief in climate efficacy' as the binary effect modifier

## Set-up the simulations using a massive sample size (1 million) to remove unwanted variability and set seed
n <- 1000000
set.seed(4655428)


## Simulate 'Confounders' - Continuous variable caused by nothing
confounders <- rnorm(n = n, mean = 0, sd = 1)

## Simulate prior mental health - Continuous variable caused by confounders (could represent, e.g., higher rates of mental health problems among women, lower SEP, etc.)
prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
summary(prior_mh)

# Check model and results are sensible
#plot(prior_mh ~ confounders)
summary(lm(prior_mh ~ confounders))


## Next, simulate exposure 'climate concern' - Three-level categorical variable (with levels 'not at all/not very concerned' [10%], 'somewhat concerned' [50%] and 'very concerned' [40%])

# Calculate the denominator of the multinomial regression model, with 'no' as the baseline
denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
  exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
head(denom)

# Calculate predicted probabilities for each outcome
prob_none <- 1 / denom
prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
head(cbind(prob_none, prob_some, prob_very))

# construct a random uniform variable and generate multinomial outcomes using this random unform variable and the predicted probabilities
rand <- runif(n = n, min = 0, max = 1)

climate_concern <- ifelse(rand < prob_none, 1,
                      ifelse(rand < prob_none + prob_some, 2, 3))
table(climate_concern)
table(climate_concern) / sum(table(climate_concern)) * 100

# Check model and results are sensible
by(prior_mh, climate_concern, summary)
by(confounders, climate_concern, summary)
summary(multinom(climate_concern ~ confounders + prior_mh))

# Also convert these to dummy variables, as need these to simulated when constructing the outcome
climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
table(climate_concern_somewhat); table(climate_concern_very)

# Remove some of the temporary variables
rm(rand)
rm(denom)
rm(prob_none)
rm(prob_some)
rm(prob_very)


## Simulate the 'climate efficacy' effect modifier
climate_efficacy <- rbinom(n = n, size = 1, prob = 0.5)
table(climate_efficacy)


## Finally, simulate the 'later MH' outcome - Continuous variable caused by confounders, prior MH and climate concern, and the interaction between climate concern and climate efficacy (with better mental health among those who belief climate efforts are effective) - Want this to be standardised, so mean = 0 and SD = 1

# Here, let's say that, when climate efficacy = 0/No, those with climate concern (either somewhat or very) have 0.5 SD worse MH scores; but, for those who believe climate efforts are effective, concern reduces by 0.2 SD units.
later_mh <- -2.35 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.5) -
  (climate_concern_somewhat * climate_efficacy * 0.2) + (climate_concern_very * 0.5) - 
  (climate_concern_very * climate_efficacy * 0.2) + rnorm(n = n, mean = 0, sd = 0.77)
summary(later_mh); sd(later_mh)
hist(later_mh)

# Check model and that results are sensible
by(later_mh, climate_concern, summary)
#plot(later_mh ~ prior_mh)
summary(lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh +
             climate_concern_somewhat:climate_efficacy + climate_concern_very:climate_efficacy))



#####################################################################################
### Okay, now plug these values into a loop with the expected complete-case sample size of 1,000, and estimate the power (with effect size of 0.2 for interaction terms)

## Set up objects to store power analysis results
i <- 1000
res <- as.data.frame(cbind(iteration = 1:i, b_somewhat = rep(NA, i), p_somewhat = rep(NA, i),
                           b_very = rep(NA, i), p_very = rep(NA, i)))
res

## Loop over 1,000 iterations, simulate data of n = 1,000, and store effect sizes and p-values
set.seed(32123)
n <- 1000

for (j in 1:i) {
  print(paste0("On iteration: ", j))
  print("")
  
  # Simulate the variables (as above)
  
  ## Confounders and prior MH
  confounders <- rnorm(n = n, mean = 0, sd = 1)
  prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
  
  ## Climate concern exposure
  # Calculate the denominator of the multinomial regression model, with 'no' as the baseline
  denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
    exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
  
  # Calculate predicted probabilities for each outcome
  prob_none <- 1 / denom
  prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
  prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
  
  # construct a random uniform variable and generate multinomial outcomes using this random uniform variable and the predicted probabilities
  rand <- runif(n = n, min = 0, max = 1)
  climate_concern <- ifelse(rand < prob_none, 1,
                        ifelse(rand < prob_none + prob_some, 2, 3))
  
  # Also convert these to dummy variables
  climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
  climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
  table(climate_concern_somewhat); table(climate_concern_very)
  
  ## Climate efficacy effect modifier
  climate_efficacy <- rbinom(n = n, size = 1, prob = 0.5)
  
  ## Later MH outcome
  later_mh <- -2.35 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.5) -
    (climate_concern_somewhat * climate_efficacy * 0.2) + (climate_concern_very * 0.5) - 
    (climate_concern_very * climate_efficacy * 0.2) + rnorm(n = n, mean = 0, sd = 0.77)
  
  ### Run the model and extract relevant statistics
  mod <- lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh +
              climate_concern_somewhat:climate_efficacy + climate_concern_very:climate_efficacy)
  
  res[j, "b_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat:climate_efficacy", "Estimate"]
  res[j, "p_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat:climate_efficacy", "Pr(>|t|)"]
  res[j, "b_very"] <- summary(mod)$coefficients["climate_concern_very:climate_efficacy", "Estimate"]
  res[j, "p_very"] <- summary(mod)$coefficients["climate_concern_very:climate_efficacy", "Pr(>|t|)"]
  
}

## Check the results table
res

## How many p-values are < 0.05

# 'Somewhat concerned' by 'climate action' interaction - ~79% power to detect an effect of this magnitude
sum(res$p_somewhat < 0.05)
sum(res$p_somewhat < 0.05) / j

# 'Very concerned' by 'climate action' interaction - ~69% power to detect an effect of this magnitude
sum(res$p_very < 0.05)
sum(res$p_very < 0.05) / j



############################################################################################
### Repeat above, reducing effect size to 0.1 for the interaction terms

## Set-up the simulations using a massive sample size (1 million) to remove unwanted variability and set seed
n <- 1000000
set.seed(4655428)


## Simulate 'Confounders' - Continuous variable caused by nothing
confounders <- rnorm(n = n, mean = 0, sd = 1)

## Simulate prior mental health - Continuous variable caused by confounders (could represent, e.g., higher rates of mental health problems among women, lower SEP, etc.)
prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
summary(prior_mh)

# Check model and results are sensible
#plot(prior_mh ~ confounders)
summary(lm(prior_mh ~ confounders))


## Next, simulate exposure 'climate concern' - Three-level categorical variable (with levels 'not at all/not very concerned' [10%], 'somewhat concerned' [50%] and 'very concerned' [40%])

# Calculate the denominator of the multinomial regression model, with 'no' as the baseline
denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
  exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
head(denom)

# Calculate predicted probabilities for each outcome
prob_none <- 1 / denom
prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
head(cbind(prob_none, prob_some, prob_very))

# construct a random uniform variable and generate multinomial outcomes using this random unform variable and the predicted probabilities
rand <- runif(n = n, min = 0, max = 1)

climate_concern <- ifelse(rand < prob_none, 1,
                      ifelse(rand < prob_none + prob_some, 2, 3))
table(climate_concern)
table(climate_concern) / sum(table(climate_concern)) * 100

# Check model and results are sensible
by(prior_mh, climate_concern, summary)
by(confounders, climate_concern, summary)
summary(multinom(climate_concern ~ confounders + prior_mh))

# Also convert these to dummy variables, as need these to simulated when constructing the outcome
climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
table(climate_concern_somewhat); table(climate_concern_very)

# Remove some of the temporary variables
rm(rand)
rm(denom)
rm(prob_none)
rm(prob_some)
rm(prob_very)


## Simulate the 'climate efficacy' effect modifier
climate_efficacy <- rbinom(n = n, size = 1, prob = 0.5)
table(climate_efficacy)


## Finally, simulate the 'later MH' outcome - Continuous variable caused by confounders, prior MH and climate concern, and the interaction between climate concern and climate efficacy (with better mental health among those who belief climate efforts are effective) - Want this to be standardised, so mean = 0 and SD = 1

# Here, let's say that, when climate efficacy = 0/No, those with climate concern (either somewhat or very) have 0.5 SD worse MH scores; but, for those who believe climate efforts are effective, concern reduces by 0.1 SD units.
later_mh <- -2.4 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.5) -
  (climate_concern_somewhat * climate_efficacy * 0.1) + (climate_concern_very * 0.5) - 
  (climate_concern_very * climate_efficacy * 0.1) + rnorm(n = n, mean = 0, sd = 0.77)
summary(later_mh); sd(later_mh)
hist(later_mh)

# Check model and that results are sensible
by(later_mh, climate_concern, summary)
#plot(later_mh ~ prior_mh)
summary(lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh +
             climate_concern_somewhat:climate_efficacy + climate_concern_very:climate_efficacy))



#####################################################################################
### Okay, now plug these values into a loop with the expected complete-case sample size of 1,000, and estimate the power (with effect size of 0.1 for interaction terms)

## Set up objects to store power analysis results
i <- 1000
res <- as.data.frame(cbind(iteration = 1:i, b_somewhat = rep(NA, i), p_somewhat = rep(NA, i),
                           b_very = rep(NA, i), p_very = rep(NA, i)))
res

## Loop over 1,000 iterations, simulate data of n = 1,000, and store effect sizes and p-values
set.seed(32123)
n <- 1000

for (j in 1:i) {
  print(paste0("On iteration: ", j))
  print("")
  
  # Simulate the variables (as above)
  
  ## Confounders and prior MH
  confounders <- rnorm(n = n, mean = 0, sd = 1)
  prior_mh <- 20 + (confounders * 2) + rnorm(n = n, mean = 0, sd = 4)
  
  ## Climate concern exposure
  # Calculate the denominator of the multinomial regression model, with 'no' as the baseline
  denom <- 1 + exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05))) + 
    exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))
  
  # Calculate predicted probabilities for each outcome
  prob_none <- 1 / denom
  prob_some <- (exp(log(3) + (confounders * log(2)) + (prior_mh * log(1.05)))) / denom
  prob_very <- (exp(log(0.85) + (confounders * log(3)) + (prior_mh * log(1.1)))) / denom
  
  # construct a random uniform variable and generate multinomial outcomes using this random uniform variable and the predicted probabilities
  rand <- runif(n = n, min = 0, max = 1)
  climate_concern <- ifelse(rand < prob_none, 1,
                        ifelse(rand < prob_none + prob_some, 2, 3))
  
  # Also convert these to dummy variables
  climate_concern_somewhat <- ifelse(climate_concern == 2, 1, 0)
  climate_concern_very <- ifelse(climate_concern == 3, 1, 0)
  table(climate_concern_somewhat); table(climate_concern_very)
  
  ## Climate efficacy effect modifier
  climate_efficacy <- rbinom(n = n, size = 1, prob = 0.5)
  
  ## Later MH outcome
  later_mh <- -2.4 + (confounders * 0.25) + (prior_mh * 0.1) + (climate_concern_somewhat * 0.5) -
    (climate_concern_somewhat * climate_efficacy * 0.1) + (climate_concern_very * 0.5) - 
    (climate_concern_very * climate_efficacy * 0.1) + rnorm(n = n, mean = 0, sd = 0.77)
  
  ### Run the model and extract relevant statistics
  mod <- lm(later_mh ~ climate_concern_somewhat + climate_concern_very + confounders + prior_mh +
              climate_concern_somewhat:climate_efficacy + climate_concern_very:climate_efficacy)
  
  res[j, "b_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat:climate_efficacy", "Estimate"]
  res[j, "p_somewhat"] <- summary(mod)$coefficients["climate_concern_somewhat:climate_efficacy", "Pr(>|t|)"]
  res[j, "b_very"] <- summary(mod)$coefficients["climate_concern_very:climate_efficacy", "Estimate"]
  res[j, "p_very"] <- summary(mod)$coefficients["climate_concern_very:climate_efficacy", "Pr(>|t|)"]
  
}

## Check the results table
res

## How many p-values are < 0.05

# 'Somewhat concerned' by 'climate action' interaction - ~79% power to detect an effect of this magnitude
sum(res$p_somewhat < 0.05)
sum(res$p_somewhat < 0.05) / j

# 'Very concerned' by 'climate action' interaction - ~69% power to detect an effect of this magnitude
sum(res$p_very < 0.05)
sum(res$p_very < 0.05) / j

