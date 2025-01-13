### Example simulated dataset to demonstrate analyses for climate concern and mental health paper
### Created by Dan Major-Smith
### R version 4.3.1

###############################################################################################################
#### Clear workspace and install/load packages

rm(list = ls())

#install.packages("tidyverse")
#install.packages("dagitty")
#install.packages("marginaleffects")
#install.packages("mice")
#install.packages("sensemakr")
#install.packages("smcfcs")
#install.packages("mitools")

library(tidyverse)
library(dagitty)
library(nnet)
library(marginaleffects)
library(mice)
library(sensemakr)
library(smcfcs)
library(mitools)


################################################################################################
#### Simulate data based on the following DAG

dag <- dagitty('dag {
               BaselineConfounders [pos = "0,0"]
               BaselineMH [pos = "1,2"]
               ClimateAction [pos = "2,2"]
               ClimateConcern [pos = "2,0"]
               ConcernActionInt [pos = "3,1"]
               MentalHealth [pos = "4,1"]
               
               BaselineConfounders -> BaselineMH
               BaselineConfounders -> ClimateConcern
               BaselineConfounders -> ClimateAction
               BaselineConfounders -> MentalHealth
               BaselineMH -> ClimateConcern
               BaselineMH -> ClimateAction
               BaselineMH -> MentalHealth
               ClimateConcern -> MentalHealth
               ClimateAction -> MentalHealth
               ClimateConcern -> ConcernActionInt
               ClimateAction -> ConcernActionInt
               ConcernActionInt -> MentalHealth
               }')
plot(dag)

# Note that the node 'ConcernActionInt' is intended to illustrate that there is an interaction between 'Climate concern' and 'climate action' on subsequent mental health


### Simulate this scenario

## Number of observations and seed
n <- 5000
set.seed(4321234)

## Simulate 'BaselineConfounders' - Will just assume is a single binary variable, for simplicity - Caused by nothing, so just do 50/50 split
confounds <- rbinom(n = n, size = 1, prob = 0.5)
table(confounds)

## Simulate 'BaselineMH' - Treat as binary variable, and caused by baseline confounders, with approx. 25% with MH problems
baseline_mh <- rbinom(n = n, size = 1, prob = plogis(log(0.25) + (log(2) * confounds)))
table(baseline_mh)

## Simulate 'ClimateConcern' - As with the real ALSPAC data, will code this as a three-level categorical variable caused by baseline confounders and MH, with approx. 10% 'not concerned', 50% 'concerned' and 40% 'very concerned'. Will simulate using a multinomial model, so code is a bit more complicated than above

# First, calculate the denominator
denom <- 1 + exp(log(4.5) + (log(2) * confounds) + (log(2) * baseline_mh)) + exp(log(2.5) + (log(3) * confounds) + (log(3) * baseline_mh))
head(denom)

# Next, predicted probabilities for each outcome
vprob1 <- 1 / denom
vprob2 <- exp(log(4.5) + (log(2) * confounds) + (log(2) * baseline_mh)) / denom
vprob3 <- exp(log(2.5) + (log(3) * confounds) + (log(3) * baseline_mh)) / denom
head(cbind(vprob1, vprob2, vprob3))

# Construct a uniform random variable and generate multinomial outcome using this and the predicted probabilities from above
u <- runif(n = n, min = 0, max = 1)
climate_concern <- ifelse(u < vprob1, "Not concerned",
                      ifelse(u < vprob1 + vprob2, "Concerned", "Very concerned"))
climate_concern <- factor(climate_concern, levels = c("Not concerned", "Concerned", "Very concerned"))

# Check descriptive stats are broadly correct
table(climate_concern)
round(prop.table(table(climate_concern)) * 100, 1)

# Check results of multinomial model
test <- multinom(climate_concern ~ confounds + baseline_mh)
summary(test)
exp(coef(test))
exp(confint(test))

# Split into dummy variables
climate_concern_concerned <- ifelse(climate_concern == "Concerned", 1, 0)
table(climate_concern_concerned)

climate_concern_veryConcerned <- ifelse(climate_concern == "Very concerned", 1, 0)
table(climate_concern_veryConcerned)

# Drop some of these scaffolding objects
rm(denom)
rm(vprob1)
rm(vprob2)
rm(vprob3)
rm(u)
rm(test)

## Simulate 'ClimateAction' - Treat as continuous variable caused by confounds and baseline MH
climate_action <- as.integer(8 + (2 * confounds) + (1 * baseline_mh) + rnorm(n = n, mean = 0, sd = 2))
summary(climate_action)
hist(climate_action)

## Finally, simulate 'MentalHealth' outcome - A continuous variable caused by baseline confounders, prior MH, climate concern, climate action, and the interaction between climate concern and climate action (with climate action reducing MH symptoms in those with climate concern)
mental_health <- as.integer(25 + (2 * confounds) + (5 * baseline_mh) + (2 * climate_concern_concerned) + 
  (5 * climate_concern_veryConcerned) + (-0.1 * climate_action) + 
    (-0.2 * climate_concern_concerned * climate_action) +
  (-0.3 * climate_concern_veryConcerned * climate_action) + rnorm(n = n, mean = 0, sd = 5))
summary(mental_health)
hist(mental_health)

# Check the model
summary(lm(mental_health ~ climate_concern + confounds + baseline_mh))
summary(lm(mental_health ~ climate_concern + confounds + baseline_mh + climate_action + 
             climate_concern:climate_action))


## Predicted values - Main effects model
a <- lm(mental_health ~ climate_concern + confounds + baseline_mh)

# Average mental health score by climate concern
avg_predictions(a, variables = "climate_concern")

# Whether 'concerned' and 'very concerned' levels differ
avg_comparisons(a, variables = list(climate_concern = "pairwise"))

## Sensitivity analysis for unmeasured confounding

# 'Concerned' exposure level - Only small levels of unmeasured confounding necessary to alpha = 0.05 level (if unmeasured confounder explains 0.2% of variance in exposure and outcome) and to null (2.95% of variance). Unmeasured confounders as strong as measured confounders would also shift effect towards null
summary(sensemakr(model = a, treatment = "climate_concernConcerned", 
                  benchmark_covariates = c("confounds", "baseline_mh"),
                  q = 1, alpha = 0.05, reduce = TRUE))
plot(sensemakr(model = a, treatment = "climate_concernConcerned", 
                  benchmark_covariates = c("confounds", "baseline_mh"),
                  q = 1, alpha = 0.05, reduce = TRUE))

# And if treat all confounders jointly, would turn the effect in the opposite direction
summary(sensemakr(model = a, treatment = "climate_concernConcerned", 
                  benchmark_covariates = list("joint" = c("confounds", "baseline_mh")),
                  q = 1, alpha = 0.05, reduce = TRUE))
plot(sensemakr(model = a, treatment = "climate_concernConcerned", 
                  benchmark_covariates = list("joint" = c("confounds", "baseline_mh")),
                  q = 1, alpha = 0.05, reduce = TRUE))

# 'Very concerned' exposure level - Now need larger levels of unmeasured confounding necessary to alpha = 0.05 level (if unmeasured confounder explains 8.3% of variance in exposure and outcome) and to null (10.9% of variance). Unmeasured confounders as strong as measured confounders would shift effect towards null, but still would observe a strong positive association
summary(sensemakr(model = a, treatment = "climate_concernVery concerned", 
                  benchmark_covariates = c("confounds", "baseline_mh"),
                  q = 1, alpha = 0.05, reduce = TRUE))
plot(sensemakr(model = a, treatment = "climate_concernVery concerned", 
               benchmark_covariates = c("confounds", "baseline_mh"),
               q = 1, alpha = 0.05, reduce = TRUE))

# And if treat all confounders jointly, would shift effect to nearly null
summary(sensemakr(model = a, treatment = "climate_concernVery concerned", 
                  benchmark_covariates = list("joint" = c("confounds", "baseline_mh")),
                  q = 1, alpha = 0.05, reduce = TRUE))
plot(sensemakr(model = a, treatment = "climate_concernVery concerned", 
               benchmark_covariates = list("joint" = c("confounds", "baseline_mh")),
               q = 1, alpha = 0.05, reduce = TRUE))


## Predicted values - Interaction model
b <- lm(mental_health ~ climate_concern + confounds + baseline_mh + climate_action + climate_concern:climate_action)

# Average mental health score by climate concern
avg_predictions(b, variables = "climate_concern")

# Average mental health score by climate concern and certain values of climate action
predictions(b, newdata = datagrid(climate_concern = c("Not concerned", "Concerned", "Very concerned"), 
                                  climate_action = c(0, 5, 10, 15)))

# Plot of predicted values showing interaction between climate concern and action
plot_predictions(b, condition = c("climate_action", "climate_concern"))


### Combine all these variables into a data frame
df <- data.frame(confounds, baseline_mh, climate_concern, climate_action, mental_health)
head(df)
summary(df)

## To make our dataset more realistic, we will code all variables as having approx. 20% missing data (with data missing completely at random, for simplicity)
df$confounds_m <- ifelse(runif(n = n, min = 0, max = 1) < 0.2, NA, df$confounds)
df$baseline_mh_m <- ifelse(runif(n = n, min = 0, max = 1) < 0.2, NA, df$baseline_mh)
df$climate_concern_m <- ifelse(runif(n = n, min = 0, max = 1) < 0.2, NA, df$climate_concern)
df$climate_concern_m <- ifelse(df$climate_concern_m == 1, "Not concerned",
                           ifelse(df$climate_concern_m == 2, "Concerned",
                                  ifelse(df$climate_concern_m == 3, "Very concerned", NA)))
df$climate_concern_m <- factor(df$climate_concern_m, levels = c("Not concerned", "Concerned", "Very concerned"))
df$climate_action_m <- ifelse(runif(n = n, min = 0, max = 1) < 0.2, NA, df$climate_action)
df$mental_health_m <- ifelse(runif(n = n, min = 0, max = 1) < 0.2, NA, df$mental_health)
summary(df)

# How many are in the final complete-case analysis? Approx. one-third of the data (~1,700 cases) - As data are missing completely at random complete-case analysis should not be biased, it will likely be inefficient (i.e., more uncertaintly/wider SEs and CIs)
sum(complete.cases(df))
round((sum(complete.cases(df)) / n) * 100, 2)

a_cca <- lm(mental_health_m ~ climate_concern_m + confounds_m + baseline_mh_m, data = df, subset = complete.cases(df))
summary(a_cca)
summary(a)

b_cca <- lm(mental_health_m ~ climate_concern_m + confounds_m + baseline_mh_m + climate_action_m + 
              climate_concern_m:climate_action_m, data = df, subset = complete.cases(df))
summary(b_cca)
summary(b)


#################################################################################################
#### Now, to demonstrate the multiple imputation (MI) approaches that will be used in the actual study.

### First, just interested in main effect of climate concern on mental health, so will exclude 'climate actions' and its interaction term in this first imputation model

# Reduce dataset to relevant vars, and categorical variables to factors (need this for MICE to work)
df_mi <- df[, c("confounds_m", "baseline_mh_m", "climate_concern_m", "mental_health_m")]
df_mi$confounds_m <- as.factor(df_mi$confounds_m)
df_mi$baseline_mh_m <- as.factor(df_mi$baseline_mh_m)
head(df_mi)
summary(df_mi)

### Now for the MI analysis

# First, set-up the imputation methods for each variable. Most look fine, but will change mental health outcome score to a normal model, rather than PMM
meth <- make.method(df_mi)
meth["mental_health_m"] <- "norm"
meth

# Second, set-up the prediction matrix, which says which variables to use to impute other variables (here, we want to use all variables when imputing all others)
pred <- make.predictorMatrix(df_mi)
pred

# Run a test imputation to make sure it looks okay, and check the amount of missing data in each variable
test <- mice(df_mi, m = 5, method = meth, predictorMatrix = pred, print = TRUE, maxit = 0)
test
table(test$nmis)

# Now run the proper imputation model. Will create 50 imputed datasets here with a burn-in period of 10 iterations
imp_mi <- mice(df_mi, m = 50, method = meth, predictorMatrix = pred, print = TRUE, maxit = 10, seed = 98765)

# Quick check that the chains converged - All look fine!
plot(imp_mi)

# Now run substantive analysis model in each imputed dataset and combine using Rubin's rules
model <- with(imp_mi, lm(mental_health_m ~ climate_concern_m + confounds_m + baseline_mh_m))
est <- pool(model)
(results_mi <- summary(est, conf.int = TRUE))

# These results are similar to the full dataset results (albeit with wider SEs, due to imputation uncertainty) and more efficient than the complete-case results (note that there's no real difference in bias as the CCA was unbiased anyway as data were MCAR)
summary(a)
summary(a_cca)


## Predicted values from imputed datasets
avg_predictions(model, variables = c("climate_concern_m"))

# Whether 'concerned' and 'very concerned' levels differ
avg_comparisons(model, variables = list(climate_concern_m = "pairwise"))


## Sensemakr for unmeasured confounding (Note: am entering the data manually here, rather than providing an 'lm' model object, as these are for multiply-imputed results; can't get benchmark covariates values using this approach, though [although,if wanted this, could loop over all imputed datasets, run 'sensemakr', store results, then take mean benchmark-adjusted value, as demonstrated below])

# 'Concerned' (0.2% to reduce to alpha = 0.05; 2.9% to null)
sensemakr(estimate = results_mi[2, "estimate"], se = results_mi[2, "std.error"], dof = nrow(df),
          treatment = "climate_concern_mConcerned", q = 1, alpha = 0.05, reduce = TRUE)

# 'Very concerned' (6.9% to reduce to alpha = 0.05; 9.5% to null)
sensemakr(estimate = results_mi[3, "estimate"], se = results_mi[3, "std.error"], dof = nrow(df),
          treatment = "climate_concern_mVery concerned", q = 1, alpha = 0.05, reduce = TRUE)


## Check these results against looping over imputed datasets and running on actual model

# Loop over each imputed dataset, storing the sensemakr R2 values, plus shift in estimate if unmeasured confounding is the same as joint observed confounding
for (i in 1:imp_mi$m) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- complete(imp_mi, i)
  mod_temp <- lm(mental_health_m ~ climate_concern_m + confounds_m + baseline_mh_m, data = df_temp)
  concerned_sens_temp <- sensemakr(model = mod_temp, treatment = "climate_concern_mConcerned", 
                                     benchmark_covariates = list("joint" = c("confounds_m1", "baseline_mh_m1")),
                                     q = 1, alpha = 0.05, reduce = TRUE)
  veryConcerned_sens_temp <- sensemakr(model = mod_temp, treatment = "climate_concern_mVery concerned", 
                                   benchmark_covariates = list("joint" = c("confounds_m1", "baseline_mh_m1")),
                                   q = 1, alpha = 0.05, reduce = TRUE)
  if (i == 1) {
    concern_temp <- as.data.frame(cbind(toNull = concerned_sens_temp$sensitivity_stats[1, "rv_q"], 
                                        toAlpha = concerned_sens_temp$sensitivity_stats[1, "rv_qa"],
                                        adjustedEst = concerned_sens_temp$bounds[1, "adjusted_estimate"],
                                        adjustedSE = concerned_sens_temp$bounds[1, "adjusted_se"])) 
    veryConcern_temp <- as.data.frame(cbind(toNull = veryConcerned_sens_temp$sensitivity_stats[1, "rv_q"], 
                                        toAlpha = veryConcerned_sens_temp$sensitivity_stats[1, "rv_qa"],
                                        adjustedEst = veryConcerned_sens_temp$bounds[1, "adjusted_estimate"],
                                        adjustedSE = veryConcerned_sens_temp$bounds[1, "adjusted_se"])) 
  } else {
    concern_temp2 <- as.data.frame(cbind(toNull = concerned_sens_temp$sensitivity_stats[1, "rv_q"], 
                                        toAlpha = concerned_sens_temp$sensitivity_stats[1, "rv_qa"],
                                        adjustedEst = concerned_sens_temp$bounds[1, "adjusted_estimate"],
                                        adjustedSE = concerned_sens_temp$bounds[1, "adjusted_se"])) 
    concern_temp <- rbind(concern_temp, concern_temp2)
    
    veryConcern_temp2 <- as.data.frame(cbind(toNull = veryConcerned_sens_temp$sensitivity_stats[1, "rv_q"], 
                                            toAlpha = veryConcerned_sens_temp$sensitivity_stats[1, "rv_qa"],
                                            adjustedEst = veryConcerned_sens_temp$bounds[1, "adjusted_estimate"],
                                            adjustedSE = veryConcerned_sens_temp$bounds[1, "adjusted_se"])) 
    veryConcern_temp <- rbind(veryConcern_temp, veryConcern_temp2)
  }
}

concern_temp
veryConcern_temp

# Mean r2 value to null and alpha level for 'concerned' (some 'toAlpha' values are 0, however, as 95% CIs already crossed the null...)
mean(concern_temp$toNull) # 3.7%
mean(concern_temp$toAlpha) # 1.1%

# Mean r2 value to null and alpha level for 'very concerned'
mean(veryConcern_temp$toNull) # 11.9%
mean(veryConcern_temp$toAlpha) # 9.4%


## For the change in estimate if unmeasured confounding was the same strength as unmeasured confounding, need to combine results using Rubin's rules

# 'Concerned'

# Convert SE to variance
concern_temp$var <- concern_temp$adjustedSE ^ 2

# Mean value and SEs using Rubin's Rules
mean_RR <- mean(concern_temp$adjustedEst)
var_within <- mean(concern_temp$var)
var_between <- ((1 / (imp_mi$m - 1)) * sum((concern_temp$adjustedEst - mean_RR) ^ 2))
var_total <- var_within + ((1 + (1 / imp_mi$m)) * var_between)
se_total <- sqrt(var_total)

mean_RR
mean_RR - (1.96 * se_total)
mean_RR + (1.96 * se_total)


# 'Very concerned'

# Convert SE to variance
veryConcern_temp$var <- veryConcern_temp$adjustedSE ^ 2

# Mean value and SEs using Rubin's Rules
mean_RR <- mean(veryConcern_temp$adjustedEst)
var_within <- mean(veryConcern_temp$var)
var_between <- ((1 / (imp_mi$m - 1)) * sum((veryConcern_temp$adjustedEst - mean_RR) ^ 2))
var_total <- var_within + ((1 + (1 / imp_mi$m)) * var_between)
se_total <- sqrt(var_total)

mean_RR
mean_RR - (1.96 * se_total)
mean_RR + (1.96 * se_total)




#################################################################################################
#### Next, want to extend the approach above to impute and model the interaction between 'climate concern' and 'climate actions' on mental health

### Will use the 'all interactions' method first

# Reduce dataset to relevant vars, and categorical variables to factors (need this for MICE to work)
df_mi_allInt <- df[, c("confounds_m", "baseline_mh_m", "climate_concern_m", "mental_health_m", "climate_action_m")]
df_mi_allInt$confounds_m <- as.factor(df_mi_allInt$confounds_m)
df_mi_allInt$baseline_mh_m <- as.factor(df_mi_allInt$baseline_mh_m)
head(df_mi_allInt)
summary(df_mi_allInt)

## Now for the MI analysis

# First, set-up the imputation methods for each variable. Most look fine, but will change mental health outcome score and climate actions to a normal model, rather than PMM
meth <- make.method(df_mi_allInt)
meth["mental_health_m"] <- "norm"
meth["climate_action_m"] <- "norm"
meth

# Next, specify the imputation model for each variable with missing data (note: We are using this rather than the 'predictor matrix' approach used above, in order to include the relevant interaction terms; note also that all imputation models need to include the 'climate_concern_m:climate_action_m' interaction term for compatability with the substantive model)
form <- as.list(c(
  as.formula(confounds_m ~ baseline_mh_m + climate_concern_m + mental_health_m + climate_action_m +
               climate_concern_m:climate_action_m),
  as.formula(baseline_mh_m ~ confounds_m + climate_concern_m + mental_health_m + climate_action_m +
               climate_concern_m:climate_action_m),
  as.formula(climate_concern_m ~ confounds_m + baseline_mh_m + mental_health_m + climate_action_m + 
               mental_health_m:climate_action_m),
  as.formula(mental_health_m ~ confounds_m + baseline_mh_m + climate_concern_m + climate_action_m + 
               climate_concern_m:climate_action_m),
  as.formula(climate_action_m ~ confounds_m + baseline_mh_m + climate_concern_m + mental_health_m + 
               climate_concern_m:mental_health_m)
))
form

# Run a test imputation to make sure it looks okay, and check the amount of missing data in each variable
test <- mice(df_mi_allInt, m = 5, method = meth, formulas = form, print = TRUE, maxit = 0)
test
table(test$nmis)

# Now run the proper imputation model. Will create 50 imputed datasets here with a burn-in period of 10 iterations
imp_allInt <- mice(df_mi_allInt, m = 50, method = meth, formulas = form, print = TRUE, maxit = 10, seed = 78987)

# Quick check that the chains converged - All look fine!
plot(imp_allInt)

# Now run substantive effect modification analysis model in each imputed dataset and combine using Rubin's rules
model <- with(imp_allInt, lm(mental_health_m ~ climate_concern_m + confounds_m + baseline_mh_m + 
                               climate_action_m + climate_concern_m:climate_action_m))
est <- pool(model)
(results_allInt <- summary(est, conf.int = TRUE))

# As above, MI did not remove bias (as there was no bias in the CCA), but has improved efficiency
summary(b)
summary(b_cca)


## Average mental health score by climate concern
avg_predictions(model, variables = c("climate_concern_m"))

# Average mental health score by climate concern and certain values of climate action - Note: I can't find a 'marginaleffects' function which can apply this automatically to imputed data, so will create a loop to do this manually.

# Loop over each imputed dataset, storing the predicted probabilities and SEs
for (i in 1:imp_allInt$m) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- complete(imp_allInt, i)
  mod_temp <- lm(mental_health_m ~ climate_concern_m + confounds_m + baseline_mh_m + 
                   climate_action_m + climate_concern_m:climate_action_m, data = df_temp)
  preds_temp <- predictions(mod_temp, newdata = datagrid(climate_concern_m = 
                                                           c("Not concerned", "Concerned", "Very concerned"), 
                                                         climate_action_m = c(0, 5, 10, 15)))
  if (i == 1) {
    temp <- as.data.frame(cbind(climate_concern = as.character(preds_temp$climate_concern), 
                                climate_action = preds_temp$climate_action,
                                est = preds_temp$estimate, se = preds_temp$std.error)) 
  } else {
    temp2 <- as.data.frame(cbind(climate_concern = as.character(preds_temp$climate_concern), 
                                 climate_action = preds_temp$climate_action,
                                 est = preds_temp$estimate, se = preds_temp$std.error))
    temp <- rbind(temp, temp2)
  }
}

temp

# Convert data to numeric, then SEs to variances
temp$est <- as.numeric(temp$est)
temp$se <- as.numeric(temp$se)
temp$var <- temp$se ^ 2

## Generate mean values and SEs using Rubin's Rules for each comparison and store in table
preds_rr <- temp2
preds_rr$est <- NA
preds_rr$se <- NA
preds_rr$lower_ci <- NA
preds_rr$upper_ci <- NA
preds_rr

for (i in 1:nrow(preds_rr)) {
  
  # Store the combination of climate concern and climate action levels
  conc_temp <- temp$climate_concern[i]
  action_temp <- temp$climate_action[i]
  
  # Mean value and SEs using Rubin's Rules
  mean_RR <- mean(temp$est[temp$climate_concern == conc_temp & temp$climate_action == action_temp])
  var_within <- mean(temp$var[temp$climate_concern == conc_temp & temp$climate_action == action_temp])
  var_between <- ((1 / (imp_allInt$m - 1)) * sum((temp$est[temp$climate_concern == conc_temp & 
                                                             temp$climate_action == action_temp] - mean_RR) ^ 2))
  var_total <- var_within + ((1 + (1 / imp_allInt$m)) * var_between)
  se_total <- sqrt(var_total)
  
  # Store these results in the table
  preds_rr$est[preds_rr$climate_concern == conc_temp & preds_rr$climate_action == action_temp] <- mean_RR
  preds_rr$se[preds_rr$climate_concern == conc_temp & preds_rr$climate_action == action_temp] <- se_total
  preds_rr$lower_ci[preds_rr$climate_concern == conc_temp & preds_rr$climate_action == action_temp] <- 
    mean_RR - (1.96 * se_total)
  preds_rr$upper_ci[preds_rr$climate_concern == conc_temp & preds_rr$climate_action == action_temp] <- 
    mean_RR + (1.96 * se_total)
  
}

preds_rr


### And also check the main effects model
model <- with(imp_allInt, lm(mental_health_m ~ climate_concern_m + confounds_m + baseline_mh_m))
est <- pool(model)
(results_allInt_main <- summary(est, conf.int = TRUE))

# Compare to 'true' and CCA results (are similar, as data are just MCAR)
summary(a)
summary(a_cca)

## Average mental health score by climate concern
avg_predictions(model, variables = c("climate_concern_m"))



### And then next, as an additional sensitivity analysis, using the 'smcfcs' method

# As SMCFCS does not permit factor interactions, need to create dummy variables for the 'climate anxiety' factor, as well as its interaction with 'climate actions'
df_mi_smcfcs <- df[, c("mental_health_m", "confounds_m", "baseline_mh_m", "climate_concern_m", "climate_action_m")]

# Create interaction variables for both levels of 'climate_concern_m'
df_mi_smcfcs$concernXaction_concerned <- ifelse(is.na(df_mi_smcfcs$climate_concern_m) | 
                                              is.na(df_mi_smcfcs$climate_action_m), NA,
                                            ifelse(df_mi_smcfcs$climate_concern_m == "Concerned", 
                                                   df_mi_smcfcs$climate_action_m, 0))
df_mi_smcfcs$concernXaction_veryConcerned <- ifelse(is.na(df_mi_smcfcs$climate_concern_m) | 
                                                  is.na(df_mi_smcfcs$climate_action_m), NA,
                                                ifelse(df_mi_smcfcs$climate_concern_m == "Very concerned", 
                                                       df_mi_smcfcs$climate_action_m, 0))

head(df_mi_smcfcs)
summary(df_mi_smcfcs)

# Run SMCFCS, deriving the interaction terms using if statements
imp_allSMCFCS <- smcfcs(df_mi_smcfcs, numit = 10, m = 50, noisy = FALSE, smtype = "lm",
               smformula = "mental_health_m ~ confounds_m + baseline_mh_m + climate_concern_m + 
               climate_action_m + concernXaction_concerned + concernXaction_veryConcerned",
               method = c("", "logreg", "logreg", "mlogit", "norm", 
                          "ifelse(climate_concern_m == 'Concerned', climate_action_m, 0)", 
                          "ifelse(climate_concern_m == 'Very concerned', climate_action_m, 0)"),
               rjlimit = 10000)

# Extract results
smcfcs_imp <- imputationList(imp_allSMCFCS$impDatasets)
model <- with(smcfcs_imp, lm(mental_health_m ~ climate_concern_m + confounds_m + baseline_mh_m + 
                               climate_action_m + climate_concern_m:climate_action_m))
est <- pool(model)
(results_smcfcs_main <- summary(est, conf.int = TRUE))

# As above, MI did not remove bias (as there was no bias in the CCA), but has improved efficiency
summary(b)
summary(b_cca)


# Average mental health score by climate concern and certain values of climate action - Note: I can't find a 'marginaleffects' function which can apply this automatically to imputed data, so will create a loop to do this manually. Will also create 'average MH score by climate concern' as 'avg_predictions' doesn't seem to with with SMCFCS objects

# Loop over each imputed dataset, storing the predicted probabilities and SEs
for (i in 1:length(imp_allSMCFCS$impDatasets)) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- smcfcs_imp$imputations[[i]]
  mod_temp <- lm(mental_health_m ~ climate_concern_m + confounds_m + baseline_mh_m + 
                   climate_action_m + climate_concern_m:climate_action_m, data = df_temp)
  
  # Predicted MH scores by climate concern and action
  preds_temp <- predictions(mod_temp, newdata = datagrid(climate_concern_m = 
                                                           c("Not concerned", "Concerned", "Very concerned"), 
                                                         climate_action_m = c(0, 5, 10, 15)))
  
  if (i == 1) {
    temp <- as.data.frame(cbind(climate_concern = as.character(preds_temp$climate_concern_m), 
                                climate_action = preds_temp$climate_action_m,
                                est = preds_temp$estimate, se = preds_temp$std.error)) 
  } else {
    temp2 <- as.data.frame(cbind(climate_concern = as.character(preds_temp$climate_concern_m), 
                                 climate_action = preds_temp$climate_action_m,
                                 est = preds_temp$estimate, se = preds_temp$std.error))
    temp <- rbind(temp, temp2)
  }
  
  # Average MH score by climate concern
  preds_temp_all <- avg_predictions(mod_temp, variables = c("climate_concern_m"))
  
  if (i == 1) {
    temp_all <- as.data.frame(cbind(climate_concern = as.character(preds_temp_all$climate_concern_m), 
                                est = preds_temp_all$estimate, se = preds_temp_all$std.error)) 
  } else {
    temp2_all <- as.data.frame(cbind(climate_concern = as.character(preds_temp_all$climate_concern_m), 
                                 est = preds_temp_all$estimate, se = preds_temp_all$std.error))
    temp_all <- rbind(temp_all, temp2_all)
  }
}

temp
temp_all

## Start with overall predictions for climate concern

# Convert data to numeric, then SEs to variances
temp_all$est <- as.numeric(temp_all$est)
temp_all$se <- as.numeric(temp_all$se)
temp_all$var <- temp_all$se ^ 2

## Generate mean values and SEs using Rubin's Rules for each comparison and store in table
preds_rr_all <- temp2_all
preds_rr_all$est <- NA
preds_rr_all$se <- NA
preds_rr_all$lower_ci <- NA
preds_rr_all$upper_ci <- NA
preds_rr_all

for (i in 1:nrow(preds_rr_all)) {
  
  # Store the combination of climate concern and climate action levels
  conc_temp <- temp_all$climate_concern[i]
  
  # Mean value and SEs using Rubin's Rules
  mean_RR <- mean(temp_all$est[temp_all$climate_concern == conc_temp])
  var_within <- mean(temp_all$var[temp_all$climate_concern == conc_temp])
  var_between <- ((1 / (length(imp_allSMCFCS$impDatasets) - 1)) * 
                    sum((temp_all$est[temp_all$climate_concern == conc_temp] - mean_RR) ^ 2))
  var_total <- var_within + ((1 + (1 / length(imp_allSMCFCS$impDatasets))) * var_between)
  se_total <- sqrt(var_total)
  
  # Store these results in the table
  preds_rr_all$est[preds_rr_all$climate_concern == conc_temp] <- mean_RR
  preds_rr_all$se[preds_rr_all$climate_concern == conc_temp] <- se_total
  preds_rr_all$lower_ci[preds_rr_all$climate_concern == conc_temp] <- mean_RR - (1.96 * se_total)
  preds_rr_all$upper_ci[preds_rr_all$climate_concern == conc_temp] <- mean_RR + (1.96 * se_total)
  
}

preds_rr_all


## Now predictions for interaction between climate concern and actions

# Convert data to numeric, then SEs to variances
temp$est <- as.numeric(temp$est)
temp$se <- as.numeric(temp$se)
temp$var <- temp$se ^ 2

## Generate mean values and SEs using Rubin's Rules for each comparison and store in table
preds_rr <- temp2
preds_rr$est <- NA
preds_rr$se <- NA
preds_rr$lower_ci <- NA
preds_rr$upper_ci <- NA
preds_rr

for (i in 1:nrow(preds_rr)) {
  
  # Store the combination of climate concern and climate action levels
  conc_temp <- temp$climate_concern[i]
  action_temp <- temp$climate_action[i]
  
  # Mean value and SEs using Rubin's Rules
  mean_RR <- mean(temp$est[temp$climate_concern == conc_temp & temp$climate_action == action_temp])
  var_within <- mean(temp$var[temp$climate_concern == conc_temp & temp$climate_action == action_temp])
  var_between <- ((1 / (length(imp_allSMCFCS$impDatasets) - 1)) * sum((temp$est[temp$climate_concern == conc_temp & 
                                                             temp$climate_action == action_temp] - mean_RR) ^ 2))
  var_total <- var_within + ((1 + (1 / length(imp_allSMCFCS$impDatasets))) * var_between)
  se_total <- sqrt(var_total)
  
  # Store these results in the table
  preds_rr$est[preds_rr$climate_concern == conc_temp & preds_rr$climate_action == action_temp] <- mean_RR
  preds_rr$se[preds_rr$climate_concern == conc_temp & preds_rr$climate_action == action_temp] <- se_total
  preds_rr$lower_ci[preds_rr$climate_concern == conc_temp & preds_rr$climate_action == action_temp] <- 
    mean_RR - (1.96 * se_total)
  preds_rr$upper_ci[preds_rr$climate_concern == conc_temp & preds_rr$climate_action == action_temp] <- 
    mean_RR + (1.96 * se_total)
  
}

preds_rr


### And also check the main effects model
model <- with(smcfcs_imp, lm(mental_health_m ~ climate_concern_m + confounds_m + baseline_mh_m + 
                               climate_action_m))
est <- pool(model)
(results_smcfcs_main <- summary(est, conf.int = TRUE))

# As above, MI did not remove bias (as there was no bias in the CCA), but has improved efficiency
summary(a)
summary(a_cca)


# Average score by climate concern (as above, 'avg_predictions' doesn't seem to with with SMCFCS objects, so will calculate manually)

# Loop over each imputed dataset, storing the predicted probabilities and SEs
for (i in 1:length(imp_allSMCFCS$impDatasets)) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- smcfcs_imp$imputations[[i]]
  mod_temp <- lm(mental_health_m ~ climate_concern_m + confounds_m + baseline_mh_m + 
                   climate_action_m, data = df_temp)

  preds_temp_all <- avg_predictions(mod_temp, variables = c("climate_concern_m"))
  
  if (i == 1) {
    temp_all <- as.data.frame(cbind(climate_concern = as.character(preds_temp_all$climate_concern_m), 
                                    est = preds_temp_all$estimate, se = preds_temp_all$std.error)) 
  } else {
    temp2_all <- as.data.frame(cbind(climate_concern = as.character(preds_temp_all$climate_concern_m), 
                                     est = preds_temp_all$estimate, se = preds_temp_all$std.error))
    temp_all <- rbind(temp_all, temp2_all)
  }
}

temp_all

# Convert data to numeric, then SEs to variances
temp_all$est <- as.numeric(temp_all$est)
temp_all$se <- as.numeric(temp_all$se)
temp_all$var <- temp_all$se ^ 2

## Generate mean values and SEs using Rubin's Rules for each comparison and store in table
preds_rr_all <- temp2_all
preds_rr_all$est <- NA
preds_rr_all$se <- NA
preds_rr_all$lower_ci <- NA
preds_rr_all$upper_ci <- NA
preds_rr_all

for (i in 1:nrow(preds_rr_all)) {
  
  # Store the combination of climate concern and climate action levels
  conc_temp <- temp_all$climate_concern[i]
  
  # Mean value and SEs using Rubin's Rules
  mean_RR <- mean(temp_all$est[temp_all$climate_concern == conc_temp])
  var_within <- mean(temp_all$var[temp_all$climate_concern == conc_temp])
  var_between <- ((1 / (length(imp_allSMCFCS$impDatasets) - 1)) * 
                    sum((temp_all$est[temp_all$climate_concern == conc_temp] - mean_RR) ^ 2))
  var_total <- var_within + ((1 + (1 / length(imp_allSMCFCS$impDatasets))) * var_between)
  se_total <- sqrt(var_total)
  
  # Store these results in the table
  preds_rr_all$est[preds_rr_all$climate_concern == conc_temp] <- mean_RR
  preds_rr_all$se[preds_rr_all$climate_concern == conc_temp] <- se_total
  preds_rr_all$lower_ci[preds_rr_all$climate_concern == conc_temp] <- mean_RR - (1.96 * se_total)
  preds_rr_all$upper_ci[preds_rr_all$climate_concern == conc_temp] <- mean_RR + (1.96 * se_total)
  
}

preds_rr_all


