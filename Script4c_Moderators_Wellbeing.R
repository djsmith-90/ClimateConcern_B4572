### Script for paper 'Does concern regarding climate change impact subsequent mental health? A longitudinal analysis using data from the Avon 2Longitudinal Study of Parents and Children (ALSPAC)' - ALSPAC B-number B4572
### Script 4c: Analysis for Research Question 2 - Moderators between climate concern and well-being
### Created 2/12/2024 by Dan Major-Smith
### R version 4.3.1

## A pre-registered analysis plan for this paper is available on the OSF: https://rr.peercommunityin.org/articles/rec?id=793


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4572 - Climate Concern")

#install.packages("tidyverse")
#install.packages("marginaleffects")
#install.packages("mice")
#install.packages("smcfcs")
#install.packages("mitools")

library(tidyverse)
library(marginaleffects)
library(mice)
library(smcfcs)
library(mitools)


##########################################################################################
#### Read in the processed data

load("data_climateConcern_B4572.RData")

# Or load synthetic data
#load("./AnalysisCode_ClimateConcern_B4572/SyntheticData/syntheticData_climateConcern_B4572.RData")
#dat <- dat_syn_df

# Check data
head(dat)
summary(dat)
glimpse(dat)



####################################################################################################
####### Complete-case analysis results

##### 'Total number of climate actions' moderator

# Make CCA marker, so unadjusted and adjusted models have the same sample size - NOTE: Only 419 complete cases
dat_actions <- dat %>%
  select(-c(actionBelief, epds, gad7))

dat_actions$cca <- complete.cases(dat_actions)
table(dat_actions$cca)


#### Linear regression models with interaction

### Unadjusted
actions_cca_unadj <- lm(wemwbs ~ climateConcern + totalActions + climateConcern:totalActions, 
                        data = dat_actions, subset = cca == TRUE)
summary(actions_cca_unadj)
round(confint(actions_cca_unadj), 2)

### Adjusted
actions_cca_adj <- lm(wemwbs ~ climateConcern + totalActions + climateConcern:totalActions 
                      + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                      + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                      + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                      + mat_imd + par_occSocClass + par_income + mat_home, 
                      data = dat_actions)
summary(actions_cca_adj)
round(confint(actions_cca_adj), 2)


## Average depression score by climate concern
avg_predictions(actions_cca_adj, variables = "climateConcern")

## Average depression score by climate concern and certain values of climate action
predictions(actions_cca_adj, newdata = datagrid(climateConcern = c("Not concerned", "Somewhat concerned", 
                                                                   "Very concerned"), 
                                                totalActions = c(0, 5, 10, 15)))

## Plot of predicted values showing interaction between climate concern and action
plot_predictions(actions_cca_adj, condition = c("totalActions", "climateConcern"))



##### 'Belief in efficacy of climate actions' moderator

# Make CCA marker, so unadjusted and adjusted models have the same sample size - NOTE: Only 418 complete cases
dat_belief <- dat %>%
  select(-c(totalActions, epds, gad7))

dat_belief$cca <- complete.cases(dat_belief)
table(dat_belief$cca)


#### Linear regression models with interaction

### Unadjusted
beliefs_cca_unadj <- lm(wemwbs ~ climateConcern + actionBelief + climateConcern:actionBelief, 
                        data = dat_belief, subset = cca == TRUE)
summary(beliefs_cca_unadj)
round(confint(beliefs_cca_unadj), 2)

### Adjusted
beliefs_cca_adj <- lm(wemwbs ~ climateConcern + actionBelief + climateConcern:actionBelief 
                      + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                      + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                      + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                      + mat_imd + par_occSocClass + par_income + mat_home, 
                      data = dat_belief)
summary(beliefs_cca_adj)
round(confint(beliefs_cca_adj), 2)


## Average depression score by climate concern
avg_predictions(beliefs_cca_adj, variables = "climateConcern")

## Average depression score by climate concern and certain values of climate action
predictions(beliefs_cca_adj, newdata = datagrid(climateConcern = c("Not concerned", "Somewhat concerned", 
                                                                   "Very concerned"), 
                                                actionBelief = c("No", "Yes")))

## Plot of predicted values showing interaction between climate concern and action
plot_predictions(beliefs_cca_adj, condition = c("actionBelief", "climateConcern"))



####################################################################################################
####### Multiple imputation analysis results - First using 'all interactions' method

#### 'Total number of climate actions' moderator

## Remove other moderator from dataset (as will impute separately below)
dat_mi <- dat %>%
  select(-c(actionBelief))

head(dat_mi)
summary(dat_mi)
glimpse(dat_mi)


### Now for the imputation

## First, set-up the imputation methods for each variable. Most look fine, but will change some to match the data better (e.g., changing from unordered to ordered models, plus some continuous models to a Gaussian model, if normally-distributed)
meth <- make.method(dat_mi)
meth["edu"] <- "polr"
meth["income"] <- "polr"
meth["imd"] <- "polr"
meth["emoStab"] <- "norm"
meth["openness"] <- "norm"
meth["mat_age"] <- "norm"
meth["mat_edu"] <- "polr"
meth["mat_imd"] <- "polr"
meth["par_occSocClass"] <- "polr"
meth["par_income"] <- "polr"
meth["climateConcern"] <- "polr"
meth

## Second, specify the imputation model for each variable with missing data (note: We are using this rather than the 'predictor matrix' approach used for standard MI, in order to include the relevant interaction terms; note also that all imputation models need to include the 'climateConcern:totalActions' interaction term for compatability with the substantive model). Note also that these formulas have to be in the same order as the variables in the dataset.
form <- as.list(c(
  as.formula(smfq ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs 
             + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(prior_dep ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs 
             + smfq + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(gad7_prior ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(prior_anx ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs 
             + smfq + prior_dep + gad7_prior + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(wemwbs_prior ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + prior_anx + gad7_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(sex ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(ethnicity ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(relationship ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(children ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(edu ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(occClass ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(income ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(imd ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(home ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs 
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(emoStab ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(openness ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs 
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(mat_dep ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(mat_anx ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(pat_dep ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(pat_anx ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(mat_age ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(mat_edu ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(mat_imd ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(par_occSocClass ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_income + mat_home + epds + gad7),
  as.formula(par_income ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + mat_home + epds + gad7),
  as.formula(mat_home ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + epds + gad7),
  as.formula(climateConcern ~ wemwbs + totalActions + wemwbs:totalActions
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(totalActions ~ wemwbs + climateConcern + wemwbs:climateConcern
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(epds ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + gad7),
  as.formula(gad7 ~ climateConcern + totalActions + climateConcern:totalActions + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds),
  as.formula(wemwbs ~ climateConcern + totalActions + climateConcern:totalActions
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7)
))
form

## Run a test imputation to make sure it looks okay, and check the amount of missing data in each variable
test <- mice(dat_mi, m = 5, maxit = 0, 
             method = meth, formulas = form, print = TRUE)
test
table(test$nmis)


## Now run the proper imputation model. Will create 50 imputed datasets here with a burn-in period of 10 iterations
imp_mi <- mice(dat_mi, m = 50, maxit = 10, 
               method = meth, formulas = form, print = TRUE, seed = 778899)

## Save these imputations, to avoid having to run the imputations again
save(imp_mi, file = "MI_wb_totalActions_allInts.RData")
#load("MI_wb_totalActions_allInts.RData")


#### Now run substantive analysis model in each imputed dataset and combine using Rubin's rules

## Unadjusted
model <- with(imp_mi, lm(wemwbs ~ climateConcern + totalActions + climateConcern:totalActions))
est <- pool(model)
(actions_mi_allInts_unadj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(actions_cca_unadj), confint(actions_cca_unadj), p = coef(summary(actions_cca_unadj))[, 4])


## Adjusted
model <- with(imp_mi, lm(wemwbs ~ climateConcern + totalActions + climateConcern:totalActions 
                         + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                         + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                         + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                         + mat_imd + par_occSocClass + par_income + mat_home))
est <- pool(model)
(actions_mi_allInts_adj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(actions_cca_adj), confint(actions_cca_adj), p = coef(summary(actions_cca_adj))[, 4])


# Average depression score by climate concern and certain values of climate action - Note: I can't find a 'marginaleffects' function which can apply this automatically to imputed data, so will create a loop to do this manually.

# Loop over each imputed dataset, storing the predicted probabilities and SEs
for (i in 1:imp_mi$m) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- complete(imp_mi, i)
  mod_temp <- lm(wemwbs ~ climateConcern + totalActions + climateConcern:totalActions 
                 + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                 + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                 + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                 + mat_imd + par_occSocClass + par_income + mat_home,
                 data = df_temp)
  preds_temp <- predictions(mod_temp, newdata = datagrid(climateConcern = 
                                                           c("Not concerned", "Somewhat concerned", "Very concerned"), 
                                                         totalActions = c(0, 5, 10, 15)))
  if (i == 1) {
    temp <- as.data.frame(cbind(climateConcern = as.character(preds_temp$climateConcern), 
                                totalActions = preds_temp$totalActions,
                                est = preds_temp$estimate, se = preds_temp$std.error)) 
  } else {
    temp2 <- as.data.frame(cbind(climateConcern = as.character(preds_temp$climateConcern), 
                                 totalActions = preds_temp$totalActions,
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
  conc_temp <- temp$climateConcern[i]
  action_temp <- temp$totalActions[i]
  
  # Mean value and SEs using Rubin's Rules
  mean_RR <- mean(temp$est[temp$climateConcern == conc_temp & temp$totalActions == action_temp])
  var_within <- mean(temp$var[temp$climateConcern == conc_temp & temp$totalActions == action_temp])
  var_between <- ((1 / (imp_mi$m - 1)) * sum((temp$est[temp$climateConcern == conc_temp & 
                                                             temp$totalActions == action_temp] - mean_RR) ^ 2))
  var_total <- var_within + ((1 + (1 / imp_mi$m)) * var_between)
  se_total <- sqrt(var_total)
  
  # Store these results in the table
  preds_rr$est[preds_rr$climateConcern == conc_temp & preds_rr$totalActions == action_temp] <- mean_RR
  preds_rr$se[preds_rr$climateConcern == conc_temp & preds_rr$totalActions == action_temp] <- se_total
  preds_rr$lower_ci[preds_rr$climateConcern == conc_temp & preds_rr$totalActions == action_temp] <- 
    mean_RR - (1.96 * se_total)
  preds_rr$upper_ci[preds_rr$climateConcern == conc_temp & preds_rr$totalActions == action_temp] <- 
    mean_RR + (1.96 * se_total)
  
}

preds_rr
write_csv(preds_rr, file = "wb_actions_MI_allInts_PredValues.csv")


### And also check the main effects model
model <- with(imp_mi, lm(wemwbs ~ climateConcern))
est <- pool(model)
(results_allInt_main <- summary(est, conf.int = TRUE))
(mi_marg <- avg_comparisons(model, variables = list(climateConcern = "pairwise")))

model <- with(imp_mi, lm(wemwbs ~ climateConcern + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                         + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                         + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                         + mat_imd + par_occSocClass + par_income + mat_home))
est <- pool(model)
(results_allInt_main <- summary(est, conf.int = TRUE))
(mi_marg <- avg_comparisons(model, variables = list(climateConcern = "pairwise")))


#### 'Belief in efficacy of climate actions' moderator

## Remove other moderator from dataset (as will impute separately below)
dat_mi <- dat %>%
  select(-c(totalActions))

head(dat_mi)
summary(dat_mi)
glimpse(dat_mi)


### Now for the imputation

## First, set-up the imputation methods for each variable. Most look fine, but will change some to match the data better (e.g., changing from unordered to ordered models, plus some continuous models to a Gaussian model, if normally-distributed)
meth <- make.method(dat_mi)
meth["edu"] <- "polr"
meth["income"] <- "polr"
meth["imd"] <- "polr"
meth["emoStab"] <- "norm"
meth["openness"] <- "norm"
meth["mat_age"] <- "norm"
meth["mat_edu"] <- "polr"
meth["mat_imd"] <- "polr"
meth["par_occSocClass"] <- "polr"
meth["par_income"] <- "polr"
meth["climateConcern"] <- "polr"
meth

## Second, specify the imputation model for each variable with missing data (note: We are using this rather than the 'predictor matrix' approach used for standard MI, in order to include the relevant interaction terms; note also that all imputation models need to include the 'climateConcern:actionBelief' interaction term for compatability with the substantive model). Note also that these formulas have to be in the same order as the variables in the dataset.
form <- as.list(c(
  as.formula(smfq ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(prior_dep ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(gad7_prior ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(prior_anx ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(wemwbs_prior ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + prior_anx + gad7_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(sex ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs 
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(ethnicity ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(relationship ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(children ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs 
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(edu ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(occClass ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs 
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(income ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs 
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(imd ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(home ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(emoStab ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(openness ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(mat_dep ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(mat_anx ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(pat_dep ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(pat_anx ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(mat_age ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs 
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(mat_edu ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(mat_imd ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(par_occSocClass ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_income + mat_home + epds + gad7),
  as.formula(par_income ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + mat_home + epds + gad7),
  as.formula(mat_home ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + epds + gad7),
  as.formula(climateConcern ~ wemwbs + actionBelief + wemwbs:actionBelief 
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(actionBelief ~ wemwbs + climateConcern + wemwbs:climateConcern 
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7),
  as.formula(epds ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + gad7),
  as.formula(gad7 ~ climateConcern + actionBelief + climateConcern:actionBelief + wemwbs
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds),
  as.formula(wemwbs ~ climateConcern + actionBelief + climateConcern:actionBelief
             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
             + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7)
))
form

## Run a test imputation to make sure it looks okay, and check the amount of missing data in each variable
test <- mice(dat_mi, m = 5, maxit = 0, 
             method = meth, formulas = form, print = TRUE)
test
table(test$nmis)


## Now run the proper imputation model. Will create 50 imputed datasets here with a burn-in period of 10 iterations
imp_mi <- mice(dat_mi, m = 50, maxit = 10, 
               method = meth, formulas = form, print = TRUE, seed = 998877)

## Save these imputations, to avoid having to run the imputations again
save(imp_mi, file = "MI_wb_actionBelief_allInts.RData")
#load("MI_wb_actionBelief_allInts.RData")


#### Now run substantive analysis model in each imputed dataset and combine using Rubin's rules

## Unadjusted
model <- with(imp_mi, lm(wemwbs ~ climateConcern + actionBelief + climateConcern:actionBelief))
est <- pool(model)
(beliefs_mi_allInts_unadj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(beliefs_cca_unadj), confint(beliefs_cca_unadj), p = coef(summary(beliefs_cca_unadj))[, 4])


## Adjusted
model <- with(imp_mi, lm(wemwbs ~ climateConcern + actionBelief + climateConcern:actionBelief 
                         + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                         + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                         + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                         + mat_imd + par_occSocClass + par_income + mat_home))
est <- pool(model)
(beliefs_mi_allInts_adj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(beliefs_cca_adj), confint(beliefs_cca_adj), p = coef(summary(beliefs_cca_adj))[, 4])


# Average depression score by climate concern and climate belief - Note: I can't find a 'marginaleffects' function which can apply this automatically to imputed data, so will create a loop to do this manually.

# Loop over each imputed dataset, storing the predicted probabilities and SEs
for (i in 1:imp_mi$m) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- complete(imp_mi, i)
  mod_temp <- lm(wemwbs ~ climateConcern + actionBelief + climateConcern:actionBelief 
                 + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                 + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                 + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                 + mat_imd + par_occSocClass + par_income + mat_home,
                 data = df_temp)
  preds_temp <- predictions(mod_temp, newdata = datagrid(climateConcern = 
                                                           c("Not concerned", "Somewhat concerned", "Very concerned"), 
                                                         actionBelief = c("No", "Yes")))
  if (i == 1) {
    temp <- as.data.frame(cbind(climateConcern = as.character(preds_temp$climateConcern), 
                                actionBelief = preds_temp$actionBelief,
                                est = preds_temp$estimate, se = preds_temp$std.error)) 
  } else {
    temp2 <- as.data.frame(cbind(climateConcern = as.character(preds_temp$climateConcern), 
                                 actionBelief = preds_temp$actionBelief,
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
  conc_temp <- temp$climateConcern[i]
  belief_temp <- temp$actionBelief[i]
  
  # Mean value and SEs using Rubin's Rules
  mean_RR <- mean(temp$est[temp$climateConcern == conc_temp & temp$actionBelief == belief_temp])
  var_within <- mean(temp$var[temp$climateConcern == conc_temp & temp$actionBelief == belief_temp])
  var_between <- ((1 / (imp_mi$m - 1)) * sum((temp$est[temp$climateConcern == conc_temp & 
                                                         temp$actionBelief == belief_temp] - mean_RR) ^ 2))
  var_total <- var_within + ((1 + (1 / imp_mi$m)) * var_between)
  se_total <- sqrt(var_total)
  
  # Store these results in the table
  preds_rr$est[preds_rr$climateConcern == conc_temp & preds_rr$actionBelief == belief_temp] <- mean_RR
  preds_rr$se[preds_rr$climateConcern == conc_temp & preds_rr$actionBelief == belief_temp] <- se_total
  preds_rr$lower_ci[preds_rr$climateConcern == conc_temp & preds_rr$actionBelief == belief_temp] <- 
    mean_RR - (1.96 * se_total)
  preds_rr$upper_ci[preds_rr$climateConcern == conc_temp & preds_rr$actionBelief == belief_temp] <- 
    mean_RR + (1.96 * se_total)
  
}

preds_rr
write_csv(preds_rr, file = "wb_efficacy_MI_allInts_PredValues.csv")


### And also check the main effects model
model <- with(imp_mi, lm(wemwbs ~ climateConcern))
est <- pool(model)
(results_allInt_main <- summary(est, conf.int = TRUE))
(mi_marg <- avg_comparisons(model, variables = list(climateConcern = "pairwise")))

model <- with(imp_mi, lm(wemwbs ~ climateConcern + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                         + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                         + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                         + mat_imd + par_occSocClass + par_income + mat_home))
est <- pool(model)
(results_allInt_main <- summary(est, conf.int = TRUE))
(mi_marg <- avg_comparisons(model, variables = list(climateConcern = "pairwise")))



####################################################################################################
####### Multiple imputation analysis results - Now using 'SMCFCS' method

#### 'Total number of climate actions' moderator

## Remove other moderator from dataset (as will impute separately below)
dat_mi <- dat %>%
  select(-c(actionBelief))

head(dat_mi)
summary(dat_mi)
glimpse(dat_mi)


### Now for the imputation

# Create interaction variables for both levels of 'climateConcern'
dat_mi$concernXaction_concerned <- ifelse(is.na(dat_mi$climateConcern) | 
                                                  is.na(dat_mi$totalActions), NA,
                                                ifelse(dat_mi$climateConcern == "Somewhat concerned", 
                                                       dat_mi$totalActions, 0))
dat_mi$concernXaction_veryConcerned <- ifelse(is.na(dat_mi$climateConcern) | 
                                                      is.na(dat_mi$totalActions), NA,
                                                    ifelse(dat_mi$climateConcern == "Very concerned", 
                                                           dat_mi$totalActions, 0))

head(dat_mi)
summary(dat_mi)


## Re-order the variables so they appear in the dataset the same order as the SMCFCS formula and methods - Also make some variables ordered factors (as SMCFCS needs this specified)
dat_mi <- dat_mi %>%
  relocate(wemwbs, climateConcern, totalActions, concernXaction_concerned, concernXaction_veryConcerned, 
           smfq, prior_dep, gad7_prior, prior_anx, wemwbs_prior,
           sex, ethnicity, relationship, children, edu, occClass, income, imd, home,
           emoStab, openness, mat_dep, mat_anx, pat_dep, pat_anx, mat_age, mat_edu,
           mat_imd, par_occSocClass, par_income, mat_home, epds, gad7) %>%
  mutate(climateConcern = factor(climateConcern, ordered = TRUE)) %>%
  mutate(edu = factor(edu, ordered = TRUE)) %>%
  mutate(occClass = factor(occClass, ordered = TRUE)) %>%
  mutate(income = factor(income, ordered = TRUE)) %>%
  mutate(imd = factor(imd, ordered = TRUE)) %>%
  mutate(mat_edu = factor(mat_edu, ordered = TRUE)) %>%
  mutate(mat_imd = factor(mat_imd, ordered = TRUE)) %>%
  mutate(par_occSocClass = factor(par_occSocClass, ordered = TRUE)) %>%
  mutate(par_income = factor(par_income, ordered = TRUE))

glimpse(dat_mi)


# NOTE: For SMCFCS, have to have dataset as data.frame, not tibble!
dat_mi <- as.data.frame(dat_mi)
class(dat_mi)


## Am had some errors in the previous SMCFCS scripts due to multinomial imputation models, will replace all these (and use 'logreg' instead for 'home', by converting the unordered variables to binary variables)
dat_mi$home <- ifelse(is.na(dat_mi$home), NA,
                      ifelse(dat_mi$home == "Owned/Mortgaged", "Owned/Mortgaged", "Rented/Council/Other"))
dat_mi$home <- factor(dat_mi$home, levels = c("Owned/Mortgaged", "Rented/Council/Other"))
table(dat_mi$home, useNA = "ifany")

dat_mi$mat_home <- ifelse(is.na(dat_mi$mat_home), NA,
                      ifelse(dat_mi$mat_home == "Owned/Mortgaged", "Owned/Mortgaged", "Rented/Council/Other"))
dat_mi$mat_home <- factor(dat_mi$home, levels = c("Owned/Mortgaged", "Rented/Council/Other"))
table(dat_mi$mat_home, useNA = "ifany")

set.seed(445566)
imp_mi <- smcfcs(dat_mi, m = 50, numit = 10, noisy = FALSE, smtype = "lm",
                 smformula = "wemwbs ~ climateConcern + totalActions + concernXaction_concerned + concernXaction_veryConcerned + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior + sex + ethnicity + relationship + children + edu + occClass + income + imd + home + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7",
                 method = c("", "podds", "norm", 
                            "ifelse(climateConcern == 'Somewhat concerned', totalActions, 0)", 
                            "ifelse(climateConcern == 'Very concerned', totalActions, 0)",
                            "norm", "logreg", "norm", "logreg", "norm", 
                            "", "logreg", "logreg", "logreg", "podds", "podds", "podds", "podds", "logreg",
                            "norm", "norm", "norm", "norm", "norm", "norm", "norm", "podds",
                            "podds", "podds", "podds", "logreg", "norm", "norm"),
                 rjlimit = 10000)


## Save these imputations, to avoid having to run the imputations again
save(imp_mi, file = "MI_wb_totalActions_SMCFCS.RData")
#load("MI_wb_totalActions_SMCFCS.RData")


#### Now run substantive analysis model in each imputed dataset and combine using Rubin's rules

## Extract results
smcfcs_imp <- imputationList(imp_mi$impDatasets)

# Convert climate concern back to unordered factor for models
for (i in 1:length(imp_mi$impDatasets)) {
  smcfcs_imp$imputations[[i]]$climateConcern <- factor(smcfcs_imp$imputations[[i]]$climateConcern, ordered = FALSE)
}

## Unadjusted
model <- with(smcfcs_imp, lm(wemwbs ~ climateConcern + totalActions + climateConcern:totalActions))
est <- pool(model)
(actions_mi_smcfcs_unadj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(actions_cca_unadj), confint(actions_cca_unadj), p = coef(summary(actions_cca_unadj))[, 4])


## Adjusted
model <- with(smcfcs_imp, lm(wemwbs ~ climateConcern + totalActions + climateConcern:totalActions 
                         + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                         + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                         + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                         + mat_imd + par_occSocClass + par_income + mat_home))
est <- pool(model)
(actions_mi_smcfcs_adj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(actions_cca_adj), confint(actions_cca_adj), p = coef(summary(actions_cca_adj))[, 4])


# Average depression score by climate concern and certain values of climate action - Note: I can't find a 'marginaleffects' function which can apply this automatically to imputed data, so will create a loop to do this manually.

# Loop over each imputed dataset, storing the predicted probabilities and SEs
for (i in 1:length(imp_mi$impDatasets)) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- smcfcs_imp$imputations[[i]]
  mod_temp <- lm(wemwbs ~ climateConcern + totalActions + climateConcern:totalActions 
                 + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                 + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                 + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                 + mat_imd + par_occSocClass + par_income + mat_home,
                 data = df_temp)
  preds_temp <- predictions(mod_temp, newdata = datagrid(climateConcern = 
                                                           c("Not concerned", "Somewhat concerned", "Very concerned"), 
                                                         totalActions = c(0, 5, 10, 15)))
  if (i == 1) {
    temp <- as.data.frame(cbind(climateConcern = as.character(preds_temp$climateConcern), 
                                totalActions = preds_temp$totalActions,
                                est = preds_temp$estimate, se = preds_temp$std.error)) 
  } else {
    temp2 <- as.data.frame(cbind(climateConcern = as.character(preds_temp$climateConcern), 
                                 totalActions = preds_temp$totalActions,
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
  conc_temp <- temp$climateConcern[i]
  action_temp <- temp$totalActions[i]
  
  # Mean value and SEs using Rubin's Rules
  mean_RR <- mean(temp$est[temp$climateConcern == conc_temp & temp$totalActions == action_temp])
  var_within <- mean(temp$var[temp$climateConcern == conc_temp & temp$totalActions == action_temp])
  var_between <- ((1 / (length(imp_mi$impDatasets) - 1)) * sum((temp$est[temp$climateConcern == conc_temp & 
                                                         temp$totalActions == action_temp] - mean_RR) ^ 2))
  var_total <- var_within + ((1 + (1 / length(imp_mi$impDatasets))) * var_between)
  se_total <- sqrt(var_total)
  
  # Store these results in the table
  preds_rr$est[preds_rr$climateConcern == conc_temp & preds_rr$totalActions == action_temp] <- mean_RR
  preds_rr$se[preds_rr$climateConcern == conc_temp & preds_rr$totalActions == action_temp] <- se_total
  preds_rr$lower_ci[preds_rr$climateConcern == conc_temp & preds_rr$totalActions == action_temp] <- 
    mean_RR - (1.96 * se_total)
  preds_rr$upper_ci[preds_rr$climateConcern == conc_temp & preds_rr$totalActions == action_temp] <- 
    mean_RR + (1.96 * se_total)
  
}

preds_rr
write_csv(preds_rr, file = "wb_actions_MI_SMCFCS_PredValues.csv")


### And also check the main effects model
model <- with(smcfcs_imp, lm(wemwbs ~ climateConcern))
est <- pool(model)
(results_smfcs_main <- summary(est, conf.int = TRUE))

model <- with(smcfcs_imp, lm(wemwbs ~ climateConcern + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                         + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                         + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                         + mat_imd + par_occSocClass + par_income + mat_home))
est <- pool(model)
(results_smfcs_main <- summary(est, conf.int = TRUE))


## And get the contrast between 'somewhat' and 'very' concerned (as doesn't work with 'marginaleffects')

# Loop over each imputed dataset, storing the predicted probabilities and SEs
for (i in 1:length(imp_mi$impDatasets)) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- smcfcs_imp$imputations[[i]]
  mod_temp_unadj <- lm(wemwbs ~ climateConcern, data = df_temp)
  mod_temp_adj <- lm(wemwbs ~ climateConcern + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                     + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                     + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                     + mat_imd + par_occSocClass + par_income + mat_home,
                     data = df_temp)
  preds_temp_unadj <- avg_comparisons(mod_temp_unadj, variables = list(climateConcern = "pairwise"))
  preds_temp_adj <- avg_comparisons(mod_temp_adj, variables = list(climateConcern = "pairwise"))
  if (i == 1) {
    temp_unadj <- as.data.frame(cbind(contrast = as.character(as.data.frame(preds_temp_unadj)[3, "contrast"]), 
                                      est = as.data.frame(preds_temp_unadj)[3, "estimate"], 
                                      se = as.data.frame(preds_temp_unadj)[3, "std.error"])) 
  } else {
    temp_unadj2 <- as.data.frame(cbind(contrast = as.character(as.data.frame(preds_temp_unadj)[3, "contrast"]), 
                                       est = as.data.frame(preds_temp_unadj)[3, "estimate"], 
                                       se = as.data.frame(preds_temp_unadj)[3, "std.error"])) 
    temp_unadj <- rbind(temp_unadj, temp_unadj2)
  }
  if (i == 1) {
    temp_adj <- as.data.frame(cbind(contrast = as.character(as.data.frame(preds_temp_adj)[3, "contrast"]), 
                                    est = as.data.frame(preds_temp_adj)[3, "estimate"], 
                                    se = as.data.frame(preds_temp_adj)[3, "std.error"])) 
  } else {
    temp_adj2 <- as.data.frame(cbind(contrast = as.character(as.data.frame(preds_temp_adj)[3, "contrast"]), 
                                     est = as.data.frame(preds_temp_adj)[3, "estimate"], 
                                     se = as.data.frame(preds_temp_adj)[3, "std.error"])) 
    temp_adj <- rbind(temp_adj, temp_adj2)
  }
}

temp_unadj
temp_adj

# Convert data to numeric, then SEs to variances
temp_unadj$est <- as.numeric(temp_unadj$est)
temp_unadj$se <- as.numeric(temp_unadj$se)
temp_unadj$var <- temp_unadj$se ^ 2

temp_adj$est <- as.numeric(temp_adj$est)
temp_adj$se <- as.numeric(temp_adj$se)
temp_adj$var <- temp_adj$se ^ 2

## Apply Rubin's rules

# Unadjusted
mean_RR <- mean(temp_unadj$est)
var_within <- mean(temp_unadj$var)
var_between <- ((1 / (length(imp_mi$impDatasets) - 1)) * sum((temp_unadj$est - mean_RR) ^ 2))
var_total <- var_within + ((1 + (1 / length(imp_mi$impDatasets))) * var_between)
se_total <- sqrt(var_total)
mean_RR; se_total; mean_RR - (1.96 * se_total); mean_RR + (1.96 * se_total); 2*pnorm(q= mean_RR / se_total, lower.tail=FALSE)

# Adjusted
mean_RR <- mean(temp_adj$est)
var_within <- mean(temp_adj$var)
var_between <- ((1 / (length(imp_mi$impDatasets) - 1)) * sum((temp_adj$est - mean_RR) ^ 2))
var_total <- var_within + ((1 + (1 / length(imp_mi$impDatasets))) * var_between)
se_total <- sqrt(var_total)
mean_RR; se_total; mean_RR - (1.96 * se_total); mean_RR + (1.96 * se_total); 2*pnorm(q= mean_RR / se_total, lower.tail=FALSE)



#### 'Belief in efficacy of climate actions' moderator

## Remove other moderator from dataset (as will impute separately below)
dat_mi <- dat %>%
  select(-c(totalActions))

head(dat_mi)
summary(dat_mi)
glimpse(dat_mi)


## Recode 'actionBelief' to binary numeric variable
dat_mi <- dat_mi %>%
  mutate(actionBelief = ifelse(is.na(actionBelief), NA,
                               ifelse(actionBelief == "No", "0", "1"))) %>%
  mutate(actionBelief = as.numeric(actionBelief))
table(dat_mi$actionBelief, useNA = "ifany")
class(dat_mi$actionBelief)


### Now for the imputation

# Create interaction variables for both levels of 'climateConcern'
dat_mi$concernXbelief_concerned <- ifelse(is.na(dat_mi$climateConcern) | 
                                            is.na(dat_mi$actionBelief), NA,
                                          ifelse(dat_mi$climateConcern == "Somewhat concerned", 
                                                 dat_mi$actionBelief, 0))
dat_mi$concernXbelief_veryConcerned <- ifelse(is.na(dat_mi$climateConcern) | 
                                                is.na(dat_mi$actionBelief), NA,
                                              ifelse(dat_mi$climateConcern == "Very concerned", 
                                                     dat_mi$actionBelief, 0))

head(dat_mi)
summary(dat_mi)


## Re-order the variables so they appear in the dataset the same order as the SMCFCS formula and methods - Also make some variables ordered factors (as SMCFCS needs this specified)
dat_mi <- dat_mi %>%
  relocate(wemwbs, climateConcern, actionBelief, concernXbelief_concerned, concernXbelief_veryConcerned, 
           smfq, prior_dep, gad7_prior, prior_anx, wemwbs_prior,
           sex, ethnicity, relationship, children, edu, occClass, income, imd, home,
           emoStab, openness, mat_dep, mat_anx, pat_dep, pat_anx, mat_age, mat_edu,
           mat_imd, par_occSocClass, par_income, mat_home, epds, gad7) %>%
  mutate(climateConcern = factor(climateConcern, ordered = TRUE)) %>%
  mutate(edu = factor(edu, ordered = TRUE)) %>%
  mutate(occClass = factor(occClass, ordered = TRUE)) %>%
  mutate(income = factor(income, ordered = TRUE)) %>%
  mutate(imd = factor(imd, ordered = TRUE)) %>%
  mutate(mat_edu = factor(mat_edu, ordered = TRUE)) %>%
  mutate(mat_imd = factor(mat_imd, ordered = TRUE)) %>%
  mutate(par_occSocClass = factor(par_occSocClass, ordered = TRUE)) %>%
  mutate(par_income = factor(par_income, ordered = TRUE))


# As got errors in SMCFCS method above, will also reduce the number of 'mlogit' models to help the imputation model work
dat_mi$home <- ifelse(is.na(dat_mi$home), NA,
                      ifelse(dat_mi$home == "Owned/Mortgaged", "Owned/Mortgaged", "Rented/Council/Other"))
dat_mi$home <- factor(dat_mi$home, levels = c("Owned/Mortgaged", "Rented/Council/Other"))
table(dat_mi$home, useNA = "ifany")

dat_mi$mat_home <- ifelse(is.na(dat_mi$mat_home), NA,
                          ifelse(dat_mi$mat_home == "Owned/Mortgaged", "Owned/Mortgaged", "Rented/Council/Other"))
dat_mi$mat_home <- factor(dat_mi$home, levels = c("Owned/Mortgaged", "Rented/Council/Other"))
table(dat_mi$mat_home, useNA = "ifany")

glimpse(dat_mi)


# NOTE: For SMCFCS, have to have dataset as data.frame, not tibble!
dat_mi <- as.data.frame(dat_mi)
class(dat_mi)


## Now run the proper SMCFCS imputation model, deriving the interaction terms using if statements. Will create 50 imputed datasets here with a burn-in period of 10 iterations
set.seed(5544565)
imp_mi <- smcfcs(dat_mi, m = 50, numit = 10, noisy = FALSE, smtype = "lm",
                 smformula = "wemwbs ~ climateConcern + actionBelief + concernXbelief_concerned + concernXbelief_veryConcerned + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior + sex + ethnicity + relationship + children + edu + occClass + income + imd + home + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu + mat_imd + par_occSocClass + par_income + mat_home + epds + gad7",
                 method = c("", "podds", "logreg", 
                            "ifelse(climateConcern == 'Somewhat concerned', actionBelief, 0)", 
                            "ifelse(climateConcern == 'Very concerned', actionBelief, 0)",
                            "norm", "logreg", "norm", "logreg", "norm", 
                            "", "logreg", "logreg", "logreg", "podds", "podds", "podds", "podds", "logreg",
                            "norm", "norm", "norm", "norm", "norm", "norm", "norm", "podds",
                            "podds", "podds", "podds", "logreg", "norm", "norm"),
                 rjlimit = 10000)

## Save these imputations, to avoid having to run the imputations again
save(imp_mi, file = "MI_wb_actionBelief_SMCFCS.RData")
#load("MI_wb_actionBelief_SMCFCS.RData")


#### Now run substantive analysis model in each imputed dataset and combine using Rubin's rules

## Extract results
smcfcs_imp <- imputationList(imp_mi$impDatasets)

# Convert climate concern back to unordered factor for models
for (i in 1:length(imp_mi$impDatasets)) {
  smcfcs_imp$imputations[[i]]$climateConcern <- factor(smcfcs_imp$imputations[[i]]$climateConcern, ordered = FALSE)
}

## Unadjusted
model <- with(smcfcs_imp, lm(wemwbs ~ climateConcern + actionBelief + climateConcern:actionBelief))
est <- pool(model)
(beliefs_mi_smcfcs_unadj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(beliefs_cca_unadj), confint(beliefs_cca_unadj), p = coef(summary(beliefs_cca_unadj))[, 4])


## Adjusted
model <- with(smcfcs_imp, lm(wemwbs ~ climateConcern + actionBelief + climateConcern:actionBelief 
                             + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                             + mat_imd + par_occSocClass + par_income + mat_home))
est <- pool(model)
(beliefs_mi_smcfcs_adj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(beliefs_cca_adj), confint(beliefs_cca_adj), p = coef(summary(beliefs_cca_adj))[, 4])


# Average depression score by climate concern and certain values of climate action - Note: I can't find a 'marginaleffects' function which can apply this automatically to imputed data, so will create a loop to do this manually.

# Loop over each imputed dataset, storing the predicted probabilities and SEs
for (i in 1:length(imp_mi$impDatasets)) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- smcfcs_imp$imputations[[i]]
  mod_temp <- lm(wemwbs ~ climateConcern + actionBelief + climateConcern:actionBelief 
                 + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                 + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                 + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                 + mat_imd + par_occSocClass + par_income + mat_home,
                 data = df_temp)
  preds_temp <- predictions(mod_temp, newdata = datagrid(climateConcern = 
                                                           c("Not concerned", "Somewhat concerned", "Very concerned"), 
                                                         actionBelief = c(0, 1)))
  if (i == 1) {
    temp <- as.data.frame(cbind(climateConcern = as.character(preds_temp$climateConcern), 
                                actionBelief = preds_temp$actionBelief,
                                est = preds_temp$estimate, se = preds_temp$std.error)) 
  } else {
    temp2 <- as.data.frame(cbind(climateConcern = as.character(preds_temp$climateConcern), 
                                 actionBelief = preds_temp$actionBelief,
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
  conc_temp <- temp$climateConcern[i]
  belief_temp <- temp$actionBelief[i]
  
  # Mean value and SEs using Rubin's Rules
  mean_RR <- mean(temp$est[temp$climateConcern == conc_temp & temp$actionBelief == belief_temp])
  var_within <- mean(temp$var[temp$climateConcern == conc_temp & temp$actionBelief == belief_temp])
  var_between <- ((1 / (length(imp_mi$impDatasets) - 1)) * sum((temp$est[temp$climateConcern == conc_temp & 
                                                         temp$actionBelief == belief_temp] - mean_RR) ^ 2))
  var_total <- var_within + ((1 + (1 / length(imp_mi$impDatasets))) * var_between)
  se_total <- sqrt(var_total)
  
  # Store these results in the table
  preds_rr$est[preds_rr$climateConcern == conc_temp & preds_rr$actionBelief == belief_temp] <- mean_RR
  preds_rr$se[preds_rr$climateConcern == conc_temp & preds_rr$actionBelief == belief_temp] <- se_total
  preds_rr$lower_ci[preds_rr$climateConcern == conc_temp & preds_rr$actionBelief == belief_temp] <- 
    mean_RR - (1.96 * se_total)
  preds_rr$upper_ci[preds_rr$climateConcern == conc_temp & preds_rr$actionBelief == belief_temp] <- 
    mean_RR + (1.96 * se_total)
  
}

preds_rr
write_csv(preds_rr, file = "wb_efficacy_MI_SMCFCS_PredValues.csv")


### And also check the main effects model
model <- with(smcfcs_imp, lm(wemwbs ~ climateConcern))
est <- pool(model)
(results_smfcs_main <- summary(est, conf.int = TRUE))

model <- with(smcfcs_imp, lm(wemwbs ~ climateConcern + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                             + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                             + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                             + mat_imd + par_occSocClass + par_income + mat_home))
est <- pool(model)
(results_smfcs_main <- summary(est, conf.int = TRUE))


## And get the contrast between 'somewhat' and 'very' concerned (as doesn't work with 'marginaleffects')

# Loop over each imputed dataset, storing the predicted probabilities and SEs
for (i in 1:length(imp_mi$impDatasets)) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- smcfcs_imp$imputations[[i]]
  mod_temp_unadj <- lm(wemwbs ~ climateConcern, data = df_temp)
  mod_temp_adj <- lm(wemwbs ~ climateConcern + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                     + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                     + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                     + mat_imd + par_occSocClass + par_income + mat_home,
                     data = df_temp)
  preds_temp_unadj <- avg_comparisons(mod_temp_unadj, variables = list(climateConcern = "pairwise"))
  preds_temp_adj <- avg_comparisons(mod_temp_adj, variables = list(climateConcern = "pairwise"))
  if (i == 1) {
    temp_unadj <- as.data.frame(cbind(contrast = as.character(as.data.frame(preds_temp_unadj)[3, "contrast"]), 
                                      est = as.data.frame(preds_temp_unadj)[3, "estimate"], 
                                      se = as.data.frame(preds_temp_unadj)[3, "std.error"])) 
  } else {
    temp_unadj2 <- as.data.frame(cbind(contrast = as.character(as.data.frame(preds_temp_unadj)[3, "contrast"]), 
                                       est = as.data.frame(preds_temp_unadj)[3, "estimate"], 
                                       se = as.data.frame(preds_temp_unadj)[3, "std.error"])) 
    temp_unadj <- rbind(temp_unadj, temp_unadj2)
  }
  if (i == 1) {
    temp_adj <- as.data.frame(cbind(contrast = as.character(as.data.frame(preds_temp_adj)[3, "contrast"]), 
                                    est = as.data.frame(preds_temp_adj)[3, "estimate"], 
                                    se = as.data.frame(preds_temp_adj)[3, "std.error"])) 
  } else {
    temp_adj2 <- as.data.frame(cbind(contrast = as.character(as.data.frame(preds_temp_adj)[3, "contrast"]), 
                                     est = as.data.frame(preds_temp_adj)[3, "estimate"], 
                                     se = as.data.frame(preds_temp_adj)[3, "std.error"])) 
    temp_adj <- rbind(temp_adj, temp_adj2)
  }
}

temp_unadj
temp_adj

# Convert data to numeric, then SEs to variances
temp_unadj$est <- as.numeric(temp_unadj$est)
temp_unadj$se <- as.numeric(temp_unadj$se)
temp_unadj$var <- temp_unadj$se ^ 2

temp_adj$est <- as.numeric(temp_adj$est)
temp_adj$se <- as.numeric(temp_adj$se)
temp_adj$var <- temp_adj$se ^ 2

## Apply Rubin's rules

# Unadjusted
mean_RR <- mean(temp_unadj$est)
var_within <- mean(temp_unadj$var)
var_between <- ((1 / (length(imp_mi$impDatasets) - 1)) * sum((temp_unadj$est - mean_RR) ^ 2))
var_total <- var_within + ((1 + (1 / length(imp_mi$impDatasets))) * var_between)
se_total <- sqrt(var_total)
mean_RR; se_total; mean_RR - (1.96 * se_total); mean_RR + (1.96 * se_total); 2*pnorm(q= mean_RR / se_total, lower.tail=FALSE)

# Adjusted
mean_RR <- mean(temp_adj$est)
var_within <- mean(temp_adj$var)
var_between <- ((1 / (length(imp_mi$impDatasets) - 1)) * sum((temp_adj$est - mean_RR) ^ 2))
var_total <- var_within + ((1 + (1 / length(imp_mi$impDatasets))) * var_between)
se_total <- sqrt(var_total)
mean_RR; se_total; mean_RR - (1.96 * se_total); mean_RR + (1.96 * se_total); 2*pnorm(q= mean_RR / se_total, lower.tail=FALSE)
