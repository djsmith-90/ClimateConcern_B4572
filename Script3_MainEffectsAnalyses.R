### Script for paper 'Does concern regarding climate change impact subsequent mental health? A longitudinal analysis using data from the Avon 2Longitudinal Study of Parents and Children (ALSPAC)' - ALSPAC B-number B4572
### Script 3: Analysis for Research Question 1 - Main effect of climate concern
### Created 25/11/2024 by Dan Major-Smith
### R version 4.3.1

## A pre-registered analysis plan for this paper is available on the OSF: https://rr.peercommunityin.org/articles/rec?id=793


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4572 - Climate Concern")

#install.packages("tidyverse")
#install.packages("marginaleffects")
#install.packages("mice")
#install.packages("sensemakr")

library(tidyverse)
library(marginaleffects)
library(mice)
library(sensemakr)


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


#### Descriptive statistics

### Climate concern exposure
table(dat$climateConcern, useNA = "ifany")
round(prop.table(table(dat$climateConcern)) * 100, 1)
round(prop.table(table(dat$climateConcern, useNA = "ifany")) * 100, 1)
sum(table(dat$climateConcern))


### Potential climate moderators

## Personal actions will make difference to long-term climate changes
table(dat$actionBelief, useNA = "ifany")
round(prop.table(table(dat$actionBelief)) * 100, 1)
sum(table(dat$actionBelief))

## Total number of climate actions engaged in
summary(dat$totalActions)
table(dat$totalActions)
sum(!is.na(dat$totalActions))

(action_hist <- ggplot(dat, aes(x = totalActions)) +
    geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
    xlab("Total number of climate actions") +
    theme_bw())

pdf("./Results/climateActions_hist.pdf", width = 8, height = 5)
action_hist
dev.off()


### Mental health and well-being outcomes

## Depression - Via Edinburgh Postnatal Depression Scale (EPDS) - Assessed at age 32 - Go through each question and code as numeric
summary(dat$epds)
sd(dat$epds, na.rm = TRUE)
table(dat$epds)
sum(!is.na(dat$epds))

(dep_hist <- ggplot(dat, aes(x = epds)) +
    geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
    xlab("Edinburgh Postnatal Depression Scale (EPDS) total score") +
    theme_bw())

pdf("./Results/dep_hist.pdf", width = 8, height = 5)
dep_hist
dev.off()


## Anxiety - Via Generalised Anxiety Disorder-7 scale (GAD) - Assessed at age 32 - Go through each question and code as numeric
summary(dat$gad7)
sd(dat$gad7, na.rm = TRUE)
table(dat$gad7)
sum(!is.na(dat$gad7))

(anx_hist <- ggplot(dat, aes(x = gad7)) +
    geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
    xlab("Generalised Anxiety Disorder 7 (GAD7) total score") +
    theme_bw())

pdf("./Results/anx_hist.pdf", width = 8, height = 5)
anx_hist
dev.off()


## Well-being - Via Warwick-Edinburgh Mental Well-Being Scale (WEMWBS) - Assessed at age 31 - Go through each question and code as numeric
summary(dat$wemwbs)
sd(dat$wemwbs, na.rm = TRUE)
table(dat$wemwbs)
sum(!is.na(dat$wemwbs))

(wb_hist <- ggplot(dat, aes(x = wemwbs)) +
    geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
    xlab("Warwick-Edinburgh Mental Well-Being Scale (WEMWBS) total score") +
    theme_bw())

pdf("./Results/wb_hist.pdf", width = 8, height = 5)
wb_hist
dev.off()


### Confounders

## Prior depressive symptoms - Short Moods and Feelings Questionnaire (SMFQ) - Age 25
summary(dat$smfq)
sd(dat$smfq, na.rm = TRUE)
table(dat$smfq)

ggplot(dat, aes(x = smfq)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

## ICD-10 depression diagnosis (from CIS-R) - Age 24
table(dat$prior_dep, useNA = "ifany")
round(prop.table(table(dat$prior_dep)) * 100, 1)
sum(table(dat$prior_dep))

## Prior anxiety - Via Generalised Anxiety Disorder-7 scale (GAD) - Assessed at age 21
summary(dat$gad7_prior)
sd(dat$gad7_prior, na.rm = TRUE)
table(dat$gad7_prior)

ggplot(dat, aes(x = gad7_prior)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

## ICD-10 generalised anxiety disorder diagnosis (from CIS-R) - Age 24
table(dat$prior_anx, useNA = "ifany")
round(prop.table(table(dat$prior_anx)) * 100, 1)
sum(table(dat$prior_anx))

## Prior well-being - Via Warwick-Edinburgh Mental Well-Being Scale (WEMWBS) - Assessed at age 23
summary(dat$wemwbs_prior)
sd(dat$wemwbs_prior, na.rm = TRUE)
table(dat$wemwbs_prior)

ggplot(dat, aes(x = wemwbs_prior)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

## Sex assigned at birth
table(dat$sex, useNA = "ifany")
round(prop.table(table(dat$sex)) * 100, 1)
sum(table(dat$sex))

## Ethnicity
table(dat$ethnicity, useNA = "ifany")
round(prop.table(table(dat$ethnicity)) * 100, 1)
sum(table(dat$ethnicity))

## Relationship status (living with partner) - Age 28
table(dat$relationship, useNA = "ifany")
round(prop.table(table(dat$relationship)) * 100, 1)
sum(table(dat$relationship))

## Has children - Age 29
table(dat$children, useNA = "ifany")
round(prop.table(table(dat$children)) * 100, 1)
sum(table(dat$children))

## Highest education qualification (GCSE/equivalent vs A-level/equivalent vs Degree vs post-graduate degree) - Age 27
table(dat$edu, useNA = "ifany")
round(prop.table(table(dat$edu)) * 100, 1)
sum(table(dat$edu))

## Occupational social class (Managerial/admin/professional vs intermediate vs small employers vs lower supervisorry/technical vs routine) - Age 23
table(dat$occClass, useNA = "ifany")
round(prop.table(table(dat$occClass)) * 100, 1)
sum(table(dat$occClass))

## Monthly household income after tax (£0-£499 vs £500-£999 vs £1000-£1499 vs £1500-£1999 vs £2000 and above) - Age 26
table(dat$income, useNA = "ifany")
round(prop.table(table(dat$income)) * 100, 1)
sum(table(dat$income))

## Area-level index of multiple deprivation (IMD; quintiles) - January 2021 (approx age 29)
table(dat$imd, useNA = "ifany")
round(prop.table(table(dat$imd)) * 100, 1)
sum(table(dat$imd))

## Home ownership status (owned/mortgaged vs rented vs Council/housing association vs other) - Age 28
table(dat$home, useNA = "ifany")
round(prop.table(table(dat$home)) * 100, 1)
sum(table(dat$home))

## 'Emotional stability' (opposite of 'neuroticism') personality trait
summary(dat$emoStab)
sd(dat$emoStab, na.rm = TRUE)

ggplot(dat, aes(x = emoStab)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

## 'Openness' personality trait
summary(dat$openness)
sd(dat$openness, na.rm = TRUE)

ggplot(dat, aes(x = openness)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

## Maternal EPDS in pregnancy - Using the pro-rated score, with modal value used for missing values
table(dat$mat_dep, useNA = "ifany")
summary(dat$mat_dep)
sd(dat$mat_dep, na.rm = TRUE)

ggplot(dat, aes(x = mat_dep)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

## Maternal Crown-Crisp Experiential Index - Anxiety subscale score (CCEI-A) in pregnancy
table(dat$mat_anx, useNA = "ifany")
summary(dat$mat_anx)
sd(dat$mat_anx, na.rm = TRUE)

ggplot(dat, aes(x = mat_anx)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

## Father EPDS in pregnancy - Using the pro-rated score, with modal value used for missing values
table(dat$pat_dep, useNA = "ifany")
summary(dat$pat_dep)
sd(dat$pat_dep, na.rm = TRUE)

ggplot(dat, aes(x = pat_dep)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

## Father Crown-Crisp Experiential Index - Anxiety subscale score (CCEI-A) in pregnancy 
table(dat$pat_anx, useNA = "ifany")
summary(dat$pat_anx)
sd(dat$pat_anx, na.rm = TRUE)

ggplot(dat, aes(x = pat_anx)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

## Maternal age at birth (years)
table(dat$mat_age, useNA = "ifany")
summary(dat$mat_age)
sd(dat$mat_age, na.rm = TRUE)

ggplot(dat, aes(x = mat_age)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

## Mother's highest education qualification (CSE/None vs Vocational vs O-level vs A-level vs Degree)
table(dat$mat_edu, useNA = "ifany")
round(prop.table(table(dat$mat_edu)) * 100, 1)
sum(table(dat$mat_edu))

## Parental occupational social class (I vs II vs III (non-manual) vs III (manual) vs IV/V) - Highest of either parent 
table(dat$par_occSocClass, useNA = "ifany")
round(prop.table(table(dat$par_occSocClass)) * 100, 1)
sum(table(dat$par_occSocClass))

## Parental weekly household income - age 3
table(dat$par_income, useNA = "ifany")
round(prop.table(table(dat$par_income)) * 100, 1)
sum(table(dat$par_income))

## Maternal area-level index of multiple deprivation (IMD; quintiles) - From Jan 1993 (around birth)
table(dat$mat_imd, useNA = "ifany")
round(prop.table(table(dat$mat_imd)) * 100, 1)
sum(table(dat$mat_imd))

## Maternal home ownership status (owned/mortgaged vs rented vs Council/housing association vs other) - In pregnancy
table(dat$mat_home, useNA = "ifany")
round(prop.table(table(dat$mat_home)) * 100, 1)
sum(table(dat$mat_home))


### Overall patterns of missing data
colSums(is.na(dat))
colSums(!is.na(dat))
round(colMeans(is.na(dat)) * 100, 1)

summary(colSums(is.na(dat)))
summary(colSums(!is.na(dat)))
summary(round(colMeans(is.na(dat)) * 100, 1))
hist(round(colMeans(is.na(dat)) * 100, 1), breaks = 10)


####################################################################################################
####### Complete-case analysis results

##### Depression outcome

# Descriptives by climate concern
by(dat$epds, dat$climateConcern, summary)
by(dat$epds, dat$climateConcern, sd, na.rm = TRUE)
by(!is.na(dat$epds), dat$climateConcern, sum, na.rm = TRUE)
sum(!is.na(dat$epds) & !is.na(dat$climateConcern))

# Make CCA marker, so unadjusted and adjusted models have the same sample size - NOTE: Only 424 complete cases
dat_dep <- dat %>%
  select(-c(actionBelief, totalActions, gad7, wemwbs))

dat_dep$cca <- complete.cases(dat_dep)
table(dat_dep$cca)

# Descriptives in complete-case sample
by(dat_dep$epds[dat_dep$cca == TRUE], dat_dep$climateConcern[dat_dep$cca == TRUE], summary)


#### Linear regression models

### Unadjusted
dep_cca_unadj <- lm(epds ~ climateConcern, data = dat_dep, subset = cca == TRUE)
summary(dep_cca_unadj)
round(confint(dep_cca_unadj), 2)

### Adjusted
dep_cca_adj <- lm(epds ~ climateConcern + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                  + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                  + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                  + mat_imd + par_occSocClass + par_income + mat_home, 
                  data = dat_dep)
summary(dep_cca_adj)
round(confint(dep_cca_adj), 2)


## Predicted values
avg_predictions(dep_cca_unadj, variables = c("climateConcern"))
avg_predictions(dep_cca_adj, variables = c("climateConcern"))

## Marginal contrasts and whether 'concerned' and 'very concerned' levels differ
avg_comparisons(dep_cca_unadj, variables = list(climateConcern = "pairwise"))
avg_comparisons(dep_cca_adj, variables = list(climateConcern = "pairwise"))


### Sensitivity analysis for unmeasured confounding

## 'Somewhat concerned' exposure level

# Overall amount of unmeasured confounding necessary to result in zero effect: Partial R2 of 6.7% with both exposure and outcome (results already cross 95% CI, so no comparison for this)
summary(sensemakr(model = dep_cca_adj, treatment = "climateConcernSomewhat concerned", 
                  q = 1, alpha = 0.05, reduce = TRUE))

# Whether unmeasured confounding equivalent to strong predictors of outcomes would impact results (they would not, as most have very little relationship with the exposure)
summary(sensemakr(model = dep_cca_adj, treatment = "climateConcernSomewhat concerned", 
                  benchmark_covariates = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale"),
                  q = 1, alpha = 0.05, reduce = TRUE))

# Whether unmeasured confounding equivalent to strong predictors of outcomes *jointly* would impact results (slightly more impact than above, but still quite weak as most have very little relationship with the exposure)
summary(sensemakr(model = dep_cca_adj, treatment = "climateConcernSomewhat concerned", 
                  benchmark_covariates = list("joint" = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale")),
                  q = 1, alpha = 0.05, reduce = TRUE))

# Contour plot of strength of unmeasured confounding necessary to result in zero effect (with some strong predictors of the outcome as benchmarks)
plot(sensemakr(model = dep_cca_adj, treatment = "climateConcernSomewhat concerned", 
               benchmark_covariates = c("smfq", "sexMale"),
               q = 1, alpha = 0.05, reduce = TRUE))

# Contour plot of strength of unmeasured confounding necessary to result in zero effect (with some strong *joint* predictors of the outcome as benchmarks)
plot(sensemakr(model = dep_cca_adj, treatment = "climateConcernSomewhat concerned", 
               benchmark_covariates = list("joint" = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale")),
               q = 1, alpha = 0.05, reduce = TRUE))


## 'Very concerned' exposure level

# Overall amount of unmeasured confounding necessary to result in zero effect: Partial R2 of 6.9% with both exposure and outcome (results already cross 95% CI, so no comparison for this)
summary(sensemakr(model = dep_cca_adj, treatment = "climateConcernVery concerned", 
                  q = 1, alpha = 0.05, reduce = TRUE))

# Whether unmeasured confounding equivalent to strong predictors of outcomes would impact results (they would not, as most have very little relationship with the exposure)
summary(sensemakr(model = dep_cca_adj, treatment = "climateConcernVery concerned", 
                  benchmark_covariates = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale"),
                  q = 1, alpha = 0.05, reduce = TRUE))

# Whether unmeasured confounding equivalent to strong predictors of outcomes *jointly* would impact results (slightly more impact than above, but still quite weak as most have very little relationship with the exposure)
summary(sensemakr(model = dep_cca_adj, treatment = "climateConcernVery concerned", 
                  benchmark_covariates = list("joint" = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale")),
                  q = 1, alpha = 0.05, reduce = TRUE))

# Contour plot of strength of unmeasured confounding necessary to result in zero effect (with some strong predictors of the outcome as benchmarks)
plot(sensemakr(model = dep_cca_adj, treatment = "climateConcernVery concerned", 
               benchmark_covariates = c("smfq", "sexMale"),
               q = 1, alpha = 0.05, reduce = TRUE))

# Contour plot of strength of unmeasured confounding necessary to result in zero effect (with some strong *joint* predictors of the outcome as benchmarks)
plot(sensemakr(model = dep_cca_adj, treatment = "climateConcernVery concerned", 
                  benchmark_covariates = list("joint" = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale")),
                  q = 1, alpha = 0.05, reduce = TRUE))


##### Anxiety outcome

# Descriptives by climate concern
by(dat$gad7, dat$climateConcern, summary)
by(dat$gad7, dat$climateConcern, sd, na.rm = TRUE)
by(!is.na(dat$gad7), dat$climateConcern, sum, na.rm = TRUE)
sum(!is.na(dat$gad7) & !is.na(dat$climateConcern))

# Make CCA marker, so unadjusted and adjusted models have the same sample size - NOTE: Only 423 complete cases
dat_gad <- dat %>%
  select(-c(actionBelief, totalActions, epds, wemwbs))

dat_gad$cca <- complete.cases(dat_gad)
table(dat_gad$cca)

# Descriptives in complete-case sample
by(dat_gad$gad7[dat_gad$cca == TRUE], dat_gad$climateConcern[dat_gad$cca == TRUE], summary)


#### Linear regression models

### Unadjusted
anx_cca_unadj <- lm(gad7 ~ climateConcern, data = dat_gad, subset = cca == TRUE)
summary(anx_cca_unadj)
round(confint(anx_cca_unadj), 2)

### Adjusted
anx_cca_adj <- lm(gad7 ~ climateConcern + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                  + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                  + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                  + mat_imd + par_occSocClass + par_income + mat_home, 
                  data = dat_gad)
summary(anx_cca_adj)
round(confint(anx_cca_adj), 2)


## Predicted values
avg_predictions(anx_cca_unadj, variables = c("climateConcern"))
avg_predictions(anx_cca_adj, variables = c("climateConcern"))

## Marginal contrasts and whether 'concerned' and 'very concerned' levels differ
avg_comparisons(anx_cca_unadj, variables = list(climateConcern = "pairwise"))
avg_comparisons(anx_cca_adj, variables = list(climateConcern = "pairwise"))


### Sensitivity analysis for unmeasured confounding

## 'Somewhat concerned' exposure level

# Overall amount of unmeasured confounding necessary to result in zero effect: Partial R2 of 3.8% with both exposure and outcome (results already cross 95% CI, so no comparison for this)
summary(sensemakr(model = anx_cca_adj, treatment = "climateConcernSomewhat concerned", 
                  q = 1, alpha = 0.05, reduce = TRUE))

# Whether unmeasured confounding equivalent to strong predictors of outcomes would impact results (they would not, as most have very little relationship with the exposure)
summary(sensemakr(model = anx_cca_adj, treatment = "climateConcernSomewhat concerned", 
                  benchmark_covariates = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale"),
                  q = 1, alpha = 0.05, reduce = TRUE))

# Whether unmeasured confounding equivalent to strong predictors of outcomes *jointly* would impact results (slightly more impact than above, but still quite weak as most have very little relationship with the exposure)
summary(sensemakr(model = anx_cca_adj, treatment = "climateConcernSomewhat concerned", 
                  benchmark_covariates = list("joint" = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale")),
                  q = 1, alpha = 0.05, reduce = TRUE))

# Contour plot of strength of unmeasured confounding necessary to result in zero effect (with some strong predictors of the outcome as benchmarks)
plot(sensemakr(model = anx_cca_adj, treatment = "climateConcernSomewhat concerned", 
               benchmark_covariates = c("gad7_prior", "sexMale"),
               q = 1, alpha = 0.05, reduce = TRUE))

# Contour plot of strength of unmeasured confounding necessary to result in zero effect (with some strong *joint* predictors of the outcome as benchmarks)
plot(sensemakr(model = anx_cca_adj, treatment = "climateConcernSomewhat concerned", 
               benchmark_covariates = list("joint" = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale")),
               q = 1, alpha = 0.05, reduce = TRUE))


## 'Very concerned' exposure level

# Overall amount of unmeasured confounding necessary to result in zero effect: Partial R2 of 7.4% with both exposure and outcome (results already cross 95% CI, so no comparison for this)
summary(sensemakr(model = anx_cca_adj, treatment = "climateConcernVery concerned", 
                  q = 1, alpha = 0.05, reduce = TRUE))

# Whether unmeasured confounding equivalent to strong predictors of outcomes would impact results (they would not, as most have very little relationship with the exposure)
summary(sensemakr(model = anx_cca_adj, treatment = "climateConcernVery concerned", 
                  benchmark_covariates = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale"),
                  q = 1, alpha = 0.05, reduce = TRUE))

# Whether unmeasured confounding equivalent to strong predictors of outcomes *jointly* would impact results (slightly more impact than above, but still quite weak as most have very little relationship with the exposure)
summary(sensemakr(model = anx_cca_adj, treatment = "climateConcernVery concerned", 
                  benchmark_covariates = list("joint" = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale")),
                  q = 1, alpha = 0.05, reduce = TRUE))

# Contour plot of strength of unmeasured confounding necessary to result in zero effect (with some strong predictors of the outcome as benchmarks)
plot(sensemakr(model = anx_cca_adj, treatment = "climateConcernVery concerned", 
               benchmark_covariates = c("gad7_prior", "sexMale"),
               q = 1, alpha = 0.05, reduce = TRUE))

# Contour plot of strength of unmeasured confounding necessary to result in zero effect (with some strong *joint* predictors of the outcome as benchmarks)
plot(sensemakr(model = anx_cca_adj, treatment = "climateConcernVery concerned", 
               benchmark_covariates = list("joint" = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale")),
               q = 1, alpha = 0.05, reduce = TRUE))


##### Well-being outcome

# Descriptives by climate concern
by(dat$wemwbs, dat$climateConcern, summary)
by(dat$wemwbs, dat$climateConcern, sd, na.rm = TRUE)
by(!is.na(dat$wemwbs), dat$climateConcern, sum, na.rm = TRUE)
sum(!is.na(dat$wemwbs) & !is.na(dat$climateConcern))

# Make CCA marker, so unadjusted and adjusted models have the same sample size - NOTE: Only 419 complete cases
dat_wb <- dat %>%
  select(-c(actionBelief, totalActions, epds, gad7))

dat_wb$cca <- complete.cases(dat_wb)
table(dat_wb$cca)

# Descriptives in complete-case sample
by(dat_wb$wemwbs[dat_wb$cca == TRUE], dat_wb$climateConcern[dat_wb$cca == TRUE], summary)


#### Linear regression models

### Unadjusted
wb_cca_unadj <- lm(wemwbs ~ climateConcern, data = dat_wb, subset = cca == TRUE)
summary(wb_cca_unadj)
round(confint(wb_cca_unadj), 3)

### Adjusted
wb_cca_adj <- lm(wemwbs ~ climateConcern + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                  + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                  + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                  + mat_imd + par_occSocClass + par_income + mat_home, 
                  data = dat_wb)
summary(wb_cca_adj)
round(confint(wb_cca_adj), 3)


## Predicted values
avg_predictions(wb_cca_unadj, variables = c("climateConcern"))
avg_predictions(wb_cca_adj, variables = c("climateConcern"))

## Marginal contrasts and whether 'concerned' and 'very concerned' levels differ
avg_comparisons(wb_cca_unadj, variables = list(climateConcern = "pairwise"))
avg_comparisons(wb_cca_adj, variables = list(climateConcern = "pairwise"))


### Sensitivity analysis for unmeasured confounding

## 'Somewhat concerned' exposure level

# Overall amount of unmeasured confounding necessary to result in zero effect: Partial R2 of 2.3% with both exposure and outcome (results already cross 95% CI, so no comparison for this)
summary(sensemakr(model = wb_cca_adj, treatment = "climateConcernSomewhat concerned", 
                  q = 1, alpha = 0.05, reduce = TRUE))

# Whether unmeasured confounding equivalent to strong predictors of outcomes would impact results (they would not, as most have very little relationship with the exposure)
summary(sensemakr(model = wb_cca_adj, treatment = "climateConcernSomewhat concerned", 
                  benchmark_covariates = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale"),
                  q = 1, alpha = 0.05, reduce = TRUE))

# Whether unmeasured confounding equivalent to strong predictors of outcomes *jointly* would impact results (slightly more impact than above, but still quite weak as most have very little relationship with the exposure)
summary(sensemakr(model = wb_cca_adj, treatment = "climateConcernSomewhat concerned", 
                  benchmark_covariates = list("joint" = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale")),
                  q = 1, alpha = 0.05, reduce = TRUE))

# Contour plot of strength of unmeasured confounding necessary to result in zero effect (with some strong predictors of the outcome as benchmarks)
plot(sensemakr(model = wb_cca_adj, treatment = "climateConcernSomewhat concerned", 
               benchmark_covariates = c("wemwbs_prior", "sexMale"),
               q = 1, alpha = 0.05, reduce = TRUE))

# Contour plot of strength of unmeasured confounding necessary to result in zero effect (with some strong *joint* predictors of the outcome as benchmarks)
plot(sensemakr(model = wb_cca_adj, treatment = "climateConcernSomewhat concerned", 
               benchmark_covariates = list("joint" = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale")),
               q = 1, alpha = 0.05, reduce = TRUE))


## 'Very concerned' exposure level

# Overall amount of unmeasured confounding necessary to result in zero effect: Partial R2 of 1.3% with both exposure and outcome (results already cross 95% CI, so no comparison for this)
summary(sensemakr(model = wb_cca_adj, treatment = "climateConcernVery concerned", 
                  q = 1, alpha = 0.05, reduce = TRUE))

# Whether unmeasured confounding equivalent to strong predictors of outcomes would impact results (they would not, as most have very little relationship with the exposure)
summary(sensemakr(model = wb_cca_adj, treatment = "climateConcernVery concerned", 
                  benchmark_covariates = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale"),
                  q = 1, alpha = 0.05, reduce = TRUE))

# Whether unmeasured confounding equivalent to strong predictors of outcomes *jointly* would impact results (slightly more impact than above, but still quite weak as most have very little relationship with the exposure)
summary(sensemakr(model = wb_cca_adj, treatment = "climateConcernVery concerned", 
                  benchmark_covariates = list("joint" = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale")),
                  q = 1, alpha = 0.05, reduce = TRUE))

# Contour plot of strength of unmeasured confounding necessary to result in zero effect (with some strong predictors of the outcome as benchmarks)
plot(sensemakr(model = wb_cca_adj, treatment = "climateConcernVery concerned", 
               benchmark_covariates = c("wemwbs_prior", "sexMale"),
               q = 1, alpha = 0.05, reduce = TRUE))

# Contour plot of strength of unmeasured confounding necessary to result in zero effect (with some strong *joint* predictors of the outcome as benchmarks)
plot(sensemakr(model = wb_cca_adj, treatment = "climateConcernVery concerned", 
               benchmark_covariates = list("joint" = c("smfq", "gad7_prior", "wemwbs_prior", "sexMale")),
               q = 1, alpha = 0.05, reduce = TRUE))




####################################################################################################
####### Multiple imputation analysis results

## Remove moderators from dataset for MI (as no need to include in imputation models here)
dat_mi <- dat %>%
  select(-c(actionBelief, totalActions))

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

## Second, set-up the prediction matrix, which says which variables to use to impute other variables (here, we want to use all variables when imputing all others)
pred <- make.predictorMatrix(dat_mi)
pred

## Run a test imputation to make sure it looks okay, and check the amount of missing data in each variable
test <- mice(dat_mi, m = 5, maxit = 0, 
             method = meth, predictorMatrix = pred, print = TRUE)
test
table(test$nmis)

## Now run a test imputation model with just 10 chains but for 20 iterations/burn-ins, just to make sure that the chains converge correctly (all seem to converge by 10 chains)
imp_test <- mice(dat_mi, m = 10, maxit = 20, 
                 method = meth, predictorMatrix = pred, print = TRUE, seed = 32123)

pdf("./Results/imputation_convergencePlots.pdf", width = 12, height = 8)
plot(imp_test)
dev.off()


## Now run the proper imputation model. Will create 50 imputed datasets here with a burn-in period of 10 iterations
imp_mi <- mice(dat_mi, m = 50, maxit = 10, 
               method = meth, predictorMatrix = pred, print = TRUE, seed = 98765)

## Save these imputations, to avoid having to run the imputations again
save(imp_mi, file = "MI_main.RData")
#load("MI_main.RData")


#### Now run substantive analysis model in each imputed dataset and combine using Rubin's rules

### Depression outcome

## Unadjusted
model <- with(imp_mi, lm(epds ~ climateConcern))
est <- pool(model)
(dep_mi_unadj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(dep_cca_unadj), confint(dep_cca_unadj), p = coef(summary(dep_cca_unadj))[, 4])

# Predicted values from imputed datasets
(dep_mi_pred <- avg_predictions(model, variables = c("climateConcern")))

# Marginal contrasts and whether 'concerned' and 'very concerned' levels differ
(dep_mi_marg <- avg_comparisons(model, variables = list(climateConcern = "pairwise")))


## Adjusted
model <- with(imp_mi, lm(epds ~ climateConcern + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                         + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                         + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                         + mat_imd + par_occSocClass + par_income + mat_home))
est <- pool(model)
(dep_mi_adj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(dep_cca_adj), confint(dep_cca_adj), p = coef(summary(dep_cca_adj))[, 4])


# Predicted values from imputed datasets
(dep_mi_pred <- avg_predictions(model, variables = c("climateConcern")))

# Marginal contrasts and whether 'concerned' and 'very concerned' levels differ
(dep_mi_marg <- avg_comparisons(model, variables = list(climateConcern = "pairwise")))


### Anxiety outcome

## Unadjusted
model <- with(imp_mi, lm(gad7 ~ climateConcern))
est <- pool(model)
(anx_mi_unadj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(anx_cca_unadj), confint(anx_cca_unadj), p = coef(summary(anx_cca_unadj))[, 4])

# Predicted values from imputed datasets
(dep_mi_pred <- avg_predictions(model, variables = c("climateConcern")))

# Marginal contrasts and whether 'concerned' and 'very concerned' levels differ
(dep_mi_marg <- avg_comparisons(model, variables = list(climateConcern = "pairwise")))


## Adjusted
model <- with(imp_mi, lm(gad7 ~ climateConcern + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                         + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                         + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                         + mat_imd + par_occSocClass + par_income + mat_home))
est <- pool(model)
(anx_mi_adj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(anx_cca_adj), confint(anx_cca_adj), p = coef(summary(anx_cca_adj))[, 4])


# Predicted values from imputed datasets
(anx_mi_pred <- avg_predictions(model, variables = c("climateConcern")))

# Marginal contrasts and whether 'concerned' and 'very concerned' levels differ
(anx_mi_marg <- avg_comparisons(model, variables = list(climateConcern = "pairwise")))



### Well-being outcome

## Unadjusted
model <- with(imp_mi, lm(wemwbs ~ climateConcern))
est <- pool(model)
(wb_mi_unadj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(wb_cca_unadj), confint(wb_cca_unadj), p = coef(summary(wb_cca_unadj))[, 4])

# Predicted values from imputed datasets
(dep_mi_pred <- avg_predictions(model, variables = c("climateConcern")))

# Marginal contrasts and whether 'concerned' and 'very concerned' levels differ
(dep_mi_marg <- avg_comparisons(model, variables = list(climateConcern = "pairwise")))


## Adjusted
model <- with(imp_mi, lm(wemwbs ~ climateConcern + smfq + prior_dep + gad7_prior + prior_anx + wemwbs_prior
                         + sex + ethnicity + relationship + children + edu + occClass + income + imd + home 
                         + emoStab + openness + mat_dep + mat_anx + pat_dep + pat_anx + mat_age + mat_edu 
                         + mat_imd + par_occSocClass + par_income + mat_home))
est <- pool(model)
(wb_mi_adj <- summary(est, conf.int = TRUE))

# Compare to CCA results
cbind(est = coef(wb_cca_adj), confint(wb_cca_adj), p = coef(summary(wb_cca_adj))[, 4])


# Predicted values from imputed datasets
(wb_mi_pred <- avg_predictions(model, variables = c("climateConcern")))

# Marginal contrasts and whether 'concerned' and 'very concerned' levels differ
(wb_mi_marg <- avg_comparisons(model, variables = list(climateConcern = "pairwise")))

