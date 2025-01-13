### Script for paper 'Does concern regarding climate change impact subsequent mental health? A longitudinal analysis using data from the Avon 2Longitudinal Study of Parents and Children (ALSPAC)' - ALSPAC B-number B4572
### Script 2: Generating synthetic datasets
### Created 17/11/2024 by Dan Major-Smith
### R version 4.3.1

## A pre-registered analysis plan for this paper is available on the OSF: https://rr.peercommunityin.org/articles/rec?id=793


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4572 - Climate Concern")

#install.packages("tidyverse")
library("tidyverse")

#install.packages("synthpop")
library(synthpop)

#install.packages("haven")
library(haven)


##########################################################################################
#### Read in the processed data and generate synthetic datasets using 'synthpop' (https://www.synthpop.org.uk/get-started.html)

load("data_climateConcern_B4572.RData")

# Check data
head(dat)
summary(dat)
glimpse(dat)


# Get information about variables in the dataset
codebook.syn(dat)$tab

# Create a synthetic dataset using default options (which are non-parametric/CART [classification and regression trees])
dat_syn <- syn(dat, seed = 41)

# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, 0 observations have been dropped (0.0% of data)
replicated.uniques(dat_syn, dat)
dat_syn <- sdc(dat_syn, dat, rm.replicated.uniques = TRUE)


## Perform a manual check on a handful of cases to check this

# Create a dataset of unique observed individuals
dat_unique <- dat[!(duplicated(dat) | duplicated(dat, fromLast = TRUE)), ]

# Create a dataset of unique synthetic individuals
syn_unique <- dat_syn$syn[!(duplicated(dat_syn$syn) | duplicated(dat_syn$syn, 
                                                                           fromLast = TRUE)), ] 

# Select 10 rows at random from the unique observed dataset
row_unique <- dat_unique[sample(nrow(dat_unique), 10), ] 

# Check there are no duplicated observations (this should equal ‘0’)
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

rm(dat_unique)
rm(syn_unique)


### Explore this synthetic dataset
dat_syn
summary(dat_syn)

# Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is fairly good). Save this as a PDF
compare(dat_syn, dat, stat = "counts", nrow = 6, ncol = 6)

pdf("./Results_SynthPop/ComparingDescStats.pdf", height = 15, width = 15)
compare(dat_syn, dat, stat = "counts", nrow = 6, ncol = 6)
dev.off()


## Univariable analysis with EPDS depression score as outcome and climate concern as exposure to check that get broadly similar results in both datasets (i.e., that the structures of the dataset are preserved)
model.syn <- lm.synds(epds ~ climateConcern, data = dat_syn)
summary(model.syn)

# Get broadly comparable pattern of results (and store as PDF)
compare(model.syn, dat)

pdf("./Results_SynthPop/ComparingUnadjustedModel.pdf", height = 8, width = 12)
compare(model.syn, dat)
dev.off()


## Next compare results of multivariable analyses (wont use all confounders here, else plot will be unreadable - So will just use a few sociodemographic variables)
model.syn2 <- lm.synds(epds ~ climateConcern + sex + ethnicity + relationship + children + edu +
                         occClass + income + imd + home, data = dat_syn)
summary(model.syn2)

# Again, check get comparable pattern of results, this time for all of the additional coefficients in the model as well (again, store as PDF)
compare(model.syn2, dat)

pdf("./Results_SynthPop/ComparingAdjustedModel.pdf", height = 12, width = 16)
compare(model.syn2, dat)
dev.off()


### Adding in a variable called 'FALSE_DATA', with the value 'FALSE_DATA' for all observations, as an additional safety check to users know the dataset is synthetic
dat_syn$syn <- cbind(FALSE_DATA = rep("FALSE_DATA", nrow(dat_syn$syn)), dat_syn$syn)
summary(dat_syn)

# Extract the synthetic dataset (rather than it being stored within a list)
dat_syn_df <- dat_syn$syn
head(dat_syn_df)
glimpse(dat_syn_df)
summary(dat_syn_df)


### Store the synthetic dataset for others to use
save(dat_syn_df, file = "./AnalysisCode_ClimateConcern_B4572/SyntheticData/syntheticData_climateConcern_B4572.RData")
write_csv(dat_syn_df, file = "./AnalysisCode_ClimateConcern_B4572/SyntheticData/syntheticData_climateConcern_B4572.csv")
write_dta(dat_syn_df, "./AnalysisCode_ClimateConcern_B4572/SyntheticData/syntheticData_climateConcern_B4572.dta")
