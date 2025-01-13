### Script for paper 'Does concern regarding climate change impact subsequent mental health? A longitudinal analysis using data from the Avon 2Longitudinal Study of Parents and Children (ALSPAC)' - ALSPAC B-number B4572
### Script 1: Data preparation/cleaning
### Created 17/11/2024 by Dan Major-Smith
### R version 4.3.1

## A pre-registered analysis plan for this paper is available on the OSF: https://rr.peercommunityin.org/articles/rec?id=793


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4572 - Climate Concern")

#install.packages("tidyverse")
library("tidyverse")

#install.packages("haven")
library(haven)

#install.packages("psych")
library(psych)


###########################################################################################
#### Read in the raw data

data_raw <- read_csv("B4572_ClimateConcern_RawData.csv")

## Quick check of data
head(data_raw)
glimpse(data_raw)

# Remove the 'problems' and 'spec' attributes, to make output easier to read
attr(data_raw, "problems") <- NULL
attr(data_raw, "spec") <- NULL
str(data_raw)


### Make copy of dataset to work from
dat <- data_raw


## Removing some observations to get to full eligible sample

# Consent withdrawn
table(dat$a006, useNA ="ifany")
table(dat$pb261, useNA ="ifany")
table(dat$YPJ3000, useNA ="ifany")

dat <- dat %>%
  filter((a006 != ".a" | is.na(a006)) & (YPJ3000 != ".b" | is.na(YPJ3000)))


# Not alive at 1 year of age
table(dat$kz011b, useNA ="ifany")

dat <- dat %>%
  filter(kz011b == "Yes")


### Keep just the variables of interest and re-order
dat <- dat %>%
  relocate(aln, qlet, YPJ3001, YPJ3003, YPJ3020, YPJ3023, YPJ3026, YPJ3029, YPJ3032, YPJ3035, YPJ3038, 
           YPJ3041, YPJ3044, YPJ3047, YPJ3050, YPJ3053, YPJ3056, YPJ3059, YPJ3062, YPJ3065, YPJ3071,
           YPM5000:YPM5009, YPM5100:YPM5106, YPL2000:YPL2013,
           YPE4080:YPE4095, FKDQ1000, YPA2160:YPA2220, FKDQ1030, YPC0490:YPC0503,
           kz021, YPH2012, YPG1052, YPH3010, YPF7970, YPC2492, YPE6020, jan2021imd2015q5_YP, YPG1060, fg7363, fg7364,
           mz028b, a006, c645a, c755, c765, c804, jan1993imd2010q5_M, h470, b371, b372, b352a, b352b,
           pb261, pb262, pb234, pb235) %>%
  select(aln:pb235)

colnames(dat)


#### Cleaning the variables

### Exposure variables - Climate concern and related moderators (assessed at age 30)

## Degree to which is concerned about the impact of climate change - ordered categorical variable
table(dat$YPJ3001, useNA = "ifany")

# As only answered if believe climate is changing (previous question), will code those who said 'not changing' as 'not concerned' (as cannot be concerned if not believe is changing)
dat <- dat %>%
  mutate(YPJ3001 = recode(YPJ3001, "Definitely does not believe climate is changing" = "Not at all concerned"))

table(dat$YPJ3001, useNA = "ifany")

# Also code 'not at all' and 'not very' concerned together, as low sample sizes in each
dat <- dat %>%
  mutate(YPJ3001 = na_if(YPJ3001, "Did not complete questionnaire")) %>%
  mutate(YPJ3001 = na_if(YPJ3001, "NS/NK")) %>%
  mutate(YPJ3001 = recode(YPJ3001, "Not at all concerned" = "Not concerned", "Not very concerned" = "Not concerned")) %>%
  mutate(YPJ3001 = factor(YPJ3001, levels = c("Not concerned", "Somewhat concerned", "Very concerned"))) %>%
  rename(climateConcern = YPJ3001)

table(dat$climateConcern, useNA = "ifany")
round(prop.table(table(dat$climateConcern)) * 100, 1)
sum(table(dat$climateConcern))


## Personal actions will make difference to long-term climate changes (only answered if believe climate is changing) - Combine 'no' and 'sure sure' to make binary variable
table(dat$YPJ3003, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3003 = na_if(YPJ3003, "Did not complete questionnaire")) %>%
  mutate(YPJ3003 = na_if(YPJ3003, "NS/NK")) %>%
  mutate(YPJ3003 = na_if(YPJ3003, "Definitely does not believe climate is changing")) %>%
  mutate(YPJ3003 = recode(YPJ3003, "Not sure" = "No")) %>%
  mutate(YPJ3003 = factor(YPJ3003, levels = c("No", "Yes"))) %>%
  rename(actionBelief = YPJ3003)

table(dat$actionBelief, useNA = "ifany")
round(prop.table(table(dat$actionBelief)) * 100, 1)
sum(table(dat$actionBelief))


## Go through each action performed for climate reasons, coding as either yes (1) or not (0)

# Changed the way travelled locally
table(dat$YPJ3020, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3020 = ifelse(YPJ3020 == "Yes" | YPJ3020 == "No", YPJ3020, NA)) %>%
  mutate(travelLocal = ifelse(is.na(YPJ3020), NA,
                              ifelse(YPJ3020 == "Yes", 1, 0)))

table(dat$travelLocal, useNA = "ifany")

# Reduced household waste
table(dat$YPJ3023, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3023 = ifelse(YPJ3023 == "Yes" | YPJ3023 == "No", YPJ3023, NA)) %>%
  mutate(waste = ifelse(is.na(YPJ3023), NA,
                              ifelse(YPJ3023 == "Yes", 1, 0)))

table(dat$waste, useNA = "ifany")

# Reduced energy use at home
table(dat$YPJ3026, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3026 = ifelse(YPJ3026 == "Yes" | YPJ3026 == "No", YPJ3026, NA)) %>%
  mutate(energy = ifelse(is.na(YPJ3026), NA,
                        ifelse(YPJ3026 == "Yes", 1, 0)))

table(dat$energy, useNA = "ifany")

# Changed what buy
table(dat$YPJ3029, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3029 = ifelse(YPJ3029 == "Yes" | YPJ3029 == "No", YPJ3029, NA)) %>%
  mutate(buy = ifelse(is.na(YPJ3029), NA,
                         ifelse(YPJ3029 == "Yes", 1, 0)))

table(dat$buy, useNA = "ifany")

# Reduced air travel
table(dat$YPJ3032, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3032 = ifelse(YPJ3032 == "Yes" | YPJ3032 == "No", YPJ3032, NA)) %>%
  mutate(airTravel = ifelse(is.na(YPJ3032), NA,
                      ifelse(YPJ3032 == "Yes", 1, 0)))

table(dat$airTravel, useNA = "ifany")

# Electric/hybrid vehicle
table(dat$YPJ3035, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3035 = ifelse(YPJ3035 == "Yes" | YPJ3035 == "No", YPJ3035, NA)) %>%
  mutate(elecCar = ifelse(is.na(YPJ3035), NA,
                            ifelse(YPJ3035 == "Yes", 1, 0)))

table(dat$elecCar, useNA = "ifany")

# Bought local food
table(dat$YPJ3038, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3038 = ifelse(YPJ3038 == "Yes" | YPJ3038 == "No", YPJ3038, NA)) %>%
  mutate(localFood = ifelse(is.na(YPJ3038), NA,
                          ifelse(YPJ3038 == "Yes", 1, 0)))

table(dat$localFood, useNA = "ifany")

# Recycled more
table(dat$YPJ3041, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3041 = ifelse(YPJ3041 == "Yes" | YPJ3041 == "No", YPJ3041, NA)) %>%
  mutate(recycle = ifelse(is.na(YPJ3041), NA,
                            ifelse(YPJ3041 == "Yes", 1, 0)))

table(dat$recycle, useNA = "ifany")  

# Reduced plastic use
table(dat$YPJ3044, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3044 = ifelse(YPJ3044 == "Yes" | YPJ3044 == "No", YPJ3044, NA)) %>%
  mutate(plastic = ifelse(is.na(YPJ3044), NA,
                          ifelse(YPJ3044 == "Yes", 1, 0)))

table(dat$plastic, useNA = "ifany") 

# Chosen sustainable items
table(dat$YPJ3047, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3047 = ifelse(YPJ3047 == "Yes" | YPJ3047 == "No", YPJ3047, NA)) %>%
  mutate(sustain = ifelse(is.na(YPJ3047), NA,
                          ifelse(YPJ3047 == "Yes", 1, 0)))

table(dat$sustain, useNA = "ifany")

# Improved home insulation
table(dat$YPJ3050, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3050 = ifelse(YPJ3050 == "Yes" | YPJ3050 == "No", YPJ3050, NA)) %>%
  mutate(insulation = ifelse(is.na(YPJ3050), NA,
                          ifelse(YPJ3050 == "Yes", 1, 0)))

table(dat$insulation, useNA = "ifany")

# Installed solar panels
table(dat$YPJ3053, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3053 = ifelse(YPJ3053 == "Yes" | YPJ3053 == "No", YPJ3053, NA)) %>%
  mutate(solar = ifelse(is.na(YPJ3053), NA,
                             ifelse(YPJ3053 == "Yes", 1, 0)))

table(dat$solar, useNA = "ifany")

# Growing vegetables
table(dat$YPJ3056, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3056 = ifelse(YPJ3056 == "Yes" | YPJ3056 == "No", YPJ3056, NA)) %>%
  mutate(veg = ifelse(is.na(YPJ3056), NA,
                        ifelse(YPJ3056 == "Yes", 1, 0)))

table(dat$veg, useNA = "ifany")

# Planted trees
table(dat$YPJ3059, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3059 = ifelse(YPJ3059 == "Yes" | YPJ3059 == "No", YPJ3059, NA)) %>%
  mutate(trees = ifelse(is.na(YPJ3059), NA,
                      ifelse(YPJ3059 == "Yes", 1, 0)))

table(dat$trees, useNA = "ifany")

# Avoided fossil fuel organisations
table(dat$YPJ3062, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3062 = ifelse(YPJ3062 == "Yes" | YPJ3062 == "No", YPJ3062, NA)) %>%
  mutate(fossilOrgs = ifelse(is.na(YPJ3062), NA,
                        ifelse(YPJ3062 == "Yes", 1, 0)))

table(dat$fossilOrgs, useNA = "ifany")

# Not had children?had fewer children
table(dat$YPJ3065, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3065 = ifelse(YPJ3065 == "Yes" | YPJ3065 == "No", YPJ3065, NA)) %>%
  mutate(children = ifelse(is.na(YPJ3065), NA,
                             ifelse(YPJ3065 == "Yes", 1, 0)))

table(dat$children, useNA = "ifany")

# Reduced meat/dairy
table(dat$YPJ3071, useNA = "ifany")

dat <- dat %>%
  mutate(YPJ3071 = ifelse(YPJ3071 == "Yes" | YPJ3071 == "No", YPJ3071, NA)) %>%
  mutate(meatDairy = ifelse(is.na(YPJ3071), NA,
                           ifelse(YPJ3071 == "Yes", 1, 0)))

table(dat$meatDairy, useNA = "ifany")


## Make a 'number of actions' variable
dat <- dat %>%
  rowwise() %>%
  mutate(totalActions = sum(c_across(travelLocal:meatDairy))) %>%
  ungroup()

summary(dat$totalActions)
table(dat$totalActions)

ggplot(dat, aes(x = totalActions)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()


# Drop all the individual action variables
dat <- dat %>%
  select(-c(YPJ3020:YPJ3071, travelLocal:meatDairy))


### Mental health and well-being outcome data

## Depression - Via Edinburgh Postnatal Depression Scale (EPDS) - Assessed at age 32 - Go through each question and code as numeric

# 1) Able to laugh/see funny side of things
table(dat$YPM5000, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5000 = na_if(YPM5000, "Did not complete questionnaire")) %>%
  mutate(YPM5000 = na_if(YPM5000, "NS/NK")) %>%
  mutate(YPM5000 = recode(YPM5000, "As much as I always could" = "0", "Not quite so much now" = "1",
                          "Definitely not so much now" = "2", "Not at all" = "3")) %>%
  mutate(YPM5000 = as.numeric(YPM5000)) %>%
  rename(epds1_laugh = YPM5000)

table(dat$epds1_laugh, useNA = "ifany")

# 2) Looked forward with enjoyment to things
table(dat$YPM5001, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5001 = na_if(YPM5001, "Did not complete questionnaire")) %>%
  mutate(YPM5001 = na_if(YPM5001, "NS/NK")) %>%
  mutate(YPM5001 = recode(YPM5001, "As much as I ever did" = "0", "Rather less than I used to" = "1",
                          "Definitely less than I used to" = "2", "Hardly at all" = "3")) %>%
  mutate(YPM5001 = as.numeric(YPM5001)) %>%
  rename(epds2_lookForward = YPM5001)

table(dat$epds2_lookForward, useNA = "ifany")

# 3) Blamed self unnecessarily when things went wrong
table(dat$YPM5002, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5002 = na_if(YPM5002, "Did not complete questionnaire")) %>%
  mutate(YPM5002 = na_if(YPM5002, "NS/NK")) %>%
  mutate(YPM5002 = recode(YPM5002, "No never" = "0", "Not very often" = "1",
                          "Yes, some of the time" = "2", "Yes, most of the time" = "3")) %>%
  mutate(YPM5002 = as.numeric(YPM5002)) %>%
  rename(epds3_blameSelf = YPM5002)

table(dat$epds3_blameSelf, useNA = "ifany")

# 4) Anxious or worried for no good reason
table(dat$YPM5003, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5003 = na_if(YPM5003, "Did not complete questionnaire")) %>%
  mutate(YPM5003 = na_if(YPM5003, "NS/NK")) %>%
  mutate(YPM5003 = recode(YPM5003, "No, not at all" = "0", "Hardly ever" = "1",
                          "Yes, sometimes" = "2", "Yes, often" = "3")) %>%
  mutate(YPM5003 = as.numeric(YPM5003)) %>%
  rename(epds4_anxious = YPM5003)

table(dat$epds4_anxious, useNA = "ifany")

# 5) Scared or panicky for no good reason
table(dat$YPM5004, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5004 = na_if(YPM5004, "Did not complete questionnaire")) %>%
  mutate(YPM5004 = na_if(YPM5004, "NS/NK")) %>%
  mutate(YPM5004 = recode(YPM5004, "No, not at all" = "0", "No, not much" = "1",
                          "Yes, sometimes" = "2", "Yes, quite a lot" = "3")) %>%
  mutate(YPM5004 = as.numeric(YPM5004)) %>%
  rename(epds5_scared = YPM5004)

table(dat$epds5_scared, useNA = "ifany")

# 6) Things have been getting on top of me
table(dat$YPM5005, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5005 = na_if(YPM5005, "Did not complete questionnaire")) %>%
  mutate(YPM5005 = na_if(YPM5005, "NS/NK")) %>%
  mutate(YPM5005 = recode(YPM5005, "No, not at all" = "0", "No, hardly ever" = "1",
                          "Yes, sometimes" = "2", "Yes, most of the time" = "3")) %>%
  mutate(YPM5005 = as.numeric(YPM5005)) %>%
  rename(epds6_overwhelmed = YPM5005)

table(dat$epds6_overwhelmed, useNA = "ifany")

# 7) Been so unhappy had difficulty sleeping
table(dat$YPM5006, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5006 = na_if(YPM5006, "Did not complete questionnaire")) %>%
  mutate(YPM5006 = na_if(YPM5006, "NS/NK")) %>%
  mutate(YPM5006 = recode(YPM5006, "No, not at all" = "0", "Not very often" = "1",
                          "Yes, sometimes" = "2", "Yes, most of the time" = "3")) %>%
  mutate(YPM5006 = as.numeric(YPM5006)) %>%
  rename(epds7_sleep = YPM5006)

table(dat$epds7_sleep, useNA = "ifany")

# 8) Felt sad or miserable
table(dat$YPM5007, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5007 = na_if(YPM5007, "Did not complete questionnaire")) %>%
  mutate(YPM5007 = na_if(YPM5007, "NS/NK")) %>%
  mutate(YPM5007 = recode(YPM5007, "No, not at all" = "0", "Not very often" = "1",
                          "Yes, quite often" = "2", "Yes, most of the time" = "3")) %>%
  mutate(YPM5007 = as.numeric(YPM5007)) %>%
  rename(epds8_sad = YPM5007)

table(dat$epds8_sad, useNA = "ifany")

# 9) So unhappy have been crying
table(dat$YPM5008, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5008 = na_if(YPM5008, "Did not complete questionnaire")) %>%
  mutate(YPM5008 = na_if(YPM5008, "NS/NK")) %>%
  mutate(YPM5008 = recode(YPM5008, "No, never" = "0", "Only occasionally" = "1",
                          "Yes, quite often" = "2", "Yes, most of the time" = "3")) %>%
  mutate(YPM5008 = as.numeric(YPM5008)) %>%
  rename(epds9_crying = YPM5008)

table(dat$epds9_crying, useNA = "ifany")

# 10) Thought of harming self
table(dat$YPM5009, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5009 = na_if(YPM5009, "Did not complete questionnaire")) %>%
  mutate(YPM5009 = na_if(YPM5009, "NS/NK")) %>%
  mutate(YPM5009 = recode(YPM5009, "Never" = "0", "Hardly ever" = "1",
                          "Sometimes" = "2", "Yes, quite often" = "3")) %>%
  mutate(YPM5009 = as.numeric(YPM5009)) %>%
  rename(epds10_selfharm = YPM5009)

table(dat$epds10_selfharm, useNA = "ifany")

# Calculate Cronbach's alpha for EPDS internal reliability = 0.90
epds <- dat %>%
  select(c(epds1_laugh:epds10_selfharm)) %>%
  filter(complete.cases(.))
alpha(epds)

# Calculate total EPDS score
dat <- dat %>%
  rowwise() %>%
  mutate(epds = sum(c_across(epds1_laugh:epds10_selfharm))) %>%
  ungroup()

summary(dat$epds)
table(dat$epds)

ggplot(dat, aes(x = epds)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

# Drop all the individual EPDS variables
dat <- dat %>%
  select(-c(epds1_laugh:epds10_selfharm))


## Anxiety - Via Generalised Anxiety Disorder-7 scale (GAD) - Assessed at age 32 - Go through each question and code as numeric

# 1) Feeling nervous, anxious or on edge
table(dat$YPM5100, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5100 = na_if(YPM5100, "Did not complete questionnaire")) %>%
  mutate(YPM5100 = na_if(YPM5100, "NS/NK")) %>%
  mutate(YPM5100 = recode(YPM5100, "Not at all" = "0", "Less than half the days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPM5100 = as.numeric(YPM5100)) %>%
  rename(gad1_anxious = YPM5100)

table(dat$gad1_anxious, useNA = "ifany")

# 2) Not able to stop or control worrying
table(dat$YPM5101, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5101 = na_if(YPM5101, "Did not complete questionnaire")) %>%
  mutate(YPM5101 = na_if(YPM5101, "NS/NK")) %>%
  mutate(YPM5101 = recode(YPM5101, "Not at all" = "0", "Less than half the days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPM5101 = as.numeric(YPM5101)) %>%
  rename(gad2_controlWorry = YPM5101)

table(dat$gad2_controlWorry, useNA = "ifany")

# 3) Worrying about different things
table(dat$YPM5102, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5102 = na_if(YPM5102, "Did not complete questionnaire")) %>%
  mutate(YPM5102 = na_if(YPM5102, "NS/NK")) %>%
  mutate(YPM5102 = recode(YPM5102, "Not at all" = "0", "Less than half the days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPM5102 = as.numeric(YPM5102)) %>%
  rename(gad3_diffWorry = YPM5102)

table(dat$gad3_diffWorry, useNA = "ifany")

# 4) Trouble relaxing
table(dat$YPM5103, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5103 = na_if(YPM5103, "Did not complete questionnaire")) %>%
  mutate(YPM5103 = na_if(YPM5103, "NS/NK")) %>%
  mutate(YPM5103 = recode(YPM5103, "Not at all" = "0", "Less than half the days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPM5103 = as.numeric(YPM5103)) %>%
  rename(gad4_relax = YPM5103)

table(dat$gad4_relax, useNA = "ifany")

# 5) Restless and hard to sit still
table(dat$YPM5104, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5104 = na_if(YPM5104, "Did not complete questionnaire")) %>%
  mutate(YPM5104 = na_if(YPM5104, "NS/NK")) %>%
  mutate(YPM5104 = recode(YPM5104, "Not at all" = "0", "Less than half the days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPM5104 = as.numeric(YPM5104)) %>%
  rename(gad5_restless = YPM5104)

table(dat$gad5_restless, useNA = "ifany")

# 6) Easily annoyed or irritable
table(dat$YPM5105, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5105 = na_if(YPM5105, "Did not complete questionnaire")) %>%
  mutate(YPM5105 = na_if(YPM5105, "NS/NK")) %>%
  mutate(YPM5105 = recode(YPM5105, "Not at all" = "0", "Less than half the days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPM5105 = as.numeric(YPM5105)) %>%
  rename(gad6_irritable = YPM5105)

table(dat$gad6_irritable, useNA = "ifany")

# 7) Afraid, as though something awful might happen
table(dat$YPM5106, useNA = "ifany")

dat <- dat %>%
  mutate(YPM5106 = na_if(YPM5106, "Did not complete questionnaire")) %>%
  mutate(YPM5106 = na_if(YPM5106, "NS/NK")) %>%
  mutate(YPM5106 = recode(YPM5106, "Not at all" = "0", "Less than half the days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPM5106 = as.numeric(YPM5106)) %>%
  rename(gad7_afraid = YPM5106)

table(dat$gad7_afraid, useNA = "ifany")

# Calculate Cronbach's alpha for GAD7 internal reliability = 0.93
gad <- dat %>%
  select(c(gad1_anxious:gad7_afraid)) %>%
  filter(complete.cases(.))
alpha(gad)

# Calculate total GAD7 score
dat <- dat %>%
  rowwise() %>%
  mutate(gad7 = sum(c_across(gad1_anxious:gad7_afraid))) %>%
  ungroup()

summary(dat$gad7)
table(dat$gad7)

ggplot(dat, aes(x = gad7)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

# Drop all the individual GAD7 variables
dat <- dat %>%
  select(-c(gad1_anxious:gad7_afraid))


## Well-being - Via Warwick-Edinburgh Mental Well-Being Scale (WEMWBS) - Assessed at age 31 - Go through each question and code as numeric

# 1) Optimistic about future
table(dat$YPL2000, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2000 = na_if(YPL2000, "Did not complete questionnaire")) %>%
  mutate(YPL2000 = na_if(YPL2000, "Missed whole section B")) %>%
  mutate(YPL2000 = na_if(YPL2000, "NS/NK")) %>%
  mutate(YPL2000 = recode(YPL2000, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2000 = as.numeric(YPL2000)) %>%
  rename(wb1_optimistic = YPL2000)

table(dat$wb1_optimistic, useNA = "ifany")

# 2) Feeling useful
table(dat$YPL2001, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2001 = na_if(YPL2001, "Did not complete questionnaire")) %>%
  mutate(YPL2001 = na_if(YPL2001, "Missed whole section B")) %>%
  mutate(YPL2001 = na_if(YPL2001, "NS/NK")) %>%
  mutate(YPL2001 = recode(YPL2001, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2001 = as.numeric(YPL2001)) %>%
  rename(wb2_useful = YPL2001)

table(dat$wb2_useful, useNA = "ifany")

# 3) Feeling relaxed
table(dat$YPL2002, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2002 = na_if(YPL2002, "Did not complete questionnaire")) %>%
  mutate(YPL2002 = na_if(YPL2002, "Missed whole section B")) %>%
  mutate(YPL2002 = na_if(YPL2002, "NS/NK")) %>%
  mutate(YPL2002 = recode(YPL2002, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2002 = as.numeric(YPL2002)) %>%
  rename(wb3_relaxed = YPL2002)

table(dat$wb3_relaxed, useNA = "ifany")

# 4) Interested in others
table(dat$YPL2003, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2003 = na_if(YPL2003, "Did not complete questionnaire")) %>%
  mutate(YPL2003 = na_if(YPL2003, "Missed whole section B")) %>%
  mutate(YPL2003 = na_if(YPL2003, "NS/NK")) %>%
  mutate(YPL2003 = recode(YPL2003, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2003 = as.numeric(YPL2003)) %>%
  rename(wb4_interestOthers = YPL2003)

table(dat$wb4_interestOthers, useNA = "ifany")

# 5) Energy to spare
table(dat$YPL2004, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2004 = na_if(YPL2004, "Did not complete questionnaire")) %>%
  mutate(YPL2004 = na_if(YPL2004, "Missed whole section B")) %>%
  mutate(YPL2004 = na_if(YPL2004, "NS/NK")) %>%
  mutate(YPL2004 = recode(YPL2004, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2004 = as.numeric(YPL2004)) %>%
  rename(wb5_energy = YPL2004)

table(dat$wb5_energy, useNA = "ifany")

# 6) Dealing with problems well
table(dat$YPL2005, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2005 = na_if(YPL2005, "Did not complete questionnaire")) %>%
  mutate(YPL2005 = na_if(YPL2005, "Missed whole section B")) %>%
  mutate(YPL2005 = na_if(YPL2005, "NS/NK")) %>%
  mutate(YPL2005 = recode(YPL2005, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2005 = as.numeric(YPL2005)) %>%
  rename(wb6_problems = YPL2005)

table(dat$wb6_problems, useNA = "ifany")

# 7) Thinking clearly
table(dat$YPL2006, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2006 = na_if(YPL2006, "Did not complete questionnaire")) %>%
  mutate(YPL2006 = na_if(YPL2006, "Missed whole section B")) %>%
  mutate(YPL2006 = na_if(YPL2006, "NS/NK")) %>%
  mutate(YPL2006 = recode(YPL2006, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2006 = as.numeric(YPL2006)) %>%
  rename(wb7_thinking = YPL2006)

table(dat$wb7_thinking, useNA = "ifany")

# 8) Feeling good about self
table(dat$YPL2007, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2007 = na_if(YPL2007, "Did not complete questionnaire")) %>%
  mutate(YPL2007 = na_if(YPL2007, "Missed whole section B")) %>%
  mutate(YPL2007 = na_if(YPL2007, "NS/NK")) %>%
  mutate(YPL2007 = recode(YPL2007, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2007 = as.numeric(YPL2007)) %>%
  rename(wb8_goodSelf = YPL2007)

table(dat$wb8_goodSelf, useNA = "ifany")

# 9) Feeling close to others
table(dat$YPL2008, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2008 = na_if(YPL2008, "Did not complete questionnaire")) %>%
  mutate(YPL2008 = na_if(YPL2008, "Missed whole section B")) %>%
  mutate(YPL2008 = na_if(YPL2008, "NS/NK")) %>%
  mutate(YPL2008 = recode(YPL2008, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2008 = as.numeric(YPL2008)) %>%
  rename(wb9_closeOthers = YPL2008)

table(dat$wb9_closeOthers, useNA = "ifany")

# 10) Feeling confident
table(dat$YPL2009, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2009 = na_if(YPL2009, "Did not complete questionnaire")) %>%
  mutate(YPL2009 = na_if(YPL2009, "Missed whole section B")) %>%
  mutate(YPL2009 = na_if(YPL2009, "NS/NK")) %>%
  mutate(YPL2009 = recode(YPL2009, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2009 = as.numeric(YPL2009)) %>%
  rename(wb10_confident = YPL2009)

table(dat$wb10_confident, useNA = "ifany")

# 11) Able to make up mind about things
table(dat$YPL2010, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2010 = na_if(YPL2010, "Did not complete questionnaire")) %>%
  mutate(YPL2010 = na_if(YPL2010, "Missed whole section B")) %>%
  mutate(YPL2010 = na_if(YPL2010, "NS/NK")) %>%
  mutate(YPL2010 = recode(YPL2010, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2010 = as.numeric(YPL2010)) %>%
  rename(wb11_makeUpMind = YPL2010)

table(dat$wb11_makeUpMind, useNA = "ifany")

# 12) Feeling loved
table(dat$YPL2011, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2011 = na_if(YPL2011, "Did not complete questionnaire")) %>%
  mutate(YPL2011 = na_if(YPL2011, "Missed whole section B")) %>%
  mutate(YPL2011 = na_if(YPL2011, "NS/NK")) %>%
  mutate(YPL2011 = recode(YPL2011, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2011 = as.numeric(YPL2011)) %>%
  rename(wb12_loved = YPL2011)

table(dat$wb12_loved, useNA = "ifany")

# 13) Interested in new things
table(dat$YPL2012, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2012 = na_if(YPL2012, "Did not complete questionnaire")) %>%
  mutate(YPL2012 = na_if(YPL2012, "Missed whole section B")) %>%
  mutate(YPL2012 = na_if(YPL2012, "NS/NK")) %>%
  mutate(YPL2012 = recode(YPL2012, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2012 = as.numeric(YPL2012)) %>%
  rename(wb13_newInterests = YPL2012)

table(dat$wb13_newInterests, useNA = "ifany")

# 14) Feeling cheerful
table(dat$YPL2013, useNA = "ifany")

dat <- dat %>%
  mutate(YPL2013 = na_if(YPL2013, "Did not complete questionnaire")) %>%
  mutate(YPL2013 = na_if(YPL2013, "Missed whole section B")) %>%
  mutate(YPL2013 = na_if(YPL2013, "NS/NK")) %>%
  mutate(YPL2013 = recode(YPL2013, "None of the time" = "1", "Rarely" = "2", "Sometimes" = "3", 
                          "Often" = "4", "All the time" = "5")) %>%
  mutate(YPL2013 = as.numeric(YPL2013)) %>%
  rename(wb14_cheerful = YPL2013)

table(dat$wb14_cheerful, useNA = "ifany")

# Calculate Cronbach's alpha for WEMWBS internal reliability = 0.93
wemwbs <- dat %>%
  select(c(wb1_optimistic:wb14_cheerful)) %>%
  filter(complete.cases(.))
alpha(wemwbs)

# Calculate total WEMWBS score
dat <- dat %>%
  rowwise() %>%
  mutate(wemwbs = sum(c_across(wb1_optimistic:wb14_cheerful))) %>%
  ungroup()

summary(dat$wemwbs)
table(dat$wemwbs)

ggplot(dat, aes(x = wemwbs)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

# Drop all the individual WEMWBS variables
dat <- dat %>%
  select(-c(wb1_optimistic:wb14_cheerful))


### Confounders

## Prior depressive symptoms - Short Moods and Feelings Questionnaire (SMFQ) - Age 25 - Go through each question and code as numeric

# 1) Felt miserable or unhappy
table(dat$YPE4080, useNA = "ifany")

dat <- dat %>%
  mutate(YPE4080 = na_if(YPE4080, "Questionnaire not completed")) %>%
  mutate(YPE4080 = na_if(YPE4080, "Missed whole section F")) %>%
  mutate(YPE4080 = na_if(YPE4080, "Missing")) %>%
  mutate(YPE4080 = recode(YPE4080, "Not true" = "0", "Sometimes" = "1", "True" = "2")) %>%
  mutate(YPE4080 = as.numeric(YPE4080)) %>%
  rename(smfq1_miserable = YPE4080)

table(dat$smfq1_miserable, useNA = "ifany")

# 2) Not enjoy anything
table(dat$YPE4082, useNA = "ifany")

dat <- dat %>%
  mutate(YPE4082 = na_if(YPE4082, "Questionnaire not completed")) %>%
  mutate(YPE4082 = na_if(YPE4082, "Missed whole section F")) %>%
  mutate(YPE4082 = na_if(YPE4082, "Missing")) %>%
  mutate(YPE4082 = recode(YPE4082, "Not true" = "0", "Sometimes" = "1", "True" = "2")) %>%
  mutate(YPE4082 = as.numeric(YPE4082)) %>%
  rename(smfq2_notEnjoy = YPE4082)

table(dat$smfq2_notEnjoy, useNA = "ifany")

# 3) So tired did nothing
table(dat$YPE4083, useNA = "ifany")

dat <- dat %>%
  mutate(YPE4083 = na_if(YPE4083, "Questionnaire not completed")) %>%
  mutate(YPE4083 = na_if(YPE4083, "Missed whole section F")) %>%
  mutate(YPE4083 = na_if(YPE4083, "Missing")) %>%
  mutate(YPE4083 = recode(YPE4083, "Not true" = "0", "Sometimes" = "1", "True" = "2")) %>%
  mutate(YPE4083 = as.numeric(YPE4083)) %>%
  rename(smfq3_didNothing = YPE4083)

table(dat$smfq3_didNothing, useNA = "ifany")

# 4) Very restless
table(dat$YPE4084, useNA = "ifany")

dat <- dat %>%
  mutate(YPE4084 = na_if(YPE4084, "Questionnaire not completed")) %>%
  mutate(YPE4084 = na_if(YPE4084, "Missed whole section F")) %>%
  mutate(YPE4084 = na_if(YPE4084, "Missing")) %>%
  mutate(YPE4084 = recode(YPE4084, "Not true" = "0", "Sometimes" = "1", "True" = "2")) %>%
  mutate(YPE4084 = as.numeric(YPE4084)) %>%
  rename(smfq4_restless = YPE4084)

table(dat$smfq4_restless, useNA = "ifany")

# 5) Felt no good anymore
table(dat$YPE4085, useNA = "ifany")

dat <- dat %>%
  mutate(YPE4085 = na_if(YPE4085, "Questionnaire not completed")) %>%
  mutate(YPE4085 = na_if(YPE4085, "Missed whole section F")) %>%
  mutate(YPE4085 = na_if(YPE4085, "Missing")) %>%
  mutate(YPE4085 = recode(YPE4085, "Not true" = "0", "Sometimes" = "1", "True" = "2")) %>%
  mutate(YPE4085 = as.numeric(YPE4085)) %>%
  rename(smfq5_noGood = YPE4085)

table(dat$smfq5_noGood, useNA = "ifany")

# 6) Cried a lot
table(dat$YPE4086, useNA = "ifany")

dat <- dat %>%
  mutate(YPE4086 = na_if(YPE4086, "Questionnaire not completed")) %>%
  mutate(YPE4086 = na_if(YPE4086, "Missed whole section F")) %>%
  mutate(YPE4086 = na_if(YPE4086, "Missing")) %>%
  mutate(YPE4086 = recode(YPE4086, "Not true" = "0", "Sometimes" = "1", "True" = "2")) %>%
  mutate(YPE4086 = as.numeric(YPE4086)) %>%
  rename(smfq6_cried = YPE4086)

table(dat$smfq6_cried, useNA = "ifany")

# 7) Hard to think properly
table(dat$YPE4088, useNA = "ifany")

dat <- dat %>%
  mutate(YPE4088 = na_if(YPE4088, "Questionnaire not completed")) %>%
  mutate(YPE4088 = na_if(YPE4088, "Missed whole section F")) %>%
  mutate(YPE4088 = na_if(YPE4088, "Missing")) %>%
  mutate(YPE4088 = recode(YPE4088, "Not true" = "0", "Sometimes" = "1", "True" = "2")) %>%
  mutate(YPE4088 = as.numeric(YPE4088)) %>%
  rename(smfq7_concentrate = YPE4088)

table(dat$smfq7_concentrate, useNA = "ifany")

# 8) Hated self
table(dat$YPE4089, useNA = "ifany")

dat <- dat %>%
  mutate(YPE4089 = na_if(YPE4089, "Questionnaire not completed")) %>%
  mutate(YPE4089 = na_if(YPE4089, "Missed whole section F")) %>%
  mutate(YPE4089 = na_if(YPE4089, "Missing")) %>%
  mutate(YPE4089 = recode(YPE4089, "Not true" = "0", "Sometimes" = "1", "True" = "2")) %>%
  mutate(YPE4089 = as.numeric(YPE4089)) %>%
  rename(smfq8_hateSelf = YPE4089)

table(dat$smfq8_hateSelf, useNA = "ifany")

# 9) Was a bad person
table(dat$YPE4091, useNA = "ifany")

dat <- dat %>%
  mutate(YPE4091 = na_if(YPE4091, "Questionnaire not completed")) %>%
  mutate(YPE4091 = na_if(YPE4091, "Missed whole section F")) %>%
  mutate(YPE4091 = na_if(YPE4091, "Missing")) %>%
  mutate(YPE4091 = recode(YPE4091, "Not true" = "0", "Sometimes" = "1", "True" = "2")) %>%
  mutate(YPE4091 = as.numeric(YPE4091)) %>%
  rename(smfq9_badPerson = YPE4091)

table(dat$smfq9_badPerson, useNA = "ifany")

# 10) Felt lonely
table(dat$YPE4092, useNA = "ifany")

dat <- dat %>%
  mutate(YPE4092 = na_if(YPE4092, "Questionnaire not completed")) %>%
  mutate(YPE4092 = na_if(YPE4092, "Missed whole section F")) %>%
  mutate(YPE4092 = na_if(YPE4092, "Missing")) %>%
  mutate(YPE4092 = recode(YPE4092, "Not true" = "0", "Sometimes" = "1", "True" = "2")) %>%
  mutate(YPE4092 = as.numeric(YPE4092)) %>%
  rename(smfq10_lonely = YPE4092)

table(dat$smfq10_lonely, useNA = "ifany")

# 11) Thought nobody really loved me
table(dat$YPE4093, useNA = "ifany")

dat <- dat %>%
  mutate(YPE4093 = na_if(YPE4093, "Questionnaire not completed")) %>%
  mutate(YPE4093 = na_if(YPE4093, "Missed whole section F")) %>%
  mutate(YPE4093 = na_if(YPE4093, "Missing")) %>%
  mutate(YPE4093 = recode(YPE4093, "Not true" = "0", "Sometimes" = "1", "True" = "2")) %>%
  mutate(YPE4093 = as.numeric(YPE4093)) %>%
  rename(smfq11_noLove = YPE4093)

table(dat$smfq11_noLove, useNA = "ifany")

# 12) Thought never be as good as others
table(dat$YPE4094, useNA = "ifany")

dat <- dat %>%
  mutate(YPE4094 = na_if(YPE4094, "Questionnaire not completed")) %>%
  mutate(YPE4094 = na_if(YPE4094, "Missed whole section F")) %>%
  mutate(YPE4094 = na_if(YPE4094, "Missing")) %>%
  mutate(YPE4094 = recode(YPE4094, "Not true" = "0", "Sometimes" = "1", "True" = "2")) %>%
  mutate(YPE4094 = as.numeric(YPE4094)) %>%
  rename(smfq12_worseOthers = YPE4094)

table(dat$smfq12_worseOthers, useNA = "ifany")

# 13) Did everything wrong
table(dat$YPE4095, useNA = "ifany")

dat <- dat %>%
  mutate(YPE4095 = na_if(YPE4095, "Questionnaire not completed")) %>%
  mutate(YPE4095 = na_if(YPE4095, "Missed whole section F")) %>%
  mutate(YPE4095 = na_if(YPE4095, "Missing")) %>%
  mutate(YPE4095 = recode(YPE4095, "Not true" = "0", "Sometimes" = "1", "True" = "2")) %>%
  mutate(YPE4095 = as.numeric(YPE4095)) %>%
  rename(smfq13_wrong = YPE4095)

table(dat$smfq13_wrong, useNA = "ifany")

# Calculate Cronbach's alpha for SMFQ internal reliability = 0.92
smfq <- dat %>%
  select(c(smfq1_miserable:smfq13_wrong)) %>%
  filter(complete.cases(.))
alpha(smfq)

# Calculate total SMFQ score
dat <- dat %>%
  rowwise() %>%
  mutate(smfq = sum(c_across(smfq1_miserable:smfq13_wrong))) %>%
  ungroup()

summary(dat$smfq)
table(dat$smfq)

ggplot(dat, aes(x = smfq)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

# Drop all the individual SMFQ variables
dat <- dat %>%
  select(-c(smfq1_miserable:smfq13_wrong))


## ICD-10 depression diagnosis (from CIS-R) - Age 24
table(dat$FKDQ1000, useNA = "ifany")

dat <- dat %>%
  mutate(FKDQ1000 = ifelse(FKDQ1000 == "Yes" | FKDQ1000 == "No", FKDQ1000, NA)) %>%
  mutate(FKDQ1000 = factor(FKDQ1000, levels = c("No", "Yes"))) %>%
  rename(prior_dep = FKDQ1000)

table(dat$prior_dep, useNA = "ifany")
round(prop.table(table(dat$prior_dep)) * 100, 1)
sum(table(dat$prior_dep))


## Prior anxiety - Via Generalised Anxiety Disorder-7 scale (GAD) - Assessed at age 21 - Go through each question and code as numeric

# 1) Feeling nervous, anxious or on edge
table(dat$YPA2160, useNA = "ifany")

dat <- dat %>%
  mutate(YPA2160 = na_if(YPA2160, "Questionnaire not Returned")) %>%
  mutate(YPA2160 = na_if(YPA2160, "NS/NA")) %>%
  mutate(YPA2160 = recode(YPA2160, "Not at all" = "0", "Several days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPA2160 = as.numeric(YPA2160)) %>%
  rename(gad1_anxious_prior = YPA2160)

table(dat$gad1_anxious_prior, useNA = "ifany")

# 2) Not able to stop or control worrying
table(dat$YPA2170, useNA = "ifany")

dat <- dat %>%
  mutate(YPA2170 = na_if(YPA2170, "Questionnaire not Returned")) %>%
  mutate(YPA2170 = na_if(YPA2170, "NS/NA")) %>%
  mutate(YPA2170 = recode(YPA2170, "Not at all" = "0", "Several days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPA2170 = as.numeric(YPA2170)) %>%
  rename(gad2_controlWorry_prior = YPA2170)

table(dat$gad2_controlWorry_prior, useNA = "ifany")

# 3) Worrying about different things
table(dat$YPA2180, useNA = "ifany")

dat <- dat %>%
  mutate(YPA2180 = na_if(YPA2180, "Questionnaire not Returned")) %>%
  mutate(YPA2180 = na_if(YPA2180, "NS/NA")) %>%
  mutate(YPA2180 = recode(YPA2180, "Not at all" = "0", "Several days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPA2180 = as.numeric(YPA2180)) %>%
  rename(gad3_diffWorry_prior = YPA2180)

table(dat$gad3_diffWorry_prior, useNA = "ifany")

# 4) Trouble relaxing
table(dat$YPA2190, useNA = "ifany")

dat <- dat %>%
  mutate(YPA2190 = na_if(YPA2190, "Questionnaire not Returned")) %>%
  mutate(YPA2190 = na_if(YPA2190, "NS/NA")) %>%
  mutate(YPA2190 = recode(YPA2190, "Not at all" = "0", "Several days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPA2190 = as.numeric(YPA2190)) %>%
  rename(gad4_relax_prior = YPA2190)

table(dat$gad4_relax_prior, useNA = "ifany")

# 5) Restless and hard to sit still
table(dat$YPA2200, useNA = "ifany")

dat <- dat %>%
  mutate(YPA2200 = na_if(YPA2200, "Questionnaire not Returned")) %>%
  mutate(YPA2200 = na_if(YPA2200, "NS/NA")) %>%
  mutate(YPA2200 = recode(YPA2200, "Not at all" = "0", "Several days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPA2200 = as.numeric(YPA2200)) %>%
  rename(gad5_restless_prior = YPA2200)

table(dat$gad5_restless_prior, useNA = "ifany")

# 6) Easily annoyed or irritable
table(dat$YPA2210, useNA = "ifany")

dat <- dat %>%
  mutate(YPA2210 = na_if(YPA2210, "Questionnaire not Returned")) %>%
  mutate(YPA2210 = na_if(YPA2210, "NS/NA")) %>%
  mutate(YPA2210 = recode(YPA2210, "Not at all" = "0", "Several days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPA2210 = as.numeric(YPA2210)) %>%
  rename(gad6_irritable_prior = YPA2210)

table(dat$gad6_irritable_prior, useNA = "ifany")

# 7) Afraid, as though something awful might happen
table(dat$YPA2220, useNA = "ifany")

dat <- dat %>%
  mutate(YPA2220 = na_if(YPA2220, "Questionnaire not Returned")) %>%
  mutate(YPA2220 = na_if(YPA2220, "NS/NA")) %>%
  mutate(YPA2220 = recode(YPA2220, "Not at all" = "0", "Several days" = "1",
                          "More than half the days" = "2", "Nearly every day" = "3")) %>%
  mutate(YPA2220 = as.numeric(YPA2220)) %>%
  rename(gad7_afraid_prior = YPA2220)

table(dat$gad7_afraid_prior, useNA = "ifany")

# Calculate Cronbach's alpha for GAD7 internal reliability = 0.91
gad <- dat %>%
  select(c(gad1_anxious_prior:gad7_afraid_prior)) %>%
  filter(complete.cases(.))
alpha(gad)

# Calculate total GAD7 score
dat <- dat %>%
  rowwise() %>%
  mutate(gad7_prior = sum(c_across(gad1_anxious_prior:gad7_afraid_prior))) %>%
  ungroup()

summary(dat$gad7_prior)
table(dat$gad7_prior)

ggplot(dat, aes(x = gad7_prior)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

# Drop all the individual GAD7 variables
dat <- dat %>%
  select(-c(gad1_anxious_prior:gad7_afraid_prior))


## ICD-10 generalised anxiety disorder diagnosis (from CIS-R) - Age 24
table(dat$FKDQ1030, useNA = "ifany")

dat <- dat %>%
  mutate(FKDQ1030 = ifelse(FKDQ1030 == "Yes" | FKDQ1030 == "No", FKDQ1030, NA)) %>%
  mutate(FKDQ1030 = factor(FKDQ1030, levels = c("No", "Yes"))) %>%
  rename(prior_anx = FKDQ1030)

table(dat$prior_anx, useNA = "ifany")
round(prop.table(table(dat$prior_anx)) * 100, 1)
sum(table(dat$prior_anx))


## Prior well-being - Via Warwick-Edinburgh Mental Well-Being Scale (WEMWBS) - Assessed at age 23 - Go through each question and code as numeric

# 1) Optimistic about future
table(dat$YPC0490, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0490 = na_if(YPC0490, "Questionnaire not completed")) %>%
  mutate(YPC0490 = na_if(YPC0490, "NS/NK")) %>%
  mutate(YPC0490 = recode(YPC0490, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0490 = as.numeric(YPC0490)) %>%
  rename(wb1_optimistic_prior = YPC0490)

table(dat$wb1_optimistic_prior, useNA = "ifany")

# 2) Feeling useful
table(dat$YPC0491, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0491 = na_if(YPC0491, "Questionnaire not completed")) %>%
  mutate(YPC0491 = na_if(YPC0491, "NS/NK")) %>%
  mutate(YPC0491 = recode(YPC0491, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0491 = as.numeric(YPC0491)) %>%
  rename(wb2_useful_prior = YPC0491)

table(dat$wb2_useful_prior, useNA = "ifany")

# 3) Feeling relaxed
table(dat$YPC0492, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0492 = na_if(YPC0492, "Questionnaire not completed")) %>%
  mutate(YPC0492 = na_if(YPC0492, "NS/NK")) %>%
  mutate(YPC0492 = recode(YPC0492, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0492 = as.numeric(YPC0492)) %>%
  rename(wb3_relaxed_prior = YPC0492)

table(dat$wb3_relaxed_prior, useNA = "ifany")

# 4) Interested in others
table(dat$YPC0493, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0493 = na_if(YPC0493, "Questionnaire not completed")) %>%
  mutate(YPC0493 = na_if(YPC0493, "NS/NK")) %>%
  mutate(YPC0493 = recode(YPC0493, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0493 = as.numeric(YPC0493)) %>%
  rename(wb4_interestOthers_prior = YPC0493)

table(dat$wb4_interestOthers_prior, useNA = "ifany")

# 5) Energy to spare
table(dat$YPC0494, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0494 = na_if(YPC0494, "Questionnaire not completed")) %>%
  mutate(YPC0494 = na_if(YPC0494, "NS/NK")) %>%
  mutate(YPC0494 = recode(YPC0494, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0494 = as.numeric(YPC0494)) %>%
  rename(wb5_energy_prior = YPC0494)

table(dat$wb5_energy_prior, useNA = "ifany")

# 6) Dealing with problems well
table(dat$YPC0495, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0495 = na_if(YPC0495, "Questionnaire not completed")) %>%
  mutate(YPC0495 = na_if(YPC0495, "NS/NK")) %>%
  mutate(YPC0495 = recode(YPC0495, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0495 = as.numeric(YPC0495)) %>%
  rename(wb6_problems_prior = YPC0495)

table(dat$wb6_problems_prior, useNA = "ifany")

# 7) Thinking clearly
table(dat$YPC0496, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0496 = na_if(YPC0496, "Questionnaire not completed")) %>%
  mutate(YPC0496 = na_if(YPC0496, "NS/NK")) %>%
  mutate(YPC0496 = recode(YPC0496, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0496 = as.numeric(YPC0496)) %>%
  rename(wb7_thinking_prior = YPC0496)

table(dat$wb7_thinking_prior, useNA = "ifany")

# 8) Feeling good about self
table(dat$YPC0497, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0497 = na_if(YPC0497, "Questionnaire not completed")) %>%
  mutate(YPC0497 = na_if(YPC0497, "NS/NK")) %>%
  mutate(YPC0497 = recode(YPC0497, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0497 = as.numeric(YPC0497)) %>%
  rename(wb8_goodSelf_prior = YPC0497)

table(dat$wb8_goodSelf_prior, useNA = "ifany")

# 9) Feeling close to others
table(dat$YPC0498, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0498 = na_if(YPC0498, "Questionnaire not completed")) %>%
  mutate(YPC0498 = na_if(YPC0498, "NS/NK")) %>%
  mutate(YPC0498 = recode(YPC0498, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0498 = as.numeric(YPC0498)) %>%
  rename(wb9_closeOthers_prior = YPC0498)

table(dat$wb9_closeOthers_prior, useNA = "ifany")

# 10) Feeling confident
table(dat$YPC0499, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0499 = na_if(YPC0499, "Questionnaire not completed")) %>%
  mutate(YPC0499 = na_if(YPC0499, "NS/NK")) %>%
  mutate(YPC0499 = recode(YPC0499, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0499 = as.numeric(YPC0499)) %>%
  rename(wb10_confident_prior = YPC0499)

table(dat$wb10_confident_prior, useNA = "ifany")

# 11) Able to make up mind about things
table(dat$YPC0500, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0500 = na_if(YPC0500, "Questionnaire not completed")) %>%
  mutate(YPC0500 = na_if(YPC0500, "NS/NK")) %>%
  mutate(YPC0500 = recode(YPC0500, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0500 = as.numeric(YPC0500)) %>%
  rename(wb11_makeUpMind_prior = YPC0500)

table(dat$wb11_makeUpMind_prior, useNA = "ifany")

# 12) Feeling loved
table(dat$YPC0501, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0501 = na_if(YPC0501, "Questionnaire not completed")) %>%
  mutate(YPC0501 = na_if(YPC0501, "NS/NK")) %>%
  mutate(YPC0501 = recode(YPC0501, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0501 = as.numeric(YPC0501)) %>%
  rename(wb12_loved_prior = YPC0501)

table(dat$wb12_loved_prior, useNA = "ifany")

# 13) Interested in new things
table(dat$YPC0502, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0502 = na_if(YPC0502, "Questionnaire not completed")) %>%
  mutate(YPC0502 = na_if(YPC0502, "NS/NK")) %>%
  mutate(YPC0502 = recode(YPC0502, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0502 = as.numeric(YPC0502)) %>%
  rename(wb13_newInterests_prior = YPC0502)

table(dat$wb13_newInterests_prior, useNA = "ifany")

# 14) Feeling cheerful
table(dat$YPC0503, useNA = "ifany")

dat <- dat %>%
  mutate(YPC0503 = na_if(YPC0503, "Questionnaire not completed")) %>%
  mutate(YPC0503 = na_if(YPC0503, "NS/NK")) %>%
  mutate(YPC0503 = recode(YPC0503, "None of the time" = "1", "Rarely" = "2", "Some of the time" = "3", 
                          "Often" = "4", "All of the time" = "5")) %>%
  mutate(YPC0503 = as.numeric(YPC0503)) %>%
  rename(wb14_cheerful_prior = YPC0503)

table(dat$wb14_cheerful_prior, useNA = "ifany")

# Calculate Cronbach's alpha for WEMWBS internal reliability = 0.93
wemwbs <- dat %>%
  select(c(wb1_optimistic_prior:wb14_cheerful_prior)) %>%
  filter(complete.cases(.))
alpha(wemwbs)

# Calculate total WEMWBS score
dat <- dat %>%
  rowwise() %>%
  mutate(wemwbs_prior = sum(c_across(wb1_optimistic_prior:wb14_cheerful_prior))) %>%
  ungroup()

summary(dat$wemwbs_prior)
table(dat$wemwbs_prior)

ggplot(dat, aes(x = wemwbs_prior)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

# Drop all the individual WEMWBS variables
dat <- dat %>%
  select(-c(wb1_optimistic_prior:wb14_cheerful_prior))


## Sex assigned at birth
table(dat$kz021, useNA = "ifany")

dat <- dat %>%
  mutate(kz021 = factor(kz021, levels = c("Female", "Male"))) %>%
  rename(sex = kz021)

table(dat$sex, useNA = "ifany")
round(prop.table(table(dat$sex)) * 100, 1)
sum(table(dat$sex))


## Ethnicity - Use data from parents in pregnancy, plus fill in gaps with more recent data
table(dat$c804, useNA = "ifany")

dat <- dat %>%
  mutate(c804 = na_if(c804, "Missing")) %>%
  mutate(c804 = recode(c804, "White" = "0", "Non-white" = "1")) %>%
  mutate(c804 = as.numeric(c804)) %>%
  rename(ethnicity = c804)

table(dat$ethnicity, useNA = "ifany")

# Fill in missing data, if possible
table(dat$YPH2012, useNA = "ifany")

dat <- dat %>%
  mutate(YPH2012 = na_if(YPH2012, "Missed whole section B")) %>%
  mutate(YPH2012 = na_if(YPH2012, "Questionnaire not completed")) %>%
  mutate(YPH2012 = recode(YPH2012, "White" = "0", "Other than White" = "1")) %>%
  mutate(YPH2012 = as.numeric(YPH2012)) %>%
  rename(ethnicity_new = YPH2012)

table(dat$ethnicity_new, useNA = "ifany")

table(dat$ethnicity, dat$ethnicity_new, useNA = "ifany")

dat <- dat %>%
  mutate(ethnicity = ifelse(is.na(ethnicity) & !is.na(ethnicity_new), ethnicity_new, ethnicity)) %>%
  mutate(ethnicity = factor(ethnicity)) %>%
  mutate(ethnicity = recode(ethnicity, "0" = "White", "1" = "Other than White")) %>%
  mutate(ethnicity = factor(ethnicity, levels = c("White", "Other than White"))) %>%
  select(-ethnicity_new)

table(dat$ethnicity, useNA = "ifany")
round(prop.table(table(dat$ethnicity)) * 100, 1)
sum(table(dat$ethnicity))


## Relationship status (living with partner) - Age 28
table(dat$YPG1052, useNA = "ifany")

dat <- dat %>%
  mutate(YPG1052 = na_if(YPG1052, "Missed whole section A")) %>%
  mutate(YPG1052 = na_if(YPG1052, "Not selected any A6 option")) %>%
  mutate(YPG1052 = na_if(YPG1052, "Questionnaire not completed")) %>%
  mutate(YPG1052 = factor(YPG1052, levels = c("No", "Yes"))) %>%
  rename(relationship = YPG1052)

table(dat$relationship, useNA = "ifany")
round(prop.table(table(dat$relationship)) * 100, 1)
sum(table(dat$relationship))


## Has children - Age 29
table(dat$YPH3010, useNA = "ifany")

dat <- dat %>%
  mutate(YPH3010 = ifelse(YPH3010 == "Yes" | YPH3010 == "No", YPH3010, NA)) %>%
  mutate(YPH3010 = factor(YPH3010, levels = c("No", "Yes"))) %>%
  rename(children = YPH3010)

table(dat$children, useNA = "ifany")
round(prop.table(table(dat$children)) * 100, 1)
sum(table(dat$children))


## Highest education qualification (GCSE/equivalent vs A-level/equivalent vs Degree vs post-graduate degree) - Age 27
table(dat$YPF7970, useNA = "ifany")

dat <- dat %>%
  mutate(YPF7970 = na_if(YPF7970, "Missing")) %>%
  mutate(YPF7970 = na_if(YPF7970, "Missed whole section I")) %>%
  mutate(YPF7970 = na_if(YPF7970, "Questionnaire not completed")) %>%
  mutate(YPF7970 = recode(YPF7970, "A-Level, NVQ 3, BTEC 3" = "A level", 
                          "GCSE (C/B/A/A*), NVQ 2, BTEC 2" = "GCSE", 
                          "GCSE (G/F/E/D) NVQ 1, BTEC 1" = "GCSE", "Masters, PGCE" = "PostGrad", 
                          "NVQ 4, BTEC 4" = "A level", "NVQ 5, BTEC 5, HNC/HND" = "A level",
                          "PhD" = "PostGrad")) %>%
  mutate(YPF7970 = factor(YPF7970, levels = c("GCSE", "A level", "Degree", "PostGrad"))) %>%
  rename(edu = YPF7970)

table(dat$edu, useNA = "ifany")
round(prop.table(table(dat$edu)) * 100, 1)
sum(table(dat$edu))


## Occupational social class (Managerial/admin/professional vs intermediate vs small employers vs lower supervisorry/technical vs routine) - Age 23
table(dat$YPC2492, useNA = "ifany")

dat <- dat %>%
  mutate(YPC2492 = na_if(YPC2492, "NS/NK")) %>%
  mutate(YPC2492 = na_if(YPC2492, "Questionnaire not completed")) %>%
  mutate(YPC2492 = recode(YPC2492, 
                          "Higher and lower managerial, administrative and professional occupations" = 
                            "Manager/Prof", "Intermediate occupations" = "Intermediate",
                          "Lower supervisory and technical occupations" = "Lower sup/tech",
                          "Semi-routine and routine occupations" = "Routine",
                          "Small employers and own account workers" = "Small employ")) %>%
  mutate(YPC2492 = factor(YPC2492, levels = c("Manager/Prof", "Intermediate", "Small employ", 
                                              "Lower sup/tech", "Routine"))) %>%
  rename(occClass = YPC2492)

table(dat$occClass, useNA = "ifany")
round(prop.table(table(dat$occClass)) * 100, 1)
sum(table(dat$occClass))


## Monthly household income after tax (£0-£499 vs £500-£999 vs £1000-£1499 vs £1500-£1999 vs £2000 and above) - Age 26
table(dat$YPE6020, useNA = "ifany")

dat <- dat %>%
  mutate(YPE6020 = na_if(YPE6020, "Missed whole section I")) %>%
  mutate(YPE6020 = na_if(YPE6020, "Missing")) %>%
  mutate(YPE6020 = na_if(YPE6020, "Questionnaire not completed")) %>%
  mutate(YPE6020 = recode(YPE6020, "\xa31 - \xa3499" = "<£500", "\xa3500 - \xa3999" = "£500-£999",
                          "\xa31000 - \xa31499" = "£1000-£1499", "Not doing paid work" = "<£500",
                          "\xa31500 - \xa31999" = "£1500-£1999", "\xa33000 and above" = ">£2000",
                          "\xa32000 - \xa32499" = ">£2000", "\xa32500 - \xa32999" = ">£2000")) %>%
  mutate(YPE6020 = factor(YPE6020, levels = c("<£500", "£500-£999", "£1000-£1499", "£1500-£1999", 
                                              ">£2000"))) %>%
  rename(income = YPE6020)

table(dat$income, useNA = "ifany")
round(prop.table(table(dat$income)) * 100, 1)
sum(table(dat$income))


## Area-level index of multiple deprivation (IMD; quintiles) - January 2021 (approx age 29)
table(dat$jan2021imd2015q5_YP, useNA = "ifany")

dat <- dat %>%
  mutate(jan2021imd2015q5_YP = na_if(jan2021imd2015q5_YP, "Missing")) %>%
  mutate(jan2021imd2015q5_YP = recode(jan2021imd2015q5_YP, "Least deprived" = "Quin. 1/Least deprived", 
                                      "2" = "Quintile 2", "3" = "Quintile 3", "4" = "Quintile 4", 
                                      "Most deprived" = "Quin. 5/Most deprived")) %>%
  mutate(jan2021imd2015q5_YP = factor(jan2021imd2015q5_YP, 
                                      levels = c("Quin. 1/Least deprived", 
                                                 "Quintile 2", "Quintile 3", 
                                                 "Quintile 4", "Quin. 5/Most deprived"))) %>%
  rename(imd = jan2021imd2015q5_YP)

table(dat$imd, useNA = "ifany")
round(prop.table(table(dat$imd)) * 100, 1)
sum(table(dat$imd))


## Home ownership status (owned/mortgaged vs rented vs Council/housing association vs other) - Age 28
table(dat$YPG1060, useNA = "ifany")

dat <- dat %>%
  mutate(YPG1060 = na_if(YPG1060, "NS/NK")) %>%
  mutate(YPG1060 = na_if(YPG1060, "Missed whole section A")) %>%
  mutate(YPG1060 = na_if(YPG1060, "Questionnaire not completed")) %>%
  mutate(YPG1060 = recode(YPG1060, "Rented from council/housing association" = "Council/HA",
                          "Being bought/mortgaged" = "Owned/Mortgaged", 
                          "Owned (with no mortgage to pay)" = "Owned/Mortgaged",
                          "Rented from private landlord" = "Rented")) %>%
  mutate(YPG1060 = factor(YPG1060, levels = c("Owned/Mortgaged", "Rented", "Council/HA", "Other"))) %>%
  rename(home = YPG1060)

table(dat$home, useNA = "ifany")
round(prop.table(table(dat$home)) * 100, 1)
sum(table(dat$home))


## 'Emotional stability' (opposite of 'neuroticism') personality trait
table(dat$fg7363, useNA = "ifany")

dat <- dat %>%
  mutate(fg7363 = na_if(fg7363, "Missing")) %>%
  mutate(fg7363 = na_if(fg7363, "Did not start session")) %>%
  mutate(fg7363 = as.numeric(fg7363)) %>%
  rename(emoStab = fg7363)

summary(dat$emoStab)
ggplot(dat, aes(x = emoStab)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()


## 'Openness' personality trait
table(dat$fg7364, useNA = "ifany")

dat <- dat %>%
  mutate(fg7364 = na_if(fg7364, "Missing")) %>%
  mutate(fg7364 = na_if(fg7364, "Did not start session")) %>%
  mutate(fg7364 = as.numeric(fg7364)) %>%
  rename(openness = fg7364)

summary(dat$openness)
ggplot(dat, aes(x = openness)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()


## Maternal EPDS in pregnancy - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though. 
table(dat$b371, useNA = "ifany")
table(dat$b372, useNA = "ifany")

dat <- dat %>%
  mutate(b371 = na_if(b371, "Missing")) %>%
  mutate(b371 = na_if(b371, "YHL")) %>%
  mutate(b371 = recode(b371, "not depressed" = "0", "very depressed" = "30")) %>%
  mutate(b371 = as.numeric(b371)) %>%
  mutate(b371 = ifelse(b372 == "6" | b372 == "7" | b372 == "8" | b372 == "9", NA, b371)) %>%
  rename(mat_dep = b371) %>%
  select(-b372)

table(dat$mat_dep, useNA = "ifany")
summary(dat$mat_dep)
ggplot(dat, aes(x = mat_dep)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()


## Maternal Crown-Crisp Experiential Index - Anxiety subscale score (CCEI-A) in pregnancy - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though.
table(dat$b352a, useNA = "ifany")
table(dat$b352b, useNA = "ifany")

dat <- dat %>%
  mutate(b352a = na_if(b352a, "Missing")) %>%
  mutate(b352a = na_if(b352a, "HaB short / YHL")) %>%
  mutate(b352a = recode(b352a, "not anxious" = "0", "very anxious" = "16")) %>%
  mutate(b352a = as.numeric(b352a)) %>%
  mutate(b352a = ifelse(b352b == "5" | b352b == "6" | b352b == "7", NA, b352a)) %>%
  rename(mat_anx = b352a) %>%
  select(-b352b)

table(dat$mat_anx, useNA = "ifany")
summary(dat$mat_anx)
ggplot(dat, aes(x = mat_anx)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()


## Father EPDS in pregnancy - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though. 
table(dat$pb261, useNA = "ifany")
table(dat$pb262, useNA = "ifany")

dat <- dat %>%
  mutate(pb261 = na_if(pb261, -1)) %>%
  mutate(pb261 = ifelse(pb262 > 5, NA, pb261)) %>%
  rename(pat_dep = pb261) %>%
  select(-pb262)

table(dat$pat_dep, useNA = "ifany")
summary(dat$pat_dep)
ggplot(dat, aes(x = pat_dep)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()


## Father Crown-Crisp Experiential Index - Anxiety subscale score (CCEI-A) in pregnancy - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though. 
table(dat$pb234, useNA = "ifany")
table(dat$pb235, useNA = "ifany")

dat <- dat %>%
  mutate(pb234 = na_if(pb234, "-1")) %>%
  mutate(pb234 = recode(pb234, "Not anxious" = "0", "Very anxious" = "16")) %>%
  mutate(pb234 = as.numeric(pb234)) %>%
  mutate(pb234 = ifelse(pb235 > 4, NA, pb234)) %>%
  rename(pat_anx = pb234) %>%
  select(-pb235)

table(dat$pat_anx, useNA = "ifany")
summary(dat$pat_anx)
ggplot(dat, aes(x = pat_anx)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()


## Maternal age at birth (years)
table(dat$mz028b, useNA = "ifany")

dat <- dat %>%
  mutate(mz028b = recode(mz028b, "< 16" = "15", ">43" = "44")) %>%
  mutate(mz028b = as.numeric(mz028b)) %>%
  rename(mat_age = mz028b)

table(dat$mat_age, useNA = "ifany")
summary(dat$mat_age)
ggplot(dat, aes(x = mat_age)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()


## Mother's highest education qualification (CSE/None vs Vocational vs O-level vs A-level vs Degree)
table(dat$c645a, useNA = "ifany")

dat <- dat %>%
  mutate(c645a = na_if(c645a, "Missing")) %>%
  mutate(c645a = recode(c645a, "CSE" = "CSE/None")) %>%
  mutate(c645a = factor(c645a, levels = c("CSE/None", "Vocational", "O level", "A level", "Degree"))) %>%
  rename(mat_edu = c645a)

table(dat$mat_edu, useNA = "ifany")
round(prop.table(table(dat$mat_edu)) * 100, 1)
sum(table(dat$mat_edu))


## Parental occupational social class (I vs II vs III (non-manual) vs III (manual) vs IV/V) - Take highest of either parent 
table(dat$c755, useNA = "ifany")

dat <- dat %>%
  mutate(c755 = na_if(c755, "Missing")) %>%
  mutate(c755 = na_if(c755, "Armed forces")) %>%
  mutate(c755 = recode(c755, "I" = "1", "II" = "2", "III (non-manual)" = "3", "III (manual)" = "4", 
                       "IV" = "5", "V" = "5")) %>%
  mutate(c755 = as.numeric(c755)) %>%
  rename(mat_occ = c755)

table(dat$mat_occ, useNA = "ifany")

table(dat$c765, useNA = "ifany")

dat <- dat %>%
  mutate(c765 = na_if(c765, "Missing")) %>%
  mutate(c765 = na_if(c765, "Armed forces")) %>%
  mutate(c765 = recode(c765, "I" = "1", "II" = "2", "III (non-manual)" = "3", "III (manual)" = "4", 
                       "IV" = "5", "V" = "5")) %>%
  mutate(c765 = as.numeric(c765)) %>%
  rename(pat_occ = c765)

table(dat$pat_occ, useNA = "ifany")

table(dat$mat_occ, dat$pat_occ, useNA = "ifany")

# Recode to highest class
dat <- dat %>%
  mutate(par_occSocClass = ifelse(is.na(mat_occ) & is.na(pat_occ), NA,
                              ifelse(is.na(mat_occ) & !is.na(pat_occ), pat_occ,
                                     ifelse(!is.na(mat_occ) & is.na(pat_occ), mat_occ,
                                            ifelse(pat_occ < mat_occ, pat_occ, mat_occ))))) %>%
  mutate(par_occSocClass = as.factor(par_occSocClass)) %>%
  mutate(par_occSocClass = recode(par_occSocClass, "1" = "I", "2" = "II", "3" = "III (non-manual)", 
                              "4" = "III (manual)", "5" = "IV/V")) %>%
  mutate(par_occSocClass = factor(par_occSocClass, levels = c("I", "II", "III (non-manual)", "III (manual)", "IV/V")))

table(dat$par_occSocClass, useNA = "ifany")
round(prop.table(table(dat$par_occSocClass)) * 100, 1)
sum(table(dat$par_occSocClass))

table(dat$par_occSocClass, dat$mat_occ, useNA = "ifany")
table(dat$par_occSocClass, dat$pat_occ, useNA = "ifany")

dat <- dat %>%
  select(-c(mat_occ, pat_occ))


## Parental weekly household income - age 3
table(dat$h470, useNA = "ifany")

dat <- dat %>%
  mutate(h470 = na_if(h470, "Not stated")) %>%
  mutate(h470 = factor(h470, levels = c("<100", "100 - 199", "200 - 299", "300 - 399", ">400"))) %>%
  rename(par_income = h470)

table(dat$par_income, useNA = "ifany")
round(prop.table(table(dat$par_income)) * 100, 1)
sum(table(dat$par_income))


## Maternal area-level index of multiple deprivation (IMD; quintiles) - From Jan 1993 (around birth)
table(dat$jan1993imd2010q5_M, useNA = "ifany")

dat <- dat %>%
  mutate(jan1993imd2010q5_M = na_if(jan1993imd2010q5_M, "Missing")) %>%
  mutate(jan1993imd2010q5_M = na_if(jan1993imd2010q5_M, "Triplets/Quadruplets")) %>%
  mutate(jan1993imd2010q5_M = recode(jan1993imd2010q5_M, "Least deprived" = "Quin. 1/Least deprived", 
                                     "2" = "Quintile 2", "3" = "Quintile 3", "4" = "Quintile 4", 
                                     "Most deprived" = "Quin. 5/Most deprived")) %>%
  mutate(jan1993imd2010q5_M = factor(jan1993imd2010q5_M, 
                                     levels = c("Quin. 1/Least deprived", 
                                                "Quintile 2", "Quintile 3", 
                                                "Quintile 4", "Quin. 5/Most deprived"))) %>%
  rename(mat_imd = jan1993imd2010q5_M)

table(dat$mat_imd, useNA = "ifany")
round(prop.table(table(dat$mat_imd)) * 100, 1)
sum(table(dat$mat_imd))


## Maternal home ownership status (owned/mortgaged vs rented vs Council/housing association vs other) - In pregnancy
table(dat$a006, useNA = "ifany")

dat <- dat %>%
  mutate(a006 = na_if(a006, "Missing")) %>%
  mutate(a006 = na_if(a006, "YE short")) %>%
  mutate(a006 = recode(a006, "Council rented" = "Council/HA", "HA rented" = "Council/HA",
                       "Mortgaged" = "Owned/Mortgaged", "Owned" = "Owned/Mortgaged",
                       "RENT PRIV FURN" = "Rented", "RENT PRIV UNFURN" = "Rented")) %>%
  mutate(a006 = factor(a006, levels = c("Owned/Mortgaged", "Rented", "Council/HA", "Other"))) %>%
  rename(mat_home = a006)

table(dat$mat_home, useNA = "ifany")
round(prop.table(table(dat$mat_home)) * 100, 1)
sum(table(dat$mat_home))


### Tidy the order of the variables (and drop ID variables) - Put outcomes and exposures at end of dataset, and often helps with synthpop
dat <- dat %>%
  select(-c(aln, qlet)) %>%
  relocate(smfq, prior_dep, gad7_prior, prior_anx, wemwbs_prior, sex, ethnicity, relationship:openness,
           mat_dep:pat_anx, mat_age, mat_edu, mat_imd, par_occSocClass, par_income, mat_home,
           climateConcern, actionBelief, totalActions,
           epds, gad7, wemwbs)


### Reduce dataset down to analytic sample (i.e., those with either exposure or outcome data)
dat <- dat %>%
  filter(!is.na(climateConcern) | !is.na(epds) | !is.na(gad7) | !is.na(wemwbs))



### Save data (in R, CSV and Stata format; the R and Stata formats will keep all the factor formatting, while the CSV file will lose this)
save(dat, file = "data_climateConcern_B4572.RData")
write_csv(dat, file = "data_climateConcern_B4572.csv")
write_dta(dat, "data_climateConcern_B4572.dta")


