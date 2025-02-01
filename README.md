# Project 1
welcome to my first data analysis project!

---
Adverse Childhood Experiences and Asthma in the United States: A Cross-Sectional Study using the 2022 Behavioral Risk Factor Surveillance System Dataset
---

## Data Import and Merging

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

setwd("/Users/maiacunningham/Library/CloudStorage/OneDrive-UW/Spring 2024/EPI 514/EPI 514 Working Directory")

library(haven)
library(dplyr)
library(tidyverse)

# read in 2022 data
LLCP22 <- read_xpt("LLCP2022.XPT")
#read in version 1
LLCP22v1 <- read_xpt("LLCP22V1.XPT")
# read in version 2
LLCP22v2 <- read_xpt("LLCP22V2.XPT")

write.csv(LLCP22, "LLCP22.csv", row.names = FALSE)
write.csv(LLCP22v1, "LLCP22v1.csv", row.names = FALSE)
write.csv(LLCP22v2, "LLCP22v2.csv", row.names = FALSE)

```
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(haven)
library(dplyr)
library(tidyverse)
library(waldo)

LLCP22 <- read.csv("LLCP22.csv")
LLCP22v1 <- read.csv("LLCP22v1.csv")
LLCP22v2 <- read.csv("LLCP22v2.csv")

```

```{r}
## extract data for each state (FIPS codes) that did ACE module

names(LLCP22)[names(LLCP22) == "X_STATE"] <- "state"
names(LLCP22v1)[names(LLCP22v1) == "X_STATE"] <- "state"
names(LLCP22v2)[names(LLCP22v2) == "X_STATE"] <- "state"

LLCP <- LLCP22[LLCP22$state %in% c(5, 12, 19, 32, 38, 41, 46, 51),]
LLCP$finalwt <- LLCP$llcpwt

LLCPv1 <- LLCP22v1[LLCP22v1$state %in% c(4, 39),]
LLCPv1$finalwt <- LLCPv1$llcpwtv1

LLCPv2 <- LLCP22v2[LLCP22v2$state %in% c(34, 40),]
LLCPv2$finalwt <- LLCPv2$llcpwtv2

# use compare from waldo package to see differences between columns for LLCP, LLCPv1, LLCPv2
compare(names(LLCPv2), names(LLCP))

# delete columns from LLCP that do not exist in LLCPv1 or LLCPv2
LLCP <- LLCP %>% select(-c("COVACGET", "COVIDINT"))

# rename columns in LLCPv1 and LLCPv2 to match LLCP
names(LLCPv1)[names(LLCPv1) == "X_CLCWTV1"] <- "X_CLLCPWT"
names(LLCPv2)[names(LLCPv2) == "X_CLCWTV2"] <- "X_CLLCPWT"
names(LLCPv1)[names(LLCPv1) == "X_LCPWTV1"] <- "X_LLCPWT"
names(LLCPv2)[names(LLCPv2) == "X_LCPWTV2"] <- "X_LLCPWT"

#combine datasets
ACEsdata <- rbind(LLCP, LLCPv1, LLCPv2)

# save new .csv file
ACEsdata <- select(ACEsdata, ASTHMA3, state, X_IMPRACE, SEXVAR, X_AGE_G, X_EDUCAG, X_INCOMG1, ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, ACEDIVRC, ACEPUNCH, ACEHURT1, ACESWEAR, ACETOUCH, ACETTHEM, ACEHVSEX, ACEADSAF, ACEADNED)

write.csv(ACEsdata, "ACEsdata.csv", row.names = FALSE)

```
## Data Cleaning and Analysis, Tables

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(haven)
library(dplyr)
library(tidyverse)
library(epiR)
library(tableone)

data <- read_csv("ACEsdata.csv")

data <- select(data, ASTHMA3, state, X_IMPRACE, SEXVAR, X_AGE_G, X_EDUCAG, X_INCOMG1, ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, ACEDIVRC, ACEPUNCH, ACEHURT1, ACESWEAR, ACETOUCH, ACETTHEM, ACEHVSEX, ACEADSAF, ACEADNED)

```

```{r}
# change don't know/not sure, refused, and not asked/missing into NA
data$ASTHMA3[data$ASTHMA3 == 7 | data$ASTHMA3 == 9 | data$ASTHMA3 == ""] <- NA
data$X_EDUCAG[data$X_EDUCAG == 9 | data$X_EDUCAG == ""] <- NA
data$X_INCOMG1[data$X_INCOMG1 == 9 | data$X_INCOMG1 == ""] <- NA

# create factor variable for education, income, smoking status, race, sex, age group, asthma
data$education <- factor(data$X_EDUCAG,
                         levels = c(1:4),
                         labels = c("Did not graduate high school","Graduated high school","Attended college or technical school","Graduated from college/technical school"))

data$income <- factor(data$X_INCOMG1,
                         levels = c(1:7),
                         labels = c("Less than 15,000","15,000-24,999","25,000-34,999","35,000-49,999","50,000-99,999","100,000-199,999","200,000 or more"))

data$race <- factor(data$X_IMPRACE,
                           levels = c(1:6),
                           labels = c("White, Non-Hispanic","Black, Non-Hispanic", "Asian, Non-Hispanic",
                                      "American Indian/Alaskan Native, Non-Hispanic", "Hispanic", 
                                      "Unlisted Race, Non-Hispanic"))

data$sex <- factor(data$SEXVAR,
                     levels = c(1:2),
                     labels = c("Male","Female"))

# recode age group to have 3 categories
#data$X_AGE_G[data$X_AGE_G == 1 | data$X_AGE_G == 2 | data$X_AGE_G == 3] <- 1
#data$X_AGE_G[data$X_AGE_G == 4 | data$X_AGE_G == 5] <- 2
#data$X_AGE_G[data$X_AGE_G == 6] <- 3

# recode age group to have 2 categories
data$X_AGE_G[data$X_AGE_G == 1 | data$X_AGE_G == 2 | data$X_AGE_G == 3] <- 1
data$X_AGE_G[data$X_AGE_G == 4 | data$X_AGE_G == 5 | data$X_AGE_G == 6] <- 2

data$age_group <- factor(data$X_AGE_G,
                   levels = c(1:2),
                   labels = c("18-44","45 or older"))

data$asthma_factor <- factor(data$ASTHMA3,
                             levels = c(1:2),
                             labels = c("Yes","No"))

# check numbers
summary(data$asthma_factor)

#remove rows where ASTHMA3 is NA
data <- data %>% filter(!is.na(ASTHMA3))
summary(data$asthma_factor)
```

```{r}
# change don't know/not sure, refused, and not asked/missing into NA for ACE variables
data$ACEDEPRS[data$ACEDEPRS == 7 | data$ACEDEPRS == 9 | data$ACEDEPRS == ""] <- NA
data$ACEDRINK[data$ACEDRINK == 7 | data$ACEDRINK == 9 | data$ACEDRINK == ""] <- NA
data$ACEDRUGS[data$ACEDRUGS == 7 | data$ACEDRUGS == 9 | data$ACEDRUGS == ""] <- NA
data$ACEPRISN[data$ACEPRISN == 7 | data$ACEPRISN == 9 | data$ACEPRISN == ""] <- NA
# for divorce, we will count "never married" as NAs
data$ACEDIVRC[data$ACEDIVRC == 7 | data$ACEDIVRC == 9 | data$ACEDIVRC == 8 | data$ACEDIVRC == ""] <- NA
data$ACEPUNCH[data$ACEPUNCH == 7 | data$ACEPUNCH == 9 | data$ACEPUNCH == ""] <- NA
data$ACEHURT1[data$ACEHURT1 == 7 | data$ACEHURT1 == 9 | data$ACEHURT1 == ""] <- NA
data$ACESWEAR[data$ACESWEAR == 7 | data$ACESWEAR == 9 | data$ACESWEAR == ""] <- NA
data$ACETOUCH[data$ACETOUCH == 7 | data$ACETOUCH == 9 | data$ACETOUCH == ""] <- NA
data$ACETTHEM[data$ACETTHEM == 7 | data$ACETTHEM == 9 | data$ACETTHEM == ""] <- NA
data$ACEHVSEX[data$ACEHVSEX == 7 | data$ACEHVSEX == 9 | data$ACEHVSEX == ""] <- NA
data$ACEADSAF[data$ACEADSAF == 7 | data$ACEADSAF == 9 | data$ACEADSAF == ""] <- NA
data$ACEADNED[data$ACEADNED == 7 | data$ACEADNED == 9 | data$ACEADNED == ""] <- NA

# dichotomize ACE variables
data$ACE_mentalill <- factor(data$ACEDEPRS,
                         levels = c(1,2),
                         labels = c("Yes","No"))

data$ACE_drink <- factor(data$ACEDRINK,
                         levels = c(1,2),
                         labels = c("Yes","No"))

data$ACE_drug <- factor(data$ACEDRUGS,
                         levels = c(1,2),
                         labels = c("Yes","No"))

data$ACE_prison <- factor(data$ACEPRISN,
                         levels = c(1,2),
                         labels = c("Yes","No"))

data$ACE_divorce <- factor(data$ACEDIVRC,
                         levels = c(1,2),
                         labels = c("Yes","No"))

## for PUNCH, HURT, SWEAR, TOUCH, THEM, HAVESEX: any occurence = YES, never = NO
data$ACEPUNCH <- ifelse(data$ACEPUNCH == 1, 2, 1)
data$ACE_punch <- factor(data$ACEPUNCH,
                        levels = c(1,2),
                        labels = c("Yes", "No"))

data$ACEHURT1 <- ifelse(data$ACEHURT1 == 1, 2, 1)
data$ACE_hurt <- factor(data$ACEHURT1,
                        levels = c(1,2),
                        labels = c("Yes", "No"))

data$ACESWEAR <- ifelse(data$ACESWEAR == 1, 2, 1)
data$ACE_swear <- factor(data$ACESWEAR,
                        levels = c(1,2),
                        labels = c("Yes", "No"))

data$ACETOUCH <- ifelse(data$ACETOUCH == 1, 2, 1)
data$ACE_touch <- factor(data$ACETOUCH,
                        levels = c(1,2),
                        labels = c("Yes", "No"))

data$ACETTHEM <- ifelse(data$ACETTHEM == 1, 2, 1)
data$ACE_them <- factor(data$ACETTHEM,
                        levels = c(1,2),
                        labels = c("Yes", "No"))

data$ACEHVSEX <- ifelse(data$ACEHVSEX == 1, 2, 1)
data$ACE_havesex <- factor(data$ACEHVSEX,
                        levels = c(1,2),
                        labels = c("Yes", "No"))

# for SAFE and NEEDS, anything not "all of the time" will be counted as an ACE

# YES means that they did experience the ACE
data$ACEADSAF <- ifelse(data$ACEADSAF == 5, 2,
                       ifelse(data$ACEADSAF %in% c(1, 2, 3, 4), 1, NA))
data$ACE_safe <- factor(data$ACEADSAF,
                        levels = c(1,2),
                        labels = c("Yes", "No"))

# YES means that they did experience the ACE
data$ACEADNED <- ifelse(data$ACEADNED == 5, 2,
                       ifelse(data$ACEADNED %in% c(1, 2, 3, 4), 1, NA))
data$ACE_needs <- factor(data$ACEADNED,
                        levels = c(1,2),
                        labels = c("Yes", "No"))

summary(data$ACE_needs)
summary(data$ACE_safe)
summary(data$ACE_havesex)
summary(data$ACE_divorce)
# as of now, NAs are included in the variables

```

```{r}
## TABLE 1

vars <- c("age_group", "sex", "race","education","income","ACE_mentalill","ACE_drink","ACE_drug","ACE_prison","ACE_divorce","ACE_punch","ACE_hurt","ACE_swear","ACE_touch","ACE_them","ACE_havesex","ACE_safe","ACE_needs") 

table1 <- CreateTableOne(vars = vars, data = data, strata = "asthma_factor", includeNA = T)

print(table1)

```

```{r}
## TABLE 2 - UNADJUSTED PRs

# ACE_mentalill
prev <- table(data$asthma_factor, data$ACE_mentalill, deparse.level = 2)
(prmentalill <- epi.2by2(dat=prev, method="cross.sectional"))

# ACE_drink
prev <- table(data$asthma_factor, data$ACE_drink, deparse.level = 2)
(prdrink <- epi.2by2(dat=prev, method="cross.sectional"))

# ACE_drug
prev <- table(data$asthma_factor, data$ACE_drug, deparse.level = 2)
(prdrug <- epi.2by2(dat=prev, method="cross.sectional"))

# ACE_prison
prev <- table(data$asthma_factor, data$ACE_prison, deparse.level = 2)
(prprison <- epi.2by2(dat=prev, method="cross.sectional"))

# ACE_divorce
prev <- table(data$asthma_factor, data$ACE_divorce, deparse.level = 2)
(prdivorce <- epi.2by2(dat=prev, method="cross.sectional"))

# ACE_punch
prev <- table(data$asthma_factor, data$ACE_punch, deparse.level = 2)
(prpunch <- epi.2by2(dat=prev, method="cross.sectional"))

# ACE_hurt
prev <- table(data$asthma_factor, data$ACE_hurt, deparse.level = 2)
(prhurt <- epi.2by2(dat=prev, method="cross.sectional"))

# ACE_swear
prev <- table(data$asthma_factor, data$ACE_swear, deparse.level = 2)
(prswear <- epi.2by2(dat=prev, method="cross.sectional"))

# ACE_touch
prev <- table(data$asthma_factor, data$ACE_touch, deparse.level = 2)
(prtouch <- epi.2by2(dat=prev, method="cross.sectional"))

# ACE_them
prev <- table(data$asthma_factor, data$ACE_them, deparse.level = 2)
(prthem <- epi.2by2(dat=prev, method="cross.sectional"))

# ACE_havesex
prev <- table(data$asthma_factor, data$ACE_havesex, deparse.level = 2)
(prhavesex <- epi.2by2(dat=prev, method="cross.sectional"))

# ACE_safe
prev <- table(data$asthma_factor, data$ACE_safe, deparse.level = 2)
(prsafe <- epi.2by2(dat=prev, method="cross.sectional"))

# ACE_needs
prev <- table(data$asthma_factor, data$ACE_needs, deparse.level = 2)
(prneeds <- epi.2by2(dat=prev, method="cross.sectional"))

```

```{r}
## TABLE 2 - ADJUSTED PRs

# Is this correct way to adjust for income and education?

strat <- xtabs(~ACE_needs + asthma_factor + sex + age_group, data = data)

#print the cross tabulation you've just created to take a look on your data and understand how it looks like
strat

mh_array <- array(strat,
               dim = c(2,2,4), # this creates a 3 dimension array with n (n = number of categories! in each adjustment variable) tables
               list(exposure = c('Yes', 'No'), # this includes our exposure variable with relevant labels
                    outcomes = c('Yes', 'No'), # this includes our outcome variable with relevant labels
                    confounders = 1:4)) 

# now we use this array with epi.2by2()
epi.2by2(mh_array, method = 'cross.sectional')

```

```{r}
## ANALYZING RACE AS AN EFFECT MODIFER between ACE_mentalill and asthma

strat_White <- xtabs(~ACE_mentalill + asthma_factor + age_group + sex, data = data, subset = race == 'White, Non-Hispanic')
strat_Black <- xtabs(~ACE_mentalill + asthma_factor + age_group + sex, data = data, subset = race == 'Black, Non-Hispanic')
strat_Asian <- xtabs(~ACE_mentalill + asthma_factor + age_group + sex, data = data, subset = race == 'Asian, Non-Hispanic')
strat_AIAN <- xtabs(~ACE_mentalill + asthma_factor + age_group + sex, data = data, subset = race == 'American Indian/Alaskan Native, Non-Hispanic')
strat_Hispanic <- xtabs(~ACE_mentalill + asthma_factor + age_group + sex, data = data, subset = race == 'Hispanic')
strat_Unlisted <- xtabs(~ACE_mentalill + asthma_factor + age_group + sex, data = data, subset = race == 'Unlisted Race, Non-Hispanic')


# print the cross tabulation you've just created to take a look on your data and understand how it looks like
strat_White
strat_Black
strat_Asian # multiple cells too small
strat_AIAN 
strat_Hispanic 
strat_Unlisted

array_White <- array(strat_White,
                 dim = c(2,2,4), 
                 list(exposure = c('Yes', 'No'), 
                      outcomes = c('Yes', 'No'), 
                      confounders = 1:4)) 

array_Black <- array(strat_Black,
                 dim = c(2,2,4), 
                 list(exposure = c('Yes', 'No'), 
                      outcomes = c('Yes', 'No'), 
                      confounders = 1:4)) 

array_AIAN <- array(strat_AIAN,
                 dim = c(2,2,4), 
                 list(exposure = c('Yes', 'No'), 
                      outcomes = c('Yes', 'No'), 
                      confounders = 1:4)) 

array_Hispanic <- array(strat_Hispanic,
                 dim = c(2,2,4), 
                 list(exposure = c('Yes', 'No'), 
                      outcomes = c('Yes', 'No'), 
                      confounders = 1:4)) 

array_Unlisted <- array(strat_Unlisted,
                 dim = c(2,2,4), 
                 list(exposure = c('Yes', 'No'), 
                      outcomes = c('Yes', 'No'), 
                      confounders = 1:4)) 

epi.2by2(array_White, method = 'cross.sectional')
epi.2by2(array_Black, method = 'cross.sectional')
epi.2by2(array_AIAN, method = 'cross.sectional')
epi.2by2(array_Hispanic, method = 'cross.sectional')
epi.2by2(array_Unlisted, method = 'cross.sectional')

```

```{r}
## ANALYZING RACE AS AN EFFECT MODIFER between ACE_havesex and asthma

strat_White <- xtabs(~ACE_havesex + asthma_factor + age_group + sex, data = data, subset = race == 'White, Non-Hispanic')
strat_Black <- xtabs(~ACE_havesex + asthma_factor + age_group + sex, data = data, subset = race == 'Black, Non-Hispanic')
strat_Asian <- xtabs(~ACE_havesex + asthma_factor + age_group + sex, data = data, subset = race == 'Asian, Non-Hispanic')
strat_AIAN <- xtabs(~ACE_havesex + asthma_factor + age_group + sex, data = data, subset = race == 'American Indian/Alaskan Native, Non-Hispanic')
strat_Hispanic <- xtabs(~ACE_havesex + asthma_factor + age_group + sex, data = data, subset = race == 'Hispanic')
strat_Unlisted <- xtabs(~ACE_havesex + asthma_factor + age_group + sex, data = data, subset = race == 'Unlisted Race, Non-Hispanic')

# print the cross tabulation you've just created to take a look on your data and understand how it looks like
strat_White
strat_Black 
strat_Asian # multiple cells have 0
strat_AIAN 
strat_Hispanic 
strat_Unlisted 

array_White <- array(strat_White,
                 dim = c(2,2,4), 
                 list(exposure = c('Yes', 'No'), 
                      outcomes = c('Yes', 'No'), 
                      confounders = 1:4)) 

array_Black <- array(strat_Black,
                 dim = c(2,2,4), 
                 list(exposure = c('Yes', 'No'), 
                      outcomes = c('Yes', 'No'), 
                      confounders = 1:4)) 

array_AIAN <- array(strat_AIAN,
                 dim = c(2,2,4), 
                 list(exposure = c('Yes', 'No'), 
                      outcomes = c('Yes', 'No'), 
                      confounders = 1:4)) 

array_Hispanic <- array(strat_Hispanic,
                 dim = c(2,2,4), 
                 list(exposure = c('Yes', 'No'), 
                      outcomes = c('Yes', 'No'), 
                      confounders = 1:4)) 

array_Unlisted <- array(strat_Black,
                 dim = c(2,2,4), 
                 list(exposure = c('Yes', 'No'), 
                      outcomes = c('Yes', 'No'), 
                      confounders = 1:4)) 

epi.2by2(array_White, method = 'cross.sectional')
epi.2by2(array_Black, method = 'cross.sectional')
epi.2by2(array_AIAN, method = 'cross.sectional')
epi.2by2(array_Hispanic, method = 'cross.sectional')
epi.2by2(array_Unlisted, method = 'cross.sectional')

```

                       ifelse(data$ACEADNED %in% c(1, 2, 3, 4), 1, NA))
data$ACE_needs <- factor(data$ACEADNED,
                        levels = c(1,2),
                        labels = c("Yes", "No"))


```
