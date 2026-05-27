## Includes cohort 1 data only

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)

options(scipen = 99)


# Prepare data
## Load production data
L2_2_Prod <- read_csv(here("CSV Files", "L2 Learners", "L2 Second Clitic Production.csv"))
L2_45_Prod <- read_csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth Clitic Production.csv"))
L2_78_Prod <- read_csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth Clitic Production.csv"))

HS_2_Prod <- read_csv(here("CSV Files", "Heritage", "Heritage Second Clitic Production.csv"))
HS_45_Prod <- read_csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth Clitic Production.csv"))
HS_78_Prod <- read_csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth Clitic Production.csv"))


## Load selection data
L2_2_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Second Clitic FCT.csv"))
L2_45_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth Clitic FCT.csv"))
L2_78_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth Clitic FCT.csv"))

HS_2_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Second Clitic FCT.csv"))
HS_45_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth Clitic FCT.csv"))
HS_78_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth Clitic FCT.csv"))


## Join dataframes
Production <- rbind(L2_2_Prod, L2_45_Prod, L2_78_Prod, HS_2_Prod, HS_45_Prod, HS_78_Prod) %>% 
  filter(!is.na(Accuracy_Clit_Gen))
Selection <- rbind(L2_2_FCT, L2_45_FCT, L2_78_FCT, HS_2_FCT, HS_45_FCT, HS_78_FCT)

Master <- rbind(Production, Selection)


# Nested model comparisons
Correlation_Null <- glmer(Accuracy ~ 1  +
                            (1 | Part_ID) + (1 | Item),
                          data = Master,
                          family = "binomial")

Correlation_Speaker <- glmer(Accuracy ~ 1 + Speaker_Group +
                               (1 | Part_ID) + (1 | Item),
                             data = Master,
                             family = "binomial")

Correlation_Age <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group +
                           (1 | Part_ID) + (1 | Item),
                         data = Master,
                         family = "binomial")

Correlation_Task <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + Task +
                              (1 | Part_ID) + (1 | Item),
                            data = Master,
                            family = "binomial")

Correlation_Gender <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + Task + Gender +
                                   (1 | Part_ID) + (1 | Item),
                                 data = Master,
                                 family = "binomial")

Correlation_Full <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + Task + Gender + Speaker_Group + Age_Group +
                            (1 | Part_ID) + (1 | Item),
                          data = Master,
                          family = "binomial")

anova(Correlation_Null, Correlation_Speaker, Correlation_Age, Correlation_Task, Correlation_Gender, Correlation_Full, test = "chisq")


# Final results
summary(Correlation_Gender)
