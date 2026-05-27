## Includes cohort 1 data only

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)

options(scipen = 99)


# Prepare data
## Load data
HS_2_FCT <- read.csv(here("CSV Files", "Heritage", "Heritage Second Clitic FCT.csv"))
HS_45_FCT <- read.csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth Clitic FCT.csv"))
HS_78_FCT <- read.csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth Clitic FCT.csv"))

L2_2_FCT <- read.csv(here("CSV Files", "L2 Learners", "L2 Second Clitic FCT.csv"))
L2_45_FCT <- read.csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth Clitic FCT.csv"))
L2_78_FCT <- read.csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth Clitic FCT.csv"))


## Rejoin data
Heritage <- rbind(HS_2_FCT, HS_45_FCT, HS_78_FCT)
L2 <- rbind(L2_2_FCT, L2_45_FCT, L2_78_FCT)

Master <- rbind(Heritage, L2)
Master$Gender <- factor(Master$Gender, levels = c("Masculine", "Feminine"))


# Correlation
Clitic_Full <- glmer(Accuracy ~ Speaker_Group + Age_Group + Gender + Speaker_Group:Age_Group +
                       (1 | Part_ID) + (1 | Item),
                     data = Master,
                     family = "binomial")


# Correlation
summary(Clitic_Full)
