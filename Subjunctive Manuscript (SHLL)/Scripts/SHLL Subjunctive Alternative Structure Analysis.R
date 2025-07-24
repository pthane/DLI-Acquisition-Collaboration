library(tidyverse)
library(here)
library(lme4)
library(lmerTest)

options(scipen = 99)


# Prepare data
## Load data
L2_45_Prod <- read.csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Subjunctive Production.csv"))
L2_45_FCT <- read.csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Subjunctive FCT.csv"))

L2_78_Prod <- read.csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Subjunctive Production.csv"))
L2_78_FCT <- read.csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Subjunctive FCT.csv"))

HS_45_Prod <- read.csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Subjunctive Production.csv"))
HS_45_FCT <- read.csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Subjunctive FCT.csv"))

HS_78_Prod <- read.csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Subjunctive Production.csv"))
HS_78_FCT <- read.csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Subjunctive FCT.csv"))


## Create combined data
Production <- rbind(L2_45_Prod, L2_78_Prod, HS_45_Prod, HS_78_Prod) %>% 
  filter(!is.na(Accuracy))

FCT <- rbind(L2_45_FCT, L2_78_FCT, HS_45_FCT, HS_78_FCT) %>% 
  filter(!is.na(Accuracy))

Master <- rbind(Production, FCT)
Master$Task <- factor(Master$Task, levels = c("Production", "FCT"))


# Analyze substitutions
Production_1 <- Production %>% 
  filter(Accuracy == 1)

Production_0 <- Production %>% 
  filter(Accuracy == 0)


# Create table with non-subjunctive responses
Alternative_Forms_Table <- Production_0 %>% 
  group_by(Speaker_Group) %>% 
  count(Error_Type) %>% 
  tibble()


# View unique individual values
unique(Production$Item)
unique(FCT$Item)

unique(Production$Part_ID)
