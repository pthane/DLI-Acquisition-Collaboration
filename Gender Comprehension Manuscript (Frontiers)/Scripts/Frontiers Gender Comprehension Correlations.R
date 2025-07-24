library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)

options(scipen = 99)


# Prepare data
## Load data
HS_2_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Second Clitic FCT.csv"))
HS_45_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Clitic FCT.csv"))
HS_78_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Clitic FCT.csv"))

L2_2_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Second Clitic FCT.csv"))
L2_45_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Clitic FCT.csv"))
L2_78_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Clitic FCT.csv"))


## Rejoin data
Heritage <- rbind(HS_2_FCT, HS_45_FCT, HS_78_FCT)
L2 <- rbind(L2_2_FCT, L2_45_FCT, L2_78_FCT)

Master <- rbind(Heritage, L2) %>% 
  filter(Part_ID != "H21") %>%
  filter(Part_ID != "H57") %>%
  filter(Part_ID != "H64") %>% 
  mutate(Grade_Std = (Grade - mean(Grade))/sd(Grade),
         Months_Std = (Months - mean(Months))/sd(Months),
         SPA_Pct_Std = (SPA_Pct - mean(SPA_Pct))/sd(SPA_Pct))

Master$Gender <- factor(Master$Gender, levels = c("Masculine", "Feminine"))
Master$Number <- factor(Master$Number, levels = c("Singular", "Plural"))




# Nested model comparisons
Clitic_Null <- glmer(Accuracy ~ 1 +
                       (1 | Part_ID) + (1 | Item),
                     data = Master,
                     family = "binomial")

Clitic_Group <- glmer(Accuracy ~ 1 + Speaker_Group +
                       (1 | Part_ID) + (1 | Item),
                     data = Master,
                     family = "binomial")

Clitic_Age <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group +
                       (1 | Part_ID) + (1 | Item),
                     data = Master,
                     family = "binomial")

Clitic_Exposure <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + SPA_Pct_Std +
                           (1 | Part_ID) + (1 | Item),
                         data = Master,
                         family = "binomial")

Clitic_Gender <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + SPA_Pct_Std + Gender +
                       (1 | Part_ID) + (1 | Item),
                     data = Master,
                     family = "binomial")

Clitic_Number <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + SPA_Pct_Std + Gender + Number +
                       (1 | Part_ID) + (1 | Item),
                     data = Master,
                     family = "binomial")

Clitic_Speaker_Age <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + SPA_Pct_Std + Gender + Number + Speaker_Group:Age_Group +
                       (1 | Part_ID) + (1 | Item),
                     data = Master,
                     family = "binomial")

Clitic_Speaker_Gender <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + SPA_Pct_Std + Gender + Number + Speaker_Group:Age_Group + Speaker_Group:Gender +
                       (1 | Part_ID) + (1 | Item),
                     data = Master,
                     family = "binomial")

Clitic_Age_Gender <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + SPA_Pct_Std + Gender + Number + Speaker_Group:Age_Group + Speaker_Group:Gender + Age_Group:Gender +
                       (1 | Part_ID) + (1 | Item),
                     data = Master,
                     family = "binomial")

Clitic_Full <- glmer(Accuracy ~ 1 + Speaker_Group + Age_Group + SPA_Pct_Std + Gender + Number + Speaker_Group:Age_Group + Speaker_Group:Gender + Age_Group:Gender + Gender:Number +
                       (1 | Part_ID) + (1 | Item),
                     data = Master,
                     family = "binomial")

anova(Clitic_Null, Clitic_Group, Clitic_Age, Clitic_Gender, Clitic_Number, Clitic_Speaker_Age, Clitic_Speaker_Gender, Clitic_Age_Gender, Clitic_Full)



# Correlation
summary(Clitic_Gender)



# Supplementary age correlations
## Whole group
Age_Model <- glmer(Accuracy ~ Months_Std +
                     (1 | Part_ID) + (1 | Item),
                   data = Master,
                   family = "binomial")

summary(Age_Model)
