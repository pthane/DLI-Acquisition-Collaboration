library(tidyverse)
library(here)


# Load Data
L2_45_Prod <- read_csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Subjunctive Production.csv"))
L2_45_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Subjunctive FCT.csv"))

HS_45_Prod <- read_csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Subjunctive Production.csv"))
HS_45_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Subjunctive FCT.csv"))

L2_78_Prod <- read_csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Subjunctive Production.csv"))
L2_78_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Subjunctive FCT.csv"))

HS_78_Prod <- read_csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Subjunctive Production.csv"))
HS_78_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Subjunctive FCT.csv"))


# Merge datasets
Prod <- rbind(L2_45_Prod, HS_45_Prod, L2_78_Prod, HS_78_Prod)
FCT <- rbind(L2_45_FCT, HS_45_FCT, L2_78_FCT, HS_78_FCT)

Joint <- rbind(Prod, FCT) %>% 
  filter(!is.na(Accuracy))

Joint$Task <- factor(Joint$Task, levels = c("Production", "Forced Choice"))
Joint$Verb_Type <- factor(Joint$Verb_Type, levels = c("Regular", "Irregular"))
Joint$Group <- factor(Joint$Group, levels = c("HS 4th/5th", "L2L 4th/5th", "HS 7th/8th", "L2L 7th/8th"))


# Generate statistics
## Including verb regularity
Descriptives_Regularity <- Joint %>% 
  group_by(Group, Task, Verb_Type) %>% 
  summarize(Mean = mean(Accuracy), SD = sd(Accuracy))


## Excluding verb regularity
Descriptives_General <- Joint %>% 
  group_by(Group, Task) %>% 
  summarize(Mean = mean(Accuracy), SD = sd(Accuracy))


## Proficiency
Proficiency <- Joint %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(BESA_Score), SD = sd(BESA_Score))

