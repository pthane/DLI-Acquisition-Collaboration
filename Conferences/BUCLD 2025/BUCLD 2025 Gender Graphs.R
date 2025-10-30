library(tidyverse)


# Load data
## Second grade
L2_2_Production <- read_csv("./CSV Files/L2 Learners/L2 Second Clitic Production.csv")
L2_2_FCT <- read_csv("./CSV Files/L2 Learners/L2 Second Clitic FCT.csv") %>% 
  mutate(Task = "Selection")

L2_45_Production <- read_csv("./CSV Files/L2 Learners/L2 Fourth and Fifth Clitic Production.csv")
L2_45_FCT <- read_csv("./CSV Files/L2 Learners/L2 Fourth and Fifth Clitic FCT.csv") %>% 
  mutate(Task = "Selection")

L2_78_Production <- read_csv("./CSV Files/L2 Learners/L2 Seventh and Eighth Clitic Production.csv")
L2_78_FCT <- read_csv("./CSV Files/L2 Learners/L2 Seventh and Eighth Clitic FCT.csv") %>% 
  mutate(Task = "Selection")

HS_2_Production <- read_csv("./CSV Files/Heritage/Heritage Second Clitic Production.csv")
HS_2_FCT <- read_csv("./CSV Files/Heritage/Heritage Second Clitic FCT.csv") %>% 
  mutate(Task = "Selection")

HS_45_Production <- read_csv("./CSV Files/Heritage/Heritage Fourth and Fifth Clitic Production.csv")
HS_45_FCT <- read_csv("./CSV Files/Heritage/Heritage Fourth and Fifth Clitic FCT.csv") %>% 
  mutate(Task = "Selection")

HS_78_Production <- read_csv("./CSV Files/Heritage/Heritage Seventh and Eighth Clitic Production.csv")
HS_78_FCT <- read_csv("./CSV Files/Heritage/Heritage Seventh and Eighth Clitic FCT.csv") %>% 
  mutate(Task = "Selection")


# Separate by gender
## Masculine clitics
L2_2_Prod_Masculine <- L2_2_Production %>% 
  filter(Gender == "Masculine")

L2_2_FCT_Masculine <- L2_2_FCT %>% 
  filter(Gender == "Masculine")

HS_2_Prod_Masculine <- HS_2_Production %>% 
  filter(Gender == "Masculine")

HS_2_FCT_Masculine <- HS_2_FCT %>% 
  filter(Gender == "Masculine")

L2_45_Prod_Masculine <- L2_45_Production %>% 
  filter(Gender == "Masculine")

L2_45_FCT_Masculine <- L2_45_FCT %>% 
  filter(Gender == "Masculine")

HS_45_Prod_Masculine <- HS_45_Production %>% 
  filter(Gender == "Masculine")

HS_45_FCT_Masculine <- HS_45_FCT %>% 
  filter(Gender == "Masculine")

L2_78_Prod_Masculine <- L2_78_Production %>% 
  filter(Gender == "Masculine")

L2_78_FCT_Masculine <- L2_78_FCT %>% 
  filter(Gender == "Masculine")

HS_78_Prod_Masculine <- HS_78_Production %>% 
  filter(Gender == "Masculine")

HS_78_FCT_Masculine <- HS_78_FCT %>% 
  filter(Gender == "Masculine")


## Feminine clitics
L2_2_Prod_Feminine <- L2_2_Production %>% 
  filter(Gender == "Feminine")

L2_2_FCT_Feminine <- L2_2_FCT %>% 
  filter(Gender == "Feminine")

HS_2_Prod_Feminine <- HS_2_Production %>% 
  filter(Gender == "Feminine")

HS_2_FCT_Feminine <- HS_2_FCT %>% 
  filter(Gender == "Feminine")

L2_45_Prod_Feminine <- L2_45_Production %>% 
  filter(Gender == "Feminine")

L2_45_FCT_Feminine <- L2_45_FCT %>% 
  filter(Gender == "Feminine")

HS_45_Prod_Feminine <- HS_45_Production %>% 
  filter(Gender == "Feminine")

HS_45_FCT_Feminine <- HS_45_FCT %>% 
  filter(Gender == "Feminine")

L2_78_Prod_Feminine <- L2_78_Production %>% 
  filter(Gender == "Feminine")

L2_78_FCT_Feminine <- L2_78_FCT %>% 
  filter(Gender == "Feminine")

HS_78_Prod_Feminine <- HS_78_Production %>% 
  filter(Gender == "Feminine")

HS_78_FCT_Feminine <- HS_78_FCT %>% 
  filter(Gender == "Feminine")


# Generate percentages by segment
## L2 learners, second grade
L2_2_Prod_Masculine_Average <- aggregate(L2_2_Prod_Masculine$Accuracy, list(L2_2_Prod_Masculine$Structure), FUN = mean, na.rm = TRUE)
L2_2_Prod_Masculine_Average <- L2_2_Prod_Masculine_Average %>% rename(Structure_Avg = x)
L2_2_Prod_Masculine_Average <- left_join(L2_2_Prod_Masculine, L2_2_Prod_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_2_FCT_Masculine_Average <- aggregate(L2_2_FCT_Masculine$Accuracy, list(L2_2_FCT_Masculine$Structure), FUN = mean, na.rm = TRUE)
L2_2_FCT_Masculine_Average <- L2_2_FCT_Masculine_Average %>% rename(Structure_Avg = x)
L2_2_FCT_Masculine_Average <- left_join(L2_2_FCT_Masculine, L2_2_FCT_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_2_Prod_Feminine_Average <- aggregate(L2_2_Prod_Feminine$Accuracy, list(L2_2_Prod_Feminine$Structure), FUN = mean, na.rm = TRUE)
L2_2_Prod_Feminine_Average <- L2_2_Prod_Feminine_Average %>% rename(Structure_Avg = x)
L2_2_Prod_Feminine_Average <- left_join(L2_2_Prod_Feminine, L2_2_Prod_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_2_FCT_Feminine_Average <- aggregate(L2_2_FCT_Feminine$Accuracy, list(L2_2_FCT_Feminine$Structure), FUN = mean, na.rm = TRUE)
L2_2_FCT_Feminine_Average <- L2_2_FCT_Feminine_Average %>% rename(Structure_Avg = x)
L2_2_FCT_Feminine_Average <- left_join(L2_2_FCT_Feminine, L2_2_FCT_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Heritage, second grade
HS_2_Prod_Masculine_Average <- aggregate(HS_2_Prod_Masculine$Accuracy, list(HS_2_Prod_Masculine$Structure), FUN = mean, na.rm = TRUE)
HS_2_Prod_Masculine_Average <- HS_2_Prod_Masculine_Average %>% rename(Structure_Avg = x)
HS_2_Prod_Masculine_Average <- left_join(HS_2_Prod_Masculine, HS_2_Prod_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_2_FCT_Masculine_Average <- aggregate(HS_2_FCT_Masculine$Accuracy, list(HS_2_FCT_Masculine$Structure), FUN = mean, na.rm = TRUE)
HS_2_FCT_Masculine_Average <- HS_2_FCT_Masculine_Average %>% rename(Structure_Avg = x)
HS_2_FCT_Masculine_Average <- left_join(HS_2_FCT_Masculine, HS_2_FCT_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_2_Prod_Feminine_Average <- aggregate(HS_2_Prod_Feminine$Accuracy, list(HS_2_Prod_Feminine$Structure), FUN = mean, na.rm = TRUE)
HS_2_Prod_Feminine_Average <- HS_2_Prod_Feminine_Average %>% rename(Structure_Avg = x)
HS_2_Prod_Feminine_Average <- left_join(HS_2_Prod_Feminine, HS_2_Prod_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_2_FCT_Feminine_Average <- aggregate(HS_2_FCT_Feminine$Accuracy, list(HS_2_FCT_Feminine$Structure), FUN = mean, na.rm = TRUE)
HS_2_FCT_Feminine_Average <- HS_2_FCT_Feminine_Average %>% rename(Structure_Avg = x)
HS_2_FCT_Feminine_Average <- left_join(HS_2_FCT_Feminine, HS_2_FCT_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## L2 learners, fourth/fifth grade
L2_45_Prod_Masculine_Average <- aggregate(L2_45_Prod_Masculine$Accuracy, list(L2_45_Prod_Masculine$Structure), FUN = mean, na.rm = TRUE)
L2_45_Prod_Masculine_Average <- L2_45_Prod_Masculine_Average %>% rename(Structure_Avg = x)
L2_45_Prod_Masculine_Average <- left_join(L2_45_Prod_Masculine, L2_45_Prod_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_45_FCT_Masculine_Average <- aggregate(L2_45_FCT_Masculine$Accuracy, list(L2_45_FCT_Masculine$Structure), FUN = mean, na.rm = TRUE)
L2_45_FCT_Masculine_Average <- L2_45_FCT_Masculine_Average %>% rename(Structure_Avg = x)
L2_45_FCT_Masculine_Average <- left_join(L2_45_FCT_Masculine, L2_45_FCT_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_45_Prod_Feminine_Average <- aggregate(L2_45_Prod_Feminine$Accuracy, list(L2_45_Prod_Feminine$Structure), FUN = mean, na.rm = TRUE)
L2_45_Prod_Feminine_Average <- L2_45_Prod_Feminine_Average %>% rename(Structure_Avg = x)
L2_45_Prod_Feminine_Average <- left_join(L2_45_Prod_Feminine, L2_45_Prod_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_45_FCT_Feminine_Average <- aggregate(L2_45_FCT_Feminine$Accuracy, list(L2_45_FCT_Feminine$Structure), FUN = mean, na.rm = TRUE)
L2_45_FCT_Feminine_Average <- L2_45_FCT_Feminine_Average %>% rename(Structure_Avg = x)
L2_45_FCT_Feminine_Average <- left_join(L2_45_FCT_Feminine, L2_45_FCT_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Heritage, fourth/fifth grade
HS_45_Prod_Masculine_Average <- aggregate(HS_45_Prod_Masculine$Accuracy, list(HS_45_Prod_Masculine$Structure), FUN = mean, na.rm = TRUE)
HS_45_Prod_Masculine_Average <- HS_45_Prod_Masculine_Average %>% rename(Structure_Avg = x)
HS_45_Prod_Masculine_Average <- left_join(HS_45_Prod_Masculine, HS_45_Prod_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_45_FCT_Masculine_Average <- aggregate(HS_45_FCT_Masculine$Accuracy, list(HS_45_FCT_Masculine$Structure), FUN = mean, na.rm = TRUE)
HS_45_FCT_Masculine_Average <- HS_45_FCT_Masculine_Average %>% rename(Structure_Avg = x)
HS_45_FCT_Masculine_Average <- left_join(HS_45_FCT_Masculine, HS_45_FCT_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_45_Prod_Feminine_Average <- aggregate(HS_45_Prod_Feminine$Accuracy, list(HS_45_Prod_Feminine$Structure), FUN = mean, na.rm = TRUE)
HS_45_Prod_Feminine_Average <- HS_45_Prod_Feminine_Average %>% rename(Structure_Avg = x)
HS_45_Prod_Feminine_Average <- left_join(HS_45_Prod_Feminine, HS_45_Prod_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_45_FCT_Feminine_Average <- aggregate(HS_45_FCT_Feminine$Accuracy, list(HS_45_FCT_Feminine$Structure), FUN = mean, na.rm = TRUE)
HS_45_FCT_Feminine_Average <- HS_45_FCT_Feminine_Average %>% rename(Structure_Avg = x)
HS_45_FCT_Feminine_Average <- left_join(HS_45_FCT_Feminine, HS_45_FCT_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## L2 learners, seventh/eighth grade
L2_78_Prod_Masculine_Average <- aggregate(L2_78_Prod_Masculine$Accuracy, list(L2_78_Prod_Masculine$Structure), FUN = mean, na.rm = TRUE)
L2_78_Prod_Masculine_Average <- L2_78_Prod_Masculine_Average %>% rename(Structure_Avg = x)
L2_78_Prod_Masculine_Average <- left_join(L2_78_Prod_Masculine, L2_78_Prod_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_78_FCT_Masculine_Average <- aggregate(L2_78_FCT_Masculine$Accuracy, list(L2_78_FCT_Masculine$Structure), FUN = mean, na.rm = TRUE)
L2_78_FCT_Masculine_Average <- L2_78_FCT_Masculine_Average %>% rename(Structure_Avg = x)
L2_78_FCT_Masculine_Average <- left_join(L2_78_FCT_Masculine, L2_78_FCT_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_78_Prod_Feminine_Average <- aggregate(L2_78_Prod_Feminine$Accuracy, list(L2_78_Prod_Feminine$Structure), FUN = mean, na.rm = TRUE)
L2_78_Prod_Feminine_Average <- L2_78_Prod_Feminine_Average %>% rename(Structure_Avg = x)
L2_78_Prod_Feminine_Average <- left_join(L2_78_Prod_Feminine, L2_78_Prod_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_78_FCT_Feminine_Average <- aggregate(L2_78_FCT_Feminine$Accuracy, list(L2_78_FCT_Feminine$Structure), FUN = mean, na.rm = TRUE)
L2_78_FCT_Feminine_Average <- L2_78_FCT_Feminine_Average %>% rename(Structure_Avg = x)
L2_78_FCT_Feminine_Average <- left_join(L2_78_FCT_Feminine, L2_78_FCT_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Heritage, seventh/eighth grade
HS_78_Prod_Masculine_Average <- aggregate(HS_78_Prod_Masculine$Accuracy, list(HS_78_Prod_Masculine$Structure), FUN = mean, na.rm = TRUE)
HS_78_Prod_Masculine_Average <- HS_78_Prod_Masculine_Average %>% rename(Structure_Avg = x)
HS_78_Prod_Masculine_Average <- left_join(HS_78_Prod_Masculine, HS_78_Prod_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_78_FCT_Masculine_Average <- aggregate(HS_78_FCT_Masculine$Accuracy, list(HS_78_FCT_Masculine$Structure), FUN = mean, na.rm = TRUE)
HS_78_FCT_Masculine_Average <- HS_78_FCT_Masculine_Average %>% rename(Structure_Avg = x)
HS_78_FCT_Masculine_Average <- left_join(HS_78_FCT_Masculine, HS_78_FCT_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_78_Prod_Feminine_Average <- aggregate(HS_78_Prod_Feminine$Accuracy, list(HS_78_Prod_Feminine$Structure), FUN = mean, na.rm = TRUE)
HS_78_Prod_Feminine_Average <- HS_78_Prod_Feminine_Average %>% rename(Structure_Avg = x)
HS_78_Prod_Feminine_Average <- left_join(HS_78_Prod_Feminine, HS_78_Prod_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_78_FCT_Feminine_Average <- aggregate(HS_78_FCT_Feminine$Accuracy, list(HS_78_FCT_Feminine$Structure), FUN = mean, na.rm = TRUE)
HS_78_FCT_Feminine_Average <- HS_78_FCT_Feminine_Average %>% rename(Structure_Avg = x)
HS_78_FCT_Feminine_Average <- left_join(HS_78_FCT_Feminine, HS_78_FCT_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


# Rejoin datasets
## Masculine
Masculine_Production <- rbind(L2_2_Prod_Masculine_Average, HS_2_Prod_Masculine_Average, L2_45_Prod_Masculine_Average, HS_45_Prod_Masculine_Average, L2_78_Prod_Masculine_Average, HS_78_Prod_Masculine_Average)
Masculine_FCT <- rbind(L2_2_FCT_Masculine_Average, HS_2_FCT_Masculine_Average, L2_45_FCT_Masculine_Average, HS_45_FCT_Masculine_Average, L2_78_FCT_Masculine_Average, HS_78_FCT_Masculine_Average)

Masculine <- rbind(Masculine_Production, Masculine_FCT)
Masculine$Group <- factor(Masculine$Group, levels = c("HS 2nd", "HS 4th/5th", "HS 7th/8th", "L2L 2nd", "L2L 4th/5th", "L2L 7th/8th"))
Masculine$Task <- factor(Masculine$Task, levels = c("Production", "Selection"))

## Feminine
Feminine_Production <- rbind(L2_2_Prod_Feminine_Average, HS_2_Prod_Feminine_Average, L2_45_Prod_Feminine_Average, HS_45_Prod_Feminine_Average, L2_78_Prod_Feminine_Average, HS_78_Prod_Feminine_Average)
Feminine_FCT <- rbind(L2_2_FCT_Feminine_Average, HS_2_FCT_Feminine_Average, L2_45_FCT_Feminine_Average, HS_45_FCT_Feminine_Average, L2_78_FCT_Feminine_Average, HS_78_FCT_Feminine_Average)

Feminine <- rbind(Feminine_Production, Feminine_FCT)
Feminine$Group <- factor(Feminine$Group, levels = c("HS 2nd", "HS 4th/5th", "HS 7th/8th", "L2L 2nd", "L2L 4th/5th", "L2L 7th/8th"))
Feminine$Task <- factor(Feminine$Task, levels = c("Production", "Selection"))


## Joint
Master <- rbind(Masculine, Feminine)
Master$Group <- factor(Master$Group, levels = c("HS 2nd", "HS 4th/5th", "HS 7th/8th", "L2L 2nd", "L2L 4th/5th", "L2L 7th/8th"))
Master$Task <- factor(Master$Task, levels = c("Production", "Selection"))
Master$Gender <- factor(Master$Gender, levels = c("Masculine", "Feminine"))


# Generate bar plots
## With labels
Master %>% 
  ggplot(aes(x = Gender, y = Structure_Percentage, fill = Task)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  facet_wrap(facets = vars(Group)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 105)) +
  geom_text(aes(label = round(Structure_Percentage)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Group", y = "Percentage of expected gender morphology", fill = "Task", title = "Direct Object Clitic Gender by Group, Gender, and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))
