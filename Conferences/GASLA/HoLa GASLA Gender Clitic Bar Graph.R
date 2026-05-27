## Prepared with cohort 1 data only

library(here)
library(tidyverse)


# Load data
## Second grade
HS_2_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Second Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "Heritage Speakers")

HS_45_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "Heritage Speakers")

HS_78_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "Heritage Speakers")

L2_2_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Second Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "L2 Learners")

L2_45_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "L2 Learners")

L2_78_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "L2 Learners")


# Separate by gender
## Masculine
HS_2_FCT_Masculine <- HS_2_FCT %>% 
  filter(Gender == "Masculine")

HS_45_FCT_Masculine <- HS_45_FCT %>% 
  filter(Gender == "Masculine")

HS_78_FCT_Masculine <- HS_78_FCT %>% 
  filter(Gender == "Masculine")

L2_2_FCT_Masculine <- L2_2_FCT %>% 
  filter(Gender == "Masculine")

L2_45_FCT_Masculine <- L2_45_FCT %>% 
  filter(Gender == "Masculine")

L2_78_FCT_Masculine <- L2_78_FCT %>% 
  filter(Gender == "Masculine")


## Feminine
HS_2_FCT_Feminine <- HS_2_FCT %>% 
  filter(Gender == "Feminine")

HS_45_FCT_Feminine <- HS_45_FCT %>% 
  filter(Gender == "Feminine")

HS_78_FCT_Feminine <- HS_78_FCT %>% 
  filter(Gender == "Feminine")

L2_2_FCT_Feminine <- L2_2_FCT %>% 
  filter(Gender == "Feminine")

L2_45_FCT_Feminine <- L2_45_FCT %>% 
  filter(Gender == "Feminine")

L2_78_FCT_Feminine <- L2_78_FCT %>% 
  filter(Gender == "Feminine")


# Generate percentages by segment
## Heritage speakers, second grade
HS_2_FCT_Masculine_Average <- aggregate(HS_2_FCT_Masculine$Accuracy, list(HS_2_FCT_Masculine$Structure), FUN = mean, na.rm = TRUE)
HS_2_FCT_Masculine_Average <- HS_2_FCT_Masculine_Average %>% rename(Structure_Avg = x)
HS_2_FCT_Masculine_Average <- left_join(HS_2_FCT_Masculine, HS_2_FCT_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_2_FCT_Feminine_Average <- aggregate(HS_2_FCT_Feminine$Accuracy, list(HS_2_FCT_Feminine$Structure), FUN = mean, na.rm = TRUE)
HS_2_FCT_Feminine_Average <- HS_2_FCT_Feminine_Average %>% rename(Structure_Avg = x)
HS_2_FCT_Feminine_Average <- left_join(HS_2_FCT_Feminine, HS_2_FCT_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Heritage speakers, fourth/fifth grade
HS_45_FCT_Masculine_Average <- aggregate(HS_45_FCT_Masculine$Accuracy, list(HS_45_FCT_Masculine$Structure), FUN = mean, na.rm = TRUE)
HS_45_FCT_Masculine_Average <- HS_45_FCT_Masculine_Average %>% rename(Structure_Avg = x)
HS_45_FCT_Masculine_Average <- left_join(HS_45_FCT_Masculine, HS_45_FCT_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_45_FCT_Feminine_Average <- aggregate(HS_45_FCT_Feminine$Accuracy, list(HS_45_FCT_Feminine$Structure), FUN = mean, na.rm = TRUE)
HS_45_FCT_Feminine_Average <- HS_45_FCT_Feminine_Average %>% rename(Structure_Avg = x)
HS_45_FCT_Feminine_Average <- left_join(HS_45_FCT_Feminine, HS_45_FCT_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Heritage speakers, seventh/eighth grade
HS_78_FCT_Masculine_Average <- aggregate(HS_78_FCT_Masculine$Accuracy, list(HS_78_FCT_Masculine$Structure), FUN = mean, na.rm = TRUE)
HS_78_FCT_Masculine_Average <- HS_78_FCT_Masculine_Average %>% rename(Structure_Avg = x)
HS_78_FCT_Masculine_Average <- left_join(HS_78_FCT_Masculine, HS_78_FCT_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_78_FCT_Feminine_Average <- aggregate(HS_78_FCT_Feminine$Accuracy, list(HS_78_FCT_Feminine$Structure), FUN = mean, na.rm = TRUE)
HS_78_FCT_Feminine_Average <- HS_78_FCT_Feminine_Average %>% rename(Structure_Avg = x)
HS_78_FCT_Feminine_Average <- left_join(HS_78_FCT_Feminine, HS_78_FCT_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## L2 learners, second grade
L2_2_FCT_Masculine_Average <- aggregate(L2_2_FCT_Masculine$Accuracy, list(L2_2_FCT_Masculine$Structure), FUN = mean, na.rm = TRUE)
L2_2_FCT_Masculine_Average <- L2_2_FCT_Masculine_Average %>% rename(Structure_Avg = x)
L2_2_FCT_Masculine_Average <- left_join(L2_2_FCT_Masculine, L2_2_FCT_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_2_FCT_Feminine_Average <- aggregate(L2_2_FCT_Feminine$Accuracy, list(L2_2_FCT_Feminine$Structure), FUN = mean, na.rm = TRUE)
L2_2_FCT_Feminine_Average <- L2_2_FCT_Feminine_Average %>% rename(Structure_Avg = x)
L2_2_FCT_Feminine_Average <- left_join(L2_2_FCT_Feminine, L2_2_FCT_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## L2 learners, fourth/fifth grade
L2_45_FCT_Masculine_Average <- aggregate(L2_45_FCT_Masculine$Accuracy, list(L2_45_FCT_Masculine$Structure), FUN = mean, na.rm = TRUE)
L2_45_FCT_Masculine_Average <- L2_45_FCT_Masculine_Average %>% rename(Structure_Avg = x)
L2_45_FCT_Masculine_Average <- left_join(L2_45_FCT_Masculine, L2_45_FCT_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_45_FCT_Feminine_Average <- aggregate(L2_45_FCT_Feminine$Accuracy, list(L2_45_FCT_Feminine$Structure), FUN = mean, na.rm = TRUE)
L2_45_FCT_Feminine_Average <- L2_45_FCT_Feminine_Average %>% rename(Structure_Avg = x)
L2_45_FCT_Feminine_Average <- left_join(L2_45_FCT_Feminine, L2_45_FCT_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## L2 learners, seventh/eighth grade
L2_78_FCT_Masculine_Average <- aggregate(L2_78_FCT_Masculine$Accuracy, list(L2_78_FCT_Masculine$Structure), FUN = mean, na.rm = TRUE)
L2_78_FCT_Masculine_Average <- L2_78_FCT_Masculine_Average %>% rename(Structure_Avg = x)
L2_78_FCT_Masculine_Average <- left_join(L2_78_FCT_Masculine, L2_78_FCT_Masculine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_78_FCT_Feminine_Average <- aggregate(L2_78_FCT_Feminine$Accuracy, list(L2_78_FCT_Feminine$Structure), FUN = mean, na.rm = TRUE)
L2_78_FCT_Feminine_Average <- L2_78_FCT_Feminine_Average %>% rename(Structure_Avg = x)
L2_78_FCT_Feminine_Average <- left_join(L2_78_FCT_Feminine, L2_78_FCT_Feminine_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


# Rejoin datasets
## By condition
Masculine <- rbind(HS_2_FCT_Masculine_Average, HS_45_FCT_Masculine_Average, HS_78_FCT_Masculine_Average, L2_2_FCT_Masculine_Average, L2_45_FCT_Masculine_Average, L2_78_FCT_Masculine_Average)
Feminine <- rbind(HS_2_FCT_Feminine_Average, HS_45_FCT_Feminine_Average, HS_78_FCT_Feminine_Average, L2_2_FCT_Feminine_Average, L2_45_FCT_Feminine_Average, L2_78_FCT_Feminine_Average)

## Master file
Master <- rbind(Masculine, Feminine)
Master$Gender <- factor(Master$Gender, levels = c("Masculine", "Feminine"))


# Generate bar plots
## With labels
Master %>% 
  ggplot(aes(x = Age_Group, y = Structure_Percentage, fill = Gender)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(rows = vars(Speaker_Group)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  geom_text(aes(label = round(Structure_Percentage)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  labs(x = "Age group/grade", y = "Percentage of expected gender morphology", fill = "Expected Gender", title = "Clitic Gender Selection by Group, Gender, and Number") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))
