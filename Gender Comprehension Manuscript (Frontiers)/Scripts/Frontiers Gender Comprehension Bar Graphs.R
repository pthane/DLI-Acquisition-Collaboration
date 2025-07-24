library(tidyverse)
library(here)


# Load data
HS_2_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Second Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "Heritage Speakers")

HS_45_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "Heritage Speakers")

HS_78_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "Heritage Speakers")

L2_2_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Second Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "L2 Learners")

L2_45_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "L2 Learners")

L2_78_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Clitic FCT.csv")) %>% 
  mutate(Speaker_Group = "L2 Learners")


# Separate by condition
## Masculine singular clitics
HS_2_FCT_Masculine_Singular <- HS_2_FCT %>% 
  filter(Gender == "Masculine") %>% 
  filter(Number == "Singular") %>% 
  mutate(Condition = "Masc. Sing.")

HS_45_FCT_Masculine_Singular <- HS_45_FCT %>% 
  filter(Gender == "Masculine") %>% 
  filter(Number == "Singular") %>% 
  mutate(Condition = "Masc. Sing.")

HS_78_FCT_Masculine_Singular <- HS_78_FCT %>% 
  filter(Gender == "Masculine") %>% 
  filter(Number == "Singular") %>% 
  mutate(Condition = "Masc. Sing.")

L2_2_FCT_Masculine_Singular <- L2_2_FCT %>% 
  filter(Gender == "Masculine") %>% 
  filter(Number == "Singular") %>% 
  mutate(Condition = "Masc. Sing.")

L2_45_FCT_Masculine_Singular <- L2_45_FCT %>% 
  filter(Gender == "Masculine") %>% 
  filter(Number == "Singular") %>% 
  mutate(Condition = "Masc. Sing.")

L2_78_FCT_Masculine_Singular <- L2_78_FCT %>% 
  filter(Gender == "Masculine") %>% 
  filter(Number == "Singular") %>% 
  mutate(Condition = "Masc. Sing.")


## Feminine singular clitics
HS_2_FCT_Feminine_Singular <- HS_2_FCT %>% 
  filter(Gender == "Feminine") %>% 
  filter(Number == "Singular") %>% 
  mutate(Condition = "Fem. Sing.")

HS_45_FCT_Feminine_Singular <- HS_45_FCT %>% 
  filter(Gender == "Feminine") %>% 
  filter(Number == "Singular") %>% 
  mutate(Condition = "Fem. Sing.")

HS_78_FCT_Feminine_Singular <- HS_78_FCT %>% 
  filter(Gender == "Feminine") %>% 
  filter(Number == "Singular") %>% 
  mutate(Condition = "Fem. Sing.")

L2_2_FCT_Feminine_Singular <- L2_2_FCT %>% 
  filter(Gender == "Feminine") %>% 
  filter(Number == "Singular") %>% 
  mutate(Condition = "Fem. Sing.")

L2_45_FCT_Feminine_Singular <- L2_45_FCT %>% 
  filter(Gender == "Feminine") %>% 
  filter(Number == "Singular") %>% 
  mutate(Condition = "Fem. Sing.")

L2_78_FCT_Feminine_Singular <- L2_78_FCT %>% 
  filter(Gender == "Feminine") %>% 
  filter(Number == "Singular") %>% 
  mutate(Condition = "Fem. Sing.")


## Masculine plural clitics
## Masculine singular clitics
HS_2_FCT_Masculine_Plural <- HS_2_FCT %>% 
  filter(Gender == "Masculine") %>% 
  filter(Number == "Plural") %>% 
  mutate(Condition = "Masc. Pl.")

HS_45_FCT_Masculine_Plural <- HS_45_FCT %>% 
  filter(Gender == "Masculine") %>% 
  filter(Number == "Plural") %>% 
  mutate(Condition = "Masc. Pl.")

HS_78_FCT_Masculine_Plural <- HS_78_FCT %>% 
  filter(Gender == "Masculine") %>% 
  filter(Number == "Plural") %>% 
  mutate(Condition = "Masc. Pl.")

L2_2_FCT_Masculine_Plural <- L2_2_FCT %>% 
  filter(Gender == "Masculine") %>% 
  filter(Number == "Plural") %>% 
  mutate(Condition = "Masc. Pl.")

L2_45_FCT_Masculine_Plural <- L2_45_FCT %>% 
  filter(Gender == "Masculine") %>% 
  filter(Number == "Plural") %>% 
  mutate(Condition = "Masc. Pl.")

L2_78_FCT_Masculine_Plural <- L2_78_FCT %>% 
  filter(Gender == "Masculine") %>% 
  filter(Number == "Plural") %>% 
  mutate(Condition = "Masc. Pl.")


## Feminine singular clitics
HS_2_FCT_Feminine_Plural <- HS_2_FCT %>% 
  filter(Gender == "Feminine") %>% 
  filter(Number == "Plural") %>% 
  mutate(Condition = "Fem. Pl.")

HS_45_FCT_Feminine_Plural <- HS_45_FCT %>% 
  filter(Gender == "Feminine") %>% 
  filter(Number == "Plural") %>% 
  mutate(Condition = "Fem. Pl.")

HS_78_FCT_Feminine_Plural <- HS_78_FCT %>% 
  filter(Gender == "Feminine") %>% 
  filter(Number == "Plural") %>% 
  mutate(Condition = "Fem. Pl.")

L2_2_FCT_Feminine_Plural <- L2_2_FCT %>% 
  filter(Gender == "Feminine") %>% 
  filter(Number == "Plural") %>% 
  mutate(Condition = "Fem. Pl.")

L2_45_FCT_Feminine_Plural <- L2_45_FCT %>% 
  filter(Gender == "Feminine") %>% 
  filter(Number == "Plural") %>% 
  mutate(Condition = "Fem. Pl.")

L2_78_FCT_Feminine_Plural <- L2_78_FCT %>% 
  filter(Gender == "Feminine") %>% 
  filter(Number == "Plural") %>% 
  mutate(Condition = "Fem. Pl.")


# Generate percentages by segment
## Heritage speakers, second grade
HS_2_FCT_Masculine_Singular_Average <- aggregate(HS_2_FCT_Masculine_Singular$Accuracy, list(HS_2_FCT_Masculine_Singular$Structure), FUN = mean, na.rm = TRUE)
HS_2_FCT_Masculine_Singular_Average <- HS_2_FCT_Masculine_Singular_Average %>% rename(Structure_Avg = x)
HS_2_FCT_Masculine_Singular_Average <- left_join(HS_2_FCT_Masculine_Singular, HS_2_FCT_Masculine_Singular_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_2_FCT_Feminine_Singular_Average <- aggregate(HS_2_FCT_Feminine_Singular$Accuracy, list(HS_2_FCT_Feminine_Singular$Structure), FUN = mean, na.rm = TRUE)
HS_2_FCT_Feminine_Singular_Average <- HS_2_FCT_Feminine_Singular_Average %>% rename(Structure_Avg = x)
HS_2_FCT_Feminine_Singular_Average <- left_join(HS_2_FCT_Feminine_Singular, HS_2_FCT_Feminine_Singular_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_2_FCT_Masculine_Plural_Average <- aggregate(HS_2_FCT_Masculine_Plural$Accuracy, list(HS_2_FCT_Masculine_Plural$Structure), FUN = mean, na.rm = TRUE)
HS_2_FCT_Masculine_Plural_Average <- HS_2_FCT_Masculine_Plural_Average %>% rename(Structure_Avg = x)
HS_2_FCT_Masculine_Plural_Average <- left_join(HS_2_FCT_Masculine_Plural, HS_2_FCT_Masculine_Plural_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_2_FCT_Feminine_Plural_Average <- aggregate(HS_2_FCT_Feminine_Plural$Accuracy, list(HS_2_FCT_Feminine_Plural$Structure), FUN = mean, na.rm = TRUE)
HS_2_FCT_Feminine_Plural_Average <- HS_2_FCT_Feminine_Plural_Average %>% rename(Structure_Avg = x)
HS_2_FCT_Feminine_Plural_Average <- left_join(HS_2_FCT_Feminine_Plural, HS_2_FCT_Feminine_Plural_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Heritage speakers, fourth/fifth grade
HS_45_FCT_Masculine_Singular_Average <- aggregate(HS_45_FCT_Masculine_Singular$Accuracy, list(HS_45_FCT_Masculine_Singular$Structure), FUN = mean, na.rm = TRUE)
HS_45_FCT_Masculine_Singular_Average <- HS_45_FCT_Masculine_Singular_Average %>% rename(Structure_Avg = x)
HS_45_FCT_Masculine_Singular_Average <- left_join(HS_45_FCT_Masculine_Singular, HS_45_FCT_Masculine_Singular_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_45_FCT_Feminine_Singular_Average <- aggregate(HS_45_FCT_Feminine_Singular$Accuracy, list(HS_45_FCT_Feminine_Singular$Structure), FUN = mean, na.rm = TRUE)
HS_45_FCT_Feminine_Singular_Average <- HS_45_FCT_Feminine_Singular_Average %>% rename(Structure_Avg = x)
HS_45_FCT_Feminine_Singular_Average <- left_join(HS_45_FCT_Feminine_Singular, HS_45_FCT_Feminine_Singular_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_45_FCT_Masculine_Plural_Average <- aggregate(HS_45_FCT_Masculine_Plural$Accuracy, list(HS_45_FCT_Masculine_Plural$Structure), FUN = mean, na.rm = TRUE)
HS_45_FCT_Masculine_Plural_Average <- HS_45_FCT_Masculine_Plural_Average %>% rename(Structure_Avg = x)
HS_45_FCT_Masculine_Plural_Average <- left_join(HS_45_FCT_Masculine_Plural, HS_45_FCT_Masculine_Plural_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_45_FCT_Feminine_Plural_Average <- aggregate(HS_45_FCT_Feminine_Plural$Accuracy, list(HS_45_FCT_Feminine_Plural$Structure), FUN = mean, na.rm = TRUE)
HS_45_FCT_Feminine_Plural_Average <- HS_45_FCT_Feminine_Plural_Average %>% rename(Structure_Avg = x)
HS_45_FCT_Feminine_Plural_Average <- left_join(HS_45_FCT_Feminine_Plural, HS_45_FCT_Feminine_Plural_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Heritage speakers, seventh/eighth grade
HS_78_FCT_Masculine_Singular_Average <- aggregate(HS_78_FCT_Masculine_Singular$Accuracy, list(HS_78_FCT_Masculine_Singular$Structure), FUN = mean, na.rm = TRUE)
HS_78_FCT_Masculine_Singular_Average <- HS_78_FCT_Masculine_Singular_Average %>% rename(Structure_Avg = x)
HS_78_FCT_Masculine_Singular_Average <- left_join(HS_78_FCT_Masculine_Singular, HS_78_FCT_Masculine_Singular_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_78_FCT_Feminine_Singular_Average <- aggregate(HS_78_FCT_Feminine_Singular$Accuracy, list(HS_78_FCT_Feminine_Singular$Structure), FUN = mean, na.rm = TRUE)
HS_78_FCT_Feminine_Singular_Average <- HS_78_FCT_Feminine_Singular_Average %>% rename(Structure_Avg = x)
HS_78_FCT_Feminine_Singular_Average <- left_join(HS_78_FCT_Feminine_Singular, HS_78_FCT_Feminine_Singular_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_78_FCT_Masculine_Plural_Average <- aggregate(HS_78_FCT_Masculine_Plural$Accuracy, list(HS_78_FCT_Masculine_Plural$Structure), FUN = mean, na.rm = TRUE)
HS_78_FCT_Masculine_Plural_Average <- HS_78_FCT_Masculine_Plural_Average %>% rename(Structure_Avg = x)
HS_78_FCT_Masculine_Plural_Average <- left_join(HS_78_FCT_Masculine_Plural, HS_78_FCT_Masculine_Plural_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_78_FCT_Feminine_Plural_Average <- aggregate(HS_78_FCT_Feminine_Plural$Accuracy, list(HS_78_FCT_Feminine_Plural$Structure), FUN = mean, na.rm = TRUE)
HS_78_FCT_Feminine_Plural_Average <- HS_78_FCT_Feminine_Plural_Average %>% rename(Structure_Avg = x)
HS_78_FCT_Feminine_Plural_Average <- left_join(HS_78_FCT_Feminine_Plural, HS_78_FCT_Feminine_Plural_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## L2 learners, second grade
L2_2_FCT_Masculine_Singular_Average <- aggregate(L2_2_FCT_Masculine_Singular$Accuracy, list(L2_2_FCT_Masculine_Singular$Structure), FUN = mean, na.rm = TRUE)
L2_2_FCT_Masculine_Singular_Average <- L2_2_FCT_Masculine_Singular_Average %>% rename(Structure_Avg = x)
L2_2_FCT_Masculine_Singular_Average <- left_join(L2_2_FCT_Masculine_Singular, L2_2_FCT_Masculine_Singular_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_2_FCT_Feminine_Singular_Average <- aggregate(L2_2_FCT_Feminine_Singular$Accuracy, list(L2_2_FCT_Feminine_Singular$Structure), FUN = mean, na.rm = TRUE)
L2_2_FCT_Feminine_Singular_Average <- L2_2_FCT_Feminine_Singular_Average %>% rename(Structure_Avg = x)
L2_2_FCT_Feminine_Singular_Average <- left_join(L2_2_FCT_Feminine_Singular, L2_2_FCT_Feminine_Singular_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_2_FCT_Masculine_Plural_Average <- aggregate(L2_2_FCT_Masculine_Plural$Accuracy, list(L2_2_FCT_Masculine_Plural$Structure), FUN = mean, na.rm = TRUE)
L2_2_FCT_Masculine_Plural_Average <- L2_2_FCT_Masculine_Plural_Average %>% rename(Structure_Avg = x)
L2_2_FCT_Masculine_Plural_Average <- left_join(L2_2_FCT_Masculine_Plural, L2_2_FCT_Masculine_Plural_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_2_FCT_Feminine_Plural_Average <- aggregate(L2_2_FCT_Feminine_Plural$Accuracy, list(L2_2_FCT_Feminine_Plural$Structure), FUN = mean, na.rm = TRUE)
L2_2_FCT_Feminine_Plural_Average <- L2_2_FCT_Feminine_Plural_Average %>% rename(Structure_Avg = x)
L2_2_FCT_Feminine_Plural_Average <- left_join(L2_2_FCT_Feminine_Plural, L2_2_FCT_Feminine_Plural_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## L2 learners, fourth/fifth grade
L2_45_FCT_Masculine_Singular_Average <- aggregate(L2_45_FCT_Masculine_Singular$Accuracy, list(L2_45_FCT_Masculine_Singular$Structure), FUN = mean, na.rm = TRUE)
L2_45_FCT_Masculine_Singular_Average <- L2_45_FCT_Masculine_Singular_Average %>% rename(Structure_Avg = x)
L2_45_FCT_Masculine_Singular_Average <- left_join(L2_45_FCT_Masculine_Singular, L2_45_FCT_Masculine_Singular_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_45_FCT_Feminine_Singular_Average <- aggregate(L2_45_FCT_Feminine_Singular$Accuracy, list(L2_45_FCT_Feminine_Singular$Structure), FUN = mean, na.rm = TRUE)
L2_45_FCT_Feminine_Singular_Average <- L2_45_FCT_Feminine_Singular_Average %>% rename(Structure_Avg = x)
L2_45_FCT_Feminine_Singular_Average <- left_join(L2_45_FCT_Feminine_Singular, L2_45_FCT_Feminine_Singular_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_45_FCT_Masculine_Plural_Average <- aggregate(L2_45_FCT_Masculine_Plural$Accuracy, list(L2_45_FCT_Masculine_Plural$Structure), FUN = mean, na.rm = TRUE)
L2_45_FCT_Masculine_Plural_Average <- L2_45_FCT_Masculine_Plural_Average %>% rename(Structure_Avg = x)
L2_45_FCT_Masculine_Plural_Average <- left_join(L2_45_FCT_Masculine_Plural, L2_45_FCT_Masculine_Plural_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_45_FCT_Feminine_Plural_Average <- aggregate(L2_45_FCT_Feminine_Plural$Accuracy, list(L2_45_FCT_Feminine_Plural$Structure), FUN = mean, na.rm = TRUE)
L2_45_FCT_Feminine_Plural_Average <- L2_45_FCT_Feminine_Plural_Average %>% rename(Structure_Avg = x)
L2_45_FCT_Feminine_Plural_Average <- left_join(L2_45_FCT_Feminine_Plural, L2_45_FCT_Feminine_Plural_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## L2 learners, seventh/eighth grade
L2_78_FCT_Masculine_Singular_Average <- aggregate(L2_78_FCT_Masculine_Singular$Accuracy, list(L2_78_FCT_Masculine_Singular$Structure), FUN = mean, na.rm = TRUE)
L2_78_FCT_Masculine_Singular_Average <- L2_78_FCT_Masculine_Singular_Average %>% rename(Structure_Avg = x)
L2_78_FCT_Masculine_Singular_Average <- left_join(L2_78_FCT_Masculine_Singular, L2_78_FCT_Masculine_Singular_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_78_FCT_Feminine_Singular_Average <- aggregate(L2_78_FCT_Feminine_Singular$Accuracy, list(L2_78_FCT_Feminine_Singular$Structure), FUN = mean, na.rm = TRUE)
L2_78_FCT_Feminine_Singular_Average <- L2_78_FCT_Feminine_Singular_Average %>% rename(Structure_Avg = x)
L2_78_FCT_Feminine_Singular_Average <- left_join(L2_78_FCT_Feminine_Singular, L2_78_FCT_Feminine_Singular_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_78_FCT_Masculine_Plural_Average <- aggregate(L2_78_FCT_Masculine_Plural$Accuracy, list(L2_78_FCT_Masculine_Plural$Structure), FUN = mean, na.rm = TRUE)
L2_78_FCT_Masculine_Plural_Average <- L2_78_FCT_Masculine_Plural_Average %>% rename(Structure_Avg = x)
L2_78_FCT_Masculine_Plural_Average <- left_join(L2_78_FCT_Masculine_Plural, L2_78_FCT_Masculine_Plural_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2_78_FCT_Feminine_Plural_Average <- aggregate(L2_78_FCT_Feminine_Plural$Accuracy, list(L2_78_FCT_Feminine_Plural$Structure), FUN = mean, na.rm = TRUE)
L2_78_FCT_Feminine_Plural_Average <- L2_78_FCT_Feminine_Plural_Average %>% rename(Structure_Avg = x)
L2_78_FCT_Feminine_Plural_Average <- left_join(L2_78_FCT_Feminine_Plural, L2_78_FCT_Feminine_Plural_Average, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


# Rejoin datasets
## By condition
Masculine_Singular <- rbind(HS_2_FCT_Masculine_Singular_Average, HS_45_FCT_Masculine_Singular_Average, HS_78_FCT_Masculine_Singular_Average, L2_2_FCT_Masculine_Singular_Average, L2_45_FCT_Masculine_Singular_Average, L2_78_FCT_Masculine_Singular_Average)
Feminine_Singular <- rbind(HS_2_FCT_Feminine_Singular_Average, HS_45_FCT_Feminine_Singular_Average, HS_78_FCT_Feminine_Singular_Average, L2_2_FCT_Feminine_Singular_Average, L2_45_FCT_Feminine_Singular_Average, L2_78_FCT_Feminine_Singular_Average)
Masculine_Plural <- rbind(HS_2_FCT_Masculine_Plural_Average, HS_45_FCT_Masculine_Plural_Average, HS_78_FCT_Masculine_Plural_Average, L2_2_FCT_Masculine_Plural_Average, L2_45_FCT_Masculine_Plural_Average, L2_78_FCT_Masculine_Plural_Average)
Feminine_Plural <- rbind(HS_2_FCT_Feminine_Plural_Average, HS_45_FCT_Feminine_Plural_Average, HS_78_FCT_Feminine_Plural_Average, L2_2_FCT_Feminine_Plural_Average, L2_45_FCT_Feminine_Plural_Average, L2_78_FCT_Feminine_Plural_Average)

## Master file
Master <- rbind(Masculine_Singular, Feminine_Singular, Masculine_Plural, Feminine_Plural)
Master$Condition <- factor(Master$Condition, levels = c("Masc. Sing.", "Masc. Pl.", "Fem. Sing.", "Fem. Pl."))


# Generate bar plots
## With labels
Bar_Graph <- Master %>% 
  ggplot(aes(x = Age_Group, y = Structure_Percentage, fill = Condition)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(facets = vars(Speaker_Group)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  geom_text(aes(label = round(Structure_Percentage)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  labs(x = "Age group/grade", y = "Percentage of expected gender morphology", fill = "Condition", title = "Clitic Gender Selection by Group, Gender, and Number") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

Bar_Graph

ggsave(filename = here("Gender Comprehension Manuscript (Frontiers)", "Graphs", "Frontiers Gender Figure 2.pdf"),
       plot = Bar_Graph,
       device = "pdf",
       width = 6.5,
       height = 3.5,
       units = "in")
