library(tidyverse)
library(here)


# Load Data
## Second grade
L2_2_Prod <- read_csv(here("CSV Files", "L2 Learners", "L2 Second DOM Production.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Grade = "2nd Grade")

L2_2_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Second DOM FCT.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Grade = "2nd Grade")

HS_2_Prod <- read_csv(here("CSV Files", "Heritage", "Heritage Second DOM Production.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Grade = "2nd Grade")

HS_2_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Second DOM FCT.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Grade = "2nd Grade")


## Fourth/fifth grade
L2_45_Prod <- read_csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth DOM Production.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Grade = "4th/5th Grade")

L2_45_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Fourth and Fifth DOM FCT.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Grade = "4th/5th Grade")

HS_45_Prod <- read_csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth DOM Production.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Grade = "4th/5th Grade")

HS_45_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Fourth and Fifth DOM FCT.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Grade = "4th/5th Grade")


## Seventh/eighth grade
L2_78_Prod <- read_csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth DOM Production.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Grade = "7th/8th Grade")

L2_78_FCT <- read_csv(here("CSV Files", "L2 Learners", "L2 Seventh and Eighth DOM FCT.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Grade = "7th/8th Grade")

HS_78_Prod <- read_csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth DOM Production.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Grade = "7th/8th Grade")

HS_78_FCT <- read_csv(here("CSV Files", "Heritage", "Heritage Seventh and Eighth DOM FCT.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Grade = "7th/8th Grade")


# Create dataset for L2 learners
## Join groups
L2_Production <- rbind(L2_2_Prod, L2_45_Prod, L2_78_Prod)
L2_FCT <- rbind(L2_2_FCT, L2_45_FCT, L2_78_FCT)


## Create single dataset
L2_Production_Group <- L2_Production %>%
  filter(!is.na(Accuracy)) %>%
  group_by(Part_ID, Grade, SpeakerType) %>%
  summarize(Production = sum(Accuracy))

L2_FCT_Group <- L2_FCT %>%
  filter(!is.na(Accuracy)) %>%
  group_by(Part_ID, Grade, SpeakerType) %>%
  summarize(Selection = sum(Accuracy))

L2_Aggregate = left_join(L2_Production_Group, L2_FCT_Group, by = "Part_ID", "Grade") %>%
  rename(Grade = Grade.x,
         SpeakerType = SpeakerType.x) %>% 
  mutate(Group = "L2 Learners")


# Create dataset for HS learners
## Join groups
HS_Production <- rbind(HS_2_Prod, HS_45_Prod, HS_78_Prod)
HS_FCT <- rbind(HS_2_FCT, HS_45_FCT, HS_78_FCT)


## Create single dataset
HS_Production_Group <- HS_Production %>%
  filter(!is.na(Accuracy)) %>%
  group_by(Part_ID, Grade, SpeakerType) %>%
  summarize(Production = sum(Accuracy))

HS_FCT_Group <- HS_FCT %>%
  filter(!is.na(Accuracy)) %>%
  group_by(Part_ID, Grade, SpeakerType) %>%
  summarize(Selection = sum(Accuracy))

HS_Aggregate = left_join(HS_Production_Group, HS_FCT_Group, by = "Part_ID", "Grade") %>%
  rename(Grade = Grade.x,
         SpeakerType = SpeakerType.x) %>% 
  mutate(Group = "Heritage Speakers")


# Create master dataset
Aggregate <- rbind(L2_Aggregate, HS_Aggregate)
Aggregate$Grade <- factor(Aggregate$Grade, levels = c("2nd Grade", "4th/5th Grade", "7th/8th Grade"))


# Plot individual differences
## Without labels
Ind_Diffs_Plot <- Aggregate %>% 
  ggplot(aes(x = Production, y = Selection)) +
  geom_jitter(mapping = aes(color = Grade)) +
  facet_grid(rows = vars(SpeakerType)) +
  scale_x_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  labs(x = "Number of sentences with DOM produced", y = "Number of sentences with DOM selected", title = "Individual Rates of DOM Selection and Production", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))

Ind_Diffs_Plot

ggsave(filename = here("DOM Manuscript (Applied Psycholinguistics)", "Graphs", "AP DOM Figure 5.pdf"),
       plot = Ind_Diffs_Plot,
       device = "pdf",
       width = 6.5,
       height = 4,
       units = "in")