library(tidyverse)
library(here)


# Load Data
## Fourth/fifth grade
L2_45_Prod <- read_csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Subjunctive Production.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Grade = "4th/5th Grade")

L2_45_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Fourth and Fifth Subjunctive FCT.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Grade = "4th/5th Grade")

HS_45_Prod <- read_csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Subjunctive Production.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Grade = "4th/5th Grade")

HS_45_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Fourth and Fifth Subjunctive FCT.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Grade = "4th/5th Grade")


## Seventh/eighth grade
L2_78_Prod <- read_csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Subjunctive Production.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Grade = "7th/8th Grade")

L2_78_FCT <- read_csv(here("./CSV Files/L2 Learners/L2 Seventh and Eighth Subjunctive FCT.csv")) %>% 
  mutate(SpeakerType = "L2 Learners",
         Grade = "7th/8th Grade")

HS_78_Prod <- read_csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Subjunctive Production.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Grade = "7th/8th Grade")

HS_78_FCT <- read_csv(here("./CSV Files/Heritage/Heritage Seventh and Eighth Subjunctive FCT.csv")) %>% 
  mutate(SpeakerType = "Heritage Speakers",
         Grade = "7th/8th Grade")


# Create dataset for L2 learners
## Join groups
Production <- rbind(L2_45_Prod, HS_45_Prod, L2_78_Prod, HS_78_Prod)
FCT <- rbind(L2_45_FCT, HS_45_FCT, L2_78_FCT, HS_78_FCT)


## Create single dataset
Production_Group <- Production %>%
  filter(!is.na(Accuracy)) %>%
  group_by(Part_ID, Grade, SpeakerType) %>%
  summarize(Production = sum(Accuracy))

FCT_Group <- FCT %>%
  filter(!is.na(Accuracy)) %>%
  group_by(Part_ID, Grade, SpeakerType) %>%
  summarize(Selection = sum(Accuracy))

Aggregate = left_join(Production_Group, FCT_Group, by = "Part_ID", "Grade") %>%
  rename(Grade = Grade.x,
         SpeakerType = SpeakerType.x) %>% 
  mutate(Total = Production + Selection)
Aggregate$Grade <- factor(Aggregate$Grade, levels = c("2nd Grade", "4th/5th Grade", "7th/8th Grade"))


# Plot individual differences
## Whole group
Ind_Diffs <- Aggregate %>% 
  ggplot(aes(x = Production, y = Selection)) +
  geom_jitter(mapping = aes(color = Grade)) +
  facet_grid(rows = vars(SpeakerType)) +
  scale_x_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  labs(x = "No. of sentences with subjunctive produced", y = "No. of sentences with subjunctive selected", title = "Individual Rates of Subjunctive Selection and Production", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))

Ind_Diffs

ggsave(filename = here("Subjunctive Manuscript (SHLL)", "Graphs", "SHLL Figure 5.pdf"),
       plot = Ind_Diffs,
       device = "pdf",
       width = 6.5,
       height = 4,
       units = "in")
