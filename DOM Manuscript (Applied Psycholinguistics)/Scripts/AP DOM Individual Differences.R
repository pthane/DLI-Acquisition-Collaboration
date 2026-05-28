## Includes cross-sectional data only


library(tidyverse)
library(here)


# Prepare Data
## Load data
Master <- read_csv(here("CSV Files", "Tidy Data", "Tidy Data DOM.csv")) %>% 
  mutate(Gender = factor(Gender, levels = c("Masculine", "Feminine")),
         Grade = factor(Grade, levels = c(2, 3, 4, 5, 6, 7, 8)))

## Create task-specific dataframes
Production <- Master %>% 
  filter(Task == "Production") %>% 
  group_by(Part_ID, Speaker_Group, Grade) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100),
         Task = "Production")
  
Selection <- Master %>% 
  filter(Task == "Selection") %>% 
  group_by(Part_ID, Speaker_Group, Grade) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100),
         Task = "Selection")


## Create master dataset
Aggregate <- bind_rows(Production, Selection) %>% 
  select(Part_ID, Speaker_Group, Grade, Task, Ratio) %>%
  pivot_wider(names_from = Task, values_from = Ratio)


# Plot individual differences
## Generate graph
Ind_Diffs_Plot <- Aggregate %>% 
  ggplot(aes(x = Production, y = Selection)) +
  geom_jitter(mapping = aes(color = Grade)) +
  facet_grid(rows = vars(Speaker_Group)) +
  scale_x_continuous(breaks = seq (0, 100, 20),
                     limits = c(-1, 101)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(-1, 101)) +
  labs(x = "Percentage of sentences with DOM produced",
       y = "Percentage of sentences with DOM selected",
       title = "Individual Rates of DOM Selection and Production",
       color = "Grade") +
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
