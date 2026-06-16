## Includes cross-sectional data only

library(tidyverse)
library(here)
library(patchwork)


# Prepare data
## Load data
Master <- read_csv(here("CSV Files", "Tidy Data", "Tidy Data DOM.csv")) %>% 
  filter(Session == 1) %>% 
  mutate(Gender = factor(Gender, levels = c("Masculine", "Feminine")),
         Grade = factor(Grade, levels = c(2, 3, 4, 5, 6, 7, 8))) %>% 
  group_by(Part_ID, Speaker_Group, Months_at_Start, Grade, Task) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))


# Plot individual differences
## Generate graph
Age_Plot <- Master %>% 
  ggplot(aes(x = Months_at_Start, y = Ratio, color = Speaker_Group)) +
  geom_jitter() +
  geom_smooth(method = glm) +  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(-1, 101)) +
  facet_grid(rows = vars(Task)) +
  scale_x_continuous(breaks = seq(84, 180, 12),
                     limits = c(83, 181)) +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(-1, 101)) +
  labs(x = "Age in months at time of testing",
       y = "Percentage of sentences with DOM",
       title = "DOM Production and Selection by Age and Group",
       color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))

Age_Plot

ggsave(filename = here("Manuscripts", "DOM Manuscript (Applied Psycholinguistics)", "Graphs", "AP DOM Figure 3.pdf"),
       plot = Age_Plot,
       device = "pdf",
       width = 6.5,
       height = 3,
       units = "in")
