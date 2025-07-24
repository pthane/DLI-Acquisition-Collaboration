library(tidyverse)
library(TOSTER)

options(sciphen = 99)


# Load data
Frequency_Chart <- read_csv("./CSV Files/Noun Frequency Chart.csv") %>% 
  filter(Task == "FCT") %>% 
  mutate(Frequency_Std = (Frequency - mean(Frequency))/sd(Frequency))


# Perform TOST
TOSTER::dataTOSTtwo(
  data = Frequency_Chart,
  deps = "Frequency_Std",
  group = "Gender",
  low_eqbound = -0.5,
  high_eqbound = 0.5,
  desc = TRUE,
  plots = TRUE)
