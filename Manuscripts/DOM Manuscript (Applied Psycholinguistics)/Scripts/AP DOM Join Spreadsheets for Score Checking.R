### Note to future readers: This code will no longer work because we will have resolved all
### discrepancies using the method here. We will include this script on the repository to indicate
### how we used R to bind a copy of the initial data with the transcriptions provided.
### We will resolve the initial dataframe as necessary. The original data before the intercoder
### reliability was computed is stored in the CSV files subdirectory and is called "Production Data
### (Before Recoding)" and is saved as a CSV file.


library(tidyverse)
library(here)

# Load files
DOM_EPT_Original <- read_csv(here("CSV Files", "Tidy Data", "Tidy Data DOM.csv")) %>%
  filter(Task == "Production",
         Session %in% c("C", "1"),
         !is.na(Non_Target_Response_Type)) %>%
  filter(Months_at_Start >= 84)

DOM_Score_Checking <- read_csv(here("CSV Files", "DOM Score Checking.csv")) %>%
  rename(Item = question_number,
         Response_Type = response_type_if_no_DOM,
         Comments = `Jorge comments`) %>%
  rename(Part_ID = Participant_ID) %>% 
  mutate(Item = paste0("Prod-", str_pad(as.integer(Item), width = 2, pad = "0")))


# Isolate rows that match/do not match
## Match
Coded_Participants <- DOM_EPT_Original %>%
  inner_join(DOM_Score_Checking, by = c("Part_ID", "Testing_Year", "Item")) %>%
  filter(Session == 1)


## No match
Missing_Participants <- DOM_EPT_Original %>%
  anti_join(DOM_Score_Checking, by = c("Part_ID", "Testing_Year", "Item")) %>%
  distinct(Part_ID, Group) %>%
  write_csv(here("Manuscripts", "DOM Manuscript (Applied Psycholinguistics)", "Data Files", "Missing Participants.csv"))


# Carry out score checking
## Create full spreadsheet
Score_Checking <- Coded_Participants %>%
  mutate(Accuracy = as.numeric(as.character(Accuracy)),
         Accuracy_DOM_coder_2 = as.numeric(as.character(Accuracy_DOM_coder_2)),
         Coding_Status = case_when(
           is.na(Accuracy) & is.na(Accuracy_DOM_coder_2)                                     ~ "Consensus - do not score",
           !is.na(Accuracy) & !is.na(Accuracy_DOM_coder_2) & Accuracy == Accuracy_DOM_coder_2 ~ "Consensus - scored",
           !is.na(Accuracy) & !is.na(Accuracy_DOM_coder_2) & Accuracy != Accuracy_DOM_coder_2 ~ "Flag - different codes",
           TRUE                                                                                ~ "Flag - only one coder scored")) %>%
  select(Part_ID, Testing_Year, Item, Response_produced, Accuracy, Accuracy_DOM_coder_2,
         Response_Type, Coding_Status, Coder, Notes) %>%
  write_csv(here("Manuscripts", "DOM Manuscript (Applied Psycholinguistics)", "Data Files", "Resolve Initial Coding Discrepancies.csv"))


## Display discrepancies
Score_Discrepancies <- Score_Checking %>% 
  filter(!Coding_Status == "Consensus - do not score",
         !Coding_Status == "Consensus - scored")

### Note to readers: this dataframe should result in 0 discrepancies because we resolved all of them
### by modifying the source code. We have included this script so that interested readers can see how
### we conducted intercoder reliability and resolved doubts.