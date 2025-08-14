# Load packages
library(tidyverse)
library(here)
library(janitor)
library(gtools)

# We first want to import our data
CCSI_data <- read.csv(here("data", "raw", "CCSI v5 student analysis v2.csv"),
                      na.strings = c("", "NA")) %>%
  clean_names() %>%
  rename_with(~ str_remove(., "^nu_"))

# Create a likert map
likert_map <- c(
  "strongly disagree" = 1, "disagree" = 2, "somewhat disagree" = 3,
  "somewhat agree" = 4, "agree" = 5, "strongly agree" = 6)

# Creats vector that only contains likert questions
likert_items <- CCSI_data %>%
  names() %>%
  str_subset("^q(3|4|5|6|7|8|9|10)_\\d+$")  

# Clean the values df to only include completed surveys with columns we want
CCSI_rel_data <- CCSI_data %>%
  slice(-1) %>% # Removes header row
  mutate(across(
    .cols = matches("^q(3|4|5|6|7|8|9|10)_\\d+$"),
    .fns = ~ as.numeric(.)
  )) %>% # Converts to numeric
  filter(rowSums(is.na(select(., all_of(likert_items)))) <= 5) %>% # Removes rows missing more than 5 responses
  filter(!is.na(response_id)) %>%
  select(-any_of(c("start_date", "end_date", "status", "ip_address", "progress",
                   "duration_in_seconds", "recorded_date", "recipient_last_name",
                   "recipient_first_name", "recipient_email", "external_reference",
                   "location_latitude", "location_longitude", "distribution_channel", 
                   "user_language", "q1", "finished",  "term_code",
                   "ageat_term_start_date", "zip_code", "first_enrollment",
                   "cumulative_credit_hours_attempted", "student_number",
                   "cumulative_credit_hours_passed","intention_to_transfer",
                   "gender_dichotomous"))) # Remove columns we don't need
  # mutate(across(
  #   .cols = matches("^q\\d+_\\d+$"),
  #   .fns = ~ {
  #     if (is.numeric(.)) {
  #       .
  #     } else {
  #       val <- tolower(trimws(as.character(.)))
  #       out <- likert_map[val]
  #       as.numeric(out)
  #     }
  #   }
  # ))

# Create a function to reverse code
reverse_code <- function(x) {ifelse(is.na(x), NA, 7 - x)}

# Apply reverse coding to relevant questions
CCSI_rel_data <- CCSI_rel_data %>%
  mutate(
    q6_1_rev = reverse_code(q6_1),
    q6_2_rev = reverse_code(q6_2),
    q8_1_rev = reverse_code(q8_1),
    q7_3_rev = reverse_code(q7_3)
  )


# Define original and reverse-coded pairs
column_pairs <- c("q6_1", "q6_1_rev", "q6_2", "q6_2_rev",
                  "q8_1", "q8_1_rev", "q7_3", "q7_3_rev")

# Keep the rest of the columns in original order and pull out columns not in pairs
all_columns <- colnames(CCSI_rel_data)
other_columns <- setdiff(all_columns, column_pairs)

# Reorder each reverse-coded question right after its original
vars_with_rev <- c("q6_1", "q6_2", "q8_1", "q7_3")
new_order <- c()
for (col in colnames(CCSI_rel_data)) {
  new_order <- c(new_order, col)
  if (col %in% vars_with_rev) {
    new_order <- c(new_order, paste0(col, "_rev"))
  }
}
new_order <- unique(new_order) # Removes duplicates in case

# Reorder the dataset
CCSI_rel_data <- CCSI_rel_data[, new_order]
# Define our item groups
meritocratic_items <- CCSI_rel_data %>%
  select(q5_1, q5_3, q6_1_rev, q6_2_rev, q7_1, q8_1_rev, q8_4) %>%
  mutate(across(everything(), as.numeric))

stem_identity_items <- CCSI_rel_data %>% 
  select(q3_1, q3_5, q4_4, q5_2, q5_4, q5_5, q9_1, q9_3, q10_5) %>%
  mutate(across(everything(), as.numeric))

growth_mindset_items <- CCSI_rel_data %>%
  select(q7_3_rev, q7_5, q8_3, q10_2, q10_3) %>%
  mutate(across(everything(), as.numeric))

# Create a question map
relevant_vars <- c(
  names(meritocratic_items),
  names(stem_identity_items),
  names(growth_mindset_items)
) %>%
  stringr::str_remove("_rev$") %>%
  unique()

questions <- names(CCSI_rel_data) %>%
  str_subset("_rev$", negate = TRUE) %>%
  setdiff("response_id")

question_row <- CCSI_data[1, ] %>% as_tibble()

question_map <- tibble(
  variable  = questions,
  question  = as.character(question_row[1, questions])
) %>%
  mutate(
    question = question %>%
      str_remove("^Please rate your level of agreement with the following statements\\. - [a-e]\\)\\s*") %>%
      str_replace_all("Rio Hondo College", "<my college>")
  )

q_items <- question_map %>% # Survey items
  filter(str_starts(variable, "q")) %>%
  arrange(mixedorder(variable))
other_items <- question_map %>%
  filter(!str_starts(variable, "q")) %>% # Demographic items
  arrange(variable)
  
question_map <- bind_rows(q_items, other_items) %>%
  mutate(variable_base = str_remove(variable, "_rev$")) %>%
  filter(variable_base %in% relevant_vars) %>%
  select(variable, question) %>%
  arrange(variable)

# Save to our cleaned data folder
write.csv(CCSI_rel_data, here("data", "processed", "CCSI_v5_rel_data.csv"), row.names = FALSE)
write.csv(question_map, here("data", "processed", "CCSI_v5_question_map.csv"), row.names = FALSE)
