library(here)
source(here("scripts", "2_reliability_checks.R"))
library(glue)
library(lavaan)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Create indices for meritocracy, stem id, and growth mindset
ccsi_indices <- ccsi %>%
  mutate(
    meritocratic_index = rowMeans(meritocratic_items, na.rm = TRUE),
    stem_id_index = rowMeans(stem_identity_items, na.rm = TRUE),
    growth_mindset_index = rowMeans(growth_mindset_items, na.rm = TRUE)
  ) %>% # Creates indices by taking mean score of all responses
  select(., response_id, meritocratic_index, stem_id_index, growth_mindset_index,
         cumulative_gpa, ever_eligible_for_pell_grant, first_generation_student,
         gender, gender_description)

# Create full descriptive table
desc_df <- ccsi_indices %>% # Create grouped dataframe with factor labels
  mutate(
    pell_group       = factor(if_else(ever_eligible_for_pell_grant==1,"Pell","Non-Pell")),
    generation_group = factor(if_else(first_generation_student==1,"First Generation","Later Generation")),
    gender_group     = factor(gender_description,levels=c("Female","Male"))
  )

make_desc <- function(df,grp_var,label){
  df %>%
    filter(!is.na({{grp_var}})) %>% # Drop rows where grouping is NA
    group_by({{grp_var}}) %>%
    summarise(
      N = n(),
      GPA_Mean = round(mean(cumulative_gpa, na.rm = TRUE), 2),
      GPA_Median = round(median(cumulative_gpa, na.rm = TRUE), 2),
      GPA_SD = round(sd(cumulative_gpa, na.rm = TRUE), 2),
      Merit_Mean = round(mean(meritocratic_index, na.rm = TRUE), 2),
      Merit_Median = round(median(meritocratic_index, na.rm= TRUE), 2),
      Merit_SD = round(sd(meritocratic_index, na.rm = TRUE), 2),
      STEM_ID_Mean = round(mean(stem_id_index, na.rm = TRUE), 2),
      STEM_ID_Median = round(median(stem_id_index, na.rm = TRUE), 2),
      STEM_ID_SD = round(sd(stem_id_index, na.rm = TRUE), 2),
      Growth_Mean = round(mean(growth_mindset_index, na.rm = TRUE), 2),
      Growth_Median = round(median(growth_mindset_index, na.rm = TRUE), 2),
      Growth_SD = round(sd(growth_mindset_index, na.rm = TRUE), 2),
      .groups="drop"
    ) %>%
    mutate(Grouping=label,.before=1) %>%
    rename(Group={{grp_var}})
}

desc_table <- bind_rows( # Creates descriptive table
  make_desc(desc_df,pell_group,"Pell"),
  make_desc(desc_df,generation_group,"Generation"),
  make_desc(desc_df,gender_group,"Gender")
)

write_csv(desc_table, here("outputs","descriptives_by_group.csv"))