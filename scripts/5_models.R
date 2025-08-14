library(here)
source(here("scripts", "2_reliability_checks.R"))
source(here("scripts", "4_data_prep.R"))

# Create df for pell model
ccsi_model_pell <- ccsi_indices %>%
  filter(
    !is.na(meritocratic_index),
    !is.na(stem_id_index),
    !is.na(growth_mindset_index),
    !is.na(cumulative_gpa),
    !is.na(ever_eligible_for_pell_grant)
  ) %>% # Ensures there are no NA values which could cause issues
  mutate(pell_group = factor(if_else(
    ever_eligible_for_pell_grant == 1, "Pell", "Non-Pell"))
  ) %>% # Splits by pell eligibility
  select(-c(ever_eligible_for_pell_grant, first_generation_student, gender, 
            response_id, gender_description))

# Create df for first-gen model
ccsi_model_firstgen <- ccsi_indices %>%
  filter(
    !is.na(meritocratic_index),
    !is.na(stem_id_index),
    !is.na(growth_mindset_index),
    !is.na(cumulative_gpa),
    !is.na(first_generation_student)
  ) %>% # Ensures there are no NA values which could cause issues
  mutate(generation_group = factor(if_else(
    first_generation_student == 1, "First Generation", "Later Generation"))
  ) %>% # Splits by genderation
  select(-c(ever_eligible_for_pell_grant, first_generation_student, gender, 
            response_id, gender_description))

# Create df for gender model
ccsi_model_gender <- ccsi_indices %>%
  filter(
    !is.na(meritocratic_index),
    !is.na(stem_id_index),
    !is.na(growth_mindset_index),
    !is.na(cumulative_gpa),
    !is.na(gender),
    gender_description %in% c("Female","Male")
  ) %>% # Ensures there are no NA values which could cause issues
  select(-c(ever_eligible_for_pell_grant, first_generation_student, gender, 
            response_id))

# Define path model
model <- 'cumulative_gpa ~ meritocratic_index + stem_id_index + growth_mindset_index'

# Fit multi-group SEM
fit_simple_pell <- sem(model, data = ccsi_model_pell, group = "pell_group", meanstructure = TRUE)
summary(fit_simple_pell, fit.measures = TRUE, standardized = TRUE) # Pell

fit_simple_firstgen <- sem(model, data = ccsi_model_firstgen, group = "generation_group", meanstructure = TRUE)
summary(fit_simple_firstgen, fit.measures = TRUE, standardized = TRUE) # Generation

fit_simple_gender <- sem(model, data = ccsi_model_gender, group = "gender_description", meanstructure = TRUE)
summary(fit_simple_gender, fit.measures = TRUE, standardized = TRUE) # Gender

# Test for moderation via constrained model comparison
fit_constrained_pell <- sem(
  model,
  data = ccsi_model_pell,
  group = "pell_group",
  group.equal = c("regressions"),
  meanstructure = TRUE
)
anova(fit_simple_pell, fit_constrained_pell) # Pell

fit_constrained_firstgen <- sem(
  model,
  data = ccsi_model_firstgen,
  group = "generation_group",
  group.equal = c("regressions"),
  meanstructure = TRUE
)
anova(fit_simple_firstgen, fit_constrained_firstgen) # Generation

fit_constrained_gender <- sem(
  model,
  data = ccsi_model_gender,
  group = "gender_description",
  group.equal = c("regressions"),
  meanstructure = TRUE
)
anova(fit_simple_gender, fit_constrained_gender) # Gender

# Compare free vs. equal-paths models to see if any grouping moderates the paths
moderation_test <- function(free_fit, constr_fit, label) {
  a <- anova(free_fit, constr_fit)                 # two-row anova table
  data.frame(
    Grouping = label,
    delta_chisq = round(a$Chisq[2] - a$Chisq[1], 3),
    delta_df = a$Df[2] - a$Df[1],
    p_value = signif(a$`Pr(>Chisq)`[2], 3),
    Sig = ifelse(a$`Pr(>Chisq)`[2] < .05, "Yes", "No")
  )
}

inv_table <- bind_rows(
  moderation_test(fit_simple_pell, fit_constrained_pell, "Pell"),
  moderation_test(fit_simple_firstgen, fit_constrained_firstgen, "Generation"),
  moderation_test(fit_simple_gender, fit_constrained_gender, "Gender")
)

write_csv(inv_table, here("outputs", "moderation_tests.csv"))

# R squared for GPA in each grouping
get_r2 <- function(fit, group_labels, grouping_name){
  r2_list <- inspect(fit, "rsquare")
  tibble(
    Grouping = grouping_name,
    Group = group_labels,
    R2 = round(sapply(r2_list, function(x) x["cumulative_gpa"]), 3)
  )
}

r2_table <- bind_rows(
  get_r2(fit_simple_pell, c("Pell","Non-Pell"), "Pell"),
  get_r2(fit_simple_firstgen, c("First Generation", "Later Generation"), "Generation"),
  get_r2(fit_simple_gender, c("Female","Male"), "Gender")
)
write_csv(r2_table, here("outputs","r2_table.csv"))

#Extract the standardized estimates, standard errors, and two-tailed p-values for each path in each group
tidy_std <- function(fit, group_labels, grouping_name) {
  standardizedSolution(fit) %>%
    filter(lhs == "cumulative_gpa", op == "~") %>%
    mutate(
      Grouping = grouping_name,
      Group = factor(group, labels = group_labels),
      Predictor = recode(rhs,
                         meritocratic_index = "Meritocratic Beliefs",
                         stem_id_index = "STEM Identity",
                         growth_mindset_index = "Growth Mindset"),
      Sig = if_else(pvalue < .05, "Yes", "No")
    ) %>%
    select(Grouping, Group, Predictor,
           beta = est.std, se, z = z, p = pvalue, Sig)
}

# Create table and save
coef_table <- bind_rows(
  tidy_std(fit_simple_pell,
           c("Pell", "Non-Pell"), "Pell eligibility"),
  tidy_std(fit_simple_firstgen,
           c("First-Gen", "Later-Gen"), "Generation status"),
  tidy_std(fit_simple_gender,
           c("Female", "Male"), "Gender")
)

write_csv(coef_table, here("outputs","path_coefficients_by_group.csv"))