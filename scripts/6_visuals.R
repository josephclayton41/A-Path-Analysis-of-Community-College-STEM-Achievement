library(here)
source(here("scripts", "2_reliability_checks.R"))
source(here("scripts", "5_models.R"))

# Extract standardized coefficients for visualization
std_est_pell <- standardizedSolution(fit_simple_pell) %>%
  filter(lhs == "cumulative_gpa" & op == "~") %>%
  mutate(
    Group = ifelse(group == 1, "Pell", "Non-Pell"),
    Predictor = recode(rhs,
                       "meritocratic_index" = "Meritocratic Beliefs",
                       "stem_id_index" = "STEM Identity",
                       "growth_mindset_index" = "Growth Mindset"
    )
  ) # Pell

std_est_firstgen <- standardizedSolution(fit_simple_firstgen) %>%
  filter(lhs == "cumulative_gpa" & op == "~") %>%
  mutate(
    Group = ifelse(group == 1, "First Generation", "Later Generation"),
    Predictor = recode(rhs,
                       "meritocratic_index" = "Meritocratic Beliefs",
                       "stem_id_index" = "STEM Identity",
                       "growth_mindset_index" = "Growth Mindset"
    )
  ) # Generation

std_est_gender <- standardizedSolution(fit_simple_gender) %>%
  filter(lhs == "cumulative_gpa" & op == "~") %>%
  mutate(
    Group = factor(group, levels = c(1, 2), labels = c("Female", "Male")),
    Predictor = recode(rhs,
                       "meritocratic_index" = "Meritocratic Beliefs",
                       "stem_id_index" = "STEM Identity",
                       "growth_mindset_index" = "Growth Mindset"
    )
  ) # Gender

# Creates subgroup output directories for Pell, Generation, and Gender
for(dir in c("Pell","Generation","Gender")) {
  dir.create(here("outputs", dir), showWarnings = FALSE, recursive = TRUE)
}

# Create bar plot of standardized coefficients by group
ggplot(std_est_pell, aes(x = Predictor, y = est.std, fill = Group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = round(est.std, 2),
        y = est.std + ifelse(est.std >= 0, 0.03, -0.03)),
    position = position_dodge(width = 0.8),
    vjust = 0, size = 4
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Standardized Effects on GPA by Pell Group",
    y = "Standardized Coefficient", x = NULL
  ) +
  scale_fill_manual(values = c("Pell" = "forestgreen", "Non-Pell" = "maroon")) +
  theme_minimal()

ggsave(
  filename = here("outputs", "Pell", "gpa_predictors_by_pell_group.png"),
  dpi = 400) # Pell

ggplot(std_est_firstgen, aes(x = Predictor, y = est.std, fill = Group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = round(est.std, 2),
        y = est.std + ifelse(est.std >= 0, 0.03, -0.03)),
    position = position_dodge(width = 0.8),
    vjust = 0, size = 4
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Standardized Effects on GPA by Generation Group",
    y = "Standardized Coefficient", x = NULL
  ) +
  scale_fill_manual(values = c("First Generation" = "darkgoldenrod2", "Later Generation" = "deepskyblue3")) +
  theme_minimal()

ggsave(
  filename = here("outputs", "Generation", "gpa_predictors_by_generation.png"),
  dpi = 400) # Generation

ggplot(std_est_gender, aes(x = Predictor, y = est.std, fill = Group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = round(est.std, 2),
        y = est.std + ifelse(est.std >= 0, 0.03, -0.03)),
    position = position_dodge(width = 0.8),
    vjust = 0, size = 4
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Standardized Effects on GPA by Gender",
    y = "Standardized Coefficient", x = NULL
  ) +
  scale_fill_manual(values = c("Female" = "lightpink", "Male" = "lightblue")) +
  theme_minimal()

ggsave(
  filename = here("outputs", "Gender", "gpa_predictors_by_gender.png"),
  dpi = 400) # Gender

# Create path diagram for each group
graph_header <- '
  graph [layout = dot, rankdir = LR, fontsize = 18]
  node [shape = box, style = "filled,rounded", fontname = Helvetica]
  edge [fontname = Helvetica]
'

make_path_graph <- function(std_df, title, fills) {
  b <- round(std_df$est.std[match(c("Meritocratic Beliefs",
                                    "STEM Identity",
                                    "Growth Mindset"),
                                  std_df$Predictor)], 3)
  
  grViz(glue('
  digraph path_diagram {{
    {graph_header}
    label = "{title}"
    meritocratic [label = "Meritocratic\\nBeliefs", fillcolor = {fills[1]}]
    stemid [label = "STEM\\nIdentity", fillcolor = {fills[1]}]
    mindset [label = "Growth\\nMindset", fillcolor = {fills[1]}]
    gpa [label = "GPA", fillcolor = {fills[2]}]

    meritocratic -> gpa [label = "β = {b[1]}" fontsize = 16]
    stemid -> gpa [label = "β = {b[2]}" fontsize = 16]
    mindset -> gpa [label = "β = {b[3]}" fontsize = 16]
  }}
  '))
}

pell_graph <- make_path_graph(std_est_pell %>% filter(Group == "Pell"), 
                              "Pell", c("forestgreen","lightgray"))
nonpell_graph <- make_path_graph(std_est_pell %>% filter(Group == "Non-Pell"),
                                 "Non-Pell", c("maroon", "lightgray"))
firstgen_graph <- make_path_graph(std_est_firstgen %>% filter(Group=="First Generation"),
                                  "First Generation", c("darkgoldenrod2","lightgray"))
latergen_graph <- make_path_graph(std_est_firstgen %>% filter(Group=="Later Generation"),
                                  "Later Generation", c("deepskyblue3","lightgray"))
female_graph <- make_path_graph(std_est_gender   %>% filter(Group=="Female"),
                                "Female", c("lightpink","lightgray"))
male_graph <- make_path_graph(std_est_gender %>% filter(Group=="Male"),"Male",
                              c("lightblue","lightgray"))

# Save diagrams as PNGs to outputs folder
pell_svg <- export_svg(pell_graph)
rsvg_png(charToRaw(pell_svg), here("outputs", "Pell", "pell_path_diagram.png"),
         width = 4800, height = 2700)

nonpell_svg <- export_svg(nonpell_graph)
rsvg_png(charToRaw(nonpell_svg), here("outputs", "Pell", "nonpell_path_diagram.png"),
         width = 4800, height = 2700)

firstgen_svg <- export_svg(firstgen_graph)
rsvg_png(charToRaw(firstgen_svg), here("outputs", "Generation", "firstgen_path_diagram.png"),
         width = 4800, height = 2700)

latergen_svg <- export_svg(latergen_graph)
rsvg_png(charToRaw(latergen_svg), here("outputs", "Generation", "latergen_path_diagram.png"),
         width = 4800, height = 2700)

female_svg <- export_svg(female_graph)
rsvg_png(charToRaw(female_svg), here("outputs", "Gender", "female_path_diagram.png"),
         width = 4800, height = 2700)

male_svg <- export_svg(male_graph)
rsvg_png(charToRaw(male_svg), here("outputs", "Gender", "male_path_diagram.png"),
         width = 4800, height = 2700)