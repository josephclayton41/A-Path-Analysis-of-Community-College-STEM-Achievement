library(here)
source(here("scripts", "1_load_and_clean.R"))

items_gm  <- c('q7_3','q7_5','q8_3','q10_2','q10_3')
items_sid <- c('q3_1','q3_5','q4_4','q5_2','q5_4','q5_5','q9_1','q9_3','q10_5')
items_mb  <- c('q5_1','q5_3','q6_1','q6_2','q7_1','q8_1','q8_4')

make_likert_plot <- function(items, data, title = "") {
  pal <- c("indianred4", "indianred", "rosybrown1",   
           "#b8e3a6", "#6ece7e", "#279d47")
  
  data %>% 
    select(all_of(items)) %>% 
    pivot_longer(everything(),
                 names_to  = "item",
                 values_to = "rating") %>% 
    filter(!is.na(rating)) %>% 
    mutate(rating = factor(rating, levels = 1:6,
                           labels = c("Strongly Disagree","Disagree",
                                      "Somewhat Disagree","Somewhat Agree",
                                      "Agree","Strongly Agree")),
           item = factor(item, levels = items)) %>%        
    count(item, rating) %>% # Tally responses 
    group_by(item) %>% 
    mutate(pct = n / sum(n)) %>% ungroup() %>% # % within item
    ggplot(aes(x = item, y = pct, fill = rating)) +
    geom_col() +  # One bar per item
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = pal, name = "Rating") +
    labs(title = title, x = NULL, y = "% of responses",
         fill = "Rating") +
    coord_flip() +                                        
    theme_minimal() +
    theme(legend.position = "bottom")
}

gm_plot <- make_likert_plot(items_gm,  CCSI_rel_data, "Growth Mindset Items")

sid_plot <- make_likert_plot(items_sid, CCSI_rel_data, "STEM Identity Items")

mb_plot  <- make_likert_plot(items_mb,  CCSI_rel_data, "Meritocratic Belief Items")

plot_list <- list(
  "Growth Mindset" = gm_plot,
  "STEM Identity" = sid_plot,
  "Meritocratic Beliefs" = mb_plot
)

for (subdir in names(plot_list)) {
  dir_path <- here("outputs", subdir)
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  
  file_path <- file.path(
    dir_path,
    paste0(gsub(" ", "_", tolower(subdir)), "_likert.png")
  )
  
  ggsave(filename = file_path,
         plot = plot_list[[subdir]],
         dpi = 400)
}

# Histogram of cumulative gpa
gpa_plot <- ggplot(ccsi, aes(x = cumulative_gpa)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", boundary = 0) +
  scale_x_continuous(
    breaks = seq(0, 4, 0.2),
    limits = c(0, 4),
    expand = c(0, 0)
  ) +
  labs(
    title = "Distribution of Cumulative GPA",
    x = "Cumulative GPA",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave(filename = here("outputs", "gpa_distribution.png"), plot = gpa_plot,
       dpi = 400)