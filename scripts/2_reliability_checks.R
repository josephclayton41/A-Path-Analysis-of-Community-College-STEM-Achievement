library(psych)
library(here)
library(ggcorrplot)
source(here("scripts", "1_load_and_clean.R"))

# Load data
ccsi <- read_csv(here("data", "processed", "CCSI_v5_rel_data.csv"))

# Extract Cronbach's alpha values and store them in a tibble

safe_alpha <- function(df) {
  suppressWarnings({
    result <- psych::alpha(df)
  })
  result$total$raw_alpha
}

alpha_table <- tibble(
  Construct = c("Meritocratic Beliefs", "STEM Identity", "Growth Mindset"),
  Alpha = c(
    safe_alpha(meritocratic_items),
    safe_alpha(stem_identity_items),
    safe_alpha(growth_mindset_items)
  )
)

# Plot cronbach's alpha scores
ggplot(alpha_table, aes(x = reorder(Construct, Alpha), y = Alpha)) +
  geom_col(fill = "deeppink4") +
  geom_text(aes(label = round(Alpha, 2)), vjust = -0.5, size = 5) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Cronbach's Alpha for Construct Reliability",
    x = NULL,
    y = "Cronbach's Alpha"
  ) +
  theme_minimal()

# Save to files
ggsave(here("outputs", "cronbach_alpha_plot.png"), dpi = 400)

# Create correlation matrices
cor_meritocratic <- cor(meritocratic_items, use = "pairwise.complete.obs")
cor_stem_identity <- cor(stem_identity_items, use = "pairwise.complete.obs")
cor_growth_mindset <- cor(growth_mindset_items, use = "pairwise.complete.obs")

# Generate and save heatmaps
ggsave(
  filename = here("outputs", "Meritocratic Beliefs", "meritocratic_heatmap.png"),
  plot = ggcorrplot(cor_meritocratic, type = "lower", lab = TRUE) +
    ggtitle("Meritocratic Beliefs: Inter-Item Correlations"),
  dpi = 400)

ggsave(
  filename = here("outputs", "STEM Identity", "stem_identity_heatmap.png"),
  plot = ggcorrplot(cor_stem_identity, type = "lower", lab = TRUE) +
    ggtitle("STEM Identity: Inter-Item Correlations"),
  dpi = 400)

ggsave(
  filename = here("outputs", "Growth Mindset", "growth_mindset_heatmap.png"),
  plot = ggcorrplot(cor_growth_mindset, type = "lower", lab = TRUE) +
    ggtitle("Growth Mindset: Inter-Item Correlations"),
  dpi = 400)
