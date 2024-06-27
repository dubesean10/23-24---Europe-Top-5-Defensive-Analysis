library(tidyverse)
library(ggrepel)
library(broom)

## DEFENSE ANALYSIS FOR ALL TEAMS IN BIG 5 EUROPEAN LEAGUES


# Perform correlation analysis
correlation_matrix <- cor(completeBig5EuropeStats[, c("L", "Tackles", "Tackles Won", "Defensive Third Tackles",
                                          "Midfield Third Tackles", "Attacking Third Tackles",
                                          "Tackles vs Dribbles", "Dribbled Past", "Pressures",
                                          "Blocks", "Interceptions", "Tackles + Interceptions",
                                          "Clearances")])

# Print correlation matrix
print(correlation_matrix)

# Perform mulitple linear regression regression analysis
model <- lm(L ~ Tackles + `Tackles Won` + `Defensive Third Tackles` +
              `Midfield Third Tackles` + `Attacking Third Tackles` +
              `Tackles vs Dribbles` + `Dribbled Past` + Pressures +
              Blocks + Interceptions + `Tackles + Interceptions` +
              Clearances, data = completeBig5EuropeStats)


# Extract the model coefficients and confidence intervals, removing the intercept
tidy_model <- tidy(model, conf.int = TRUE) %>% filter(term != "(Intercept)")

# Ensure the terms are factors for better plotting
tidy_model$term <- factor(tidy_model$term, levels = tidy_model$term)

# Determine the range for the y-axis based on the estimates and confidence intervals
y_min <- -0.75
y_max <- 0.75

# Plot the coefficients with confidence intervals

tidy_model_plot <- ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Coefficients of Defensive Statistics in Predicting Matches Lost",
       x = "Defensive Statistic",
       y = "Coefficient Estimate") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(y_min, y_max))




################################################################################################################
## PLOTS ##
################################################################################################################

# Scatter plot for Tackles vs. Matches Lost with squad names

ggplot(completeBig5EuropeStats, aes(x = Tackles, y = L, label = Squad)) +
  geom_text(hjust = 0, vjust = 0.5, size = 3) +  # Add text labels for squad names
  labs(x = "Tackles", y = "Matches Lost") +
  ggtitle("Scatter Plot of Tackles vs. Matches Lost with Squad Names")


# Similar plots can be created for other defensive statistics

library(ggplot2)

# Scatter plot for Tackles vs. Matches Lost with squad names and different colors
ggplot(completeBig5EuropeStats, aes(x = Tackles, y = L)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = Squad), position = position_nudge(x = 0.1, y = 0.1), size = 5) + 
  labs(x = "Tackles", y = "Matches Lost") +
  ggtitle("Scatter Plot of Tackles vs. Matches Lost with Squad Names and Different Colors")



# Faceted plot for defensive statistics vs. matches lost
ggplot(completeBig5EuropeStats, aes(x = L)) +
  geom_point(aes(y = `Tackles Won`)) +
  labs(x = "Matches Lost", y = "Tackles Won") +
  ggtitle("Tackles Won vs. Matches Lost") +
  facet_wrap(~ "Tackles Won", nrow = 2) +
  theme_minimal()

ggplot(completeBig5EuropeStats, aes(x = L)) +
  geom_point(aes(y = `Tackles + Interceptions`)) +
  labs(x = "Matches Lost", y = "Tackles + Interceptions") +
  ggtitle("Tackles + Interceptions vs. Matches Lost") +
  facet_wrap(~ "Tackles + Interceptions", nrow = 2) +
  theme_minimal()

ggplot(completeBig5EuropeStats, aes(x = L)) +
  geom_point(aes(y = `Dribbled Past`)) +
  labs(x = "Matches Lost", y = "Dribbled Past") +
  ggtitle("Dribbled Past vs. Matches Lost") +
  facet_wrap(~ "Dribbled Past", nrow = 2) +
  theme_minimal()

ggplot(completeBig5EuropeStats, aes(x = L)) +
  geom_point(aes(y = `Clearances`)) +
  labs(x = "Matches Lost", y = "Clearances") +
  ggtitle("Clearances vs. Matches Lost") +
  facet_wrap(~ "Clearances", nrow = 2) +
  theme_minimal()


# Combine all the variables into a long format for faceting
long_stats <- completeBig5EuropeStats %>%
  pivot_longer(cols = c(`Tackles Won`, `Tackles + Interceptions`, `Dribbled Past`, `Clearances`),
               names_to = "Statistic", values_to = "Value")

# Faceted plot with squad names instead of dots and trend lines
ggplot(long_stats, aes(x = L, y = Value)) +
  geom_point(alpha = 0) + # Make points transparent
  geom_text_repel(aes(label = Squad), size = 3, max.overlaps = 100) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Matches Lost", y = "") +
  ggtitle("Defensive Statistics vs. Matches Lost") +
  facet_wrap(~ Statistic, scales = "free_y", ncol = 2) +
  theme_minimal() +
  theme(legend.position = "none")


####

# Plot for Tackles Won
ggplot(completeBig5EuropeStats, aes(x = L, y = `Tackles Won`)) +
  geom_point(alpha = 0) + # Make points transparent
  geom_text_repel(aes(label = Squad), size = 15, max.overlaps = 100) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Matches Lost", y = "Tackles Won", size = 20) +
  ggtitle("Tackles Won vs. Matches Lost") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 30))

# Plot for Tackles + Interceptions
ggplot(completeBig5EuropeStats, aes(x = L, y = `Tackles + Interceptions`)) +
  geom_point(alpha = 0) + # Make points transparent
  geom_text_repel(aes(label = Squad), size = 3, max.overlaps = 100) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Matches Lost", y = "Tackles + Interceptions") +
  ggtitle("Tackles + Interceptions vs. Matches Lost") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot for Dribbled Past
ggplot(completeBig5EuropeStats, aes(x = L, y = `Dribbled Past`)) +
  geom_point(alpha = 0) + # Make points transparent
  geom_text_repel(aes(label = Squad), size = 3, max.overlaps = 100) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Matches Lost", y = "Dribbled Past") +
  ggtitle("Dribbled Past vs. Matches Lost") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot for Clearances
ggplot(completeBig5EuropeStats, aes(x = L, y = `Clearances`)) +
  geom_point(alpha = 0) + # Make points transparent
  geom_text_repel(aes(label = Squad), size = 3, max.overlaps = 100) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Matches Lost", y = "Clearances") +
  ggtitle("Clearances vs. Matches Lost") +
  theme_minimal() +
  theme(legend.position = "none")


