# Descriptive analysis of the NHANES cognitive-aging sample
# Author: Jacob Markarian

library(dplyr)
library(ggplot2)
library(here)

# Load the cleaned dataset created by 02_clean_data.R
analysis_data <- readRDS(
  here("data", "processed", "analysis_data.rds")
)

# Ensure the output folders exist
dir.create(
  here("results"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("figures"),
  recursive = TRUE,
  showWarnings = FALSE
)

# Verify that the correct dataset loaded

cat("Rows loaded:", nrow(analysis_data), "\n")
cat("Columns loaded:", ncol(analysis_data), "\n")
# Variables used in the primary analysis
analysis_variables <- c(
  "age",
  "recreation_met_minutes",
  "log_recreation_met",
  "phq9_total",
  "cerad_learning",
  "cerad_delayed",
  "animal_fluency",
  "digit_symbol"
)

# Create descriptive statistics for each variable
summarize_variable <- function(variable_name) {
  
  values <- analysis_data[[variable_name]]
  
  data.frame(
    variable = variable_name,
    available_n = sum(!is.na(values)),
    mean = mean(values, na.rm = TRUE),
    standard_deviation = sd(values, na.rm = TRUE),
    median = median(values, na.rm = TRUE),
    first_quartile = unname(quantile(values, 0.25, na.rm = TRUE)),
    third_quartile = unname(quantile(values, 0.75, na.rm = TRUE)),
    minimum = min(values, na.rm = TRUE),
    maximum = max(values, na.rm = TRUE)
  )
}

descriptive_statistics <- bind_rows(
  lapply(analysis_variables, summarize_variable)
)

# Round results for easier reading to two decimals
descriptive_statistics[-c(1, 2)] <- round(
  descriptive_statistics[-c(1, 2)],
  digits = 2
)

# Calculate missing data
missing_data <- data.frame(
  variable = analysis_variables,
  available_n = sapply(
    analysis_variables,
    function(variable_name) {
      sum(!is.na(analysis_data[[variable_name]]))
    }
  ),
  missing_n = sapply(
    analysis_variables,
    function(variable_name) {
      sum(is.na(analysis_data[[variable_name]]))
    }
  )
)

missing_data$missing_percent <- round(
  100 * missing_data$missing_n / nrow(analysis_data),
  digits = 1
)

# Save the tables as CSV files for later easy access

write.csv(
  descriptive_statistics,
  here("results", "descriptive_statistics.csv"),
  row.names = FALSE
)

write.csv(
  missing_data,
  here("results", "missing_data.csv"),
  row.names = FALSE
)

# Display the tables in the Console within R itself

print(descriptive_statistics)
print(missing_data)

# 7. Plot distributions ------------------------------------------------

# Physical-activity is extremely skewed, need to transform via log transformation

# Physical-activity distribution after the log transformation
activity_distribution <- ggplot(
  analysis_data,
  aes(x = log_recreation_met)
) +
  geom_histogram(
    binwidth = 0.25,
    boundary = 0,
    color = "white",
    fill = "#2C7FB8"
  ) +
  labs(
    title = "Distribution of Recreational Physical Activity",
    subtitle = "NHANES participants aged 60 and older, 2011–2014",
    x = "Log(1 + recreational MET-minutes per week)",
    y = "Number of participants"
  ) +
  theme_minimal(base_size = 13)

# Depressive-symptom distribution
phq9_distribution <- ggplot(
  analysis_data,
  aes(x = phq9_total)
) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5,
    color = "white",
    fill = "#7A5195"
  ) +
  scale_x_continuous(
    breaks = seq(0, 27, by = 3)
  ) +
  labs(
    title = "Distribution of PHQ-9 Depressive-Symptom Scores",
    subtitle = "NHANES participants aged 60 and older, 2011–2014",
    x = "PHQ-9 total score",
    y = "Number of participants"
  ) +
  theme_minimal(base_size = 13)

# Save high quality versions
ggsave(
  filename = here("figures", "physical_activity_distribution.png"),
  plot = activity_distribution,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = here("figures", "phq9_distribution.png"),
  plot = phq9_distribution,
  width = 8,
  height = 5,
  dpi = 300
)

# Display the figures in R
print(activity_distribution)
print(phq9_distribution)

cat("Distribution figures saved in the figures folder.\n")

# need to make my correlation matrix for the SEM later 

# 8. Examine correlations among primary variables ----------------------

correlation_variables <- c(
  "log_recreation_met",
  "phq9_total",
  "cerad_learning",
  "cerad_delayed",
  "animal_fluency",
  "digit_symbol"
)

# Pairwise-complete correlations use all available observations
# for each individual pair of variables
correlation_matrix <- cor(
  analysis_data[correlation_variables],
  use = "pairwise.complete.obs"
)

correlation_output <- round(correlation_matrix, digits = 2)

# Save the numerical correlation table
write.csv(
  correlation_output,
  here("results", "correlation_matrix.csv"),
  row.names = TRUE
)

# Convert the matrix into a format ggplot2 can take
correlation_long <- as.data.frame(
  as.table(correlation_output)
)

names(correlation_long) <- c(
  "variable_x",
  "variable_y",
  "correlation"
)

correlation_long$variable_x <- factor(
  correlation_long$variable_x,
  levels = correlation_variables
)

correlation_long$variable_y <- factor(
  correlation_long$variable_y,
  levels = rev(correlation_variables)
)

correlation_labels <- c(
  log_recreation_met = "Physical activity",
  phq9_total = "PHQ-9",
  cerad_learning = "CERAD learning",
  cerad_delayed = "CERAD delayed",
  animal_fluency = "Animal fluency",
  digit_symbol = "Digit symbol"
)

# Create the correlation 
correlation_plot <- ggplot(
  correlation_long,
  aes(
    x = variable_x,
    y = variable_y,
    fill = correlation
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = sprintf("%.2f", correlation)),
    size = 3.5
  ) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  scale_x_discrete(labels = correlation_labels) +
  scale_y_discrete(labels = correlation_labels) +
  coord_equal() +
  labs(
    title = "Correlations Among Primary Analysis Variables",
    subtitle = "Pairwise-complete Pearson correlations",
    x = NULL,
    y = NULL,
    fill = "Correlation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = element_blank()
  )

ggsave(
  filename = here("figures", "correlation_heatmap.png"),
  plot = correlation_plot,
  width = 8,
  height = 7,
  dpi = 300
)

print(correlation_output)
print(correlation_plot)

cat("Correlation table and heatmap saved.\n")
