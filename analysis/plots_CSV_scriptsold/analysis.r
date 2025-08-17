# Statistical Analysis Script for FDP and PDTM Experiment

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(car)
library(psych)

# 1. Data Import -----------------------------------------------------------
# Adjust file paths as needed
df_fdp <- read.csv("/Users/debduttaguharoy/Developer/Y2 - Master's Thesis/Usecases/experiment-runner/examples/uc1/experiments/flight_delay_two_phase_ssh/run_table.csv")  # Flight Delay Prediction
df_pdtm <- read.csv("/Users/debduttaguharoy/Developer/Y2 - Master's Thesis/Usecases/experiment-runner/examples/uc2/experiments/predicting_death_time_and_mortality_ssh/run_table.csv")  # Predicting Death Time and Mortality

if (!dir.exists("plots")) dir.create("plots") #Creating a plots folder to store all plots

# 2. Descriptive Statistics -----------------------------------------------
# Function to compute mean, median, sd, and 95% CI for a given numeric vector
describe_with_ci <- function(x, conf.level = 0.95) {
  desc <- describe(x)
  se <- sd(x, na.rm=TRUE) / sqrt(length(na.omit(x)))
  alpha <- 1 - conf.level
  ci_mult <- qt(1 - alpha/2, df = length(na.omit(x)) - 1)
  ci_lower <- mean(x, na.rm=TRUE) - ci_mult * se
  ci_upper <- mean(x, na.rm=TRUE) + ci_mult * se
  data.frame(
    mean = desc$mean,
    median = desc$median,
    sd = desc$sd,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

# Descriptives for FDP
desc_fdp <- df_fdp %>%
  select(energy_j, runtime_s, accuracy, invocation_rate) %>%
  summarise_all(~ list(describe_with_ci(.))) %>%
  unnest(cols = everything(), names_sep = "_")

View(desc_fdp)

# Descriptives for PDTM by window
desc_pdtm_global <- df_pdtm %>% 
  select(energy_j, runtime_s, invocation_rate) %>% 
  summarise_all(~ list(describe_with_ci(.))) %>% 
  # unpack into one table with a metric column
  tidyr::pivot_longer(everything(), names_to="metric", values_to="descr") %>% 
  unnest(descr)

View(desc_pdtm_global)
# 1. Point‐and‐error‐bar plot for global metrics

# desc_pdtm_global should have columns:
#   metric, mean, sd, ci_lower, ci_upper
ggplot(desc_pdtm_global, aes(x = metric, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(
    title = "PDTM Global Metrics (95% CI)",
    x = "Metric",
    y = "Value"
  ) +
  theme_minimal()

desc_pdtm_by_window <- df_pdtm %>% 
  pivot_longer(
    cols = matches("^(roc_auc|accuracy|f1_macro)_\\d+h$"),
    names_to  = c("metric", "window"),
    names_sep = "_"
  ) %>% 
  group_by(window, metric) %>% 
  summarise(descr = list(describe_with_ci(value)), .groups="drop") %>% 
  unnest(descr)

View(desc_pdtm_by_window)

# 2. Faceted point‐and‐error‐bar plot for windowed metrics

# desc_pdtm_by_window should have:
#   window  (e.g. "6h","12h","24h"),
#   metric  (e.g. "accuracy","roc_auc","f1_macro"),
#   mean, ci_lower, ci_upper
ggplot(desc_pdtm_by_window, aes(x = window, y = mean, group = 1)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    title = "PDTM Windowed Metrics by Time Window",
    x = "Prediction Window",
    y = "Mean ± 95% CI"
  ) +
  theme_minimal()

# 2. Energy by threshold
p_energy <- ggplot(df_pdtm, aes(x = factor(threshold), y = energy_j)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "PDTM: Energy Consumption by Threshold",
    x     = "Threshold Value",
    y     = "Energy (Joules)"
  ) +
  theme_minimal()

ggsave("plots/energy_by_threshold.png", p_energy, width = 8, height = 6)

# 3. Runtime by threshold (milliseconds)
df_pdtm <- df_pdtm %>%
  mutate(runtime_ms = runtime_s * 1000)  # convert seconds → ms

p_runtime <- ggplot(df_pdtm, aes(x = factor(threshold), y = runtime_ms)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "PDTM: Execution Time by Threshold",
    x     = "Threshold Value",
    y     = "Runtime (ms)"
  ) +
  theme_minimal()

ggsave("plots/runtime_by_threshold.png", p_runtime, width = 8, height = 6)

# 4. Accuracy by window (percentage)
df_acc <- df_pdtm %>%
  pivot_longer(
    cols = matches("^accuracy_\\d+h$"),
    names_to  = "window",
    names_prefix = "accuracy_",
    values_to = "accuracy"
  ) %>%
  mutate(accuracy_pct = accuracy * 100)  # convert [0–1] → %

p_accuracy <- ggplot(df_acc, aes(x = window, y = accuracy_pct)) +
  geom_boxplot(fill = "lightcoral") +
  labs(
    title = "PDTM: Accuracy by Prediction Window",
    x     = "Prediction Window",
    y     = "Accuracy (%)"
  ) +
  theme_minimal()

ggsave("plots/accuracy_by_window.png", p_accuracy, width = 8, height = 6)


# 3. Hypothesis Testing ---------------------------------------------------

# 3.1 Paired Tests: two-phase thresholds vs baseline
perform_paired_tests <- function(data, var) {
  baseline <- data %>% filter(threshold == 1) %>% pull({{var}})
  results <- list()
  for(t in c(0.1,0.3,0.5,0.7)){
    sample_t <- data %>% filter(threshold == t) %>% pull({{var}})
    # Normality check
    if(shapiro.test(sample_t - baseline)$p.value > 0.05) {
      test <- t.test(sample_t, baseline, paired = TRUE)
    } else {
      test <- wilcox.test(sample_t, baseline, paired = TRUE)
    }
    results[[as.character(t)]] <- tidy(test)
  }
  bind_rows(results, .id = "threshold")
}

# Apply to FDP
tests_fdp_energy <- perform_paired_tests(df_fdp, energy)
tests_fdp_accuracy <- perform_paired_tests(df_fdp, accuracy)

# Apply to PDTM for each window
tests_pdtm <- df_pdtm %>%
  group_by(window) %>%
  do(
    energy  = perform_paired_tests(., energy) %>% mutate(metric = "energy"),
    accuracy = perform_paired_tests(., accuracy) %>% mutate(metric = "accuracy")
  ) %>%
  unnest(cols = c(energy, accuracy))

# 3.2 Correlation: invocation rate vs energy and accuracy
data_cor_fdp <- df_fdp %>% select(invocation_rate, energy, accuracy)
cor_energy <- cor.test(data_cor_fdp$invocation_rate, data_cor_fdp$energy, method = ifelse(shapiro.test(data_cor_fdp$invocation_rate)$p.value>0.05,"pearson","spearman"))
cor_accuracy <- cor.test(data_cor_fdp$invocation_rate, data_cor_fdp$accuracy, method = ifelse(shapiro.test(data_cor_fdp$invocation_rate)$p.value>0.05,"pearson","spearman"))

# 3.3 ANOVA: multi-factor effects (threshold × use case)
df_fdp$usecase <- "FDP"
df_pdtm$usecase <- paste0("PDTM_", df_pdtm$window)

df_combined <- bind_rows(
  df_fdp %>% select(energy, accuracy, invocation_rate, threshold, usecase),
  df_pdtm %>% select(energy, accuracy, invocation_rate, threshold, usecase)
)

anova_energy <- aov(energy ~ threshold * usecase, data = df_combined)
anova_accuracy <- aov(accuracy ~ threshold * usecase, data = df_combined)

# 4. Visualizations --------------------------------------------------------

# 4.1 Boxplots across thresholds
ggplot(df_combined, aes(factor(threshold), energy)) + geom_boxplot() +
  labs(title = "Energy by Threshold", x = "Threshold", y = "Energy")

ggplot(df_combined, aes(factor(threshold), accuracy)) + geom_boxplot() +
  labs(title = "Accuracy by Threshold", x = "Threshold", y = "Accuracy")

# 4.2 Trade-off curves: accuracy vs energy
tradeoff <- df_combined %>%
  group_by(threshold, usecase) %>%
  summarise(mean_energy = mean(energy), mean_acc = mean(accuracy),
            se_energy = sd(energy)/sqrt(n()), se_acc = sd(accuracy)/sqrt(n()))

ggplot(tradeoff, aes(mean_energy, mean_acc, color = factor(threshold))) +
  geom_line() + geom_point() +
  geom_ribbon(aes(x = mean_energy, ymin = mean_acc - 1.96*se_acc, ymax = mean_acc + 1.96*se_acc), alpha = 0.2) +
  facet_wrap(~usecase) +
  labs(title = "Accuracy vs Energy Trade-off", x = "Mean Energy", y = "Mean Accuracy")

# 4.3 Scatter plots: invocation vs energy/accuracy
ggplot(df_combined, aes(invocation_rate, energy)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Invocation Rate vs Energy")

ggplot(df_combined, aes(invocation_rate, accuracy)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Invocation Rate vs Accuracy")

# End of Script
