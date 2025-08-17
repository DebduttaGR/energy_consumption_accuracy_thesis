# analyze_FDP.R
# Usage: edit file_in to point to your FDP CSV, then run in RStudio or Rscript.

library(tidyverse)
library(rstatix)
library(effsize)
library(ggpubr)

# ----- USER: path to CSV -----
file_in <- "/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/experiment-runner/examples/uc1/experiments/flight_delay_two_phase_ssh/run_table.csv"  # CHANGE to your CSV path

dat <- read_csv(file_in, col_types = cols())
dat  <- dat  %>% filter(!is.na(energy_j) & energy_j >= 0)

# ensure expected columns exist
required_cols <- c("__run_id","threshold","energy_j","runtime_s","accuracy","invocation_rate")
stopifnot(all(required_cols %in% names(dat)))

# parse repetition id from __run_id (expects 'repetition_N' in id)
dat <- dat %>%
  mutate(rep = str_extract('__run_id', "repetition_\\d+") %>% str_remove("repetition_") %>% as.integer()) %>%
  group_by(threshold) %>% mutate(n_in_group = n()) %>% ungroup()

# fallback if rep is NA: assign sequential repetition id within threshold
if (any(is.na(dat$rep))) {
  dat <- dat %>%
    group_by(threshold) %>%
    mutate(rep = row_number()-1) %>% ungroup()
  warning("Some __run_id values lacked 'repetition_N'; assigned rep by order within threshold.")
}

# convert threshold to factor for plotting
dat <- dat %>% mutate(thr_f = as.factor(threshold))

# Metrics to analyze
metrics <- c("energy_j", "runtime_s","accuracy","invocation_rate")

# 1) Descriptive stats per threshold
desc <- dat %>%
  pivot_longer(cols = all_of(metrics), names_to = "metric", values_to = "value") %>%
  group_by(metric, threshold) %>%
  summarise(n = n(),
            mean = mean(value, na.rm=TRUE),
            median = median(value, na.rm=TRUE),
            sd = sd(value, na.rm=TRUE),
            min = min(value, na.rm=TRUE),
            max = max(value, na.rm=TRUE)) %>%
  arrange(metric, threshold)

write_csv(desc, "descriptive_FDP.csv")
message("Wrote descriptive_FDP.csv")

# 2) Boxplots + QQ plots per metric
metrics <- c("energy_j", "runtime_s")
x_labels <- c(
  energy_j      = "Energy (J)",
  runtime_s     = "Runtime (ms)",
  accuracy   = "Accuracy"
  #accuracy_12h  = "Accuracy (12hr) class",
  #accuracy_24h  = "Accuracy (24hr) class"
)

for (m in metrics) {
  pbox <- ggplot(dat, aes(x = thr_f, y = .data[[m]], fill = thr_f)) +
    geom_boxplot(alpha=0.9, outlier.shape = 21, outlier.size = 1) +
    labs(title = paste0("FDP: ", m, " by threshold"), x = "Gating threshold", y = x_labels[[m]]) +
    theme_minimal() + theme(legend.position = "none")
  
  ggsave(paste0("boxplot_", m, "_FDP.png"), pbox, width=8, height=5, dpi=300)
  
  # QQ plots: facet per threshold
  pqq <- ggplot(dat, aes(sample = .data[[m]])) +
    stat_qq() + stat_qq_line() +
    facet_wrap(~ thr_f, nrow = 1, scales = "free") +
    labs(title = paste0("FDP: QQ plots of ", x_labels[[m]], " by gating threshold")) +
    theme_minimal() + theme(legend.position = "none")
  ggsave(paste0("qqplot_", m, "_FDP.png"), pqq, width=14, height=3.5, dpi=300)
}

library(tidyverse)

# assume 'dat' is already loaded and filtered (energy >= 0 etc)
selected_metrics <- c("energy_j", "runtime_s")
baseline_thr <- 1.0

shapiro_results <- list()

for (m in selected_metrics) {
  # make wide table: rows = rep, cols = threshold
  wide <- dat %>%
    select(rep, threshold, value = .data[[m]]) %>%
    pivot_wider(names_from = threshold, values_from = value, names_prefix = "thr_")
  
  thr_values <- sort(unique(dat$threshold))
  thr_values <- thr_values[thr_values != baseline_thr]
  
  for (thr in thr_values) {
    baseline_col <- paste0("thr_", baseline_thr)
    thr_col <- paste0("thr_", thr)
    # skip if column missing
    if (!(baseline_col %in% names(wide)) || !(thr_col %in% names(wide))) next
    
    df_pair <- wide %>%
      select(rep, baseline = !!sym(baseline_col), test = !!sym(thr_col)) %>%
      drop_na()
    diff_vec <- df_pair$test - df_pair$baseline
    n_non_na <- length(diff_vec)
    n_non_na_nonconst <- sum(!is.na(diff_vec))
    sd_diff <- ifelse(n_non_na > 0, sd(diff_vec, na.rm = TRUE), NA_real_)
    
    if (n_non_na < 3) {
      pval <- NA_real_; reason <- "n<3"
    } else if (is.na(sd_diff) || sd_diff == 0) {
      pval <- NA_real_; reason <- "zero_variance"
    } else {
      # safe call to base shapiro.test
      st <- tryCatch(shapiro.test(diff_vec), error = function(e) NULL)
      if (is.null(st)) {
        pval <- NA_real_; reason <- "shapiro_error"
      } else {
        pval <- st$p.value; reason <- NA_character_
      }
    }
    
    shapiro_results[[length(shapiro_results) + 1]] <- tibble(
      metric = m,
      threshold = thr,
      n_pairs = nrow(df_pair),
      sd_diff = sd_diff,
      shapiro_p = pval,
      reason = reason
    )
  }
}

shapiro_df <- bind_rows(shapiro_results)
print(shapiro_df)
# optionally write out
write_csv(shapiro_df, "shapiro_checks_FDP.csv")
message("Wrote shapiro_checks_FDP.csv")


metrics <- c("energy_j", "runtime_s", "accuracy","invocation_rate")

# 3) Statistical testing: per metric, compare each threshold to baseline (threshold == 1.0)
baseline_thr <- 1.0
res_list <- list()

for (m in metrics) {
  # build wide table: rows = rep, cols = threshold
  wide <- dat %>%
    select(rep, threshold, value = .data[[m]]) %>%
    pivot_wider(names_from = threshold, values_from = value, names_prefix = "thr_")
  
  # for each non-baseline threshold perform tests paired with baseline
  thr_values <- sort(unique(dat$threshold))
  thr_values <- thr_values[thr_values != baseline_thr]
  
  for (thr in thr_values) {
    baseline_col <- paste0("thr_", baseline_thr)
    thr_col <- paste0("thr_", thr)
    if (!(baseline_col %in% names(wide)) || !(thr_col %in% names(wide))) {
      next
    }
    df_pair <- wide %>% select(rep, baseline = !!sym(baseline_col), test = !!sym(thr_col)) %>% drop_na()
    # normality of differences
    diff_vec <- df_pair$test - df_pair$baseline
    
    sw <- tryCatch(shapiro_test(diff_vec), error = function(e) tibble(statistic = NA_real_, p = NA_real_))
    # choose Wilcoxon because often non-normal
    wil <- tryCatch(wilcox.test(df_pair$test, df_pair$baseline, paired = TRUE, exact = FALSE), error = function(e) NULL)
    
    # cliff's delta (effect)
    cl <- tryCatch(cliff.delta(df_pair$test, df_pair$baseline), error = function(e) NULL)
    
    row <- tibble(metric = m, threshold = thr,
                  n_pairs = nrow(df_pair),
                  shapiro_stat = sw$statistic, shapiro_p = sw$p,
                  wilcox_V = ifelse(!is.null(wil), wil$statistic, NA_real_),
                  wilcox_p = ifelse(!is.null(wil), wil$p.value, NA_real_),
                  cliffs_d = ifelse(!is.null(cl), cl$estimate, NA_real_),
                  cliffs_mag = ifelse(!is.null(cl), cl$magnitude, NA_character_))
    res_list[[length(res_list)+1]] <- row
  }
}

res_df <- bind_rows(res_list)

# Holm correction per metric (adjust p-values across thresholds for each metric)
res_df <- res_df %>%
  group_by(metric) %>%
  mutate(p_holm = p.adjust(wilcox_p, method = "holm"),
         sig_holm = ifelse(!is.na(p_holm) & p_holm < 0.05, TRUE, FALSE)) %>%
  ungroup()

write_csv(res_df, "summary_tests_FDP.csv")
message("Wrote summary_tests_FDP.csv")

# 4) Correlations: Spearman between threshold and metric and between invocation_rate and metric
# (treat threshold numeric)
cors <- map_dfr(metrics, function(m) {
  test <- cor.test(dat[[m]], dat$threshold, method = "spearman")
  tibble(metric = m, rho = as.numeric(test$estimate), p = test$p.value)
})
write_csv(cors, "spearman_FDP.csv")
message("Wrote spearman_FDP.csv")

# Done
message("FDP analysis complete. Check CSVs and PNGs.")
