# --------- Setup ---------
library(dplyr)
library(tidyr)
library(ggplot2)
library(effsize)   # for cliff.delta
library(grid)      # for textGrob()
library(gridExtra) # for arrangeGrob()
# install.packages(c("dplyr","tidyr","ggplot2","effsize","gridExtra")) if missing

# ---- Path: change to your csv file ----
data_path <- "/Users/debduttaguharoy/Developer/Y2 - Master's Thesis/Usecases/experiment-runner/examples/uc1/experiments/flight_delay_two_phase_ssh/run_table.csv"

# --------- Read data and basic cleaning ---------
df_fdp <- read.csv(data_path, stringsAsFactors = FALSE)

# keep only sensible rows
df_fdp <- df_fdp %>%
  filter(!is.na(energy_j), energy_j > 0, !is.na(runtime_s), runtime_s < 999999)

# thresholds and baseline configuration
thresholds <- c(0.1, 0.3, 0.5, 0.7, 0.9)
baseline   <- 1.0

# helper: extract repetition id (assumes X__run_id contains 'repetition_N' suffix)
df_fdp <- df_fdp %>%
  mutate(repetition = as.integer(sub(".*repetition_(\\d+)$", "\\1", X__run_id)))

# --------- Safe testing helper ---------
safe_pair_tests <- function(x, y) {
  # x,y: numeric vectors (paired)
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]; y <- y[keep]
  n <- length(x)
  
  out <- list(n_pairs = n,
              shapiro_W = NA_real_, shapiro_p = NA_real_,
              wilcox_V = NA_real_, wilcox_p = NA_real_,
              cliffs_d = NA_real_, cliffs_mag = NA_character_)
  
  if (n == 0) return(out)
  
  diffs <- x - y
  
  # Shapiro-Wilk: only if n>=3 and diffs not constant
  if (n >= 3 && !all(diffs == diffs[1])) {
    sw <- tryCatch(shapiro.test(diffs), error = function(e) NULL)
    if (!is.null(sw)) { out$shapiro_W <- unname(sw$statistic); out$shapiro_p <- sw$p.value }
  }
  
  # Wilcoxon signed-rank test: if all equal -> p=1
  if (all(x == y)) {
    out$wilcox_V <- NA_real_; out$wilcox_p <- 1.0
  } else {
    wt <- tryCatch(wilcox.test(x, y, paired = TRUE, alternative = "two.sided", exact = FALSE),
                   error = function(e) NULL)
    if (!is.null(wt)) { out$wilcox_V <- as.numeric(unname(wt$statistic)); out$wilcox_p <- wt$p.value }
  }
  
  # Cliff's delta
  cd <- tryCatch(cliff.delta(x, y), error = function(e) NULL)
  if (!is.null(cd)) {
    out$cliffs_d <- as.numeric(cd$estimate)
    out$cliffs_mag <- as.character(cd$magnitude)
  }
  
  out
}

# --------- Pairwise tests (FDP) for a metric function ---------
# corrected run_pairwise_fdp: explicit defaults (no recursive reference)
run_pairwise_fdp <- function(df,
                             metric_col,
                             thresholds = c(0.1, 0.3, 0.5, 0.7, 0.9),
                             baseline   = 1.0) {
  
  # Basic checks
  if (!("repetition" %in% colnames(df))) {
    stop("Dataframe must contain a 'repetition' column. Create it from X__run_id as in the script.")
  }
  if (!("threshold" %in% colnames(df))) {
    stop("Dataframe must contain a 'threshold' column.")
  }
  if (! (metric_col %in% colnames(df)) ) {
    stop("Metric column '", metric_col, "' not found in dataframe.")
  }
  
  # pivot wide (one row per repetition)
  df_wide <- df %>%
    select(repetition, threshold, !!rlang::sym(metric_col)) %>%
    pivot_wider(
      id_cols = repetition,
      names_from = threshold,
      names_prefix = "thr_",
      values_from = !!rlang::sym(metric_col)
    )
  
  # helper from your script (safe_pair_tests)
  safe_pair_tests <- function(x, y) {
    keep <- !is.na(x) & !is.na(y)
    x <- x[keep]; y <- y[keep]; n <- length(x)
    out <- list(n_pairs = n,
                shapiro_W = NA_real_, shapiro_p = NA_real_,
                wilcox_V = NA_real_, wilcox_p = NA_real_,
                cliffs_d = NA_real_, cliffs_mag = NA_character_)
    if (n == 0) return(out)
    diffs <- x - y
    if (n >= 3 && !all(diffs == diffs[1])) {
      sw <- tryCatch(shapiro.test(diffs), error = function(e) NULL)
      if (!is.null(sw)) { out$shapiro_W <- unname(sw$statistic); out$shapiro_p <- sw$p.value }
    }
    if (all(x == y)) {
      out$wilcox_V <- NA_real_; out$wilcox_p <- 1.0
    } else {
      wt <- tryCatch(wilcox.test(x, y, paired = TRUE, alternative = "two.sided", exact = FALSE),
                     error = function(e) NULL)
      if (!is.null(wt)) { out$wilcox_V <- as.numeric(unname(wt$statistic)); out$wilcox_p <- wt$p.value }
    }
    cd <- tryCatch(cliff.delta(x, y), error = function(e) NULL)
    if (!is.null(cd)) { out$cliffs_d <- as.numeric(cd$estimate); out$cliffs_mag <- as.character(cd$magnitude) }
    out
  }
  
  # iterate thresholds
  results <- lapply(thresholds, function(th) {
    col_th   <- paste0("thr_", th)
    col_base <- paste0("thr_", baseline)
    if (!(col_th %in% colnames(df_wide))) {
      stop("Missing column ", col_th, " in pivoted data. Available columns: ",
           paste(colnames(df_wide), collapse = ", "))
    }
    if (!(col_base %in% colnames(df_wide))) {
      stop("Baseline column ", col_base, " not found in pivoted data.")
    }
    x <- df_wide[[col_th]]; y <- df_wide[[col_base]]
    tst <- safe_pair_tests(x, y)
    data.frame(metric = metric_col,
               threshold = th,
               n_pairs = tst$n_pairs,
               shapiro_W = tst$shapiro_W,
               shapiro_p = tst$shapiro_p,
               wilcox_V = tst$wilcox_V,
               wilcox_p = tst$wilcox_p,
               cliffs_d = tst$cliffs_d,
               cliffs_mag = tst$cliffs_mag,
               stringsAsFactors = FALSE)
  })
  
  bind_rows(results)
}


# --------- Run tests for each metric (energy_j, runtime_s, accuracy) ---------
res_fdp_energy <- run_pairwise_fdp(df_fdp, "energy_j")
res_fdp_time   <- run_pairwise_fdp(df_fdp, "runtime_s")
res_fdp_acc    <- run_pairwise_fdp(df_fdp, "accuracy")

results_fdp_all <- bind_rows(res_fdp_energy, res_fdp_time, res_fdp_acc) %>%
  mutate(usecase = "FDP") %>%
  arrange(metric, threshold)

# --------- Holm correction within metric family (FDP) ---------
results_fdp_all <- results_fdp_all %>%
  group_by(metric) %>%
  mutate(p_holm = p.adjust(wilcox_p, method = "holm"),
         sig_holm = ifelse(is.na(p_holm), NA, p_holm < 0.05)) %>%
  ungroup()

# write results to CSV for inclusion in paper / further inspection
write.csv(results_fdp_all, file = "results_fdp_pairwise.csv", row.names = FALSE)

# Print nicely
print(results_fdp_all)

# --------- Descriptive summary table (medians) ---------
summary_table_fdp <- df_fdp %>%
  group_by(threshold) %>%
  summarise(
    median_energy = median(energy_j, na.rm = TRUE),
    mean_energy   = mean(energy_j, na.rm = TRUE),
    median_time   = median(runtime_s, na.rm = TRUE),
    mean_time     = mean(runtime_s, na.rm = TRUE),
    median_acc    = median(accuracy, na.rm = TRUE),
    mean_acc      = mean(accuracy, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(threshold)

write.csv(summary_table_fdp, "summary_table_fdp.csv", row.names = FALSE)
print(summary_table_fdp)

# --------- Plotting: boxplots + QQ plots (Energy and Time) ---------
# prepare factor ordering for thresholds
thr_levels <- c(0.1, 0.3, 0.5, 0.7, 0.9, 1.0)
df_fdp_plot <- df_fdp %>% filter(threshold %in% thr_levels) %>%
  mutate(thr_f = factor(threshold, levels = thr_levels))

# colour palette
pal <- RColorBrewer::brewer.pal(6, "Set2")

# helper to add black median crossbar, point and numeric label
add_median_layer <- function(df, ycol) {
  med_df <- df %>%
    group_by(thr_f) %>%
    summarise(med = median(.data[[ycol]], na.rm = TRUE)) %>% ungroup()
  
  list(
    stat_summary(fun = median, geom = "crossbar", width = 0.6,
                 colour = "black", fatten = 0, size = 0.8, show.legend = FALSE),
    stat_summary(fun = median, geom = "point", size = 3, colour = "black", show.legend = FALSE),
    geom_text(data = med_df, aes(x = thr_f, y = med, label = sprintf("%.1f", med)),
              vjust = -0.8, colour = "black", size = 3)
  )
}

# ENERGY: boxplot
p_energy <- ggplot(df_fdp_plot, aes(x = thr_f, y = energy_j, fill = thr_f)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.9) +
  scale_fill_manual(values = pal) +
  labs(title = "FDP: Energy per Sample by Threshold",
       x = "Gating Threshold (α)", y = "Energy per Sample (J)", fill = "Threshold") +
  theme_minimal() + theme(legend.position = "bottom") +
  add_median_layer(df_fdp_plot, "energy_j")

ggsave("plots/energy_boxplot_FDP_median_black.png", p_energy, width = 8, height = 5, dpi = 300)

# ENERGY: QQ plots (faceted by threshold)
p_energy_qq <- ggplot(df_fdp_plot, aes(sample = energy_j, colour = thr_f)) +
  stat_qq() + stat_qq_line() +
  scale_colour_manual(values = pal) +
  facet_wrap(~ thr_f, nrow = 1, scales = "free") +
  labs(title = "FDP: Q–Q Plots of Energy by Threshold",
       x = "Theoretical Quantiles", y = "Sample Quantiles", colour = "Threshold") +
  theme_minimal() + theme(legend.position = "none", strip.text = element_text(size = 9))

ggsave("plots/energy_qq_FDP.png", p_energy_qq, width = 14, height = 3, dpi = 300)

# TIME: boxplot
p_time <- ggplot(df_fdp_plot, aes(x = thr_f, y = runtime_s, fill = thr_f)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.9) +
  scale_fill_manual(values = pal) +
  labs(title = "FDP: Execution Time per Sample by Threshold",
       x = "Gating Threshold (α)", y = "Execution Time per Sample (ms)", fill = "Threshold") +
  theme_minimal() + theme(legend.position = "bottom") +
  add_median_layer(df_fdp_plot, "runtime_s")

ggsave("plots/time_boxplot_FDP_median_black.png", p_time, width = 8, height = 5, dpi = 300)

# TIME: QQ plots
p_time_qq <- ggplot(df_fdp_plot, aes(sample = runtime_s, colour = thr_f)) +
  stat_qq() + stat_qq_line() +
  scale_colour_manual(values = pal) +
  facet_wrap(~ thr_f, nrow = 1, scales = "free") +
  labs(title = "FDP: Q–Q Plots of Execution Time by Threshold",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() + theme(legend.position = "none", strip.text = element_text(size = 9))

ggsave("plots/time_qq_FDP.png", p_time_qq, width = 14, height = 3, dpi = 300)

message("FDP analysis complete. Results saved:")
message(" - results_fdp_pairwise.csv (pairwise tests)")
message(" - summary_table_fdp.csv (medians/measures)")
message(" - plots/energy_boxplot_FDP_median_black.png")
message(" - plots/energy_qq_FDP.png")
message(" - plots/time_boxplot_FDP_median_black.png")
message(" - plots/time_qq_FDP.png")
