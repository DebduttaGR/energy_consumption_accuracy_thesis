suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
  library(effsize)   # cliff.delta
  library(grid); library(gridExtra); library(RColorBrewer)
  library(rlang)
})

# ----------------- User config -----------------
data_path <- "/Users/debduttaguharoy/Developer/Y2 - Master's Thesis/Usecases/experiment-runner/examples/uc2/experiments/predicting_death_time_and_mortality_ssh/run_table.csv"
plots_dir <- "plots"
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# thresholds and baseline
thresholds <- c(0.1, 0.3, 0.5, 0.7, 0.9)
baseline   <- 1.0
thr_levels <- c(thresholds, baseline)  # for plotting factor order

# ----------------- Read & clean -----------------
df_pdtm <- read.csv(data_path, stringsAsFactors = FALSE)

# Correct potential scaling issue in accuracy_24h
df_pdtm <- df_pdtm %>%
  mutate(
    accuracy_24h = if_else(!is.na(accuracy_24h) & accuracy_24h > 1,
                           accuracy_24h / 1000, accuracy_24h)
  ) %>%
  # sensible filtering
  filter(!is.na(energy_j), energy_j > 0, !is.na(runtime_s), runtime_s < 999999)

# extract repetition id (expects X__run_id contains 'repetition_N' suffix)
df_pdtm <- df_pdtm %>%
  mutate(repetition = as.integer(sub(".*repetition_(\\d+)$", "\\1", X__run_id)))

# ----------------- Safe paired-test helper -----------------
safe_pair_tests <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]; y <- y[keep]
  n <- length(x)
  
  out <- list(
    n_pairs = n,
    shapiro_W = NA_real_, shapiro_p = NA_real_,
    wilcox_V  = NA_real_, wilcox_p = NA_real_,
    cliffs_d  = NA_real_, cliffs_mag = NA_character_
  )
  if (n == 0) return(out)
  
  diffs <- x - y
  # Shapiro on diffs only when n >= 3 and diffs not constant
  if (n >= 3 && !all(diffs == diffs[1])) {
    sw <- tryCatch(shapiro.test(diffs), error = function(e) NULL)
    if (!is.null(sw)) { out$shapiro_W <- unname(sw$statistic); out$shapiro_p <- sw$p.value }
  }
  # Wilcoxon signed-rank
  if (all(x == y)) {
    out$wilcox_V <- NA_real_; out$wilcox_p <- 1.0
  } else {
    wt <- tryCatch(wilcox.test(x, y, paired = TRUE, alternative = "two.sided", exact = FALSE),
                   error = function(e) NULL)
    if (!is.null(wt)) { out$wilcox_V <- as.numeric(unname(wt$statistic)); out$wilcox_p <- wt$p.value }
  }
  # Cliff's delta
  cd <- tryCatch(cliff.delta(x, y), error = function(e) NULL)
  if (!is.null(cd)) { out$cliffs_d <- as.numeric(cd$estimate); out$cliffs_mag <- as.character(cd$magnitude) }
  out
}

# ----------------- Pairwise runner -----------------
run_pairwise_pdtm <- function(df, metric_col,
                              thresholds = c(0.1,0.3,0.5,0.7,0.9),
                              baseline = 1.0, id_col = "repetition") {
  if (!id_col %in% colnames(df)) stop("id_col not found: ", id_col)
  if (!"threshold" %in% colnames(df)) stop("'threshold' column missing")
  if (! metric_col %in% colnames(df)) stop("Metric not present: ", metric_col)
  
  df_wide <- df %>%
    select(!!sym(id_col), threshold, !!sym(metric_col)) %>%
    pivot_wider(
      id_cols = !!sym(id_col),
      names_from = threshold,
      names_prefix = "thr_",
      values_from = !!sym(metric_col)
    )
  
  results <- lapply(thresholds, function(th) {
    col_th   <- paste0("thr_", th)
    col_base <- paste0("thr_", baseline)
    if (! (col_th %in% colnames(df_wide))) stop("Missing column: ", col_th)
    if (! (col_base %in% colnames(df_wide))) stop("Missing baseline column: ", col_base)
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

# ----------------- Run pairwise tests -----------------
res_pdtm_energy <- run_pairwise_pdtm(df_pdtm, "energy_j", thresholds = thresholds, baseline = baseline)
res_pdtm_time   <- run_pairwise_pdtm(df_pdtm, "runtime_s", thresholds = thresholds, baseline = baseline)
res_pdtm_acc_6  <- run_pairwise_pdtm(df_pdtm, "accuracy_6h", thresholds = thresholds, baseline = baseline)
res_pdtm_acc_12 <- run_pairwise_pdtm(df_pdtm, "accuracy_12h", thresholds = thresholds, baseline = baseline)
res_pdtm_acc_24 <- run_pairwise_pdtm(df_pdtm, "accuracy_24h", thresholds = thresholds, baseline = baseline)

results_pdtm_all <- bind_rows(res_pdtm_energy, res_pdtm_time,
                              res_pdtm_acc_6, res_pdtm_acc_12, res_pdtm_acc_24) %>%
  mutate(usecase = "PDTM") %>%
  arrange(metric, threshold)

# ----------------- Holm correction within metric family -----------------
results_pdtm_all <- results_pdtm_all %>%
  group_by(metric) %>%
  mutate(p_holm = p.adjust(wilcox_p, method = "holm"),
         sig_holm = ifelse(is.na(p_holm), NA, p_holm < 0.05)) %>%
  ungroup()

# write out pairwise results
write.csv(results_pdtm_all, file = file.path(plots_dir, "results_pdtm_pairwise.csv"), row.names = FALSE)
print(results_pdtm_all)

# ----------------- Descriptive summary (medians & means by threshold) -----------------
summary_table_pdtm <- df_pdtm %>%
  group_by(threshold) %>%
  summarise(
    median_energy = median(energy_j, na.rm = TRUE),
    mean_energy   = mean(energy_j, na.rm = TRUE),
    median_time   = median(runtime_s, na.rm = TRUE),
    mean_time     = mean(runtime_s, na.rm = TRUE),
    median_acc_6  = median(accuracy_6h, na.rm = TRUE),
    median_acc_12 = median(accuracy_12h, na.rm = TRUE),
    median_acc_24 = median(accuracy_24h, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>% arrange(threshold)

write.csv(summary_table_pdtm, file = file.path(plots_dir, "summary_table_pdtm.csv"), row.names = FALSE)
print(summary_table_pdtm)

# ----------------- Plotting helpers -----------------
pal <- RColorBrewer::brewer.pal(6, "Set2")
add_median_layer <- function(df, ycol) {
  med_df <- df %>%
    group_by(thr_f) %>%
    summarise(med = median(.data[[ycol]], na.rm = TRUE), .groups = "drop")
  list(
    stat_summary(fun = median, geom = "crossbar", width = 0.6, colour = "black", fatten = 0, size = 0.8, show.legend = FALSE),
    stat_summary(fun = median, geom = "point", size = 3, colour = "black", show.legend = FALSE),
    geom_text(data = med_df, aes(x = thr_f, y = med, label = sprintf("%.1f", med)), vjust = -0.8, colour = "black", size = 3)
  )
}

df_pdtm_plot <- df_pdtm %>%
  filter(threshold %in% thr_levels) %>%
  mutate(thr_f = factor(threshold, levels = thr_levels))

# ENERGY boxplot (PDTM)
p_energy_pdtm <- ggplot(df_pdtm_plot, aes(x = thr_f, y = energy_j, fill = thr_f)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.9) +
  scale_fill_manual(values = pal) +
  labs(title = "PDTM: Energy per Sample by Threshold",
       x = "Gating Threshold (α)", y = "Energy per Sample (J)", fill = "Threshold") +
  theme_minimal() + theme(legend.position = "bottom") +
  add_median_layer(df_pdtm_plot, "energy_j")
ggsave(file.path(plots_dir, "energy_boxplot_PDTM_median_black.png"), p_energy_pdtm, width = 8, height = 5, dpi = 300)

# ENERGY QQ (PDTM) - faceted by threshold (1 row)
p_energy_qq_pdtm <- ggplot(df_pdtm_plot, aes(sample = energy_j, colour = thr_f)) +
  stat_qq() + stat_qq_line() +
  scale_colour_manual(values = pal) +
  facet_wrap(~ thr_f, nrow = 1, scales = "free") +
  labs(title = "PDTM: Q–Q Plots of Energy by Threshold",
       x = "Theoretical Quantiles", y = "Sample Quantiles", colour = "Threshold") +
  theme_minimal() + theme(legend.position = "none", strip.text = element_text(size = 9))
ggsave(file.path(plots_dir, "energy_qq_PDTM.png"), p_energy_qq_pdtm, width = 14, height = 3.5, dpi = 300)

# TIME boxplot (PDTM)
p_time_pdtm <- ggplot(df_pdtm_plot, aes(x = thr_f, y = runtime_s, fill = thr_f)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.9) +
  scale_fill_manual(values = pal) +
  labs(title = "PDTM: Execution Time per Sample by Threshold",
       x = "Gating Threshold (α)", y = "Execution Time per Sample (ms)", fill = "Threshold") +
  theme_minimal() + theme(legend.position = "bottom") +
  add_median_layer(df_pdtm_plot, "runtime_s")
ggsave(file.path(plots_dir, "time_boxplot_PDTM_median_black.png"), p_time_pdtm, width = 8, height = 5, dpi = 300)

# TIME QQ (PDTM)
p_time_qq_pdtm <- ggplot(df_pdtm_plot, aes(sample = runtime_s, colour = thr_f)) +
  stat_qq() + stat_qq_line() +
  scale_colour_manual(values = pal) +
  facet_wrap(~ thr_f, nrow = 1, scales = "free") +
  labs(title = "PDTM: Q–Q Plots of Execution Time by Threshold",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() + theme(legend.position = "none", strip.text = element_text(size = 9))
ggsave(file.path(plots_dir, "time_qq_PDTM.png"), p_time_qq_pdtm, width = 14, height = 3.5, dpi = 300)

# Median accuracy plot across horizons (6h/12h/24h)
# prepare median table for plotting
pdtm_med_long <- summary_table_pdtm %>%
  select(threshold, median_acc_6, median_acc_12, median_acc_24) %>%
  pivot_longer(
    cols      = starts_with("median_acc_"),
    names_to  = "horizon",
    values_to = "median_accuracy"
  ) %>%
  mutate(
    horizon = dplyr::recode(
      horizon,
      "median_acc_6"  = "PDTM-6h",
      "median_acc_12" = "PDTM-12h",
      "median_acc_24" = "PDTM-24h"
    ),
    threshold = as.numeric(threshold)
  )


p_med <- ggplot(pdtm_med_long, aes(x = threshold, y = median_accuracy, color = horizon, shape = horizon)) +
  geom_line(size = 1) + geom_point(size = 3) +
  scale_color_manual(values = c("PDTM-6h"="#33a02c","PDTM-12h"="#e31a1c","PDTM-24h"="#ff7f00")) +
  scale_shape_manual(values = c(16,17,15)) +
  scale_x_continuous(breaks = c(thr_levels)) +
  labs(x = "Offloading Threshold (α)", y = "Median Accuracy", color = "Horizon", shape = "Horizon",
       title = "PDTM: Median Accuracy vs Threshold (6h/12h/24h)") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(plots_dir, "median_accuracy_pdtm.png"), p_med, width = 8, height = 5, dpi = 300)

# ----------------- Done -----------------
message("PDTM analysis complete. Files saved in: ", normalizePath(plots_dir))
message(" - results_pdtm_pairwise.csv")
message(" - summary_table_pdtm.csv")
message(" - energy_boxplot_PDTM_median_black.png")
message(" - energy_qq_PDTM.png")
message(" - time_boxplot_PDTM_median_black.png")
message(" - time_qq_PDTM.png")
message(" - median_accuracy_pdtm.png")

# reproducibility info
sessionInfo()
