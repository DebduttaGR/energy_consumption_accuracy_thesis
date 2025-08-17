# analyze_PDTM.R
# Usage: edit file_in and run

library(tidyverse)
library(rstatix)
library(effsize)
library(ggpubr)

file_in <- "/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/experiment-runner/examples/uc2/experiments/predicting_death_time_and_mortality_ssh/run_table.csv"  # CHANGE to your CSV

dat <- read_csv(file_in, col_types = cols())
dat  <- dat  %>% filter(!is.na(energy_j) & energy_j >= 0)

# required columns
req <- c("__run_id","threshold","energy_j","runtime_s",
         "invocation_rate_6h","accuracy_6h","invocation_rate_12h","accuracy_12h","invocation_rate_24h","accuracy_24h")
stopifnot(all(req %in% names(dat)))

# extract repetition id
dat <- dat %>%
  mutate(rep = str_extract('__run_id', "repetition_\\d+") %>% str_remove("repetition_") %>% as.integer())

if (any(is.na(dat$rep))) {
  dat <- dat %>% group_by(threshold) %>% mutate(rep = row_number()-1) %>% ungroup()
  warning("Assigned rep fallback.")
}

# convert threshold to factor for plotting
dat <- dat %>% mutate(thr_f = as.factor(threshold))

# We'll analyze these metrics:
# energy_j, runtime_s (global) and accuracy per horizon
global_metrics <- c("energy_j", "runtime_s")
horizons <- c("6h","12h","24h")

# 1) Descriptive stats (global and per-horizon)
desc_global <- dat %>%
  pivot_longer(cols = all_of(global_metrics), names_to = "metric", values_to = "value") %>%
  group_by(metric, threshold) %>%
  summarise(n=n(), mean=mean(value,na.rm=TRUE), median=median(value,na.rm=TRUE), sd=sd(value,na.rm=TRUE), min=min(value,na.rm=TRUE), max=max(value,na.rm=TRUE)) %>%
  arrange(metric, threshold)

desc_horiz <- map_dfr(horizons, function(h) {
  acc_col <- paste0("accuracy_", h)
  inv_col <- paste0("invocation_rate_", h)
  dat %>%
    select(rep, threshold, !!sym(acc_col), !!sym(inv_col)) %>%
    pivot_longer(cols = c(!!sym(acc_col), !!sym(inv_col)), names_to = "metric", values_to = "value") %>%
    group_by(metric, threshold) %>%
    summarise(n=n(), mean=mean(value,na.rm=TRUE), median=median(value,na.rm=TRUE), sd=sd(value,na.rm=TRUE), min=min(value,na.rm=TRUE), max=max(value,na.rm=TRUE))
})

write_csv(bind_rows(desc_global, desc_horiz), "descriptive_PDTM.csv")
message("Wrote descriptive_PDTM.csv")

x_labels <- c(
  energy_j      = "Energy (J)",
  runtime_s     = "Runtime (ms)",
  accuracy_6h   = "Accuracy (6hr) class",
  accuracy_12h  = "Accuracy (12hr) class",
  accuracy_24h  = "Accuracy (24hr) class",
  invocation_rate_6h = "Invocation Rate (6hr) class",
  invocation_rate_12h = "Invocation Rate (12hr) class",
  invocation_rate_24h = "Invocation Rate (24hr) class"
)

# 2) Boxplots and QQ plots:
# combined energy and runtime
for (m in global_metrics) {
  pbox <- ggplot(dat, aes(x = thr_f, y = .data[[m]], fill = thr_f)) +
    geom_boxplot(alpha=0.9, outlier.shape = 21, outlier.size = 1) +
    labs(title = paste0("PDTM: ", m, " by gating threshold"), x = "threshold", y = x_labels[[m]]) +
    theme_minimal() + theme(legend.position = "none")
  ggsave(paste0("boxplot_", m, "_PDTM.png"), pbox, width=8, height=5, dpi=300)
  
  pqq <- ggplot(dat, aes(sample = .data[[m]])) + stat_qq() + stat_qq_line() +
    facet_wrap(~ thr_f, nrow = 1, scales = "free") +
    labs(title = paste0("PDTM: QQ plots of ", x_labels[[m]], " by gating threshold")) + theme_minimal()
  ggsave(paste0("qqplot_", m, "_PDTM.png"), pqq, width=14, height=3.5, dpi=300)
}

# per-horizon accuracy boxplots
#for (h in horizons) {
#  acc_col <- paste0("accuracy_", h)
#  pbox <- ggplot(dat, aes(x = thr_f, y = .data[[acc_col]], fill = thr_f)) +
#    geom_boxplot(alpha=0.9, outlier.shape=21, outlier.size=1) +
#    labs(title = paste0("PDTM: accuracy_", h, " by threshold"), x="threshold", y="accuracy") +
#    theme_minimal() + theme(legend.position = "none")
#  ggsave(paste0("boxplot_accuracy_", h, "_PDTM.png"), pbox, width=8, height=5, dpi=300)
  
#  pqq <- ggplot(dat, aes(sample = .data[[acc_col]])) + stat_qq() + stat_qq_line() +
#    facet_wrap(~ thr_f, nrow = 1, scales = "free") +
#    labs(title = paste0("PDTM: Q-Q plots of accuracy_", h, " by threshold")) + theme_minimal()
#  ggsave(paste0("qqplot_accuracy_", h, "_PDTM.png"), pqq, width=14, height=3.5, dpi=300)
#}

# 3+4+5+6) Tests: for each metric compare each threshold vs baseline (paired)
baseline_thr <- 1.0
res_list <- list()

# helper to run tests on a vector per-rep pairing
run_tests_paired <- function(df_pair) {
  diff_vec <- df_pair$test - df_pair$baseline
  sw <- tryCatch(shapiro_test(diff_vec), error = function(e) tibble(statistic=NA_real_, p=NA_real_))
  wil <- tryCatch(wilcox.test(df_pair$test, df_pair$baseline, paired = TRUE, exact = FALSE), error = function(e) NULL)
  cl <- tryCatch(cliff.delta(df_pair$test, df_pair$baseline), error = function(e) NULL)
  tibble(shapiro_stat = sw$statistic, shapiro_p = sw$p,
         wilcox_V = ifelse(!is.null(wil), wil$statistic, NA_real_),
         wilcox_p = ifelse(!is.null(wil), wil$p.value, NA_real_),
         cliffs_d = ifelse(!is.null(cl), cl$estimate, NA_real_),
         cliffs_mag = ifelse(!is.null(cl), cl$magnitude, NA_character_))
}

# global metrics
global_res <- map_dfr(global_metrics, function(m) {
  thr_values <- sort(unique(dat$threshold))
  thr_values <- thr_values[thr_values != baseline_thr]
  map_dfr(thr_values, function(thr) {
    wide <- dat %>% select(rep, threshold, value = .data[[m]]) %>% pivot_wider(names_from = threshold, values_from = value, names_prefix = "thr_")
    baseline_col <- paste0("thr_", baseline_thr); thr_col <- paste0("thr_", thr)
    if (!(baseline_col %in% names(wide)) || !(thr_col %in% names(wide))) return(tibble())
    df_pair <- wide %>% select(rep, baseline = !!sym(baseline_col), test = !!sym(thr_col)) %>% drop_na()
    tests <- run_tests_paired(df_pair)
    tibble(metric = m, threshold = thr, n_pairs = nrow(df_pair)) %>% bind_cols(tests)
  })
})

# per-horizon accuracy
h_res <- map_dfr(horizons, function(h) {
  acc_col <- paste0("accuracy_", h)
  thr_values <- sort(unique(dat$threshold)); thr_values <- thr_values[thr_values != baseline_thr]
  map_dfr(thr_values, function(thr) {
    wide <- dat %>% select(rep, threshold, value = .data[[acc_col]]) %>% pivot_wider(names_from = threshold, values_from = value, names_prefix = "thr_")
    baseline_col <- paste0("thr_", baseline_thr); thr_col <- paste0("thr_", thr)
    if (!(baseline_col %in% names(wide)) || !(thr_col %in% names(wide))) return(tibble())
    df_pair <- wide %>% select(rep, baseline = !!sym(baseline_col), test = !!sym(thr_col)) %>% drop_na()
    tests <- run_tests_paired(df_pair)
    tibble(metric = acc_col, threshold = thr, n_pairs = nrow(df_pair)) %>% bind_cols(tests)
  })
})

res_df <- bind_rows(global_res, h_res)

# Holm correction within each metric
res_df <- res_df %>%
  group_by(metric) %>%
  mutate(p_holm = p.adjust(wilcox_p, method = "holm"),
         sig_holm = ifelse(!is.na(p_holm) & p_holm < 0.05, TRUE, FALSE)) %>%
  ungroup()

write_csv(res_df, "summary_tests_PDTM.csv")
message("Wrote summary_tests_PDTM.csv")

# 7) Correlations: Spearman between threshold and energy/runtime and accuracy per horizon
cors <- tibble()
for (m in c(global_metrics, paste0("accuracy_", horizons), paste0("invocation_rate_",horizons))) {
  # skip missing
  if (! (m %in% names(dat))) next
  ct <- cor.test(dat[[m]], dat$threshold, method="spearman")
  cors <- bind_rows(cors, tibble(metric = m, rho = as.numeric(ct$estimate), p = ct$p.value))
}
write_csv(cors, "spearman_PDTM.csv")
message("Wrote spearman_PDTM.csv")

message("PDTM analysis complete.")
