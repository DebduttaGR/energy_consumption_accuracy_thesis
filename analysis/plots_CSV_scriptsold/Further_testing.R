#––– 0. Setup & data load ––––––––––––––––––––––––––––––––––––––––––––––––––

library(dplyr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(broom)
library(purrr)

# adjust these paths if needed:
df_fdp <- read.csv("/Users/debduttaguharoy/Developer/Y2 - Master's Thesis/Usecases/experiment-runner/examples/uc1/experiments/flight_delay_two_phase_ssh/run_table.csv")  # Flight Delay Prediction
df_pdtm <- read.csv("/Users/debduttaguharoy/Developer/Y2 - Master's Thesis/Usecases/experiment-runner/examples/uc2/experiments/predicting_death_time_and_mortality_ssh/run_table.csv")  # Predicting Death Time and Mortality

# create output folder
if (!dir.exists("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots")) dir.create("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots")
if (!dir.exists("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/normality")) dir.create("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/normality")

baseline <- df_fdp %>% filter(threshold == 1)

wilcox_fdp <- lapply(c("energy_j","runtime_s","accuracy"), function(m) {
  map_dfr(c(0.1,0.3,0.5,0.7,0.9), function(t) {
    sample_t <- df_fdp %>% filter(threshold == t) %>% pull(.data[[m]])
    sample_1 <- baseline %>% pull(.data[[m]])
    tidy(wilcox.test(sample_t, sample_1, paired = TRUE)) %>%
      mutate(metric = m, threshold = t)
  })
}) %>% bind_rows()

print(wilcox_fdp)
write.csv(wilcox_fdp, "/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/FDP_wilcoxon_results.csv", row.names = FALSE)

# prepare label positions
sigs <- wilcox_fdp %>%
  filter(metric == "runtime_s") %>%
  mutate(label = ifelse(p.value < 0.001, "***",
                        ifelse(p.value < 0.01, "**",
                               ifelse(p.value < 0.05, "*", "ns"))))

# build the plot
p_e <- ggplot(df_fdp, aes(factor(threshold), runtime_s)) +
  geom_boxplot(fill = "lightblue") +
  geom_text(
    data = sigs,
    aes(x = factor(threshold), 
        y = max(df_fdp$runtime_s) * 1.05, # position just above max
        label = label),
    vjust = 0
  ) +
  labs(
    title = "FDP: Execution time (in ms) by Threshold\n(* p<.05, ** p<.01, *** p<.001)",
    x     = "Threshold",
    y     = "Execution time (ms)"
  ) +
  theme_minimal()

print(p_e)
ggsave("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/FDP_execution_time_with_significance.png", p_e, width = 7, height = 5)


#––– 2. Wilcoxon signed‑rank tests for PDTM per window ––––––––––––––––––––

# 1. Find your run‑ID column name (e.g. "__run_id" → "X.run_id")
run_col <- names(df_pdtm)[grepl("run_id", names(df_pdtm), ignore.case = TRUE)][1]

# 2. Define thresholds (add 0.9 if you like)
thresh_vals <- c(0.1, 0.3, 0.5, 0.7, 0.9)

# 3. Helper to run one metric/window
wilcox_for <- function(metric, window) {
  colname <- paste0(metric, "_", window)
  map_dfr(thresh_vals, function(t) {
    # treatment & baseline subsets
    df_t <- df_pdtm %>%
      filter(threshold == t) %>%
      select(all_of(run_col), value_t = all_of(colname))
    df_1 <- df_pdtm %>%
      filter(threshold == 1) %>%
      select(all_of(run_col), value_1 = all_of(colname))
    
    # inner join by run ID
    df_pair <- inner_join(df_t, df_1, by = run_col)
    
    vec_t  <- df_pair$value_t
    vec_1  <- df_pair$value_1
    
    # only test if there’s at least one non‐zero difference
    if (nrow(df_pair) >= 1 && any(vec_t != vec_1, na.rm = TRUE)) {
      res <- tidy(wilcox.test(vec_t, vec_1, paired = TRUE))
    } else {
      res <- tibble(
        statistic   = NA_real_,
        p.value     = NA_real_,
        method      = "insufficient variation",
        alternative = NA_character_
      )
    }
    
    # annotate
    res %>% mutate(
      metric    = metric,
      window    = window,
      threshold = t
    )
  })
}

# 4. Run for all metrics and windows
wilcox_pdtm <- map_dfr(
  c("accuracy","roc_auc","f1_macro"),
  ~map_dfr(c("6h","12h","24h"), function(win) wilcox_for(.x, win))
)

print(wilcox_pdtm)
write.csv(wilcox_pdtm, "/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/PDTM_wilcoxon_results.csv", row.names = FALSE)

# pivot accuracy windows long
df_acc <- df_pdtm %>%
  pivot_longer(
    cols = matches("^accuracy_\\d+h$"),
    names_to    = "window",
    names_prefix= "accuracy_",
    values_to   = "accuracy"
  )

p <- ggplot(df_acc, aes(x = factor(threshold), y = accuracy * 100)) +
  geom_boxplot(fill = "lightgray") +
  facet_wrap(~ window) +
  labs(
    title = "PDTM: Accuracy (%) by Threshold and Prediction Window",
    x     = "Threshold",
    y     = "Accuracy (%)"
  ) +
  theme_minimal()

print(p)
ggsave("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/PDTM_accuracy_flat_by_window.png", p, width = 8, height = 6)

# 1. FDP: energy, runtime (s→ms), accuracy
metrics_fdp <- list(
  energy    = list(col="energy_j", ylab="Energy (J)"),
  runtime   = list(col="runtime_s", ylab="Runtime (s)"),
  accuracy  = list(col="accuracy",  ylab="Accuracy (%)", transform=function(x) x*100)
)

for (metric in names(metrics_fdp)) {
  col    <- metrics_fdp[[metric]]$col
  ylab   <- metrics_fdp[[metric]]$ylab
  xform  <- metrics_fdp[[metric]]$transform %||% identity
  
  p <- ggplot(df_fdp, aes(factor(threshold), xform(.data[[col]]))) +
    geom_boxplot(fill="skyblue") +
    labs(
      title = paste("FDP:", metric, "by Threshold"),
      x     = "Threshold",
      y     = ylab
    ) +
    theme_minimal()
  
  print(p)
  ggsave(sprintf("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/FDP_%s_by_threshold.png", metric), p, width=7, height=5)
}

# 2. PDTM: energy, runtime (s→ms), accuracy by window

# (a) energy
p_e <- ggplot(df_pdtm, aes(factor(threshold), energy_j)) +
  geom_boxplot(fill="lightgreen") +
  labs(
    title = "PDTM: Energy (J) by Threshold",
    x     = "Threshold",
    y     = "Energy (J)"
  ) + theme_minimal()
print(p_e)
ggsave("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/PDTM_energy_by_threshold.png", p_e, width=7, height=5)

# (b) runtime
p_r <- ggplot(df_pdtm, aes(factor(threshold), runtime_s * 1000)) +
  geom_boxplot(fill="lightpink") +
  labs(
    title = "PDTM: Runtime (ms) by Threshold",
    x     = "Threshold",
    y     = "Runtime (ms)"
  ) + theme_minimal()
print(p_r)
ggsave("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/PDTM_runtime_by_threshold.png", p_r, width=7, height=5)

# (c) accuracy
p_a <- ggplot(df_acc, aes(factor(threshold), accuracy * 100)) +
  geom_boxplot(fill="lightgray") +
  facet_wrap(~ window) +
  labs(
    title = "PDTM: Accuracy (%) by Threshold and Window",
    x     = "Threshold",
    y     = "Accuracy (%)"
  ) + theme_minimal()
print(p_a)
ggsave("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/PDTM_accuracy_by_threshold_window.png", p_a, width=8, height=6)

#–– FDP correlations
cors_fdp <- df_fdp %>%
  summarise(
    spearman_energy  = list(cor.test(invocation_rate, energy_j,  method="spearman")),
    spearman_runtime = list(cor.test(invocation_rate, runtime_s, method="spearman")),
    spearman_acc     = list(cor.test(invocation_rate, accuracy,  method="spearman"))
  ) %>%
  pivot_longer(everything(),
               names_to = "test",
               values_to = "result") %>%
  mutate(tidy = map(result, tidy)) %>%
  unnest(tidy) %>%
  select(test, estimate, p.value)

print(cors_fdp)

#–– PDTM correlations (global + per window)
# energy & runtime
cors_pdtm_globals <- tibble(
  metric = c("energy_j","runtime_s"),
  test   = c(
    list(cor.test(df_pdtm$invocation_rate, df_pdtm$energy_j,  method="spearman")),
    list(cor.test(df_pdtm$invocation_rate, df_pdtm$runtime_s, method="spearman"))
  )
) %>%
  mutate(tidy = map(test, tidy)) %>%
  unnest(tidy) %>%
  select(metric, estimate, p.value)

# accuracy windows
cors_pdtm_acc <- map_dfr(c("6h","12h","24h"), function(w) {
  col <- paste0("accuracy_", w)
  test <- cor.test(df_pdtm$invocation_rate, df_pdtm[[col]], method="spearman")
  tidy(test) %>% mutate(metric="accuracy", window=w)
})

cors_pdtm <- bind_rows(cors_pdtm_globals, cors_pdtm_acc)
print(cors_pdtm)

# Combine FDP & PDTM into one long dataset
df_pdtm$usecase <- "PDTM"
df_fdp$usecase  <- "FDP"
df_all <- bind_rows(
  df_fdp  %>% mutate(runtime_ms = runtime_s),
  df_pdtm %>% mutate(runtime_ms = runtime_s)
) %>%
  select(usecase, threshold, invocation_rate, energy_j, runtime_ms, accuracy)

# Trade-off: accuracy vs energy
p1 <- ggplot(df_all, aes(energy_j, accuracy*100, color=factor(threshold))) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", se=TRUE) +
  facet_wrap(~ usecase) +
  labs(
    title = "Accuracy vs Energy Trade-off",
    x     = "Energy (Joules)",
    y     = "Accuracy (%)",
    color = "Threshold"
  ) +
  theme_minimal()
print(p1)
ggsave("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/tradeoff_accuracy_energy.png", p1, width=8, height=6)

# Invocation vs Energy/runtime
p2 <- ggplot(df_all, aes(invocation_rate, energy_j)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", se=TRUE) +
  facet_wrap(~ usecase) +
  labs(
    title = "Invocation Rate vs Energy",
    x     = "Invocation Rate (α)",
    y     = "Energy (Joules)"
  ) +
  theme_minimal()
print(p2)
ggsave("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/invocation_vs_energy.png", p2, width=8, height=6)

p3 <- ggplot(df_all, aes(invocation_rate, runtime_ms)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", se=TRUE) +
  facet_wrap(~ usecase) +
  labs(
    title = "Invocation Rate vs Runtime",
    x     = "Invocation Rate (α)",
    y     = "Runtime (ms)"
  ) +
  theme_minimal()
print(p3)
ggsave("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/invocation_vs_runtime.png", p3, width=8, height=6)

# Friedman post‑hoc for FDP energy
pairwise.wilcox.test(
  df_fdp$energy_j, df_fdp$threshold,
  paired = TRUE,
  p.adjust.method = "BH"
)

