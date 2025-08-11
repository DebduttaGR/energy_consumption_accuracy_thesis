# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(car)
library(psych)
#library(patchwork)   # for wrap_plots()
library(grid)      # for textGrob(), gpar(), grid.newpage(), grid.draw()
library(gridExtra) # for arrangeGrob()
#install.packages(c("dplyr","tidyr","effsize"))  # if not already installed

# 1. Data Import -----------------------------------------------------------
# Adjust file paths as needed
df_fdp <- read.csv("/Users/debduttaguharoy/Developer/Y2 - Master's Thesis/Usecases/experiment-runner/examples/uc1/experiments/flight_delay_two_phase_ssh/run_table.csv")  # Flight Delay Prediction
df_pdtm <- read.csv("/Users/debduttaguharoy/Developer/Y2 - Master's Thesis/Usecases/experiment-runner/examples/uc2/experiments/predicting_death_time_and_mortality_ssh/run_table.csv")  # Predicting Death Time and Mortality

head(df_fdp)
head(df_pdtm)

#descriptive statistics
energy_desc_fdp <- df_fdp %>%
  filter(runtime_s<999999) %>%
  group_by(usecase, threshold) %>%
  summarise(
    mean_accuracy = mean(accuracy, na.rm = TRUE)*100,
    median_accuracy = median(accuracy, na.rm = TRUE)*100,
    sd_accuracy = sd(accuracy, na.rm = TRUE),
    min_accuracy = min(accuracy, na.rm = TRUE)*100,
    max_accuracy = max(accuracy, na.rm = TRUE)*100,
    .groups = 'drop'
  )

head(energy_desc_fdp)

energy_desc_pdtm <- df_pdtm %>%
  filter(runtime_s<999999) %>%
  group_by(usecase, threshold) %>%
  summarise(
    #mean_accuracy_6h = mean(accuracy_6h, na.rm = TRUE)*100,
    #median_accuracy_6h = median(accuracy_6h, na.rm = TRUE)*100,
    #sd_accuracy_6h = sd(accuracy_6h, na.rm = TRUE),
    #min_accuracy_6h = min(accuracy_6h, na.rm = TRUE)*100,
    #max_accuracy_6h = max(accuracy_6h, na.rm = TRUE)*100,
    mean_accuracy_12h = mean(accuracy_12h, na.rm = TRUE)*100,
    median_accuracy_12h = median(accuracy_12h, na.rm = TRUE)*100,
    sd_accuracy_12h = sd(accuracy_12h, na.rm = TRUE),
    #min_accuracy_12h = min(accuracy_12h, na.rm = TRUE)*100,
    #max_accuracy_12h = max(accuracy_12h, na.rm = TRUE)*100,
    mean_accuracy_24h = mean(accuracy_24h, na.rm = TRUE)*100,
    median_accuracy_24h = median(accuracy_24h, na.rm = TRUE)*100,
    sd_accuracy_24h = sd(accuracy_24h, na.rm = TRUE),
    #min_accuracy_24h = min(accuracy_24h, na.rm = TRUE)*100,
    #max_accuracy_24h = max(accuracy_24h, na.rm = TRUE)*100,
    .groups = 'drop'
  )

head(energy_desc_pdtm)

dependent_variables_fdp <- c("energy_j","runtime_s","accuracy")
dependent_variables_pdtm <- c("energy_j","runtime_s","accuracy_6h", "accuracy_12h", "accuracy_24h")

summary_table_fdp <- df_fdp %>%
  filter(energy_j>0, runtime_s<999999) %>%
  group_by(usecase, threshold) %>%
  summarise(
    mean_energy = mean(energy_j, na.rm = TRUE),
    median_energy = median(energy_j, na.rm = TRUE),
    sd_energy = sd(energy_j, na.rm = TRUE),
    min_energy = min(energy_j, na.rm = TRUE),
    max_energy = max(energy_j, na.rm = TRUE),
    mean_time = mean(runtime_s, na.rm = TRUE),
    median_time = median(runtime_s, na.rm = TRUE),
    sd_time = sd(runtime_s, na.rm = TRUE),
    min_time = min(runtime_s, na.rm = TRUE),
    max_time = max(runtime_s, na.rm = TRUE),
    mean_accuracy = mean(accuracy, na.rm = TRUE),
    median_accuracy = median(accuracy, na.rm = TRUE),
    sd_accuracy = sd(accuracy, na.rm = TRUE),
    min_accuracy = min(accuracy, na.rm = TRUE),
    max_accuracy = max(accuracy, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_table_fdp)

df_pdtm <- df_pdtm %>%
  mutate(
    accuracy_24h = if_else(accuracy_24h > 1,
                           accuracy_24h / 1000,
                           accuracy_24h)
  )

summary_table_pdtm <- df_pdtm %>%
  group_by(usecase, threshold) %>%
  summarise(
    mean_energy = mean(energy_j, na.rm = TRUE),
    median_energy = median(energy_j, na.rm = TRUE),
    sd_energy = sd(energy_j, na.rm = TRUE),
    min_energy = min(energy_j, na.rm = TRUE),
    max_energy = max(energy_j, na.rm = TRUE),
    mean_time = mean(runtime_s, na.rm = TRUE),
    median_time = median(runtime_s, na.rm = TRUE),
    sd_time = sd(runtime_s, na.rm = TRUE),
    min_time = min(runtime_s, na.rm = TRUE),
    max_time = max(runtime_s, na.rm = TRUE),
    mean_accuracy_6h = mean(accuracy_6h, na.rm = TRUE),
    median_accuracy_6h = median(accuracy_6h, na.rm = TRUE),
    sd_accuracy_6h = sd(accuracy_6h, na.rm = TRUE),
    min_accuracy_6h = min(accuracy_6h, na.rm = TRUE),
    max_accuracy_6h = max(accuracy_6h, na.rm = TRUE),
    mean_accuracy_12h = mean(accuracy_12h, na.rm = TRUE),
    median_accuracy_12h = median(accuracy_12h, na.rm = TRUE),
    sd_accuracy_12h = sd(accuracy_12h, na.rm = TRUE),
    min_accuracy_12h = min(accuracy_12h, na.rm = TRUE),
    max_accuracy_12h = max(accuracy_12h, na.rm = TRUE),
    mean_accuracy_24h = mean(accuracy_24h, na.rm = TRUE),
    median_accuracy_24h = median(accuracy_24h, na.rm = TRUE),
    sd_accuracy_24h = sd(accuracy_24h, na.rm = TRUE),
    min_accuracy_24h = min(accuracy_24h, na.rm = TRUE),
    max_accuracy_24h = max(accuracy_24h, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_table_pdtm)

#Histogram to check the distribution of the data
# Define the columns to examine
columns_to_examine_fdp <- c("energy_j", "runtime_s", "accuracy")
columns_to_examine_pdtm <- c("energy_j","runtime_s","accuracy_6h", "accuracy_12h", "accuracy_24h")

# Cleaning the dataframe
df_fdp <- df_fdp %>%
  filter(
    energy_j  >  0,        # drop negatives
    runtime_s < 999999     # drop absurdly large runtimes
  )

# Examine summary statistics
summary(df_fdp[columns_to_examine_fdp])
summary(df_pdtm[columns_to_examine_pdtm])

# Create a list to store all plots
all_plots <- list()

# 3. Draw and save histograms for each
for (col in columns_to_examine_fdp) {
  p <- ggplot(df_fdp, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    labs(
      title = paste("Histogram of", col, "(FDP)"),
      x     = if (col == "energy_j") "Energy (J)" 
      else if (col == "runtime_s") "Runtime (s)" 
      else "Accuracy",
      y     = "Count"
    ) +
    theme_minimal()
  
  print(p)  # show in RStudio
  ggsave(
    filename = paste0("plots/FDP_hist_", col, ".png"),
    plot     = p,
    width    = 6, height = 4
  )
}

# 3. Pre‑define a named vector of axis labels
x_labels <- c(
  energy_j      = "Energy (J)",
  runtime_s     = "Runtime (s)",
  accuracy_6h   = "Accuracy (6hr) class",
  accuracy_12h  = "Accuracy (12hr) class",
  accuracy_24h  = "Accuracy (24hr) class"
)

# 4. Loop and plot from the *filtered* df_pdtm
for (col in columns_to_examine_pdtm) {
  p <- ggplot(df_pdtm, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    labs(
      title = paste("Histogram of", col, "(PDTM)"),
      x     = x_labels[[col]],
      y     = "Count"
    ) +
    theme_minimal()
  
  print(p)  # show each one in RStudio’s Plots pane
  ggsave(
    filename = paste0("plots/PDTM_hist_", col, ".png"),
    plot     = p,
    width    = 6, height = 4
  )
}

# Combine all plots into a single plot
combined_plot <- plot_grid(plotlist = all_plots, ncol = 1)

# Save the combined plot to a file
ggsave(filename = "combined_histograms.png", plot = combined_plot, width = 16, height = 48)


# Combined plot of histogram per usecase
# 2. Define your metrics and axis‐labels
metrics_fdp <- list(
  energy   = list(col = "energy_j",    ylab = "Energy (J)"),
  runtime  = list(col = "runtime_s",   ylab = "Runtime (s)"),
  accuracy = list(col = "accuracy",    ylab = "Accuracy")
)

# 3. Build and collect plots in a list
fdp_plots <- list()
for (name in names(metrics_fdp)) {
  col  <- metrics_fdp[[name]]$col
  ylab <- metrics_fdp[[name]]$ylab
  
  p <- ggplot(df_fdp, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    labs(
      title = paste("FDP:", name),
      x     = ylab,
      y     = "Count"
    ) +
    theme_minimal()
  
  fdp_plots[[name]] <- p
}

combined_fdp <- arrangeGrob(
  grobs = fdp_plots,
  ncol  = 2,
  top   = textGrob("FDP Histograms", gp = gpar(fontsize = 16, fontface = "bold"))
)

# 4. Draw to screen
grid.newpage()
grid.draw(combined_fdp)

# 5. Save to file
ggsave(
  filename = "plots/FDP_combined_histograms.png",
  plot     = combined_fdp,
  width    = 12,
  height   = 18  # adjust so each panel has ~6 units of height
)

# Combined for PDTM
metrics_pdtm <- list(
  energy      = list(col = "energy_j",     ylab = "Energy (J)"),
  runtime     = list(col = "runtime_s",    ylab = "Runtime (s)"),
  acc_6h      = list(col = "accuracy_6h",  ylab = "Accuracy (6hr)"),
  acc_12h     = list(col = "accuracy_12h", ylab = "Accuracy (12hr)"),
  acc_24h     = list(col = "accuracy_24h", ylab = "Accuracy (24hr)")
)

pdtm_plots <- list()
for (nm in names(metrics_pdtm)) {
  col  <- metrics_pdtm[[nm]]$col
  ylab <- metrics_pdtm[[nm]]$ylab
  
  pdtm_plots[[nm]] <- ggplot(df_pdtm, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    labs(
      title = paste("PDTM:", nm),
      x     = ylab,
      y     = "Count"
    ) +
    theme_minimal()
}

combined_pdtm <- arrangeGrob(
  grobs = pdtm_plots,
  ncol  = 2,
  top   = textGrob(
    "PDTM Histograms",
    gp = gpar(fontsize = 16, fontface = "bold")
  )
)

grid.newpage()
grid.draw(combined_pdtm)

ggsave(
  filename = "plots/PDTM_combined_histograms.png",
  plot     = combined_pdtm,
  width    = 12,
  height   = 30
)

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 3. Build the FDP and PDTM box‐plots
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

fdp_boxplots <- lapply(names(metrics_fdp), function(name) {
  m <- metrics_fdp[[name]]
  ggplot(df_fdp, aes(x = factor(threshold), y = .data[[m$col]])) +
    geom_boxplot(fill = "tomato") +
    labs(title = paste("FDP Box-plot :", m$ylab),
         x     = "Threshold",
         y     = m$ylab) +
    theme_minimal()
})

pdtm_boxplots <- lapply(names(metrics_pdtm), function(name) {
  m <- metrics_pdtm[[name]]
  ggplot(df_pdtm, aes(x = factor(threshold), y = .data[[m$col]])) +
    geom_boxplot(fill = "steelblue") +
    labs(title = paste("PDTM Box-plot :", m$ylab),
         x     = "Threshold",
         y     = m$ylab) +
    theme_minimal()
})

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 4. Build the FDP and PDTM QQ‐plots
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

fdp_qq <- lapply(names(metrics_fdp), function(name) {
  m <- metrics_fdp[[name]]
  ggplot(df_fdp, aes(sample = .data[[m$col]])) +
    stat_qq(color = "tomato") +
    stat_qq_line() +
    labs(title = paste("FDP QQ:", m$ylab),
         x     = "Theoretical Quantiles",
         y     = m$ylab) +
    theme_minimal()
})

pdtm_qq <- lapply(names(metrics_pdtm), function(name) {
  m <- metrics_pdtm[[name]]
  ggplot(df_pdtm, aes(sample = .data[[m$col]])) +
    stat_qq(color = "steelblue") +
    stat_qq_line() +
    labs(title = paste("PDTM QQ:", m$ylab),
         x     = "Theoretical Quantiles",
         y     = m$ylab) +
    theme_minimal()
})

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 5. Pad FDP lists so they and PDTM lists have equal length
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

pad_count     <- length(pdtm_boxplots) - length(fdp_boxplots)
fdp_boxplots  <- c(fdp_boxplots, replicate(pad_count, nullGrob(), simplify = FALSE))
fdp_qq        <- c(fdp_qq,       replicate(pad_count, nullGrob(), simplify = FALSE))

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 6. Combine & save box‐plots (2‐col: FDP left, PDTM right)
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

combined_box <- arrangeGrob(
  grobs = c(fdp_boxplots, pdtm_boxplots),
  ncol  = 2,
  top   = textGrob("Box plots: FDP (Top) vs PDTM (Bottom)",
                   gp = gpar(fontsize = 18, fontface = "bold"))
)

grid.newpage()
grid.draw(combined_box)
ggsave("plots/combined_boxplots.png", combined_box, width = 12, height = 20)

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 7. Combine & save QQ‐plots 
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

combined_qq <- arrangeGrob(
  grobs = c(fdp_qq, pdtm_qq),
  ncol  = 2,
  top   = textGrob("QQ plots: FDP (Top) vs PDTM (Bottom)",
                   gp = gpar(fontsize = 18, fontface = "bold"))
)

grid.newpage()
grid.draw(combined_qq)
ggsave("plots/combined_qqplots.png", combined_qq, width = 12, height = 20)

head(df_fdp)

#-----------------------------------------------------------------------------------
# Wilcoxon Test Prep
df_wide <- df_fdp %>% 
  # extract the repetition index so we can group by it
  mutate(
    repetition = as.integer(sub(".*repetition_(\\d+)$", "\\1", X__run_id))
  ) %>%
  # keep only what we need
  select(repetition, threshold, energy_j) %>%
  # pivot so that each repetition is one row, and each threshold becomes a column
  pivot_wider(
    id_cols      = repetition,        # <— only repetition here
    names_from   = threshold,
    names_prefix = "thr_",
    values_from  = energy_j
  )
head(df_wide)

diffs <- df_wide$thr_0.5 - df_wide$thr_1

qqnorm(diffs); qqline(diffs)
shapiro.test(diffs)

wilcox.test(
  df_wide$thr_0.5,
  df_wide$thr_1,
  paired      = TRUE,
  alternative = "two.sided"
)

# RQ1.1 FDP
# install.packages(c("dplyr","tidyr","effsize"))  # if not already installed

library(dplyr)
library(tidyr)
library(effsize)

# 1) Pivot to wide form (one row per repetition)
df_wide <- df_fdp %>%
  mutate(
    repetition = as.integer(sub(".*repetition_(\\d+)$", "\\1", X__run_id))
  ) %>%
  select(repetition, threshold, energy_j) %>%
  pivot_wider(
    id_cols      = repetition,
    names_from   = threshold,
    names_prefix = "thr_",
    values_from  = energy_j
  )

# 2) Define thresholds and baseline
thresholds <- c(0.1, 0.3, 0.5, 0.7, 0.9)
baseline   <- 1.0

# 3) Loop and run tests
results <- lapply(thresholds, function(th) {
  col_th   <- paste0("thr_", th)
  col_base <- paste0("thr_", baseline)
  
  # Paired differences
  diffs <- df_wide[[col_th]] - df_wide[[col_base]]
  
  # 3a) Shapiro–Wilk normality on diffs
  sw <- shapiro.test(diffs)
  
  # 3b) Wilcoxon signed‑rank test
  wt <- wilcox.test(
    df_wide[[col_th]],
    df_wide[[col_base]],
    paired      = TRUE,
    alternative = "two.sided"
  )
  
  # 3c) Cliff’s delta
  cd <- cliff.delta(
    df_wide[[col_th]],
    df_wide[[col_base]]
  )
  
  # Return a list/data.frame
  data.frame(
    threshold   = th,
    shapiro_W   = sw$statistic,
    shapiro_p   = sw$p.value,
    wilcox_V    = wt$statistic,
    wilcox_p    = wt$p.value,
    cliffs_d    = cd$estimate,
    cliffs_magnitude = cd$magnitude
  )
})

# Bind into a single data frame and print
results_df <- do.call(rbind, results)
print(results_df)


# Install required packages if not already installed
# install.packages(c("dplyr", "tidyr", "effsize"))

library(dplyr)
library(tidyr)
library(effsize)

# 1) Pivot the long PDTM data to wide form (one row per repetition)
df_pdtm_wide <- df_pdtm %>%
  mutate(
    repetition = as.integer(sub(".*repetition_(\\d+)$", "\\1", X__run_id))
  ) %>%
  select(repetition, threshold, energy_j) %>%
  pivot_wider(
    id_cols      = repetition,
    names_from   = threshold,
    names_prefix = "thr_",
    values_from  = energy_j
  )

# 2) Define the thresholds (excluding the baseline) and baseline
thresholds <- c(0.1, 0.3, 0.5, 0.7, 0.9)
baseline   <- 1.0

# 3) Loop through each threshold, compare to baseline via:
#    - Shapiro-Wilk normality test on paired differences
#    - Wilcoxon signed-rank test
#    - Cliff's delta for nonparametric effect size
results_pdtm <- lapply(thresholds, function(th) {
  col_th   <- paste0("thr_", th)
  col_base <- paste0("thr_", baseline)
  
  # Paired differences vector
  diffs <- df_pdtm_wide[[col_th]] - df_pdtm_wide[[col_base]]
  
  # 3a) Shapiro-Wilk test
  sw <- shapiro.test(diffs)
  
  # 3b) Wilcoxon signed-rank test
  wt <- wilcox.test(
    df_pdtm_wide[[col_th]],
    df_pdtm_wide[[col_base]],
    paired      = TRUE,
    alternative = "two.sided"
  )
  
  # 3c) Cliff's delta
  cd <- cliff.delta(
    df_pdtm_wide[[col_th]],
    df_pdtm_wide[[col_base]]
  )
  
  # Return results as a data frame
  data.frame(
    threshold        = th,
    shapiro_W        = unname(sw$statistic),
    shapiro_p        = sw$p.value,
    wilcox_V         = unname(wt$statistic),
    wilcox_p         = wt$p.value,
    cliffs_delta     = cd$estimate,
    cliffs_magnitude = cd$magnitude
  )
})

# 4) Combine into a single data frame and display
results_pdtm_df <- bind_rows(results_pdtm)
print(results_pdtm_df)

# install.packages(c("dplyr","tidyr","ggplot2","gridExtra"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# 1) Combine FDP and PDTM into one data frame
df_fdp2  <- df_fdp  %>% mutate(usecase = "FDP")
df_pdtm2 <- df_pdtm %>% mutate(usecase = "PDTM")

df_all <- bind_rows(
  df_fdp2  %>% select(usecase, threshold, energy_j),
  df_pdtm2 %>% select(usecase, threshold, energy_j)
)

# make threshold a factor to preserve ordering
df_all$threshold <- factor(df_all$threshold, levels = c(0.1,0.3,0.5,0.7,0.9,1.0))

# 2) Energy box‑plots side by side
p1 <- ggplot(df_all, aes(x = threshold, y = energy_j)) +
  geom_boxplot() +
  facet_wrap(~usecase, nrow = 1, scales = "free_y") +
  labs(
    x = "Offloading Threshold (α)",
    y = "Energy per Sample (J)",
    title = "Energy Consumption by Use Case and Threshold"
  ) +
  theme_minimal()

ggsave("energy_boxplots.png", p1, width = 10, height = 4, dpi = 300)

# 3) QQ‑plots side by side
# We'll compute the paired baseline differences just for visualization,
# but here we QQ‐plot the raw energy distributions at each threshold.
p2 <- ggplot(df_all, aes(sample = energy_j)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(usecase ~ threshold, scales = "free") +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
    title = "Q–Q Plots of Energy by Use Case and Threshold"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8)
  )

ggsave("energy_qqplots.png", p2, width = 12, height = 6, dpi = 300)

# Install required packages if not already installed
# install.packages(c("dplyr", "tidyr", "effsize", "ggplot2", "gridExtra"))

library(dplyr)
library(tidyr)
library(effsize)
library(ggplot2)
library(gridExtra)

### 1) Prepare wide‐form data and run Shapiro/Wilcoxon/Cliff’s δ for FDP execution time

# Pivot FDP to wide form (one row per repetition)
df_fdp_wide <- df_fdp %>%
  mutate(
    repetition = as.integer(sub(".*repetition_(\\d+)$", "\\1", X__run_id))
  ) %>%
  select(repetition, threshold, runtime_s) %>%     # runtime_s is in ms
  pivot_wider(
    id_cols      = repetition,
    names_from   = threshold,
    names_prefix = "thr_",
    values_from  = runtime_s
  )

# Define thresholds to compare vs. baseline = 1.0
thresholds <- c(0.1, 0.3, 0.5, 0.7, 0.9)
baseline   <- 1.0

# Loop over thresholds for FDP
results_fdp_time <- lapply(thresholds, function(th) {
  col_th   <- paste0("thr_", th)
  col_base <- paste0("thr_", baseline)
  
  diffs <- df_fdp_wide[[col_th]] - df_fdp_wide[[col_base]]
  
  sw <- shapiro.test(diffs)
  wt <- wilcox.test(
    df_fdp_wide[[col_th]],
    df_fdp_wide[[col_base]],
    paired      = TRUE,
    alternative = "two.sided"
  )
  cd <- cliff.delta(
    df_fdp_wide[[col_th]],
    df_fdp_wide[[col_base]]
  )
  
  data.frame(
    usecase         = "FDP",
    threshold       = th,
    shapiro_W       = unname(sw$statistic),
    shapiro_p       = sw$p.value,
    wilcox_V        = unname(wt$statistic),
    wilcox_p        = wt$p.value,
    cliffs_delta    = cd$estimate,
    cliffs_magnitude= cd$magnitude
  )
})

results_fdp_time_df <- bind_rows(results_fdp_time)
print(results_fdp_time_df)


### 2) Prepare wide‐form data and run Shapiro/Wilcoxon/Cliff’s δ for PDTM execution time

df_pdtm_wide <- df_pdtm %>%
  mutate(
    repetition = as.integer(sub(".*repetition_(\\d+)$", "\\1", X__run_id))
  ) %>%
  select(repetition, threshold, runtime_s) %>%
  pivot_wider(
    id_cols      = repetition,
    names_from   = threshold,
    names_prefix = "thr_",
    values_from  = runtime_s
  )

results_pdtm_time <- lapply(thresholds, function(th) {
  col_th   <- paste0("thr_", th)
  col_base <- paste0("thr_", baseline)
  
  diffs <- df_pdtm_wide[[col_th]] - df_pdtm_wide[[col_base]]
  
  sw <- shapiro.test(diffs)
  wt <- wilcox.test(
    df_pdtm_wide[[col_th]],
    df_pdtm_wide[[col_base]],
    paired      = TRUE,
    alternative = "two.sided"
  )
  cd <- cliff.delta(
    df_pdtm_wide[[col_th]],
    df_pdtm_wide[[col_base]]
  )
  
  data.frame(
    usecase         = "PDTM",
    threshold       = th,
    shapiro_W       = unname(sw$statistic),
    shapiro_p       = sw$p.value,
    wilcox_V        = unname(wt$statistic),
    wilcox_p        = wt$p.value,
    cliffs_delta    = cd$estimate,
    cliffs_magnitude= cd$magnitude
  )
})

results_pdtm_time_df <- bind_rows(results_pdtm_time)
print(results_pdtm_time_df)


### 3) Generate combined box‐plots and QQ‐plots for execution time

# Combine FDP and PDTM long‐form for plotting
df_fdp2  <- df_fdp  %>% mutate(usecase = "FDP", exec_time = runtime_s)
df_pdtm2 <- df_pdtm %>% mutate(usecase = "PDTM", exec_time = runtime_s)

df_all_time <- bind_rows(
  df_fdp2  %>% select(usecase, threshold, exec_time),
  df_pdtm2 %>% select(usecase, threshold, exec_time)
)

# Ensure threshold ordering
df_all_time$threshold <- factor(df_all_time$threshold,
                                levels = c(0.1,0.3,0.5,0.7,0.9,1.0))

# (a) Box‐plots side by side
p_time_box <- ggplot(df_all_time, aes(x = threshold, y = exec_time)) +
  geom_boxplot() +
  facet_wrap(~usecase, nrow = 1, scales = "free_y") +
  labs(
    x = "Offloading Threshold (α)",
    y = "Execution Time per Sample (ms)",
    title = "Execution Time by Use Case and Threshold"
  ) +
  theme_minimal()

ggsave("time_boxplots.png", p_time_box, width = 10, height = 4, dpi = 300)

# (b) Q–Q plots side by side
p_time_qq <- ggplot(df_all_time, aes(sample = exec_time)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(usecase ~ threshold, scales = "free") +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
    title = "Q–Q Plots of Execution Time by Use Case and Threshold"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8)
  )

ggsave("time_qqplots.png", p_time_qq, width = 12, height = 6, dpi = 300)

# Accuracy
# Install required packages if needed
# install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

# 1) Prepare median‑only summary for plotting
# Assuming you already have summary tables:
#   summary_table_fdp with columns usecase, threshold, median_accuracy
#   summary_table_pdtm with columns usecase, threshold, median_accuracy_6h, etc.

# Pivot PDTM summary to long form
pdtm_med <- summary_table_pdtm %>%
  select(threshold,
         median_accuracy_6h,
         median_accuracy_12h,
         median_accuracy_24h) %>%
  pivot_longer(
    cols      = starts_with("median_accuracy_"),
    names_to  = "usecase",
    values_to = "median_accuracy"
  ) %>%
  mutate(
    usecase = dplyr::recode(usecase,
                     "median_accuracy_6h"  = "PDTM-6h",
                     "median_accuracy_12h" = "PDTM-12h",
                     "median_accuracy_24h" = "PDTM-24h")
  )

# FDP summary (rename for consistency)
fdp_med <- summary_table_fdp %>%
  select(threshold, median_accuracy) %>%
  mutate(usecase = "FDP")

# Combine
df_med_all <- bind_rows(fdp_med, pdtm_med)

# 2) Convert threshold to numeric and usecase to factor
df_med_all <- df_med_all %>%
  mutate(
    threshold = as.numeric(threshold),
    usecase   = factor(usecase,
                       levels = c("FDP", "PDTM-6h", "PDTM-12h", "PDTM-24h"))
  )

# 3) Plot medians only with distinct colours and shapes
p_med <- ggplot(df_med_all, aes(x = threshold, y = median_accuracy,
                                color = usecase, shape = usecase)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "FDP"        = "#1f78b4",  # blue
    "PDTM-6h"    = "#33a02c",  # green
    "PDTM-12h"   = "#e31a1c",  # red
    "PDTM-24h"   = "#ff7f00"   # orange
  )) +
  scale_shape_manual(values = c(
    "FDP"        = 18,  # diamond
    "PDTM-6h"    = 16,  # circle
    "PDTM-12h"   = 17,  # triangle
    "PDTM-24h"   = 15   # square
  )) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x     = "Offloading Threshold (α)",
    y     = "Median Accuracy",
    color = "Use Case",
    shape = "Use Case",
    title = "Median Accuracy vs. Threshold for FDP and PDTM"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 4) Save and display
ggsave("median_accuracy_plot.png", p_med, width = 8, height = 5, dpi = 300)
print(p_med)

# For RQ2
# install.packages("dplyr")    # if needed
library(dplyr)

#
# 1) FDP: Spearman ρ tests
#
with(df_fdp, {
  cat("### FDP use case ###\n")
  # Energy vs. α
  e_test <- cor.test(threshold, energy_j, method = "spearman")
  cat("Energy vs. α:  rho =", round(e_test$estimate,3),
      ", p =", signif(e_test$p.value,3), "\n")
  # Execution time vs. α
  t_test <- cor.test(threshold, runtime_s, method = "spearman")
  cat("Time vs. α:    rho =", round(t_test$estimate,3),
      ", p =", signif(t_test$p.value,3), "\n")
  # Accuracy vs. α
  a_test <- cor.test(threshold, accuracy, method = "spearman")
  cat("Accuracy vs. α: rho =", round(a_test$estimate,3),
      ", p =", signif(a_test$p.value,3), "\n\n")
})

#
# 2) PDTM: Spearman ρ tests
#
with(df_pdtm, {
  cat("### PDTM use case ###\n")
  # Energy vs. α
  e_test <- cor.test(threshold, energy_j, method = "spearman")
  cat("Energy vs. α:  rho =", round(e_test$estimate,3),
      ", p =", signif(e_test$p.value,3), "\n")
  # Execution time vs. α
  t_test <- cor.test(threshold, runtime_s, method = "spearman")
  cat("Time vs. α:    rho =", round(t_test$estimate,3),
      ", p =", signif(t_test$p.value,3), "\n")
  # Accuracy for each horizon vs. α
  for(h in c("accuracy_6h","accuracy_12h","accuracy_24h")) {
    a_test <- cor.test(df_pdtm[[ "threshold" ]],
                       df_pdtm[[ h ]], method = "spearman")
    cat(sprintf("%s vs. α: rho = %.3f, p = %s\n",
                h, round(a_test$estimate,3), signif(a_test$p.value,3)))
  }
})

#-----
# For Holm's
# Robust pairwise test function (handles constant diffs / few samples)
library(dplyr)
library(tidyr)
library(effsize)   # for cliff.delta
library(stats)

run_pairwise_tests <- function(df_long, metric_col, id_col = "X__run_id",
                               baseline = 1.0,
                               thresholds = c(0.1,0.3,0.5,0.7,0.9)) {
  # df_long must have columns: id_col, threshold, <metric_col>
  # Returns tidy data.frame with one row per threshold (compared to baseline)
  
  # 1) pivot wide (one row per repetition)
  df_wide <- df_long %>%
    mutate(repetition = as.integer(sub(".*repetition_(\\d+)$", "\\1", .data[[id_col]]))) %>%
    select(repetition, threshold, !!rlang::sym(metric_col)) %>%
    pivot_wider(
      id_cols = repetition,
      names_from = threshold,
      names_prefix = "thr_",
      values_from = !!rlang::sym(metric_col)
    )
  
  # helper to safe-run tests on two numeric vectors (paired)
  safe_tests <- function(x, y) {
    # drop NA pairs
    keep <- !is.na(x) & !is.na(y)
    x <- x[keep]; y <- y[keep]
    n <- length(x)
    
    # initialize
    out <- list(
      n_pairs = n,
      shapiro_W = NA_real_,
      shapiro_p = NA_real_,
      wilcox_V  = NA_real_,
      wilcox_p  = NA_real_,
      cliffs_d  = NA_real_,
      cliffs_mag= NA_character_
    )
    
    if (n == 0) {
      return(out)
    }
    
    # paired diffs
    diffs <- x - y
    # Shapiro: only when >= 3 samples AND not constant
    if (n >= 3 && !all(diffs == diffs[1])) {
      sw <- tryCatch(shapiro.test(diffs), error = function(e) NULL)
      if (!is.null(sw)) {
        out$shapiro_W <- unname(sw$statistic)
        out$shapiro_p <- sw$p.value
      }
    } else {
      out$shapiro_W <- NA_real_
      out$shapiro_p <- NA_real_
    }
    
    # Wilcoxon signed rank (paired) - if all x == y then trivially no difference
    if (all(x == y)) {
      out$wilcox_V <- NA_real_
      out$wilcox_p <- 1.0
    } else {
      wt <- tryCatch(
        wilcox.test(x, y, paired = TRUE, alternative = "two.sided", exact = FALSE),
        error = function(e) NULL
      )
      if (!is.null(wt)) {
        # wt$statistic may be named; coerce to numeric
        out$wilcox_V <- as.numeric(unname(wt$statistic))
        out$wilcox_p <- wt$p.value
      }
    }
    
    # Cliff's delta (effsize::cliff.delta) - handle errors
    cd <- tryCatch(cliff.delta(x, y), error = function(e) NULL)
    if (!is.null(cd)) {
      # cd$estimate is numeric; cd$magnitude is text
      out$cliffs_d   <- as.numeric(cd$estimate)
      out$cliffs_mag <- as.character(cd$magnitude)
    }
    return(out)
  }
  
  # 2) iterate thresholds and collect results
  results_list <- lapply(thresholds, function(th) {
    col_th   <- paste0("thr_", th)
    col_base <- paste0("thr_", baseline)
    if (! (col_th %in% colnames(df_wide)) ) {
      stop("Column ", col_th, " not found in wide data. Check thresholds present.")
    }
    if (! (col_base %in% colnames(df_wide)) ) {
      stop("Baseline column ", col_base, " not found in wide data.")
    }
    
    x <- df_wide[[col_th]]
    y <- df_wide[[col_base]]
    tst <- safe_tests(x, y)
    
    data.frame(
      metric     = metric_col,
      threshold  = th,
      n_pairs    = tst$n_pairs,
      shapiro_W  = tst$shapiro_W,
      shapiro_p  = tst$shapiro_p,
      wilcox_V   = tst$wilcox_V,
      wilcox_p   = tst$wilcox_p,
      cliffs_d   = tst$cliffs_d,
      cliffs_mag = tst$cliffs_mag,
      stringsAsFactors = FALSE
    )
  })
  
  bind_rows(results_list)
}

res_fdp_energy <- run_pairwise_tests(df_fdp, "energy_j")
res_fdp_time   <- run_pairwise_tests(df_fdp, "runtime_s")
res_fdp_acc    <- run_pairwise_tests(df_fdp, "accuracy")

# Combine FDP results into one table (add usecase column)
results_fdp_all <- bind_rows(res_fdp_energy, res_fdp_time, res_fdp_acc) %>%
  mutate(usecase = "FDP")
print(results_fdp_all)

res_pdtm_energy <- run_pairwise_tests(df_pdtm, "energy_j")
res_pdtm_time   <- run_pairwise_tests(df_pdtm, "runtime_s")

res_pdtm_acc_6  <- run_pairwise_tests(df_pdtm, "accuracy_6h")
res_pdtm_acc_12 <- run_pairwise_tests(df_pdtm, "accuracy_12h")
res_pdtm_acc_24 <- run_pairwise_tests(df_pdtm, "accuracy_24h")

results_pdtm_all <- bind_rows(res_pdtm_energy, res_pdtm_time,
                              res_pdtm_acc_6, res_pdtm_acc_12, res_pdtm_acc_24) %>%
  mutate(usecase = "PDTM")
print(results_pdtm_all)


library(dplyr)

# make a single combined results table
all_results_raw <- bind_rows(results_fdp_all, results_pdtm_all)

# Apply Holm within each family (usecase x metric)
all_results_adj <- all_results_raw %>%
  group_by(usecase, metric) %>%
  arrange(wilcox_p) %>%   # optional
  mutate(
    p_holm = p.adjust(wilcox_p, method = "holm"),
    sig_holm = ifelse(is.na(p_holm), NA, p_holm < 0.05)
  ) %>%
  ungroup()

# View
print(all_results_adj)


#-------------------------------------------------------------------------------------
# Repeating the plots for better visibility
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Ensure thresholds have the proper factor ordering
thr_levels <- c(0.1, 0.3, 0.5, 0.7, 0.9, 1.0)
df_fdp2  <- df_fdp  %>% mutate(thr_f = factor(threshold, levels = thr_levels))
df_pdtm2 <- df_pdtm %>% mutate(thr_f = factor(threshold, levels = thr_levels))

# colour palette (6 colours)
pal <- RColorBrewer::brewer.pal(6, "Set2")

# ---------- Helper to add median line, point and label ----------
add_median_layer <- function(df, ycol) {
  # compute medians per threshold (for numeric labels)
  med_df <- df %>%
    group_by(thr_f) %>%
    summarize(med = median(.data[[ycol]], na.rm = TRUE)) %>%
    ungroup()
  
  # return list of geom/stat layers to add to ggplot
  list(
    # black median horizontal line (crossbar with zero height)
    stat_summary(fun = median, geom = "crossbar", width = 0.6,
                 colour = "black", fatten = 0, size = 0.8, show.legend = FALSE),
    # black median point
    stat_summary(fun = median, geom = "point", size = 3, colour = "black", show.legend = FALSE),
    # numeric label above median
    geom_text(data = med_df, aes(x = thr_f, y = med, label = sprintf("%.1f", med)),
              vjust = -0.8, colour = "black", size = 3)
  )
}

# ---------- ENERGY: FDP ----------
p_energy_fdp <- ggplot(df_fdp2, aes(x = thr_f, y = energy_j, fill = thr_f)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.9) +
  scale_fill_manual(values = pal) +
  labs(title = "FDP: Energy per Sample by Threshold",
       x = "Gating Threshold (α)", y = "Energy per Sample (J)", fill = "Threshold") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  add_median_layer(df_fdp2, "energy_j")  # add median line/point/label

ggsave("plots/energy_boxplot_FDP_median_black.png", p_energy_fdp, width = 8, height = 5, dpi = 300)

# ---------- ENERGY QQ (FDP) ----------
# QQ panels keep coloured points but medians aren't relevant for QQ; keep legend hidden
p_energy_qq_fdp <- ggplot(df_fdp2, aes(sample = energy_j, colour = thr_f)) +
  stat_qq() + stat_qq_line() +
  scale_colour_manual(values = pal) +
  facet_wrap(~ thr_f, nrow = 1, scales = "free") +
  labs(title = "FDP: Q–Q Plots of Energy by Threshold",
       x = "Theoretical Quantiles", y = "Sample Quantiles", colour = "Threshold") +
  theme_minimal() + theme(legend.position = "none", strip.text = element_text(size = 9))

ggsave("plots/energy_qq_FDP.png", p_energy_qq_fdp, width = 12, height = 3, dpi = 300)


# ---------- ENERGY: PDTM ----------
p_energy_pdtm <- ggplot(df_pdtm2, aes(x = thr_f, y = energy_j, fill = thr_f)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.9) +
  scale_fill_manual(values = pal) +
  labs(title = "PDTM: Energy per Sample by Threshold",
       x = "Gating Threshold (α)", y = "Energy per Sample (J)", fill = "Threshold") +
  theme_minimal() + theme(legend.position = "bottom") +
  add_median_layer(df_pdtm2, "energy_j")

ggsave("plots/energy_boxplot_PDTM_median_black.png", p_energy_pdtm, width = 8, height = 5, dpi = 300)

p_energy_qq_pdtm <- ggplot(df_pdtm2, aes(sample = energy_j, colour = thr_f)) +
  stat_qq() + stat_qq_line() +
  scale_colour_manual(values = pal) +
  facet_wrap(~ thr_f, nrow = 1, scales = "free") +
  labs(title = "PDTM: Q–Q Plots of Energy by Threshold",
       x = "Theoretical Quantiles", y = "Sample Quantiles", colour = "Threshold") +
  theme_minimal() + theme(legend.position = "none", strip.text = element_text(size = 9))

ggsave("plots/energy_qq_PDTM.png", p_energy_qq_pdtm, width = 12, height = 3, dpi = 300)


# ---------- EXECUTION TIME: FDP ----------
p_time_fdp <- ggplot(df_fdp2, aes(x = thr_f, y = runtime_s, fill = thr_f)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.9) +
  scale_fill_manual(values = pal) +
  labs(title = "FDP: Execution Time per Sample by Threshold",
       x = "Gating Threshold (α)", y = "Execution Time per Sample (ms)", fill = "Threshold") +
  theme_minimal() + theme(legend.position = "bottom") +
  add_median_layer(df_fdp2, "runtime_s")

ggsave("plots/time_boxplot_FDP_median_black.png", p_time_fdp, width = 8, height = 5, dpi = 300)

p_time_qq_fdp <- ggplot(df_fdp2, aes(sample = runtime_s, colour = thr_f)) +
  stat_qq() + stat_qq_line() +
  scale_colour_manual(values = pal) +
  facet_wrap(~ thr_f, nrow = 1, scales = "free") +
  labs(title = "FDP: Q–Q Plots of Execution Time by Threshold",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() + theme(legend.position = "none", strip.text = element_text(size = 9))

ggsave("plots/time_qq_FDP.png", p_time_qq_fdp, width = 12, height = 3, dpi = 300)


# ---------- EXECUTION TIME: PDTM ----------
p_time_pdtm <- ggplot(df_pdtm2, aes(x = thr_f, y = runtime_s, fill = thr_f)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.9) +
  scale_fill_manual(values = pal) +
  labs(title = "PDTM: Execution Time per Sample by Threshold",
       x = "Gating Threshold (α)", y = "Execution Time per Sample (ms)", fill = "Threshold") +
  theme_minimal() + theme(legend.position = "bottom") +
  add_median_layer(df_pdtm2, "runtime_s")

ggsave("plots/time_boxplot_PDTM_median_black.png", p_time_pdtm, width = 8, height = 5, dpi = 300)

p_time_qq_pdtm <- ggplot(df_pdtm2, aes(sample = runtime_s, colour = thr_f)) +
  stat_qq() + stat_qq_line() +
  scale_colour_manual(values = pal) +
  facet_wrap(~ thr_f, nrow = 1, scales = "free") +
  labs(title = "PDTM: Q–Q Plots of Execution Time by Threshold",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() + theme(legend.position = "none", strip.text = element_text(size = 9))

ggsave("plots/time_qq_PDTM.png", p_time_qq_pdtm, width = 12, height = 3, dpi = 300)

message("Saved black-median boxplots and QQ plots into the 'plots/' folder.")

