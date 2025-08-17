# plot_accuracy_invocation.R
# Run: edit file paths below, then Rscript plot_accuracy_invocation.R

library(tidyverse)

# ----- EDIT: point to your CSV files -----
file_in_fdp  <- "/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/experiment-runner/examples/uc1/experiments/flight_delay_two_phase_ssh/run_table.csv"   # FDP CSV (has columns: threshold, accuracy, invocation_rate)
file_in_pdtm <- "/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/experiment-runner/examples/uc2/experiments/predicting_death_time_and_mortality_ssh/run_table.csv"  # PDTM CSV (columns like: threshold, accuracy_6h, accuracy_12h, accuracy_24h, invocation_rate_6h, ...)

# ----- read -----
fdp  <- read_csv(file_in_fdp, col_types = cols()) %>% mutate(threshold = as.numeric(threshold))
pdtm <- read_csv(file_in_pdtm, col_types = cols()) %>% mutate(threshold = as.numeric(threshold))

# optional: drop negative energy rows if present (keeps thresholds consistent)
if ("energy_j" %in% names(fdp))  fdp  <- fdp  %>% filter(is.na(energy_j) | energy_j >= 0)
if ("energy_j" %in% names(pdtm)) pdtm <- pdtm %>% filter(is.na(energy_j) | energy_j >= 0)

# ----- FDP medians -----
fdp_med <- fdp %>%
  group_by(threshold) %>%
  summarise(
    median_accuracy = median(accuracy, na.rm = TRUE) * 100,
    median_invocation = median(invocation_rate, na.rm = TRUE) * 100
  ) %>%
  mutate(usecase = "FDP") %>%
  ungroup()

# ----- PDTM medians (per horizon) -----
# detect which horizons exist in file
horizons <- c("6h","12h","24h")
acc_cols <- paste0("accuracy_", horizons)
inv_cols <- paste0("invocation_rate_", horizons)

pdtm_med_list <- list()
for (h in horizons) {
  a_col <- paste0("accuracy_", h)
  i_col <- paste0("invocation_rate_", h)
  if (a_col %in% names(pdtm)) {
    tmp <- pdtm %>%
      group_by(threshold) %>%
      summarise(
        median_accuracy = median(.data[[a_col]], na.rm = TRUE) * 100,
        median_invocation = median(.data[[i_col]], na.rm = TRUE) * 100
      ) %>%
      mutate(usecase = paste0("PDTM-", h)) %>%
      ungroup()
    pdtm_med_list[[h]] <- tmp
  }
}
pdtm_med <- bind_rows(pdtm_med_list)

# ----- Combine into long format for plotting -----
accuracy_long <- bind_rows(
  fdp_med %>% select(threshold, median_accuracy, usecase) %>% rename(median = median_accuracy),
  pdtm_med %>% select(threshold, median_accuracy, usecase) %>% rename(median = median_accuracy)
) %>% arrange(usecase, threshold)

invocation_long <- bind_rows(
  fdp_med %>% select(threshold, median_invocation, usecase) %>% rename(median = median_invocation),
  pdtm_med %>% select(threshold, median_invocation, usecase) %>% rename(median = median_invocation)
) %>% arrange(usecase, threshold)

# ensure thresholds plotted in numeric order and used as continuous x
breaks_x <- sort(unique(c(accuracy_long$threshold, invocation_long$threshold)))

# ----- Plot: Accuracy -----
p_acc <- ggplot(accuracy_long, aes(x = threshold, y = median, colour = usecase, group = usecase)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = breaks_x) +
  labs(title = "Median End-to-end Accuracy by Gating Threshold",
       x = "Gating Threshold (α)", y = "Accuracy (%)", colour = "Use case / Horizon") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("accuracy_by_threshold.png", p_acc, width = 8, height = 4.5, dpi = 300)
message("Saved accuracy_by_threshold.png")

# ----- Plot: Invocation Rate -----
p_inv <- ggplot(invocation_long, aes(x = threshold, y = median, colour = usecase, group = usecase)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = breaks_x) +
  labs(title = "Median Invocation Rate by Gating Threshold",
       x = "Gating Threshold (α)", y = "Invocation Rate (%)", colour = "Use case / Horizon") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("invocation_by_threshold.png", p_inv, width = 8, height = 4.5, dpi = 300)
message("Saved invocation_by_threshold.png")
