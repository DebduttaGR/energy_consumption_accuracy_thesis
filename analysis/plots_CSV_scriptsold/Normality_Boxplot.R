#––– 0. Setup & data load ––––––––––––––––––––––––––––––––––––––––––––––––––

library(dplyr)
library(tidyr)
library(ggplot2)

# adjust these paths if needed:
df_fdp <- read.csv("/Users/debduttaguharoy/Developer/Y2 - Master's Thesis/Usecases/experiment-runner/examples/uc1/experiments/flight_delay_two_phase_ssh/run_table.csv")  # Flight Delay Prediction
df_pdtm <- read.csv("/Users/debduttaguharoy/Developer/Y2 - Master's Thesis/Usecases/experiment-runner/examples/uc2/experiments/predicting_death_time_and_mortality_ssh/run_table.csv")  # Predicting Death Time and Mortality

# create output folder
if (!dir.exists("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots")) dir.create("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots")
if (!dir.exists("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/normality")) dir.create("/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/normality")

#––– Utility to save hist & QQ ––––––––––––––––––––––––––––––––––––––––––––

save_normality_plots <- function(vec, name, prefix="/Users/debduttaguharoy/Developer/Y2\ -\ Master\'s\ Thesis/Usecases/analysis/plots/normality") {
  # histogram
  hist_file <- file.path(prefix, paste0(name, "_hist.png"))
  png(hist_file, width = 600, height = 600)
  hist(vec,
       main = paste("Histogram of", name),
       xlab = name,
       col  = "lightgray")
  dev.off()
  message("Saved ", hist_file)
  
  # QQ plot
  qq_file <- file.path(prefix, paste0(name, "_qq.png"))
  png(qq_file, width = 600, height = 600)
  qqnorm(vec, main = paste("QQ-plot of", name))
  qqline(vec, col = "blue")
  dev.off()
  message("Saved ", qq_file)
  
  # also print the Shapiro–Wilk result
  sw <- shapiro.test(vec)
  cat("Shapiro-Wilk for", name, "W =", round(sw$statistic,3),
      "p =", signif(sw$p.value,3), "\n\n")
}
#––– FDP normality & saving ––––––––––––––––––––––––––––––––––––––––––––––

metrics_fdp <- c("energy_j", "runtime_s", "accuracy")
for (m in metrics_fdp) {
  vec <- df_fdp[[m]]
  save_normality_plots(vec, paste0("FDP_", m))
}

#––– PDTM normality & saving –––––––––––––––––––––––––––––––––––––––––––––

# Energy & runtime
save_normality_plots(df_pdtm$energy_j,  "PDTM_energy_j")
save_normality_plots(df_pdtm$runtime_s, "PDTM_runtime_s")

# Accuracy per window
df_pdtm %>%
  pivot_longer(
    cols       = matches("^accuracy_\\d+h$"),
    names_to   = "window",
    names_prefix = "accuracy_",
    values_to  = "value"
  ) %>%
  group_by(window) %>%
  summarise(vec = list(value)) %>%
  rowwise() %>%
  do({
    w <- .$window
    v <- unlist(.$vec)
    save_normality_plots(v, paste0("PDTM_accuracy_", w))
    data.frame()  # dummy
  })