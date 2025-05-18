# Performs the real data analysis in Section 6 of the paper "Multiple testing with anytime-valid Monte-Carlo p-values"

library(reticulate)
np <- import("numpy")

source("sequential_BH.R")


r2_perm <- c()
r2_true <- c()

for (j in 1:8) {
  # read the data for each participant and save it in vector
  # The data needs to be downloaded at https://osf.io/8fsnx/
  data_perm <- np$load(paste("model_fits/S0", j, "/all_fit_params_perm.npy", sep = ""), allow_pickle = TRUE)
  data_true <- np$load(paste("model_fits/S0", j, "/all_fit_params.npy", sep = ""), allow_pickle = TRUE)

  r2_perm <- rbind(r2_perm, drop(data_perm[[1]]$val_r2))
  r2_true <- c(r2_true, drop(data_true[[1]]$val_r2))
}


M <- length(r2_true)
B <- 1000
alpha <- 0.01

# Apply sequential permutation tests
results_bm <- sequential_BH(true_stat = r2_true, perm_stat = r2_perm, M = M, B = B, alpha = alpha, param_bm = 0.6, method = "bm")
results_bc <- sequential_BH(true_stat = r2_true, perm_stat = r2_perm, M = M, B = B, alpha = alpha, param_bc = 3, method = "bc")
results_agg <- sequential_BH(true_stat = r2_true, perm_stat = r2_perm, M = M, B = B, alpha = alpha, param_bc = 1, method = "bc")
results_amt <- sequential_BH(
  true_stat = r2_true, perm_stat = r2_perm, M = M, B = B, alpha = (alpha - 0.001), param_amt = 0.001,
  peeking_times_AMT = c(cumsum(c(10, 10 * cumprod(rep(1.1, 24)))), 1000), method = "AMT"
)

# Calculate proportion of rejections and mean stopping times
length(results_bm$rejects) / M
length(results_bc$rejects) / M
length(results_agg$rejects) / M
length(results_amt$rejects) / M
mean(results_bm$stop)
mean(results_bc$stop)
mean(results_agg$stop)
mean(results_amt$stop)

# Calculate proportion of rejections for the classical permutation p-value
p_vals <- (rowSums(r2_perm[1:M, ] >= r2_true[1:M]) + 1) / (B + 1)
p_vals_BH <- p.adjust(p_vals, "fdr")
mean(p_vals_BH <= 0.01)


# Reference
# Margaret M Henderson, Michael J Tarr, and Leila Wehbe. A texture statistics encoding model
# reveals hierarchical feature selectivity across human visual cortex. Journal of Neuroscience, 43
# (22):4144–4161, 2023.
