# load packages
library(ggplot2)
library(MASS)
library(binom)

# load BH procedure with sequential p-values
source("sequential_BH.R")

# load function to simulate data
source("data_generator.R")
source("data_generator_PRDS.R")

# results for different proportions of false hypotheses
set.seed(123)
data_generator(
  nsim = 10, nhyp = 1000, mu_As = 2.5, mu_N = 0, pis = seq(0.1, 0.9, 0.1), alphas = 0.1,
  param_bm = 0.9, param_bc = 10, param_amt = 0.01, peeking_times_AMT = floor(c(cumsum(c(100, 100 * cumprod(rep(1.1, 24)))), 10000)),
  B_large = 10000, B_small = 200,
  filename = "results/Plot_FDR_pis.rda"
)

# results for different overall significance levels
set.seed(123)
data_generator(
  nsim = 10, nhyp = 1000, mu_As = 2.5, mu_N = 0, pis = 0.4, alphas = c(0.01, 0.05, 0.1, 0.15, 0.2),
  param_bm = 0.9, param_bc = 10, param_amt = 0.01, peeking_times_AMT = floor(c(cumsum(c(100, 100 * cumprod(rep(1.1, 24)))), 10000)),
  B_large = 10000, B_small = 200,
  filename = "results/Plot_FDR_alphas.rda"
)

# results for different strengths of the alternative
set.seed(123)
data_generator(
  nsim = 10, nhyp = 1000, mu_As = c(1, 1.5, 2, 2.5, 3, 3.5, 4), mu_N = 0, pis = 0.4, alphas = 0.1,
  param_bm = 0.9, param_bc = 10, param_amt = 0.01, peeking_times_AMT = floor(c(cumsum(c(100, 100 * cumprod(rep(1.1, 24)))), 10000)),
  B_large = 10000, B_small = 200,
  filename = "results/Plot_FDR_mus.rda"
)

# results for different numbers of hypotheses
set.seed(123)
data_generator(
  nsim = 10, nhyp = c(100, 500, 1000, 3000, 5000, 10000), mu_As = 2.5, mu_N = 0, pis = 4, alphas = 0.1,
  param_bm = 0.9, param_bc = 10, param_amt = 0.01, peeking_times_AMT = floor(c(cumsum(c(100, 100 * cumprod(rep(1.1, 24)))), 10000)),
  B_large = 10000, B_small = 200,
  filename = "results/Plot_FDR_Ms.rda"
)

# Results under PRDS p-values for different strengths of correlation
set.seed(123)
plot_generator_PRDS(
  nsim = 1000, nhyp = 1000, mu_As = 2.5, mu_N = 0, pis = 0.4, rhos = c(0, 0.1, 0.3, 0.5, 0.7, 0.9), alphas = 0.1,
  param_bm = 0.9, param_bc = 10, param_amt = 0.01, peeking_times_AMT = floor(c(cumsum(c(100, 100 * cumprod(rep(1.1, 24)))), 10000)),
  B_large = 10000, B_small = 200,
  filename = "results/Plot_FDR_rhos.rda"
)

# results for a standard permutation run
set.seed(123)
data_generator(
  nsim = 1, nhyp = 1000, mu_As = 2.5, mu_N = 0, pis = 0.4, alphas = 0.1,
  param_bm = 0.9, param_bc = 10, param_amt = 0.01, peeking_times_AMT = floor(c(cumsum(c(100, 100 * cumprod(rep(1.1, 24)))), 10000)),
  B_large = 10000, B_small = 200,
  filename = "results/Plot_FDR_standard.rda"
)
