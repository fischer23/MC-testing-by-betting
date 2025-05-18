# R Code for generating the data of the Figures S.1 and S.2 in the paper "Sequential Monte-Carlo testing by betting"

rm(list = ls())
library(ggplot2)
library(patchwork)

# Input: strength of the alternative mu, significance level alpha, filename output.

data_logp <- function(mu, alpha, filename) {
  n <- 1000 # Number of obs. per trial
  m <- 2000 # Number of simulated trials
  B <- 1000 # Number of perm. per trial
  prop_treated <- 0.5 # Probability of obs. being treated
  c <- alpha * 0.90 # Parameter for binomial mixture strategy
  p_zero <- 1 / ceiling(sqrt(2 * pi * exp(1 / 6)) / alpha) # Parameter for binomial strategy with uniform prior
  a <- 1 # Parameter for binomial mixture strategy with beta prior
  b1 <- 50 # Parameter for binomial mixture strategy with beta prior
  b2 <- 100 # Parameter for binomial mixture strategy with beta prior
  b3 <- 200 # Parameter for binomial mixture strategy with beta prior


  p_perm <- rep(0, m) # permutation p-value
  p_bin <- rep(0, m) # p-value binomial strategy
  p_agg <- rep(0, m) # p-value aggressive strategy
  p_bm <- rep(0, m) # p-value binomial mixture strategy with uniform prior
  p_bm_beta_b1 <- rep(0, m) # p-value binomial mixture strategy with beta prior (b=50)
  p_bm_beta_b2 <- rep(0, m) # p-value binomial mixture strategy with beta prior (b=100)
  p_bm_beta_b3 <- rep(0, m) # p-value binomial mixture strategy with beta prior (b=200)

  for (j in 1:m) {
    X <- rnorm(n)

    treated <- (runif(n) >= prop_treated)
    X <- X + mu * treated

    test_stat <- mean(X[treated]) - mean(X[!treated]) # Observed test statistic

    test_stat_perm <- c()
    bet_bin <- c()
    bet_agg <- c()
    rank <- 1 # Rank observed test statistic
    wealth_bin <- c(1)
    wealth_agg <- c(1)
    wealth_bm_beta_b1 <- c()
    wealth_bm_beta_b2 <- c()
    wealth_bm_beta_b3 <- c()
    wealth_bm <- c()

    for (i in 1:B) {
      X_perm <- sample(X)

      test_stat_perm[i] <- mean(X_perm[treated]) - mean(X_perm[!treated]) # Permuted test statistic

      if (test_stat_perm[i] >= test_stat) {
        bet_bin[i] <- p_zero * (i + 1) / rank
        bet_agg[i] <- 0
        rank <- rank + 1
      } else {
        bet_bin[i] <- (1 - p_zero) * (i + 1) / (i - rank + 1)
        bet_agg[i] <- (i + 1) / i
      }

      wealth_bin <- cumprod(bet_bin)
      wealth_agg <- cumprod(bet_agg)

      wealth_bm[i] <- (1 - pbinom(rank - 1, i + 1, c)) / c
      wealth_bm_beta_b1[i] <- beta(a + rank - 1, b1 + i - rank + 1) / (beta(rank, i - rank + 2) * beta(a, b1))
      wealth_bm_beta_b2[i] <- beta(a + rank - 1, b2 + i - rank + 1) / (beta(rank, i - rank + 2) * beta(a, b2))
      wealth_bm_beta_b3[i] <- beta(a + rank - 1, b3 + i - rank + 1) / (beta(rank, i - rank + 2) * beta(a, b3))
    }


    p_perm[j] <- (1 + sum(test_stat_perm[1:B] >= test_stat)) / (B + 1)

    p_bin[j] <- 1 / max(wealth_bin)

    p_agg[j] <- 1 / max(wealth_agg)

    p_bm[j] <- 1 / max(wealth_bm)
    p_bm_beta_b1[j] <- 1 / max(wealth_bm_beta_b1)
    p_bm_beta_b2[j] <- 1 / max(wealth_bm_beta_b2)
    p_bm_beta_b3[j] <- 1 / max(wealth_bm_beta_b3)
  }

  save(p_perm, p_agg, p_bm, p_bin, p_bm_beta_b1, p_bm_beta_b2, p_bm_beta_b3, file = filename)
}

set.seed(123)
data_logp(mu = 0.1, alpha = 0.05, filename = "pval_alpha005_mu01.rda")
data_logp(mu = 1, alpha = 0.05, filename = "pval_alpha005_mu1.rda")
data_logp(mu = 0.01, alpha = 0.05, filename = "pval_alpha005_mu001.rda")
data_logp(mu = 0.1, alpha = 0.01, filename = "pval_alpha001_mu01.rda")
data_logp(mu = 1, alpha = 0.01, filename = "pval_alpha001_mu1.rda")
data_logp(mu = 0.01, alpha = 0.01, filename = "pval_alpha001_mu001.rda")
