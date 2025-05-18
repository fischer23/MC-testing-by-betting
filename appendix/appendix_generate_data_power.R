# R Code for generating the data of the Figures S.3 - S.9 in the paper "Sequential Monte-Carlo testing by betting"

rm(list = ls())
library(ggplot2)
library(patchwork)
library(tibble)
library(reshape)

generate_power_data <- function(alpha, c, futility_stop, mus, n, dist = "normal") {
  # Set seed for reproducibility
  set.seed(123)

  # Set parameters                             #Number of obs. per trial
  m <- 2000 # Number of simulated trials
  B <- 1000 # Maximum number of permutations
  prop_treated <- 0.5 # Probability of obs. being treated
  h_bc <- alpha * B # Parameter for Besag Clifford


  # Power vectors
  power_bc <- rep(0, length(mus)) # Besag-Clifford strategy
  power_bm_r <- rep(0, length(mus)) # Randomized binomial mixture strategy

  # Number permutation vectors
  nPerm_bc <- rep(0, length(mus))
  nPerm_rej_bc <- rep(0, length(mus))
  nPerm_stop_bc <- rep(0, length(mus))

  nPerm_bm <- rep(0, length(mus))
  nPerm_bm_rej <- rep(0, length(mus))
  nPerm_bm_stop <- rep(0, length(mus))

  counter <- 1
  for (mu in mus) {
    # Initialize decision and index when decision was obtained

    idx_dec_bc <- rep(B, m)
    dec_bc <- rep(0, m)

    idx_dec_bm <- rep(B, m)
    dec_bm_r <- rep(0, m)

    for (j in 1:m) {
      X <- rnorm(n)

      treated <- (runif(n) >= prop_treated)
      X <- X + mu * treated

      if (dist == "log-normal") {
        X <- exp(X)
      }


      test_stat <- mean(X[treated]) - mean(X[!treated]) # Observed test statistic

      test_stat_perm <- c() # Permuted test statistics

      rank <- 1 # Rank of the observed test statistic

      # Wealth vector
      wealth_bm <- c()

      bc_count <- 1

      for (i in 1:B) {
        X_perm <- sample(X) # Permute data
        test_stat_perm[i] <- mean(X_perm[treated]) - mean(X_perm[!treated]) # Calculate permuted test statistic

        # We do not check for ties as X is continuously distributed and number of observations is large

        if (test_stat_perm[i] >= test_stat) {
          rank <- rank + 1
        }

        # Calculate wealth for each strategy

        if ((rank - 1) == h_bc & bc_count == 1 & i < B) {
          dec_bc[j] <- -1
          idx_dec_bc[j] <- i
          bc_count <- 0
        } else if (i == B & bc_count == 1) {
          idx_dec_bc[j] <- i
          dec_bc[j] <- 1
        }


        wealth_bm[i] <- (1 - pbinom(rank - 1, i + 1, c)) / c
        if (((min(wealth_bm) <= futility_stop & i > 1) | max(wealth_bm) > (1 / alpha)) & idx_dec_bm[j] == B) {
          idx_dec_bm[j] <- i
        }

        # if((idx_dec_bm[j] < B) & (idx_dec_bc[j] < B)){
        #   break
        # }
      }

      # dec=1 -> rejected, dec=-1 -> stopped for futility, dec=0 -> accepted but did not stop earlier

      if (wealth_bm[idx_dec_bm[j]] >= (runif(1) / alpha)) {
        dec_bm_r[j] <- 1
      } else if (wealth_bm[idx_dec_bm[j]] < futility_stop) {
        dec_bm_r[j] <- -1
      } else {
        dec_bm_r[j] <- 0
      }
    }

    power_bc[counter] <- mean((dec_bc > 0))
    power_bm_r[counter] <- mean((dec_bm_r > 0))

    nPerm_bc[counter] <- mean(idx_dec_bc)
    nPerm_rej_bc[counter] <- mean(idx_dec_bc[which(dec_bc == 1)])
    nPerm_stop_bc[counter] <- mean(idx_dec_bc[which(dec_bc == -1)])

    nPerm_bm[counter] <- mean(idx_dec_bm)
    nPerm_bm_rej[counter] <- mean(idx_dec_bm[which(dec_bm_r == 1)])
    nPerm_bm_stop[counter] <- mean(idx_dec_bm[which(dec_bm_r == -1)])

    counter <- counter + 1
  }

  return(data.frame(power_bc, power_bm_r, nPerm_bc, nPerm_rej_bc, nPerm_stop_bc, nPerm_bm, nPerm_bm_rej, nPerm_bm_stop))
}

alphas <- c(0.05, 0.01)
c_factors <- c(0.8, 0.9, 0.99)
dists <- c("normal", "log-normal")
ns <- c(100, 200, 500, 1000, 2000)
labs <- c("Binomial mixture randomized", "Besag-Clifford")
types <- c("Power", "Total #permutations", "#permutations till rejection", "#permutations till futility stop")
mus <- c(0.01, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5)

results_arr <- array(
  NA, c(length(mus), length(alphas), length(ns), length(labs), length(types), length(c_factors), length(dists)),
  list(mus, alphas, ns, labs, types, c_factors, dists)
)

for (i in 1:length(ns)) {
  for (k in 1:length(alphas)) {
    print(paste0("alpha", alphas[k], "n", ns[i]))
    df <- generate_power_data(alphas[k], alphas[k] * 0.9, alphas[k], mus, ns[i], "normal")
    results_arr[1:length(mus), k, i, "Besag-Clifford", "Power", 2, 1] <- df$power_bc
    results_arr[1:length(mus), k, i, "Besag-Clifford", "Total #permutations", 2, 1] <- df$nPerm_bc
    results_arr[1:length(mus), k, i, "Besag-Clifford", "#permutations till rejection", 2, 1] <- df$nPerm_rej_bc
    results_arr[1:length(mus), k, i, "Besag-Clifford", "#permutations till futility stop", 2, 1] <- df$nPerm_stop_bc
    results_arr[1:length(mus), k, i, "Binomial mixture randomized", "Power", 2, 1] <- df$power_bm
    results_arr[1:length(mus), k, i, "Binomial mixture randomized", "Total #permutations", 2, 1] <- df$nPerm_bm
    results_arr[1:length(mus), k, i, "Binomial mixture randomized", "#permutations till rejection", 2, 1] <- df$nPerm_bm_rej
    results_arr[1:length(mus), k, i, "Binomial mixture randomized", "#permutations till futility stop", 2, 1] <- df$nPerm_bm_stop
  }
}

for (i in 1:length(c_factors)) {
  for (k in 1:length(alphas)) {
    print(paste0("alpha", alphas[k], "c_factor", c_factors[i]))
    df <- generate_power_data(alphas[k], alphas[k] * c_factors[i], alphas[k], mus, 1000, "normal")
    results_arr[1:length(mus), k, 4, "Besag-Clifford", "Power", i, 1] <- df$power_bc
    results_arr[1:length(mus), k, 4, "Besag-Clifford", "Total #permutations", i, 1] <- df$nPerm_bc
    results_arr[1:length(mus), k, 4, "Besag-Clifford", "#permutations till rejection", i, 1] <- df$nPerm_rej_bc
    results_arr[1:length(mus), k, 4, "Besag-Clifford", "#permutations till futility stop", i, 1] <- df$nPerm_stop_bc
    results_arr[1:length(mus), k, 4, "Binomial mixture randomized", "Power", i, 1] <- df$power_bm
    results_arr[1:length(mus), k, 4, "Binomial mixture randomized", "Total #permutations", i, 1] <- df$nPerm_bm
    results_arr[1:length(mus), k, 4, "Binomial mixture randomized", "#permutations till rejection", i, 1] <- df$nPerm_bm_rej
    results_arr[1:length(mus), k, 4, "Binomial mixture randomized", "#permutations till futility stop", i, 1] <- df$nPerm_bm_stop
  }
}

for (i in 1:length(dists)) {
  for (k in 1:length(alphas)) {
    print(paste0("alpha", alphas[k], "dist", dists[i]))
    df <- generate_power_data(alphas[k], alphas[k] * 0.9, alphas[k], mus, 1000, dists[i])
    results_arr[1:length(mus), k, 4, "Besag-Clifford", "Power", 2, i] <- df$power_bc
    results_arr[1:length(mus), k, 4, "Besag-Clifford", "Total #permutations", 2, i] <- df$nPerm_bc
    results_arr[1:length(mus), k, 4, "Besag-Clifford", "#permutations till rejection", 2, i] <- df$nPerm_rej_bc
    results_arr[1:length(mus), k, 4, "Besag-Clifford", "#permutations till futility stop", 2, i] <- df$nPerm_stop_bc
    results_arr[1:length(mus), k, 4, "Binomial mixture randomized", "Power", 2, i] <- df$power_bm
    results_arr[1:length(mus), k, 4, "Binomial mixture randomized", "Total #permutations", 2, i] <- df$nPerm_bm
    results_arr[1:length(mus), k, 4, "Binomial mixture randomized", "#permutations till rejection", 2, i] <- df$nPerm_bm_rej
    results_arr[1:length(mus), k, 4, "Binomial mixture randomized", "#permutations till futility stop", 2, i] <- df$nPerm_bm_stop
  }
}

results_df <- as_tibble(melt(results_arr))
names(results_df) <- c("mus", "alphas", "ns", "labs", "types", "c_factors", "dists", "values")

save(results_df, file = "appendix/results/power_appendix.rda")
