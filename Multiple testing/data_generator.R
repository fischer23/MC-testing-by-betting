### Simulates data and applies different procedures to it

# input

# nsim:                Number of simulation runs per parameter combination
# nhyp:                Number of hypotheses per trial
# mu_As:               Strength of the alternative (vector of different values)
# mu_N:                Conservativeness of null p-values
# pis:                 Proportion of false hypotheses (vector of different values)
# alphas:              Individual significance levels (vector of different values)
# param_bm:            Parameter 0<b<1 for the binomial mixture strategy.
# param_bc:            Parameter h>=1 (integer) for the anytime-valid BC method.
# param_amt:           Parameter 0<delta<alpha for the AMT algorithm by Zhang et al. (2019).
# peeking_times_AMT:   Times at which it is peaked at the data with AMT algorithm. Number of peeking times
#                     should be of the order log(B)
# B_large:             Number of maximum permutations for all algorithms
# B_small:             Number of permutations smaller than B_large used for the permutation p-value as reference
# filename:            Filename for the results


data_generator <- function(nsim, nhyp, mu_As, mu_N, pis, alphas, param_bm, param_bc, param_amt, peeking_times_AMT, B_large, B_small, filename) {
  ### Predefine vectors for power, FDR and mean number of permutations for different procedures
  power_bm <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  FDR_bm <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_mean_bm <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_mean_rej_bm <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_mean_stop_bm <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_median_bm <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_max_bm <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  result_bm <- list()

  power_bc <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  FDR_bc <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_mean_bc <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_mean_rej_bc <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_mean_stop_bc <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_median_bc <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_max_bc <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  result_bc <- list()

  power_agg <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  FDR_agg <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_mean_agg <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_mean_rej_agg <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_mean_stop_agg <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_median_agg <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_max_agg <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  result_agg <- list()

  power_amt <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  FDR_amt <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_mean_amt <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_mean_rej_amt <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_mean_stop_amt <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_median_amt <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  nperm_max_amt <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  result_amt <- list()

  power_BH <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  FDR_BH <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))

  power_BH_smallB <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))
  FDR_BH_smallB <- matrix(0, nsim, length(pis) * length(alphas) * length(mu_As) * length(nhyp))


  count <- 1
  # Generate test statistics for all combinations of simulation parameters
  for (M in nhyp) {
    for (alpha in alphas) {
      for (mu_A in mu_As) {
        for (pi_A in pis) {
          Z <- matrix(, nrow = M, ncol = nsim)
          hypo <- matrix(, nrow = M, ncol = nsim)
          for (j in 1:nsim) {
            hypo[, j] <- rbinom(M, 1, pi_A)
            X <- rnorm(M)
            # Calculate true test statistics
            Z[, j] <- mu_N * (hypo[, j] - 1) * (-1) + mu_A * hypo[, j] + X
          }
          for (k in 1:nsim) {
            print(k)
            # Generate simulated test statistics
            simZ <- matrix(rnorm(M * B_small), ncol = B_small)
            simZ_largeB <- matrix(rnorm(M * B_large), ncol = B_large)

            # Apply algorithms
            results_bm <- sequential_BH(Z[, k], simZ_largeB, M, B_large, (1:B_large), alpha, param_bm, param_bc, param_AMT, peeking_times_AMT, "bm")
            results_bc <- sequential_BH(Z[, k], simZ_largeB, M, B_large, (1:B_large), alpha, param_bm, param_bc, param_AMT, peeking_times_AMT, "bc")
            results_agg <- sequential_BH(Z[, k], simZ_largeB, M, B_large, (1:B_large), alpha, param_bm, 1, param_amt, peeking_times_AMT, "bc")
            results_amt <- sequential_BH(Z[, k], simZ_largeB, M, B_large, (1:B_large), (alpha - param_amt), param_bm, param_bc, param_amt, peeking_times_AMT, "AMT")

            # Calculate results
            result_bm[[k]] <- results_bm
            nperm_mean_bm[k, count] <- mean(results_bm$stop)
            nperm_mean_rej_bm[k, count] <- mean(results_bm$stop[results_bm$rejects])
            nperm_mean_stop_bm[k, count] <- mean(results_bm$stop[setdiff(1:M, results_bm$rejects)])
            nperm_median_bm[k, count] <- median(results_bm$stop)
            nperm_max_bm[k, count] <- max(results_bm$stop)
            power_bm[k, count] <- length(intersect(which(hypo[, k] == 1), results_bm$rejects)) / max(1, sum(hypo[, k]))
            FDR_bm[k, count] <- length(intersect(which(hypo[, k] == 0), results_bm$rejects)) / max(1, max(1, length(results_bm$rejects)))

            result_bc[[k]] <- results_bc
            nperm_mean_bc[k, count] <- mean(results_bc$stop)
            nperm_mean_rej_bc[k, count] <- mean(results_bc$stop[results_bc$rejects])
            nperm_mean_stop_bc[k, count] <- mean(results_bc$stop[setdiff(1:M, results_bc$rejects)])
            nperm_median_bc[k, count] <- median(results_bc$stop)
            nperm_max_bc[k, count] <- max(results_bc$stop)
            power_bc[k, count] <- length(intersect(which(hypo[, k] == 1), results_bc$rejects)) / max(1, sum(hypo[, k]))
            FDR_bc[k, count] <- length(intersect(which(hypo[, k] == 0), results_bc$rejects)) / max(1, length(results_bc$rejects))

            result_agg[[k]] <- results_agg
            nperm_mean_agg[k, count] <- mean(results_agg$stop)
            nperm_mean_rej_agg[k, count] <- mean(results_agg$stop[results_agg$rejects])
            nperm_mean_stop_agg[k, count] <- mean(results_agg$stop[setdiff(1:M, results_agg$rejects)])
            nperm_median_agg[k, count] <- median(results_agg$stop)
            nperm_max_agg[k, count] <- max(results_agg$stop)
            power_agg[k, count] <- length(intersect(which(hypo[, k] == 1), results_agg$rejects)) / max(1, sum(hypo[, k]))
            FDR_agg[k, count] <- length(intersect(which(hypo[, k] == 0), results_agg$rejects)) / max(1, length(results_agg$rejects))

            result_amt[[k]] <- results_amt
            nperm_mean_amt[k, count] <- mean(results_amt$stop)
            nperm_mean_rej_amt[k, count] <- mean(results_amt$stop[results_amt$rejects])
            nperm_mean_stop_amt[k, count] <- mean(results_amt$stop[setdiff(1:M, results_amt$rejects)])
            nperm_median_amt[k, count] <- median(results_amt$stop)
            nperm_max_amt[k, count] <- max(results_amt$stop)
            power_amt[k, count] <- length(intersect(which(hypo[, k] == 1), results_amt$rejects)) / max(1, sum(hypo[, k]))
            FDR_amt[k, count] <- length(intersect(which(hypo[, k] == 0), results_amt$rejects)) / max(1, length(results_amt$rejects))


            # Calculate classical permutation p-values
            ps_perm <- (rowSums(simZ_largeB >= Z[, k]) + 1) / (B_large + 1)

            # Apply BH to classical permutation p-values
            ps_perm_BH <- p.adjust(ps_perm, "fdr")
            power_BH[k, count] <- sum(hypo[, k] == 1 & ps_perm_BH <= alpha) / max(1, sum(hypo[, k]))
            FDR_BH[k, count] <- sum(hypo[, k] == 0 & ps_perm_BH <= alpha) / max(1, sum(ps_perm_BH <= alpha))

            # Calculate classical permutation p-values with small B
            ps_perm_smallB <- (rowSums(simZ >= Z[, k]) + 1) / (B_small + 1)

            # Apply BH to classical permutation p-values with small B
            ps_perm_BH_smallB <- p.adjust(ps_perm_smallB, "fdr")
            power_BH_smallB[k, count] <- sum(hypo[, k] == 1 & ps_perm_BH_smallB <= alpha) / max(1, sum(hypo[, k]))
            FDR_BH_smallB[k, count] <- sum(hypo[, k] == 0 & ps_perm_BH_smallB <= alpha) / max(1, sum(ps_perm_BH_smallB <= alpha))
          }
          count <- count + 1
        }
      }
    }
  }

  # Average over different simulations runs
  power_BH <- colMeans(power_BH)
  FDR_BH <- colMeans(FDR_BH)
  power_BH_smallB <- colMeans(power_BH_smallB)
  FDR_BH_smallB <- colMeans(FDR_BH_smallB)
  power_bm <- colMeans(power_bm)
  FDR_bm <- colMeans(FDR_bm)
  nperm_mean_bm <- colMeans(nperm_mean_bm)
  nperm_median_bm <- colMeans(nperm_median_bm)
  nperm_max_bm <- colMeans(nperm_max_bm)
  nperm_mean_rej_bm <- colMeans(nperm_mean_rej_bm)
  nperm_mean_stop_bm <- colMeans(nperm_mean_stop_bm)
  power_bc <- colMeans(power_bc)
  FDR_bc <- colMeans(FDR_bc)
  nperm_mean_bc <- colMeans(nperm_mean_bc)
  nperm_median_bc <- colMeans(nperm_median_bc)
  nperm_max_bc <- colMeans(nperm_max_bc)
  nperm_mean_rej_bc <- colMeans(nperm_mean_rej_bc)
  nperm_mean_stop_bc <- colMeans(nperm_mean_stop_bc)
  power_agg <- colMeans(power_agg)
  FDR_agg <- colMeans(FDR_agg)
  nperm_mean_agg <- colMeans(nperm_mean_agg)
  nperm_median_agg <- colMeans(nperm_median_agg)
  nperm_max_agg <- colMeans(nperm_max_agg)
  nperm_mean_rej_agg <- colMeans(nperm_mean_rej_agg)
  nperm_mean_stop_agg <- colMeans(nperm_mean_stop_agg)
  power_amt <- colMeans(power_amt)
  FDR_amt <- colMeans(FDR_amt)
  nperm_mean_amt <- colMeans(nperm_mean_amt)
  nperm_median_amt <- colMeans(nperm_median_amt)
  nperm_max_amt <- colMeans(nperm_max_amt)
  nperm_mean_rej_amt <- colMeans(nperm_mean_rej_amt)
  nperm_mean_stop_amt <- colMeans(nperm_mean_stop_amt)

  # save results

  save(power_BH, FDR_BH, power_BH_smallB, FDR_BH_smallB,
    power_bm, FDR_bm, nperm_mean_bm, nperm_median_bm, nperm_max_bm, nperm_mean_rej_bm, nperm_mean_stop_bm,
    power_bc, FDR_bc, nperm_mean_bc, nperm_median_bc, nperm_max_bc, nperm_mean_rej_bc, nperm_mean_stop_bc,
    power_agg, FDR_agg, nperm_mean_agg, nperm_median_agg, nperm_max_agg, nperm_mean_rej_agg, nperm_mean_stop_agg,
    power_amt, FDR_amt, nperm_mean_amt, nperm_median_amt, nperm_max_amt, nperm_mean_rej_amt, nperm_mean_stop_amt,
    result_bm, result_bc, result_agg, result_amt,
    file = filename
  )
}
