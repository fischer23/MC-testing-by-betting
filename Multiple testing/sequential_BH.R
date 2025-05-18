### Applies Benjamini Hochberg procedure to sequential permutation p-values

# input

# true_stat:         M-dimensional vector of observed test statistics, where M is the number of hypotheses.
# perm_stat:         MxB-dimensional matrix of permuted test statistics, where B is the number of permutations.
# M:                 Number of hypotheses.
# B:                 Number of permutations.
# peeking_times:     Prespecify times at which it is peaked at the data to reduce run time (optional).
# alpha:             Overall significance level.
# param_bm:          Parameter 0<b<1 for the binomial mixture strategy.
# param_bc:          Parameter h>=1 (integer) for the anytime-valid BC method.
# param_amt:         Parameter 0<delta<alpha for the AMT algorithm by Zhang et al. (2019).
# peeking_times_AMT: Times at which it is peaked at the data with AMT algorithm. Number of peeking times
#                   should be of the order log(B)
# method:            "bm" for binomial mixture, "bc" for anytime-valid Besag-Clifford, "amt" for AMT algorithm
#                   by Zhang et al. (2019).

# output

# list of rejects and stop

# stop:              Stopping times of the hypotheses
# rejects:           Index set of the rejected hypotheses

sequential_BH <- function(true_stat, perm_stat, M, B, peeking_times = (1:B), alpha = 0.05, param_bm = 0.9, param_bc = 10,
                          param_amt = 0.01, peeking_times_AMT = floor(c(cumsum(c(B / 100, B / 100 * cumprod(rep(1.1, 24)))), B)),
                          method) {
  if (method == "bm") {
    c <- param_bm * alpha
    rejects <- c()
    consider <- (1:M)
    losses <- rep(0, M)
    lowest_rej <- rep(M + 1, M)
    m_star <- 1

    stop <- rep(B, M)

    for (i in peeking_times) {
      # Calculate critical vector for number of losses
      critical <- qbinom(1 - param_bm, i + 1, param_bm * alpha * (1:M) / M) - 1
      # Calculate number of losses
      if (length(consider) > 1 & i > 1) {
        losses[consider] <- rowSums(perm_stat[consider, (1:i)] >= true_stat[consider])
      } else if (length(consider) == 1) {
        losses[consider] <- sum(perm_stat[consider, (1:i)] >= true_stat[consider])
      } else {
        losses[consider] <- (perm_stat[consider, 1] >= true_stat[consider])
      }
      max_crit <- max(critical)


      if (max_crit >= 0) {
        idx_critical <- match((0:max_crit), critical)
        idx_critical <- idx_critical[!is.na(idx_critical)]
        critical <- critical[idx_critical]
        idx_critical <- c(idx_critical, M + 1)


        # Calculate lowest level a p-value could reject at
        if (length(consider) > 1) {
          lowest_rej[consider] <- pmin(lowest_rej[consider], idx_critical[length(idx_critical) - rowSums(outer((losses[consider]), critical, "<="))])
        } else {
          lowest_rej[consider] <- pmin(lowest_rej[consider], idx_critical[length(idx_critical) - sum((losses[consider]) <= critical)])
        }

        # Calculate m_star of the BH procedure
        sort_lowest_rej <- sort(lowest_rej)
        m_star <- max(c(which(sort_lowest_rej <= (1:M)), 1), na.rm = TRUE)
        # Set rejections and stops
        rejects <- which(lowest_rej <= m_star)
        consider <- setdiff(consider, rejects)
        stop[setdiff(1:M, consider)] <- pmin(i, stop[setdiff(1:M, consider)])
      }
      # Stop for futility
      consider <- setdiff(consider, which(ifelse((1 - pbinom(
        losses, i + 1,
        param_bm * alpha * (length(consider) + m_star) / M
      )) / (param_bm * alpha *
        (length(consider) + m_star) / M) < alpha * (length(consider) + m_star) / M,
      TRUE, FALSE
      )))

      if (length(consider) == 0) {
        break
      }
    }
    return(list(rejects = rejects, stop = stop))
  }

  if (method == "bc") {
    h <- param_bc
    rejects <- c()
    consider <- (1:M)
    losses <- rep(0, M)
    lowest_rej <- rep(M + 1, M)

    stop <- rep(B, M)

    for (i in peeking_times) {
      # Calculate critical vector for number of losses
      critical <- pmin(floor((i + h - h / (alpha * (1:M) / M))), h - 1)
      # Calculate number of losses
      if (length(consider) > 1 & i > 1) {
        losses[consider] <- rowSums(perm_stat[consider, (1:i)] >= true_stat[consider])
      } else if (length(consider) == 1) {
        losses[consider] <- sum(perm_stat[consider, (1:i)] >= true_stat[consider])
      } else {
        losses[consider] <- (perm_stat[consider, 1] >= true_stat[consider])
      }
      max_crit <- max(critical)

      if (max_crit >= 0) {
        idx_critical <- match((0:max_crit), critical)
        idx_critical <- idx_critical[!is.na(idx_critical)]
        critical <- critical[idx_critical]
        idx_critical <- c(idx_critical, M + 1)


        # Calculate lowest level a p-value could reject at
        if (length(consider) > 1) {
          lowest_rej[consider] <- pmin(lowest_rej[consider], idx_critical[length(idx_critical) - rowSums(outer((losses[consider]), critical, "<="))])
        } else {
          lowest_rej[consider] <- pmin(lowest_rej[consider], idx_critical[length(idx_critical) - sum((losses[consider]) <= critical)])
        }

        # Calculate m_star of the BH procedure
        sort_lowest_rej <- sort(lowest_rej)
        m_star <- max(c(which(sort_lowest_rej <= (1:M)), 1), na.rm = TRUE)
        # Set rejections and stops
        rejects <- which(lowest_rej <= m_star)
        consider <- setdiff(consider, rejects)
        stop[setdiff(1:M, consider)] <- pmin(i, stop[setdiff(1:M, consider)])
      }
      # Stop for futility
      consider <- setdiff(consider, which(losses >= h))

      if (length(consider) == 0) {
        break
      }
    }
    return(list(rejects = rejects, stop = stop))
  }


  # AMT by Zhang et al. (2019)

  if (method == "AMT") {
    rejects <- c()
    consider <- (1:M)
    losses <- rep(0, M)
    r_hat <- alpha
    p_lower <- rep(0, M)
    p_upper <- rep(1, M)
    C_g <- c()

    time <- rep(0, M)

    repeat{
      # Count number of times it was peaked at the data for each hypothesis
      time[consider] <- time[consider] + 1
      for (u in consider) {
        # Calculate number of losses
        losses[u] <- sum(perm_stat[u, (1:peeking_times_AMT[time[u]])] >= true_stat[u])
        # Calculate confidence interval for permutation p-values
        if (time[u] < length(peeking_times_AMT)) {
          ac <- binom.confint(losses[u],
            n = peeking_times_AMT[time[u]],
            conf.level = 1 - (param_amt / (2 * M * length(peeking_times_AMT))), method = "ac"
          )
          p_lower[u] <- ac$lower
          p_upper[u] <- ac$upper
        } else {
          p_lower[u] <- (losses[u] + 1) / (B + 1)
          p_upper[u] <- (losses[u] + 1) / (B + 1)
        }
      }
      # Perform AMT algorithm
      for (j in (M:0)) {
        r_hat <- j
        C_g <- which(p_lower > j / M * alpha)
        if (r_hat == M - length(C_g)) {
          break
        }
      }

      tau_hat <- r_hat * alpha / M
      consider <- which((p_lower <= tau_hat & p_upper > tau_hat))
      rejects <- which(p_upper <= tau_hat)


      if (length(consider) == 0) {
        break
      }
    }
    stop <- peeking_times_AMT[time]
    return(list(rejects = rejects, stop = stop))
  }
}

# References

# Martin Zhang, James Zou, and David Tse. Adaptive Monte Carlo multiple testing via multi-armed
# bandits. In International Conference on Machine Learning, pages 7512–7522. PMLR, 2019
