# Generates Figures 1,2,3,4 of the paper "Sequential Monte-Carlo testing by betting"

rm(list = ls())

library(ggplot2)
library(patchwork)

##### Figure 1 (alpha=0.05)

load("results/power_alpha005.rda")
B <- 1000
mus <- c(0.01, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5)

lab <- c("Permutation p-value", "Aggressive", "Binomial", "Besag-Clifford", "Binomial mixture")
col <- c("cornflowerblue", "red", "limegreen", "cornflowerblue", "orange")

results_df <- data.frame(
  idx = mus, power_bin = power_bin, power_agg = power_agg, power_bc = power_bc, power_bm = power_bm,
  nPerm = nPerm, nPerm_agg = nPerm_agg, nPerm_rej = nPerm_rej,
  nPerm_stop = nPerm_stop, nPerm_agg_rej = nPerm_agg_rej, nPerm_agg_stop = nPerm_agg_stop,
  nPerm_bc = nPerm_bc, nPerm_stop_bc = nPerm_stop_bc, nPerm_rej_bc = nPerm_rej_bc,
  nPerm_bm = nPerm_bm, nPerm_bm_stop = nPerm_bm_stop, nPerm_bm_rej = nPerm_bm_rej
)

p1 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = power_bin, colour = "3", linetype = "3")) +
  geom_point(aes(y = power_bin, colour = "3", shape = "3")) +
  geom_line(aes(y = power_agg, colour = "2", linetype = "3")) +
  geom_point(aes(y = power_agg, colour = "2", shape = "2")) +
  geom_line(aes(y = power_bc, colour = "4", linetype = "3")) +
  geom_point(aes(y = power_bc, colour = "4", shape = "4")) +
  geom_line(aes(y = power_bm, colour = "5", linetype = "3")) +
  geom_point(aes(y = power_bm, colour = "5", shape = "5")) +
  scale_linetype_manual(guide = "none", values = c("3" = "solid", "4" = "dashed", "5" = "dotted")) +
  scale_shape_manual(
    name = "Strategy", values = c("3" = 6, "2" = 0, "4" = 8, "5" = 1),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("3" = col[3], "2" = col[2], "4" = col[4], "5" = col[5]),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  xlab(bquote(mu)) +
  ylab("Power") +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p2 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = nPerm, colour = "3", linetype = "3")) +
  geom_point(aes(y = nPerm, colour = "3", shape = "3")) +
  geom_line(aes(y = nPerm_agg, colour = "2", linetype = "3")) +
  geom_point(aes(y = nPerm_agg, colour = "2", shape = "2")) +
  geom_line(aes(y = nPerm_bc, colour = "4", linetype = "3")) +
  geom_point(aes(y = nPerm_bc, colour = "4", shape = "4")) +
  geom_line(aes(y = nPerm_bm, colour = "5", linetype = "3")) +
  geom_point(aes(y = nPerm_bm, colour = "5", shape = "5")) +
  scale_linetype_manual(guide = "none", values = c("3" = "solid", "4" = "dashed", "5" = "dotted")) +
  scale_shape_manual(
    name = "Strategy", values = c("3" = 6, "2" = 0, "4" = 8, "5" = 1),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("3" = col[3], "2" = col[2], "4" = col[4], "5" = col[5]),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, B, 100), limits = c(0, (B + B / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p3 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = nPerm_rej, colour = "3", linetype = "3")) +
  geom_point(aes(y = nPerm_rej, colour = "3", shape = "3")) +
  geom_line(aes(y = nPerm_agg_rej, colour = "2", linetype = "3")) +
  geom_point(aes(y = nPerm_agg_rej, colour = "2", shape = "2")) +
  geom_line(aes(y = nPerm_rej_bc, colour = "4", linetype = "3")) +
  geom_point(aes(y = nPerm_rej_bc, colour = "4", shape = "4")) +
  geom_line(aes(y = nPerm_bm_rej, colour = "5", linetype = "3")) +
  geom_point(aes(y = nPerm_bm_rej, colour = "5", shape = "5")) +
  scale_linetype_manual(guide = "none", values = c("3" = "solid", "4" = "dashed", "5" = "dotted")) +
  scale_shape_manual(
    name = "Strategy", values = c("3" = 6, "2" = 0, "4" = 8, "5" = 1),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("3" = col[3], "2" = col[2], "4" = col[4], "5" = col[5]),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  xlab(bquote(mu)) +
  ylab("#permutations till rejection") +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, B, 100), limits = c(0, (B + B / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

p4 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = nPerm_stop, colour = "3", linetype = "3")) +
  geom_point(aes(y = nPerm_stop, colour = "3", shape = "3")) +
  geom_line(aes(y = nPerm_agg_stop, colour = "2", linetype = "3")) +
  geom_point(aes(y = nPerm_agg_stop, colour = "2", shape = "2")) +
  geom_line(aes(y = nPerm_stop_bc, colour = "4", linetype = "3")) +
  geom_point(aes(y = nPerm_stop_bc, colour = "4", shape = "4")) +
  geom_line(aes(y = nPerm_bm_stop, colour = "5", linetype = "3")) +
  geom_point(aes(y = nPerm_bm_stop, colour = "5", shape = "5")) +
  scale_linetype_manual(guide = "none", values = c("3" = "solid", "4" = "dashed", "5" = "dotted")) +
  scale_shape_manual(
    name = "Strategy", values = c("3" = 6, "2" = 0, "4" = 8, "5" = 1),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("3" = col[3], "2" = col[2], "4" = col[4], "5" = col[5]),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  xlab(bquote(mu)) +
  ylab("#permutations till futility stop") +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, B, 100), limits = c(0, (B + B / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

combined <- p1 + p2 + p3 + p4 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("results/Plot_power_alpha005.pdf", plot = combined, width = 12, height = 7.5)

##### Figure 2 (alpha=0.01)

load("results/power_alpha001.rda")
B <- 1000

lab <- c("Permutation p-value", "Aggressive", "Binomial", "Besag-Clifford", "Binomial mixture")
col <- c("cornflowerblue", "red", "limegreen", "cornflowerblue", "orange")

results_df <- data.frame(
  idx = mus, power_bin = power_bin, power_agg = power_agg, power_bc = power_bc, power_bm = power_bm,
  nPerm = nPerm, nPerm_agg = nPerm_agg, nPerm_rej = nPerm_rej,
  nPerm_stop = nPerm_stop, nPerm_agg_rej = nPerm_agg_rej, nPerm_agg_stop = nPerm_agg_stop,
  nPerm_bc = nPerm_bc, nPerm_stop_bc = nPerm_stop_bc, nPerm_rej_bc = nPerm_rej_bc,
  nPerm_bm = nPerm_bm, nPerm_bm_stop = nPerm_bm_stop, nPerm_bm_rej = nPerm_bm_rej
)

p1 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = power_bin, colour = "3", linetype = "3")) +
  geom_point(aes(y = power_bin, colour = "3", shape = "3")) +
  geom_line(aes(y = power_agg, colour = "2", linetype = "3")) +
  geom_point(aes(y = power_agg, colour = "2", shape = "2")) +
  geom_line(aes(y = power_bc, colour = "4", linetype = "3")) +
  geom_point(aes(y = power_bc, colour = "4", shape = "4")) +
  geom_line(aes(y = power_bm, colour = "5", linetype = "3")) +
  geom_point(aes(y = power_bm, colour = "5", shape = "5")) +
  scale_linetype_manual(guide = "none", values = c("3" = "solid", "4" = "dashed", "5" = "dotted")) +
  scale_shape_manual(
    name = "Strategy", values = c("3" = 6, "2" = 0, "4" = 8, "5" = 1),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("3" = col[3], "2" = col[2], "4" = col[4], "5" = col[5]),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  xlab(bquote(mu)) +
  ylab("Power") +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p2 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = nPerm, colour = "3", linetype = "3")) +
  geom_point(aes(y = nPerm, colour = "3", shape = "3")) +
  geom_line(aes(y = nPerm_agg, colour = "2", linetype = "3")) +
  geom_point(aes(y = nPerm_agg, colour = "2", shape = "2")) +
  geom_line(aes(y = nPerm_bc, colour = "4", linetype = "3")) +
  geom_point(aes(y = nPerm_bc, colour = "4", shape = "4")) +
  geom_line(aes(y = nPerm_bm, colour = "5", linetype = "3")) +
  geom_point(aes(y = nPerm_bm, colour = "5", shape = "5")) +
  scale_linetype_manual(guide = "none", values = c("3" = "solid", "4" = "dashed", "5" = "dotted")) +
  scale_shape_manual(
    name = "Strategy", values = c("3" = 6, "2" = 0, "4" = 8, "5" = 1),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("3" = col[3], "2" = col[2], "4" = col[4], "5" = col[5]),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, B, 100), limits = c(0, (B + B / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p3 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = nPerm_rej, colour = "3", linetype = "3")) +
  geom_point(aes(y = nPerm_rej, colour = "3", shape = "3")) +
  geom_line(aes(y = nPerm_agg_rej, colour = "2", linetype = "3")) +
  geom_point(aes(y = nPerm_agg_rej, colour = "2", shape = "2")) +
  geom_line(aes(y = nPerm_rej_bc, colour = "4", linetype = "3")) +
  geom_point(aes(y = nPerm_rej_bc, colour = "4", shape = "4")) +
  geom_line(aes(y = nPerm_bm_rej, colour = "5", linetype = "3")) +
  geom_point(aes(y = nPerm_bm_rej, colour = "5", shape = "5")) +
  scale_linetype_manual(guide = "none", values = c("3" = "solid", "4" = "dashed", "5" = "dotted")) +
  scale_shape_manual(
    name = "Strategy", values = c("3" = 6, "2" = 0, "4" = 8, "5" = 1),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("3" = col[3], "2" = col[2], "4" = col[4], "5" = col[5]),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  xlab(bquote(mu)) +
  ylab("#permutations till rejection") +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, B, 100), limits = c(0, (B + B / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

p4 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = nPerm_stop, colour = "3", linetype = "3")) +
  geom_point(aes(y = nPerm_stop, colour = "3", shape = "3")) +
  geom_line(aes(y = nPerm_agg_stop, colour = "2", linetype = "3")) +
  geom_point(aes(y = nPerm_agg_stop, colour = "2", shape = "2")) +
  geom_line(aes(y = nPerm_stop_bc, colour = "4", linetype = "3")) +
  geom_point(aes(y = nPerm_stop_bc, colour = "4", shape = "4")) +
  geom_line(aes(y = nPerm_bm_stop, colour = "5", linetype = "3")) +
  geom_point(aes(y = nPerm_bm_stop, colour = "5", shape = "5")) +
  scale_linetype_manual(guide = "none", values = c("3" = "solid", "4" = "dashed", "5" = "dotted")) +
  scale_shape_manual(
    name = "Strategy", values = c("3" = 6, "2" = 0, "4" = 8, "5" = 1),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("3" = col[3], "2" = col[2], "4" = col[4], "5" = col[5]),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  xlab(bquote(mu)) +
  ylab("#permutations till futility stop") +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, B, 100), limits = c(0, (B + B / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

combined <- p1 + p2 + p3 + p4 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("results/Plot_power_alpha001.pdf", plot = combined, width = 12, height = 7.5)


##### Figure 3 (Comparison randomized vs. non-randomized binomial and binomial mixture strategy)

### For alpha=0.05

load("results/power_alpha005.rda")

mus <- c(0.01, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5)
alpha <- 0.05

## Binomial mixture strategy
lab <- c("Binomial mixture", "Binomial mixture randomized")
col <- c("orange")

results_df <- data.frame(idx = mus, power_bm = power_bm, power_bm_r = power_bm_r)

p1 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = power_bm_r, colour = "2", linetype = "2")) +
  geom_point(aes(y = power_bm_r, colour = "2", shape = "2")) +
  geom_line(aes(y = power_bm, colour = "1", linetype = "1")) +
  geom_point(aes(y = power_bm, colour = "1", shape = "1")) +
  scale_linetype_manual(
    name = "Strategy", values = c("1" = "solid", "2" = "dashed"),
    labels = c("1" = lab[1], "2" = lab[2])
  ) +
  scale_shape_manual(
    name = "Strategy", values = c("1" = 1, "2" = 1),
    labels = c("1" = lab[1], "2" = lab[2])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("1" = col[1], "2" = col[1]),
    labels = c("1" = lab[1], "2" = lab[2])
  ) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(bquote(alpha == .(alpha))) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

## Binomial strategy
lab <- c("Binomial", "Binomial randomized")
col <- c("limegreen")

results_df_2 <- data.frame(idx = mus, power_bin = power_bin, power_bin_r = power_bin_r)

p2 <- ggplot(results_df_2, aes(idx)) +
  geom_line(aes(y = power_bin_r, colour = "2", linetype = "2")) +
  geom_point(aes(y = power_bin_r, colour = "2", shape = "2")) +
  geom_line(aes(y = power_bin, colour = "1", linetype = "1")) +
  geom_point(aes(y = power_bin, colour = "1", shape = "1")) +
  scale_linetype_manual(
    name = "Strategy", values = c("1" = "solid", "2" = "dashed"),
    labels = c("1" = lab[1], "2" = lab[2])
  ) +
  scale_shape_manual(
    name = "Strategy", values = c("1" = 1, "2" = 1),
    labels = c("1" = lab[1], "2" = lab[2])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("1" = col[1], "2" = col[1]),
    labels = c("1" = lab[1], "2" = lab[2])
  ) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(bquote(alpha == .(alpha))) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


### For alpha=0.01

load("results/power_alpha001.rda")

alpha <- 0.01

## Binomial mixture strategy
lab <- c("Binomial mixture", "Binomial mixture randomized")
col <- c("orange")

results_df <- data.frame(idx = mus, power_bm = power_bm, power_bm_r = power_bm_r)

p3 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = power_bm_r, colour = "2", linetype = "2")) +
  geom_point(aes(y = power_bm_r, colour = "2", shape = "2")) +
  geom_line(aes(y = power_bm, colour = "1", linetype = "1")) +
  geom_point(aes(y = power_bm, colour = "1", shape = "1")) +
  scale_linetype_manual(
    name = "Strategy", values = c("1" = "solid", "2" = "dashed"),
    labels = c("1" = lab[1], "2" = lab[2])
  ) +
  scale_shape_manual(
    name = "Strategy", values = c("1" = 1, "2" = 1),
    labels = c("1" = lab[1], "2" = lab[2])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("1" = col[1], "2" = col[1]),
    labels = c("1" = lab[1], "2" = lab[2])
  ) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(bquote(alpha == .(alpha))) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

## Binomial strategy
lab <- c("Binomial", "Binomial randomized")
col <- c("limegreen")

results_df_2 <- data.frame(idx = mus, power_bin = power_bin, power_bin_r = power_bin_r)

p4 <- ggplot(results_df_2, aes(idx)) +
  geom_line(aes(y = power_bin_r, colour = "2", linetype = "2")) +
  geom_point(aes(y = power_bin_r, colour = "2", shape = "2")) +
  geom_line(aes(y = power_bin, colour = "1", linetype = "1")) +
  geom_point(aes(y = power_bin, colour = "1", shape = "1")) +
  scale_linetype_manual(
    name = "Strategy", values = c("1" = "solid", "2" = "dashed"),
    labels = c("1" = lab[1], "2" = lab[2])
  ) +
  scale_shape_manual(
    name = "Strategy", values = c("1" = 1, "2" = 1),
    labels = c("1" = lab[1], "2" = lab[2])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("1" = col[1], "2" = col[1]),
    labels = c("1" = lab[1], "2" = lab[2])
  ) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(bquote(alpha == .(alpha))) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


combined_b <- p2 + p4 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
combined_bm <- p1 + p3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

combined <- combined_b / combined_bm

ggsave("results/Plot_power_randomized_comb.pdf", plot = combined, width = 12, height = 9)


##### Figure 4 (Besag-Clifford vs. randomized strategies)

### For alpha=0.05

load("results/power_alpha005.rda")
B <- 1000
alpha <- 0.05

lab <- c("Permutation p-value", "Aggressive", "Binomial randomized", "Besag-Clifford", "Binomial mixture randomized")
col <- c("cornflowerblue", "red", "limegreen", "cornflowerblue", "orange")

results_df <- data.frame(idx = mus, power_bc = power_bc, power_bin_r = power_bin_r, power_agg = power_agg, power_bm_r = power_bm_r)

p1 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = power_bin_r, colour = "3", linetype = "3")) +
  geom_point(aes(y = power_bin_r, colour = "3", shape = "3")) +
  geom_line(aes(y = power_agg, colour = "2", linetype = "3")) +
  geom_point(aes(y = power_agg, colour = "2", shape = "2")) +
  geom_line(aes(y = power_bc, colour = "4", linetype = "3")) +
  geom_point(aes(y = power_bc, colour = "4", shape = "4")) +
  geom_line(aes(y = power_bm_r, colour = "5", linetype = "3")) +
  geom_point(aes(y = power_bm_r, colour = "5", shape = "5")) +
  scale_linetype_manual(guide = "none", values = c("3" = "solid", "4" = "dashed", "5" = "dotted")) +
  scale_shape_manual(
    name = "Strategy", values = c("3" = 6, "2" = 0, "4" = 8, "5" = 1),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("3" = col[3], "2" = col[2], "4" = col[4], "5" = col[5]),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(bquote(alpha == .(alpha))) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


### For alpha=0.01
load("results/power_alpha001.rda")
B <- 1000
alpha <- 0.01

results_df <- data.frame(idx = mus, power_bc = power_bc, power_bin = power_bin_r, power_bm_r = power_bm_r, power_agg = power_agg)

p2 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = power_bin_r, colour = "3", linetype = "3")) +
  geom_point(aes(y = power_bin_r, colour = "3", shape = "3")) +
  geom_line(aes(y = power_agg, colour = "2", linetype = "3")) +
  geom_point(aes(y = power_agg, colour = "2", shape = "2")) +
  geom_line(aes(y = power_bc, colour = "4", linetype = "3")) +
  geom_point(aes(y = power_bc, colour = "4", shape = "4")) +
  geom_line(aes(y = power_bm_r, colour = "5", linetype = "3")) +
  geom_point(aes(y = power_bm_r, colour = "5", shape = "5")) +
  scale_linetype_manual(guide = "none", values = c("3" = "solid", "4" = "dashed", "5" = "dotted")) +
  scale_shape_manual(
    name = "Strategy", values = c("3" = 6, "2" = 0, "4" = 8, "5" = 1),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("3" = col[3], "2" = col[2], "4" = col[4], "5" = col[5]),
    labels = c("3" = lab[3], "2" = lab[2], "4" = lab[4], "5" = lab[5])
  ) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(bquote(alpha == .(alpha))) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


combined <- p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("results/Plot_power_randomized.pdf", plot = combined, width = 12, height = 4.5)
