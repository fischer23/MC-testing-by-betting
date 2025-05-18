# Generates Figures S.1-S.9 of the paper "Sequential Monte-Carlo testing by betting"

rm(list = ls())

library(ggplot2)
library(patchwork)

load("appendix/results/power_appendix.rda")

col <- c("orange", "cornflowerblue")
shapes <- c(1, 8)

colnames(results_df)[colnames(results_df) == "labs"] <- "Strategy"

### Figure S.6

p1 <- ggplot(results_df[(results_df$alphas == 0.05 & results_df$ns == 100 & results_df$c_factors == 0.9 &
  results_df$dists == "normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p2 <- ggplot(
  results_df[(results_df$alphas == 0.05 & results_df$ns == 100 & results_df$c_factors == 0.9 &
    results_df$dists == "normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

p3 <- ggplot(results_df[(results_df$alphas == 0.01 & results_df$ns == 100 & results_df$c_factors == 0.9 &
  results_df$dists == "normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p4 <- ggplot(
  results_df[(results_df$alphas == 0.01 & results_df$ns == 100 & results_df$c_factors == 0.9 &
    results_df$dists == "normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


combined <- p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("results/appendix/plot_n100.pdf", plot = combined, width = 12, height = 7.5)


### Figure S.7

p1 <- ggplot(results_df[(results_df$alphas == 0.05 & results_df$ns == 200 & results_df$c_factors == 0.9 &
  results_df$dists == "normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p2 <- ggplot(
  results_df[(results_df$alphas == 0.05 & results_df$ns == 200 & results_df$c_factors == 0.9 &
    results_df$dists == "normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

p3 <- ggplot(results_df[(results_df$alphas == 0.01 & results_df$ns == 200 & results_df$c_factors == 0.9 &
  results_df$dists == "normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p4 <- ggplot(
  results_df[(results_df$alphas == 0.01 & results_df$ns == 200 & results_df$c_factors == 0.9 &
    results_df$dists == "normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


combined <- p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("results/appendix/plot_n200.pdf", plot = combined, width = 12, height = 7.5)

### Figure S.8

p1 <- ggplot(results_df[(results_df$alphas == 0.05 & results_df$ns == 500 & results_df$c_factors == 0.9 &
  results_df$dists == "normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p2 <- ggplot(
  results_df[(results_df$alphas == 0.05 & results_df$ns == 500 & results_df$c_factors == 0.9 &
    results_df$dists == "normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

p3 <- ggplot(results_df[(results_df$alphas == 0.01 & results_df$ns == 500 & results_df$c_factors == 0.9 &
  results_df$dists == "normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p4 <- ggplot(
  results_df[(results_df$alphas == 0.01 & results_df$ns == 500 & results_df$c_factors == 0.9 &
    results_df$dists == "normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


combined <- p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("results/appendix/plot_n500.pdf", plot = combined, width = 12, height = 7.5)

### Figure S.9

p1 <- ggplot(results_df[(results_df$alphas == 0.05 & results_df$ns == 2000 & results_df$c_factors == 0.9 &
  results_df$dists == "normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p2 <- ggplot(
  results_df[(results_df$alphas == 0.05 & results_df$ns == 2000 & results_df$c_factors == 0.9 &
    results_df$dists == "normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

p3 <- ggplot(results_df[(results_df$alphas == 0.01 & results_df$ns == 2000 & results_df$c_factors == 0.9 &
  results_df$dists == "normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p4 <- ggplot(
  results_df[(results_df$alphas == 0.01 & results_df$ns == 2000 & results_df$c_factors == 0.9 &
    results_df$dists == "normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


combined <- p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("results/appendix/plot_n2000.pdf", plot = combined, width = 12, height = 7.5)

### Figure S.3

p1 <- ggplot(results_df[(results_df$alphas == 0.05 & results_df$ns == 1000 & results_df$c_factors == 0.9 &
  results_df$dists == "log-normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p2 <- ggplot(
  results_df[(results_df$alphas == 0.05 & results_df$ns == 1000 & results_df$c_factors == 0.9 &
    results_df$dists == "log-normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

p3 <- ggplot(results_df[(results_df$alphas == 0.01 & results_df$ns == 1000 & results_df$c_factors == 0.9 &
  results_df$dists == "log-normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p4 <- ggplot(
  results_df[(results_df$alphas == 0.01 & results_df$ns == 1000 & results_df$c_factors == 0.9 &
    results_df$dists == "log-normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


combined <- p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("results/appendix/plot_log-normal.pdf", plot = combined, width = 12, height = 7.5)


### Figure S.4

p1 <- ggplot(results_df[(results_df$alphas == 0.05 & results_df$ns == 1000 & results_df$c_factors == 0.8 &
  results_df$dists == "normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p2 <- ggplot(
  results_df[(results_df$alphas == 0.05 & results_df$ns == 1000 & results_df$c_factors == 0.8 &
    results_df$dists == "normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

p3 <- ggplot(results_df[(results_df$alphas == 0.01 & results_df$ns == 1000 & results_df$c_factors == 0.8 &
  results_df$dists == "normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p4 <- ggplot(
  results_df[(results_df$alphas == 0.01 & results_df$ns == 1000 & results_df$c_factors == 0.8 &
    results_df$dists == "normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


combined <- p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("results/appendix/plot_c0.8.pdf", plot = combined, width = 12, height = 7.5)

### Figure S.5

p1 <- ggplot(results_df[(results_df$alphas == 0.05 & results_df$ns == 1000 & results_df$c_factors == 0.99 &
  results_df$dists == "normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p2 <- ggplot(
  results_df[(results_df$alphas == 0.05 & results_df$ns == 1000 & results_df$c_factors == 0.99 &
    results_df$dists == "normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.05)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

p3 <- ggplot(results_df[(results_df$alphas == 0.01 & results_df$ns == 1000 & results_df$c_factors == 0.99 &
  results_df$dists == "normal" & results_df$types == "Power"), ], aes(x = mus, y = values)) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Power") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


p4 <- ggplot(
  results_df[(results_df$alphas == 0.01 & results_df$ns == 1000 & results_df$c_factors == 0.99 &
    results_df$dists == "normal" & results_df$types == "Total #permutations"), ],
  aes(x = mus, y = values)
) +
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values = col) +
  scale_shape_manual(values = shapes) +
  xlab(bquote(mu)) +
  ylab("Total #permutations") +
  ggtitle(expression(alpha == 0.01)) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1000, 1000 / 5), limits = c(0, (1000 + 1000 / 20)), expand = c(0, 0)) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 18), axis.title.x = element_text(size = 18),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


combined <- p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("appendix/results/plot_c0.99.pdf", plot = combined, width = 12, height = 7.5)

### Figure S.2

# For alpha=0.05

load("results/pval_alpha005_mu01.rda")

mu <- 0.1
alpha <- 0.05
m <- 2000

lab <- c("Permutation p-value", "Binomial mixture", "Binomial", "Besag Clifford", "Aggressive")
col <- c("cornflowerblue", "orange", "limegreen", "cornflowerblue", "red")


results_df <- data.frame(
  idx = (1:m), p_perm = sort(log(p_perm)), p_bm = sort(log(p_bm)),
  p_bin = sort(log(p_bin)), p_agg = sort(log(p_agg))
)

p1 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = p_perm, colour = "1", linewidth = "1")) +
  geom_line(aes(y = p_bin, colour = "3", linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_hline(yintercept = log(alpha), linewidth = 1, colour = "darkgrey") +
  scale_linewidth_manual(
    name = "Strategy", values = c("1" = 1, "3" = 1, "5" = 1, "2" = 1),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("1" = col[1], "3" = col[3], "5" = col[5], "2" = col[2]),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2])
  ) +
  xlab("Index") +
  ylab("Log-transformed p-values") +
  scale_x_continuous(breaks = seq(100, 1900, 200), limits = c(0, 2000), expand = c(0, 0)) +
  scale_y_continuous(breaks = (-6:0), limits = c(-7, 0), expand = c(0, 0)) +
  ggtitle(bquote(mu == .(mu))) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 20), legend.title = element_text(size = 20)
  )


load("results/pval_alpha005_mu1.rda")

mu <- 1
alpha <- 0.05
m <- 2000

lab <- c("Permutation p-value", "Binomial mixture", "Binomial", "Besag Clifford", "Aggressive")
col <- c("cornflowerblue", "orange", "limegreen", "cornflowerblue", "red")


results_df <- data.frame(
  idx = (1:m), p_perm = sort(log(p_perm)), p_bm = sort(log(p_bm)),
  p_bin = sort(log(p_bin)), p_agg = sort(log(p_agg))
)

p2 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = p_perm, colour = "1", linewidth = "1")) +
  geom_line(aes(y = p_bin, colour = "3", linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_hline(yintercept = log(alpha), linewidth = 1, colour = "darkgrey") +
  scale_linewidth_manual(
    name = "Strategy", values = c("1" = 1, "3" = 1, "5" = 1, "2" = 1),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("1" = col[1], "3" = col[3], "5" = col[5], "2" = col[2]),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2])
  ) +
  xlab("Index") +
  ylab("Log-transformed p-values") +
  scale_x_continuous(breaks = seq(100, 1900, 200), limits = c(0, 2000), expand = c(0, 0)) +
  scale_y_continuous(breaks = (-6:0), limits = c(-7, 0), expand = c(0, 0)) +
  ggtitle(bquote(mu == .(mu))) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 20), legend.title = element_text(size = 20)
  )


load("results/pval_alpha005_mu001.rda")

mu <- 0.01
alpha <- 0.05
m <- 2000

lab <- c("Permutation p-value", "Binomial mixture", "Binomial", "Besag Clifford", "Aggressive")
col <- c("cornflowerblue", "orange", "limegreen", "green", "red")


results_df <- data.frame(
  idx = (1:m), p_perm = sort(log(p_perm)), p_bm = sort(log(p_bm)),
  p_bin = sort(log(p_bin)), p_agg = sort(log(p_agg))
)

p3 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = p_perm, colour = "1", linewidth = "1")) +
  geom_line(aes(y = p_bin, colour = "3", linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_hline(yintercept = log(alpha), linewidth = 1, colour = "darkgrey") +
  scale_linewidth_manual(
    name = "Strategy", values = c("1" = 1, "3" = 1, "5" = 1, "2" = 1),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("1" = col[1], "3" = col[3], "5" = col[5], "2" = col[2]),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2])
  ) +
  xlab("Index") +
  ylab("Log-transformed p-values") +
  scale_x_continuous(breaks = seq(100, 1900, 200), limits = c(0, 2000), expand = c(0, 0)) +
  scale_y_continuous(breaks = (-6:0), limits = c(-7, 0), expand = c(0, 0)) +
  ggtitle(bquote(mu == .(mu))) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 20), legend.title = element_text(size = 20)
  )



combined <- p3 + p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave("results/Plot_logp_alpha005.pdf", plot = combined, width = 15, height = 5)


# For alpha=0.01

load("results/pval_alpha001_mu01.rda")

mu <- 0.1
alpha <- 0.01
m <- 2000

lab <- c("Permutation p-value", "Binomial mixture", "Binomial", "Besag Clifford", "Aggressive")
col <- c("cornflowerblue", "orange", "limegreen", "cornflowerblue", "red")


results_df <- data.frame(
  idx = (1:m), p_perm = sort(log(p_perm)), p_bm = sort(log(p_bm)),
  p_bin = sort(log(p_bin)), p_agg = sort(log(p_agg))
)

p1 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = p_perm, colour = "1", linewidth = "1")) +
  geom_line(aes(y = p_bin, colour = "3", linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_hline(yintercept = log(alpha), linewidth = 1, colour = "darkgrey") +
  scale_linewidth_manual(
    name = "Strategy", values = c("1" = 1, "3" = 1, "5" = 1, "2" = 1),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("1" = col[1], "3" = col[3], "5" = col[5], "2" = col[2]),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2])
  ) +
  xlab("Index") +
  ylab("Log-transformed p-values") +
  scale_x_continuous(breaks = seq(100, 1900, 200), limits = c(0, 2000), expand = c(0, 0)) +
  scale_y_continuous(breaks = (-6:0), limits = c(-7, 0), expand = c(0, 0)) +
  ggtitle(bquote(mu == .(mu))) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 20), legend.title = element_text(size = 20)
  )


load("results/pval_alpha001_mu1.rda")

mu <- 1
alpha <- 0.01
m <- 2000

lab <- c("Permutation p-value", "Binomial mixture", "Binomial", "Besag Clifford", "Aggressive")
col <- c("cornflowerblue", "orange", "limegreen", "cornflowerblue", "red")


results_df <- data.frame(
  idx = (1:m), p_perm = sort(log(p_perm)), p_bm = sort(log(p_bm)),
  p_bin = sort(log(p_bin)), p_agg = sort(log(p_agg))
)

p2 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = p_perm, colour = "1", linewidth = "1")) +
  geom_line(aes(y = p_bin, colour = "3", linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_hline(yintercept = log(alpha), linewidth = 1, colour = "darkgrey") +
  scale_linewidth_manual(
    name = "Strategy", values = c("1" = 1, "3" = 1, "5" = 1, "2" = 1),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("1" = col[1], "3" = col[3], "5" = col[5], "2" = col[2]),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2])
  ) +
  xlab("Index") +
  ylab("Log-transformed p-values") +
  scale_x_continuous(breaks = seq(100, 1900, 200), limits = c(0, 2000), expand = c(0, 0)) +
  scale_y_continuous(breaks = (-6:0), limits = c(-7, 0), expand = c(0, 0)) +
  ggtitle(bquote(mu == .(mu))) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 20), legend.title = element_text(size = 20)
  )


load("results/pval_alpha001_mu001.rda")

mu <- 0.01
alpha <- 0.01
m <- 2000

lab <- c("Permutation p-value", "Binomial mixture", "Binomial", "Besag Clifford", "Aggressive")
col <- c("cornflowerblue", "orange", "limegreen", "green", "red")


results_df <- data.frame(
  idx = (1:m), p_perm = sort(log(p_perm)), p_bm = sort(log(p_bm)),
  p_bin = sort(log(p_bin)), p_agg = sort(log(p_agg))
)

p3 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = p_perm, colour = "1", linewidth = "1")) +
  geom_line(aes(y = p_bin, colour = "3", linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_hline(yintercept = log(alpha), linewidth = 1, colour = "darkgrey") +
  scale_linewidth_manual(
    name = "Strategy", values = c("1" = 1, "3" = 1, "5" = 1, "2" = 1),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("1" = col[1], "3" = col[3], "5" = col[5], "2" = col[2]),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2])
  ) +
  xlab("Index") +
  ylab("Log-transformed p-values") +
  scale_x_continuous(breaks = seq(100, 1900, 200), limits = c(0, 2000), expand = c(0, 0)) +
  scale_y_continuous(breaks = (-6:0), limits = c(-7, 0), expand = c(0, 0)) +
  ggtitle(bquote(mu == .(mu))) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 20), legend.title = element_text(size = 20)
  )



combined <- p3 + p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave("results/Plot_logp_alpha001.pdf", plot = combined, width = 15, height = 5)

### Figure S.1

load("results/pval_alpha005_mu01.rda")

b1 <- 50
b2 <- 100
b3 <- 200
alpha <- 0.05

lab <- c("Permutation p-value", "Binomial mixture (uniform prior)", "Binomial", "Besag Clifford", "Aggressive", "Binomial mixture (beta prior)")
col <- c("cornflowerblue", "orange", "limegreen", "cornflowerblue", "red", "purple")


results_df <- data.frame(
  idx = (1:m), p_perm = sort(log(p_perm)), p_bm = sort(log(p_bm)),
  p_bin = sort(log(p_bin)), p_agg = sort(log(p_agg)), p_bm_beta = sort(log(p_bm_beta_b1))
)

p1 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = p_perm, colour = "1", linewidth = "1")) +
  geom_line(aes(y = p_bin, colour = "3", linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_line(aes(y = p_bm_beta, colour = "6", linewidth = "6")) +
  geom_hline(yintercept = log(alpha), linewidth = 1, colour = "darkgrey") +
  scale_linewidth_manual(
    name = "Strategy", values = c("1" = 1, "3" = 1, "5" = 1, "2" = 1, "6" = 1),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2], "6" = lab[6])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("1" = col[1], "3" = col[3], "5" = col[5], "2" = col[2], "6" = col[6]),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2], "6" = lab[6])
  ) +
  xlab("Index") +
  ylab("Log-transformed p-values") +
  scale_x_continuous(breaks = seq(100, 1900, 200), limits = c(0, 2000), expand = c(0, 0)) +
  scale_y_continuous(breaks = (-6:0), limits = c(-7, 0), expand = c(0, 0)) +
  ggtitle(bquote(b == .(b1))) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


results_df <- data.frame(
  idx = (1:m), p_perm = sort(log(p_perm)), p_bm = sort(log(p_bm)),
  p_bin = sort(log(p_bin)), p_agg = sort(log(p_agg)), p_bm_beta = sort(log(p_bm_beta_b2))
)

p2 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = p_perm, colour = "1", linewidth = "1")) +
  geom_line(aes(y = p_bin, colour = "3", linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_line(aes(y = p_bm_beta, colour = "6", linewidth = "6")) +
  geom_hline(yintercept = log(alpha), linewidth = 1, colour = "darkgrey") +
  scale_linewidth_manual(
    name = "Strategy", values = c("1" = 1, "3" = 1, "5" = 1, "2" = 1, "6" = 1),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2], "6" = lab[6])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("1" = col[1], "3" = col[3], "5" = col[5], "2" = col[2], "6" = col[6]),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2], "6" = lab[6])
  ) +
  xlab("Index") +
  ylab("Log-transformed p-values") +
  scale_x_continuous(breaks = seq(100, 1900, 200), limits = c(0, 2000), expand = c(0, 0)) +
  scale_y_continuous(breaks = (-6:0), limits = c(-7, 0), expand = c(0, 0)) +
  ggtitle(bquote(b == .(b2))) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )


results_df <- data.frame(
  idx = (1:m), p_perm = sort(log(p_perm)), p_bm = sort(log(p_bm)),
  p_bin = sort(log(p_bin)), p_agg = sort(log(p_agg)), p_bm_beta = sort(log(p_bm_beta_b3))
)

p3 <- ggplot(results_df, aes(idx)) +
  geom_line(aes(y = p_perm, colour = "1", linewidth = "1")) +
  geom_line(aes(y = p_bin, colour = "3", linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_line(aes(y = p_bm_beta, colour = "6", linewidth = "6")) +
  geom_hline(yintercept = log(alpha), linewidth = 1, colour = "darkgrey") +
  scale_linewidth_manual(
    name = "Strategy", values = c("1" = 1, "3" = 1, "5" = 1, "2" = 1, "6" = 1),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2], "6" = lab[6])
  ) +
  scale_colour_manual(
    name = "Strategy", values = c("1" = col[1], "3" = col[3], "5" = col[5], "2" = col[2], "6" = col[6]),
    labels = c("1" = lab[1], "3" = lab[3], "5" = lab[5], "2" = lab[2], "6" = lab[6])
  ) +
  xlab("Index") +
  ylab("Log-transformed p-values") +
  scale_x_continuous(breaks = seq(100, 1900, 200), limits = c(0, 2000), expand = c(0, 0)) +
  scale_y_continuous(breaks = (-6:0), limits = c(-7, 0), expand = c(0, 0)) +
  ggtitle(bquote(b == .(b3))) +
  theme(
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20), title = element_text(size = 15),
    axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 18), legend.title = element_text(size = 18)
  )

combined <- p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave("results/Plot_alpha_005_beta.pdf", plot = combined, width = 15, height = 5)
