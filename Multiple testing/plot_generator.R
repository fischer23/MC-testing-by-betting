#Generates all figures of the paper "Multiple testing with anytime-valid Monte-Carlo p-values"

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggrepel)

#######################Plots power and number of permutations

#####Different pis
load("results/Plot_FDR_pis.rda")

#Prepare the data
results_df_1_power=data.frame(idx_1=seq(0.1, 0.9, 0.1), power_BH=power_BH, power_BH_smallB=power_BH_smallB, 
                              power_bm=power_bm,power_bc=power_bc,power_agg=power_agg,power_amt=power_amt)

results_df_1_nperm=data.frame(idx_1=seq(0.1, 0.9, 0.1), nperm_mean_bm=nperm_mean_bm, nperm_mean_bc=nperm_mean_bc,
                              nperm_mean_agg=nperm_mean_agg, nperm_mean_BH=10000, nperm_mean_BH_smallB=200, nperm_mean_amt=nperm_mean_amt)

results_df_1_power_long=pivot_longer(data=results_df_1_power, cols=power_BH:power_amt, names_to = "Strategy", names_prefix = "power_", values_to="power")
results_df_1_nperm_long=pivot_longer(data=results_df_1_nperm, cols=nperm_mean_bm:nperm_mean_amt, names_to = "Strategy", names_prefix = "nperm_mean_", values_to="nperm")
results_df_1_long=left_join(results_df_1_power_long, results_df_1_nperm_long, by = c("idx_1" = "idx_1", "Strategy" = "Strategy"))  

results_df_1_long$Strategy=as.factor(results_df_1_long$Strategy)
levels(results_df_1_long$Strategy)=c("Aggressive (ours)", "AMT", "Anytime-valid BC (ours)", "Permutation p-value", "Permutation p-value (B=200)", "Binomial mixture (ours)")


cols=c( "Permutation p-value"="cornflowerblue", "Permutation p-value (B=200)"="purple", "Aggressive (ours)"="red", "Anytime-valid BC (ours)"="limegreen", 
        "Binomial mixture (ours)"="orange", "AMT"="aquamarine")
shapes=c( "Permutation p-value"=6, "Permutation p-value (B=200)"=3, "Aggressive (ours)"=0, "Anytime-valid BC (ours)"=8, "Binomial mixture (ours)"=1, "AMT"=4)

p1=ggplot(results_df_1_long, aes(x=idx_1, y=power)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  geom_text_repel(data=dplyr::filter(results_df_1_long, idx_1==0.9),
                  aes(colour = Strategy, label = Strategy, x=idx_1, y=power, segment.square = TRUE, segment.inflect = TRUE),
                  segment.colour="darkgrey", force=1, nudge_x=0.25, direction = "y", segment.size = 0.1, segment.curvature = -0.001,
                  max.overlaps = Inf, min.segment.length = 0) +
  scale_shape_manual(values=shapes)+
  scale_colour_manual(values=cols)+
  xlab(expression(pi[A]))+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2), limits=c(0.05, 1.35), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits=c(0, 1), expand = c(0, 0)) +
  theme(legend.position="none", panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p2=ggplot(results_df_1_long, aes(x=idx_1, y=nperm)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  geom_text_repel(data=dplyr::filter(results_df_1_long, idx_1==0.9),
                  aes(colour = Strategy, label = Strategy, x=idx_1, y=nperm, segment.square = TRUE, segment.inflect = TRUE),
                  segment.colour="darkgrey", force=1, nudge_x=0.25, direction = "y", segment.size = 0.1, segment.curvature = -0.001,
                  max.overlaps = Inf, min.segment.length = 0) +
  scale_shape_manual(values=shapes)+
  scale_colour_manual(values=cols)+
  xlab(expression(pi[A]))+
  ylab("#Permutations")+
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2), limits=c(0.05, 1.35), expand = c(0, 0)) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
  theme(legend.position="none", panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15))


combined <- p1 + p2

ggsave("results/Plot_power_pis.pdf",plot=combined, width=14, height=4.5)


#####Different alphas
load("results/Plot_FDR_alphas.rda")

#Prepare the data
results_df_2_power=data.frame(idx_2=c(0.01,0.05, 0.1, 0.15, 0.2), power_BH=power_BH, power_BH_smallB=power_BH_smallB, 
                              power_bm=power_bm,power_bc=power_bc,power_agg=power_agg,power_amt=power_amt)

results_df_2_nperm=data.frame(idx_2=c(0.01,0.05, 0.1, 0.15, 0.2), nperm_mean_bm=nperm_mean_bm, nperm_mean_bc=nperm_mean_bc,
                              nperm_mean_agg=nperm_mean_agg, nperm_mean_BH=10000, nperm_mean_BH_smallB=200, nperm_mean_amt=nperm_mean_amt)

results_df_2_power_long=pivot_longer(data=results_df_2_power, cols=power_BH:power_amt, names_to = "Strategy", names_prefix = "power_", values_to="power")
results_df_2_nperm_long=pivot_longer(data=results_df_2_nperm, cols=nperm_mean_bm:nperm_mean_amt, names_to = "Strategy", names_prefix = "nperm_mean_", values_to="nperm")
results_df_2_long=left_join(results_df_2_power_long, results_df_2_nperm_long, by = c("idx_2" = "idx_2", "Strategy" = "Strategy"))  

results_df_2_long$Strategy=as.factor(results_df_2_long$Strategy)
levels(results_df_2_long$Strategy)=c("Aggressive (ours)", "AMT", "Anytime-valid BC (ours)", "Permutation p-value", "Permutation p-value (B=200)", "Binomial mixture (ours)")

p3=ggplot(results_df_2_long, aes(x=idx_2, y=power)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  geom_text_repel(data=dplyr::filter(results_df_2_long, idx_2==0.2),
                  aes(colour = Strategy, label = Strategy, x=idx_2, y=power, segment.square = TRUE, segment.inflect = TRUE),
                  segment.colour="darkgrey", force=1, nudge_x=0.1, direction = "y", segment.size = 0.1, segment.curvature = -0.001,
                  max.overlaps = Inf, min.segment.length = 0) +
  scale_shape_manual(values=shapes)+
  scale_colour_manual(values=cols)+
  xlab(expression(alpha))+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0, 0.2, 0.05), limits=c(0, 0.33), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits=c(0, 1), expand = c(0, 0)) +
  theme(legend.position="none",panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p4=ggplot(results_df_2_long, aes(x=idx_2, y=nperm)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  geom_text_repel(data=dplyr::filter(results_df_2_long, idx_2==0.2),
                  aes(colour = Strategy, label = Strategy, x=idx_2, y=nperm, segment.square = TRUE, segment.inflect = TRUE),
                  segment.colour="darkgrey", force=1, nudge_x=0.1, direction = "y", segment.size = 0.1, segment.curvature = -0.001,
                  max.overlaps = Inf, min.segment.length = 0) +
  scale_shape_manual(values=shapes)+
  scale_colour_manual(values=cols)+
  xlab(expression(alpha))+
  ylab("#permutations")+
  scale_x_continuous(breaks = seq(0, 0.2, 0.05), limits=c(0, 0.33), expand = c(0, 0)) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
  theme(legend.position="none", panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

combined <- p3 + p4 

ggsave("results/Plot_power_alphas.pdf",plot=combined, width=14, height=4.5)


#####Different mu_As
load("results/Plot_FDR_mus.rda")

#Prepare the data
results_df_3_power=data.frame(idx_3=c(1,1.5,2,2.5,3,3.5,4), power_BH=power_BH, power_BH_smallB=power_BH_smallB, 
                              power_bm=power_bm,power_bc=power_bc,power_agg=power_agg,power_amt=power_amt)

results_df_3_nperm=data.frame(idx_3=c(1,1.5,2,2.5,3,3.5,4), nperm_mean_bm=nperm_mean_bm, nperm_mean_bc=nperm_mean_bc,
                              nperm_mean_agg=nperm_mean_agg, nperm_mean_BH=10000, nperm_mean_BH_smallB=200, nperm_mean_amt=nperm_mean_amt)

results_df_3_power_long=pivot_longer(data=results_df_3_power, cols=power_BH:power_amt, names_to = "Strategy", names_prefix = "power_", values_to="power")
results_df_3_nperm_long=pivot_longer(data=results_df_3_nperm, cols=nperm_mean_bm:nperm_mean_amt, names_to = "Strategy", names_prefix = "nperm_mean_", values_to="nperm")
results_df_3_long=left_join(results_df_3_power_long, results_df_3_nperm_long, by = c("idx_3" = "idx_3", "Strategy" = "Strategy"))  

results_df_3_long$Strategy=as.factor(results_df_3_long$Strategy)
levels(results_df_3_long$Strategy)=c("Aggressive (ours)", "AMT", "Anytime-valid BC (ours)", "Permutation p-value", "Permutation p-value (B=200)", "Binomial mixture (ours)")


p5=ggplot(results_df_3_long, aes(x=idx_3, y=power)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  geom_text_repel(data=dplyr::filter(results_df_3_long, idx_3==4),
                  aes(colour = Strategy, label = Strategy, x=idx_3, y=power, segment.square = TRUE, segment.inflect = TRUE),
                  segment.colour="darkgrey", force=1, nudge_x=1, direction = "y", segment.size = 0.1, segment.curvature = -0.001,
                  max.overlaps = Inf, min.segment.length = 0) +
  scale_shape_manual(values=shapes)+
  scale_colour_manual(values=cols)+
  xlab(expression(mu[A]))+
  ylab("Power")+
  scale_x_continuous(breaks = seq(1, 4, 1), limits=c(0.8, 6), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits=c(0, 1), expand = c(0, 0)) +
  theme(legend.position="none",panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p6=ggplot(results_df_3_long, aes(x=idx_3, y=nperm)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  geom_text_repel(data=dplyr::filter(results_df_3_long, idx_3==4),
                  aes(colour = Strategy, label = Strategy, x=idx_3, y=nperm, segment.square = TRUE, segment.inflect = TRUE),
                  segment.colour="darkgrey", force=1, nudge_x=1, direction = "y", segment.size = 0.1, segment.curvature = -0.001,
                  max.overlaps = Inf, min.segment.length = 0) +
  scale_shape_manual(values=shapes)+
  scale_colour_manual(values=cols)+
  xlab(expression(mu[A]))+
  ylab("#permutations")+
  scale_x_continuous(breaks = seq(1, 4, 1), limits=c(0.8, 6), expand = c(0, 0)) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
  theme(legend.position="none", panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 



combined <- p5 + p6

ggsave("results/Plot_power_mus.pdf",plot=combined, width=14, height=4.5)

#####Different numbers of hypotheses
load("results/Plot_FDR_Ms.rda")

#Prepare the data
results_df_4_power=data.frame(idx_4=c(100,500, 1000,3000,5000,10000), power_BH=power_BH, power_BH_smallB=power_BH_smallB, 
                              power_bm=power_bm,power_bc=power_bc,power_agg=power_agg,power_amt=power_amt)

results_df_4_nperm=data.frame(idx_4=c(100,500, 1000,3000,5000,10000), nperm_mean_bm=nperm_mean_bm, nperm_mean_bc=nperm_mean_bc,
                              nperm_mean_agg=nperm_mean_agg, nperm_mean_BH=10000, nperm_mean_BH_smallB=200, nperm_mean_amt=nperm_mean_amt)

results_df_4_power_long=pivot_longer(data=results_df_4_power, cols=power_BH:power_amt, names_to = "Strategy", names_prefix = "power_", values_to="power")
results_df_4_nperm_long=pivot_longer(data=results_df_4_nperm, cols=nperm_mean_bm:nperm_mean_amt, names_to = "Strategy", names_prefix = "nperm_mean_", values_to="nperm")
results_df_4_long=left_join(results_df_4_power_long, results_df_4_nperm_long, by = c("idx_4" = "idx_4", "Strategy" = "Strategy"))  

results_df_4_long$Strategy=as.factor(results_df_4_long$Strategy)
levels(results_df_4_long$Strategy)=c("Aggressive (ours)", "AMT", "Anytime-valid BC (ours)", "Permutation p-value", "Permutation p-value (B=200)", "Binomial mixture (ours)")


p7=ggplot(results_df_4_long, aes(x=idx_4, y=power)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  geom_text_repel(data=dplyr::filter(results_df_4_long, idx_4==10000),
                  aes(colour = Strategy, label = Strategy, x=idx_4, y=power, segment.square = TRUE, segment.inflect = TRUE),
                  segment.colour="darkgrey", force=3, nudge_x=1, direction = "y", segment.size = 0.1, segment.curvature = -0.001,
                  max.overlaps = Inf, min.segment.length = 0) +
  scale_shape_manual(values=shapes)+
  scale_colour_manual(values=cols)+
  xlab("Number of hypotheses")+
  ylab("Power")+
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits = c(100,100000)
  )+
  scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits=c(0, 1), expand = c(0, 0)) +
  theme(legend.position="none",panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p8=ggplot(results_df_4_long, aes(x=idx_4, y=nperm)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  geom_text_repel(data=dplyr::filter(results_df_4_long, idx_4==10000),
                  aes(colour = Strategy, label = Strategy, x=idx_4, y=nperm, segment.square = TRUE, segment.inflect = TRUE),
                  segment.colour="darkgrey", force=1, nudge_x=1, direction = "y", segment.size = 0.1, segment.curvature = -0.001,
                  max.overlaps = Inf, min.segment.length = 0) +
  scale_shape_manual(values=shapes)+
  scale_colour_manual(values=cols)+
  xlab("Number of hypotheses")+
  ylab("#permutations")+
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits = c(100,100000)
  )+
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
  theme(legend.position="none", panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


combined <- p7 + p8

ggsave("results/Plot_power_Ms.pdf",plot=combined, width=14, height=4.5)

###All plots together
combined <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + plot_layout( ncol=2) 
ggsave("results/Plot_power_all.pdf",plot=combined, width=14, height=18)


#####Different rhos
load("results/Plot_FDR_rhos.rda")

lab=c("Aggressive (ours)", "Anytime-valid BC (ours)", "Permutation p-value", "Permutation p-value (B=200)", "Binomial mixture (ours)")
col=c("cornflowerblue", "purple", "red", "limegreen", "orange", "aquamarine")


results_df_5=data.frame(idx_5=c(0,0.1,0.3,0.5,0.7,0.9), FDR_BH=FDR_BH, FDR_BH_smallB=FDR_BH_smallB,
                        FDR_bm=FDR_bm,FDR_bc=FDR_bc,FDR_agg=FDR_agg)

p9=ggplot(results_df_5, aes(idx_5)) + 
  geom_line(aes(y = FDR_BH,colour = "1", linetype="3")) +
  geom_point(aes(y = FDR_BH,colour = "1",shape = "1")) +
  geom_line(aes(y = FDR_BH_smallB,colour = "2", linetype="3")) +
  geom_point(aes(y = FDR_BH_smallB,colour = "2",shape = "2")) +
  geom_line(aes(y = FDR_agg,colour = "3", linetype="3")) +
  geom_point(aes(y = FDR_agg,colour = "3",shape = "3")) +
  geom_line(aes(y = FDR_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = FDR_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = FDR_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = FDR_bm,colour = "5",shape = "5")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "1"=6, "2"=3, "3"=0, "4"=8, "5"=1, "6"=4), 
                     labels=c("1"=lab[3], "2"=lab[4], "3"=lab[1], "4"=lab[2], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[2], "3"=col[3], "4"=col[4], "5"=col[5], "6"=col[6]), 
                      labels=c("1"=lab[3], "2"=lab[4], "3"=lab[1], "4"=lab[2], "5"=lab[5]))+
  xlab(expression(rho))+
  ylab("FDR")+
  scale_x_continuous(breaks = c(0.1,0.3,0.5,0.7), limits=c(0, 0.9), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 0.14, 0.02), limits=c(0, 0.15), expand = c(0, 0)) +
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

ggsave("results/Plot_power_rhos.pdf",plot=p9, width=9, height=4)

#####Distribution of stops for rejection
load("results/Plot_FDR_standard.rda")


lab=c("Perm p-value", "Perm p-value (B=200)", "Aggressive", "Anytime-valid BC (ours)", "Binomial mixture (ours)") 
col=c( "cornflowerblue", "purple", "red", "limegreen", "orange")

results_df=data.frame(stops=c(result_bm[[1]]$stop[result_bm[[1]]$rejects],result_bc[[1]]$stop[result_bc[[1]]$rejects]),
                      group=c(rep("bm",length(result_bm[[1]]$rejects)), rep("bc",length(result_bc[[1]]$rejects))))

mean_perm_rej=c(mean(result_bm[[1]]$stop[result_bm[[1]]$rejects]), mean(result_bc[[1]]$stop[result_bc[[1]]$rejects]))
group=c("bm", "bc")
mean_df=data.frame(mean_perm_rej, group)

p=ggplot(results_df, aes(stops, color=group)) + 
  geom_histogram(fill="white", bins=33) +
  geom_vline(data=mean_df, aes(xintercept=mean_perm_rej, color=group),
             linetype="dashed")+
  scale_colour_manual(name="Strategy", values=c( "bm"=col[5], "bc"=col[4]), 
                      labels=c("bm"=lab[5], "bc"=lab[4]))+
  xlab("Time of rejection")+
  ylab("Frequency")+
  scale_x_log10(
    breaks = c(min(result_bm[[1]]$stop[result_bm[[1]]$rejects]), min(result_bc[[1]]$stop[result_bc[[1]]$rejects]),459, 1000, 10000),
    #labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


ggsave("results/Plot_nperm_dist.pdf",plot=p, width=8, height=4.5)



