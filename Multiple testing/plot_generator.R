#Generates all figures of the paper "Multiple testing with anytime-valid Monte-Carlo p-values"

rm(list=ls())

library(ggplot2)
library(patchwork)


#######################Plots power and number of permutations

#####Different pis
load("results/Plot_FDR_pis.rda")


lab=c("Permutation p-value", "Permutation p-value (B=200)", "Aggressive", "Anytime-valid BC", "Binomial mixture", "AMT") 
col=c( "cornflowerblue", "purple", "red", "limegreen", "orange", "aquamarine")

results_df_1=data.frame(idx_1=seq(0.1, 0.9, 0.1), power_BH=power_BH, power_BH_smallB=power_BH_smallB, 
                      power_bm=power_bm,power_bc=power_bc,power_agg=power_agg,power_amt=power_amt,nperm_mean_bm=nperm_mean_bm, nperm_mean_bc=nperm_mean_bc,
                      nperm_mean_agg=nperm_mean_agg, nperm_mean_BH=10000, nperm_mean_BH_smallB=200, nperm_mean_amt=nperm_mean_amt)
                     

p1=ggplot(results_df_1, aes(idx_1)) + 
  geom_line(aes(y = power_BH,colour = "1", linetype="3")) +
  geom_point(aes(y = power_BH,colour = "1",shape = "1")) +
  geom_line(aes(y = power_BH_smallB,colour = "2", linetype="3")) +
  geom_point(aes(y = power_BH_smallB,colour = "2",shape = "2")) +
  geom_line(aes(y = power_agg,colour = "3", linetype="3")) +
  geom_point(aes(y = power_agg,colour = "3",shape = "3")) +
  geom_line(aes(y = power_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = power_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = power_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = power_bm,colour = "5",shape = "5")) +
  geom_line(aes(y = power_amt,colour = "6", linetype="3")) +
  geom_point(aes(y = power_amt,colour = "6",shape = "6")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "1"=6, "2"=3, "3"=0, "4"=8, "5"=1, "6"=4), 
                     labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[2], "3"=col[3], "4"=col[4], "5"=col[5], "6"=col[6]), 
                      labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  xlab(expression(pi[A]))+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.2, 0.8, 0.2), limits=c(0.1, 0.9), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits=c(0, 1), expand = c(0, 0)) +
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p2=ggplot(results_df_1, aes(idx_1)) + 
  geom_line(aes(y = nperm_mean_BH,colour = "1", linetype="3")) +
  geom_point(aes(y = nperm_mean_BH,colour = "1",shape = "1")) +
  geom_line(aes(y = nperm_mean_BH_smallB,colour = "2", linetype="3")) +
  geom_point(aes(y = nperm_mean_BH_smallB,colour = "2",shape = "2")) +
  geom_line(aes(y = nperm_mean_agg,colour = "3", linetype="3")) +
  geom_point(aes(y = nperm_mean_agg,colour = "3",shape = "3")) +
  geom_line(aes(y = nperm_mean_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = nperm_mean_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = nperm_mean_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = nperm_mean_bm,colour = "5",shape = "5")) +
  geom_line(aes(y = nperm_mean_amt,colour = "6", linetype="3")) +
  geom_point(aes(y = nperm_mean_amt,colour = "6",shape = "6")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "1"=6, "2"=3, "3"=0, "4"=8, "5"=1, "6"=4), 
                     labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[2], "3"=col[3], "4"=col[4], "5"=col[5], "6"=col[6]), 
                      labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  xlab(expression(pi[A]))+
  ylab("#permutations")+
  scale_x_continuous(breaks = seq(0.2, 0.8, 0.2), limits=c(0.1, 0.9), expand = c(0, 0)) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


combined <- p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("results/Plot_power_pis.pdf",plot=combined, width=12, height=4.5)



#####Different alphas
load("results/Plot_FDR_alphas.rda")

results_df_2=data.frame(idx_2=c(0.01,0.05, 0.1, 0.15, 0.2), power_BH=power_BH, power_BH_smallB=power_BH_smallB, 
                        power_bm=power_bm,power_bc=power_bc,power_agg=power_agg,power_amt=power_amt,nperm_mean_bm=nperm_mean_bm, nperm_mean_bc=nperm_mean_bc,
                        nperm_mean_agg=nperm_mean_agg, nperm_mean_BH=10000, nperm_mean_BH_smallB=200, nperm_mean_amt=nperm_mean_amt)


p3=ggplot(results_df_2, aes(idx_2)) + 
  geom_line(aes(y = power_BH,colour = "1", linetype="3")) +
  geom_point(aes(y = power_BH,colour = "1",shape = "1")) +
  geom_line(aes(y = power_BH_smallB,colour = "2", linetype="3")) +
  geom_point(aes(y = power_BH_smallB,colour = "2",shape = "2")) +
  geom_line(aes(y = power_agg,colour = "3", linetype="3")) +
  geom_point(aes(y = power_agg,colour = "3",shape = "3")) +
  geom_line(aes(y = power_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = power_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = power_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = power_bm,colour = "5",shape = "5")) +
  geom_line(aes(y = power_amt,colour = "6", linetype="3")) +
  geom_point(aes(y = power_amt,colour = "6",shape = "6")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "1"=6, "2"=3, "3"=0, "4"=8, "5"=1, "6"=4), 
                     labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[2], "3"=col[3], "4"=col[4], "5"=col[5], "6"=col[6]), 
                      labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  xlab(expression(alpha))+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0, 0.2, 0.05), limits=c(0, 0.2), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits=c(0, 1), expand = c(0, 0)) +
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p4=ggplot(results_df_2, aes(idx_2)) + 
  geom_line(aes(y = nperm_mean_BH,colour = "1", linetype="3")) +
  geom_point(aes(y = nperm_mean_BH,colour = "1",shape = "1")) +
  geom_line(aes(y = nperm_mean_BH_smallB,colour = "2", linetype="3")) +
  geom_point(aes(y = nperm_mean_BH_smallB,colour = "2",shape = "2")) +
  geom_line(aes(y = nperm_mean_agg,colour = "3", linetype="3")) +
  geom_point(aes(y = nperm_mean_agg,colour = "3",shape = "3")) +
  geom_line(aes(y = nperm_mean_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = nperm_mean_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = nperm_mean_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = nperm_mean_bm,colour = "5",shape = "5")) +
  geom_line(aes(y = nperm_mean_amt,colour = "6", linetype="3")) +
  geom_point(aes(y = nperm_mean_amt,colour = "6",shape = "6")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "1"=6, "2"=3, "3"=0, "4"=8, "5"=1, "6"=4), 
                     labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[2], "3"=col[3], "4"=col[4], "5"=col[5], "6"=col[6]), 
                      labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  xlab(expression(alpha))+
  ylab("#permutations")+
  scale_x_continuous(breaks = seq(0, 0.2, 0.05), limits=c(0, 0.2), expand = c(0, 0)) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


combined <- p3 + p4 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("results/Plot_power_alphas.pdf",plot=combined, width=12, height=4.5)


#####Different mu_As
load("results/Plot_FDR_mus.rda")

results_df_3=data.frame(idx_3=c(1,1.5,2,2.5,3,3.5,4), power_BH=power_BH, power_BH_smallB=power_BH_smallB, 
                        power_bm=power_bm,power_bc=power_bc,power_agg=power_agg,power_amt=power_amt,nperm_mean_bm=nperm_mean_bm, nperm_mean_bc=nperm_mean_bc,
                        nperm_mean_agg=nperm_mean_agg, nperm_mean_BH=10000, nperm_mean_BH_smallB=200, nperm_mean_amt=nperm_mean_amt)


p5=ggplot(results_df_3, aes(idx_3)) + 
  geom_line(aes(y = power_BH,colour = "1", linetype="3")) +
  geom_point(aes(y = power_BH,colour = "1",shape = "1")) +
  geom_line(aes(y = power_BH_smallB,colour = "2", linetype="3")) +
  geom_point(aes(y = power_BH_smallB,colour = "2",shape = "2")) +
  geom_line(aes(y = power_agg,colour = "3", linetype="3")) +
  geom_point(aes(y = power_agg,colour = "3",shape = "3")) +
  geom_line(aes(y = power_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = power_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = power_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = power_bm,colour = "5",shape = "5")) +
  geom_line(aes(y = power_amt,colour = "6", linetype="3")) +
  geom_point(aes(y = power_amt,colour = "6",shape = "6")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "1"=6, "2"=3, "3"=0, "4"=8, "5"=1, "6"=4), 
                     labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[2], "3"=col[3], "4"=col[4], "5"=col[5], "6"=col[6]), 
                      labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  xlab(expression(mu[A]))+
  ylab("Power")+
  scale_x_continuous(breaks = seq(1, 4, 1), limits=c(1, 4), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits=c(0, 1), expand = c(0, 0)) +
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p6=ggplot(results_df_3, aes(idx_3)) + 
  geom_line(aes(y = nperm_mean_BH,colour = "1", linetype="3")) +
  geom_point(aes(y = nperm_mean_BH,colour = "1",shape = "1")) +
  geom_line(aes(y = nperm_mean_BH_smallB,colour = "2", linetype="3")) +
  geom_point(aes(y = nperm_mean_BH_smallB,colour = "2",shape = "2")) +
  geom_line(aes(y = nperm_mean_agg,colour = "3", linetype="3")) +
  geom_point(aes(y = nperm_mean_agg,colour = "3",shape = "3")) +
  geom_line(aes(y = nperm_mean_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = nperm_mean_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = nperm_mean_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = nperm_mean_bm,colour = "5",shape = "5")) +
  geom_line(aes(y = nperm_mean_amt,colour = "6", linetype="3")) +
  geom_point(aes(y = nperm_mean_amt,colour = "6",shape = "6")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "1"=6, "2"=3, "3"=0, "4"=8, "5"=1, "6"=4), 
                     labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[2], "3"=col[3], "4"=col[4], "5"=col[5], "6"=col[6]), 
                      labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  xlab(expression(mu[A]))+
  ylab("#permutations")+
  scale_x_continuous(breaks = seq(1, 4, 1), limits=c(1, 4), expand = c(0, 0)) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


combined <- p5 + p6 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("results/Plot_power_mus.pdf",plot=combined, width=12, height=4.5)

#####Different numbers of hypotheses
load("results/Plot_FDR_Ms.rda")

results_df_4=data.frame(idx_4=c(100,500, 1000,3000,5000,10000), power_BH=power_BH, power_BH_smallB=power_BH_smallB, 
                        power_bm=power_bm,power_bc=power_bc,power_agg=power_agg,power_amt=power_amt,nperm_mean_bm=nperm_mean_bm, nperm_mean_bc=nperm_mean_bc,
                        nperm_mean_agg=nperm_mean_agg, nperm_mean_BH=10000, nperm_mean_BH_smallB=200, nperm_mean_amt=nperm_mean_amt)


p7=ggplot(results_df_4, aes(idx_4)) + 
  geom_line(aes(y = power_BH,colour = "1", linetype="3")) +
  geom_point(aes(y = power_BH,colour = "1",shape = "1")) +
  geom_line(aes(y = power_BH_smallB,colour = "2", linetype="3")) +
  geom_point(aes(y = power_BH_smallB,colour = "2",shape = "2")) +
  geom_line(aes(y = power_agg,colour = "3", linetype="3")) +
  geom_point(aes(y = power_agg,colour = "3",shape = "3")) +
  geom_line(aes(y = power_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = power_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = power_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = power_bm,colour = "5",shape = "5")) +
  geom_line(aes(y = power_amt,colour = "6", linetype="3")) +
  geom_point(aes(y = power_amt,colour = "6",shape = "6")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "1"=6, "2"=3, "3"=0, "4"=8, "5"=1, "6"=4), 
                     labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[2], "3"=col[3], "4"=col[4], "5"=col[5], "6"=col[6]), 
                      labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  xlab("Number of hypotheses")+
  ylab("Power")+
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
  scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits=c(0, 1), expand = c(0, 0)) +
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p8=ggplot(results_df_4, aes(idx_4)) + 
  geom_line(aes(y = nperm_mean_BH,colour = "1", linetype="3")) +
  geom_point(aes(y = nperm_mean_BH,colour = "1",shape = "1")) +
  geom_line(aes(y = nperm_mean_BH_smallB,colour = "2", linetype="3")) +
  geom_point(aes(y = nperm_mean_BH_smallB,colour = "2",shape = "2")) +
  geom_line(aes(y = nperm_mean_agg,colour = "3", linetype="3")) +
  geom_point(aes(y = nperm_mean_agg,colour = "3",shape = "3")) +
  geom_line(aes(y = nperm_mean_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = nperm_mean_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = nperm_mean_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = nperm_mean_bm,colour = "5",shape = "5")) +
  geom_line(aes(y = nperm_mean_amt,colour = "6", linetype="3")) +
  geom_point(aes(y = nperm_mean_amt,colour = "6",shape = "6")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "1"=6, "2"=3, "3"=0, "4"=8, "5"=1, "6"=4), 
                     labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[2], "3"=col[3], "4"=col[4], "5"=col[5], "6"=col[6]), 
                      labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  xlab("Number of hypotheses")+
  ylab("#permutations")+
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


combined <- p7 + p8 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("results/Plot_power_Ms.pdf",plot=combined, width=12, height=4.5)

###All plots together
combined <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + plot_layout(guides = "collect", ncol=2) & theme(legend.position = "bottom")
ggsave("results/Plot_power_all.pdf",plot=combined, width=12, height=18)


#####Different rhos
load("results/Plot_FDR_rhos.rda")

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
                     labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[2], "3"=col[3], "4"=col[4], "5"=col[5], "6"=col[6]), 
                      labels=c("1"=lab[1], "2"=lab[2], "3"=lab[3], "4"=lab[4], "5"=lab[5], "6"=lab[6]))+
  xlab(expression(rho))+
  ylab("FDR")+
  scale_x_continuous(breaks = seq(0.2, 0.8, 0.2), limits=c(0, 0.9), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 0.14, 0.02), limits=c(0, 0.15), expand = c(0, 0)) +
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

ggsave("results/Plot_power_rhos.pdf",plot=p9, width=9, height=4)

#####Distribution of stops for rejection
load("results/Plot_FDR_standard.rda")


lab=c("Perm p-value", "Perm p-value (B=200)", "Aggressive", "Anytime-valid BC", "Binomial mixture") 
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



