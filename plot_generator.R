#Generates Figures 1,2,3,4,5,6,8 of the paper "Sequential Monte-Carlo testing by betting"

library(ggplot2)
library(patchwork)


#######################Plots power and number of permutations

#####Figure 2 (alpha=0.05)

load("results/power_alpha005.rda")
B=1000
mus=c(0.01,0.05,0.1,0.15,0.2,0.3,0.4,0.5)

lab=c("Permutation p-value", "Aggressive", "Binomial", "Besag-Clifford", "Binomial mixture") 
col=c( "cornflowerblue","red", "limegreen", "cornflowerblue", "orange")

results_df=data.frame(idx=mus, power_bin=power_bin, power_agg=power_agg, power_bc=power_bc,power_bm=power_bm,
                      nPerm=nPerm, nPerm_agg=nPerm_agg, nPerm_rej=nPerm_rej,
                      nPerm_stop=nPerm_stop, nPerm_agg_rej=nPerm_agg_rej, nPerm_agg_stop=nPerm_agg_stop,
                      nPerm_bc=nPerm_bc, nPerm_stop_bc=nPerm_stop_bc, nPerm_rej_bc=nPerm_rej_bc,
                      nPerm_bm=nPerm_bm, nPerm_bm_stop=nPerm_bm_stop, nPerm_bm_rej=nPerm_bm_rej)

p1=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = power_bin,colour = "3", linetype="3")) +
  geom_point(aes(y = power_bin,colour = "3",shape = "3")) +
  geom_line(aes(y = power_agg,colour = "2", linetype="3")) +
  geom_point(aes(y = power_agg,colour = "2",shape = "2")) +
  geom_line(aes(y = power_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = power_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = power_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = power_bm,colour = "5",shape = "5")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.1), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p2=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = nPerm,colour = "3", linetype="3")) +
  geom_point(aes(y = nPerm,colour = "3",shape = "3")) +
  geom_line(aes(y = nPerm_agg,colour = "2", linetype="3")) +
  geom_point(aes(y = nPerm_agg,colour = "2",shape = "2")) +
  geom_line(aes(y = nPerm_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = nPerm_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = nPerm_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = nPerm_bm,colour = "5",shape = "5")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,B,100), limits=c(0,(B+B/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p3=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = nPerm_rej,colour = "3", linetype="3")) +
  geom_point(aes(y = nPerm_rej,colour = "3",shape = "3")) +
  geom_line(aes(y = nPerm_agg_rej,colour = "2", linetype="3")) +
  geom_point(aes(y = nPerm_agg_rej,colour = "2",shape = "2")) +
  geom_line(aes(y = nPerm_rej_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = nPerm_rej_bc,colour = "4",shape = "4"))+ 
  geom_line(aes(y = nPerm_bm_rej,colour = "5", linetype="3")) +
  geom_point(aes(y = nPerm_bm_rej,colour = "5",shape = "5")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ylab("#permutations till rejection")+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,B,100), limits=c(0,(B+B/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15))

p4=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = nPerm_stop,colour = "3", linetype="3")) +
  geom_point(aes(y = nPerm_stop,colour = "3",shape = "3")) +
  geom_line(aes(y = nPerm_agg_stop,colour = "2", linetype="3")) +
  geom_point(aes(y = nPerm_agg_stop,colour = "2",shape = "2")) +
  geom_line(aes(y = nPerm_stop_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = nPerm_stop_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = nPerm_bm_stop,colour = "5", linetype="3")) +
  geom_point(aes(y = nPerm_bm_stop,colour = "5",shape = "5")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ylab("#permutations till futility stop")+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,B,100), limits=c(0,(B+B/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15))

combined <- p1 + p2 + p3 + p4 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("results/Plot_power_alpha005.pdf",plot=combined, width=12, height=7.5)

#####Figure 3 (alpha=0.01)

load("results/power_alpha001.rda")
B=1000

lab=c("Permutation p-value", "Aggressive", "Binomial", "Besag-Clifford", "Binomial mixture") 
col=c( "cornflowerblue","red", "limegreen", "cornflowerblue", "orange")

results_df=data.frame(idx=mus, power_bin=power_bin, power_agg=power_agg, power_bc=power_bc, power_bm=power_bm,
                      nPerm=nPerm, nPerm_agg=nPerm_agg, nPerm_rej=nPerm_rej,
                      nPerm_stop=nPerm_stop, nPerm_agg_rej=nPerm_agg_rej, nPerm_agg_stop=nPerm_agg_stop,
                      nPerm_bc=nPerm_bc, nPerm_stop_bc=nPerm_stop_bc, nPerm_rej_bc=nPerm_rej_bc,
                      nPerm_bm=nPerm_bm, nPerm_bm_stop=nPerm_bm_stop, nPerm_bm_rej=nPerm_bm_rej)

p1=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = power_bin,colour = "3", linetype="3")) +
  geom_point(aes(y = power_bin,colour = "3",shape = "3")) +
  geom_line(aes(y = power_agg,colour = "2", linetype="3")) +
  geom_point(aes(y = power_agg,colour = "2",shape = "2")) +
  geom_line(aes(y = power_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = power_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = power_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = power_bm,colour = "5",shape = "5")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.1), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p2=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = nPerm,colour = "3", linetype="3")) +
  geom_point(aes(y = nPerm,colour = "3",shape = "3")) +
  geom_line(aes(y = nPerm_agg,colour = "2", linetype="3")) +
  geom_point(aes(y = nPerm_agg,colour = "2",shape = "2")) +
  geom_line(aes(y = nPerm_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = nPerm_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = nPerm_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = nPerm_bm,colour = "5",shape = "5")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,B,100), limits=c(0,(B+B/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p3=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = nPerm_rej,colour = "3", linetype="3")) +
  geom_point(aes(y = nPerm_rej,colour = "3",shape = "3")) +
  geom_line(aes(y = nPerm_agg_rej,colour = "2", linetype="3")) +
  geom_point(aes(y = nPerm_agg_rej,colour = "2",shape = "2")) +
  geom_line(aes(y = nPerm_rej_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = nPerm_rej_bc,colour = "4",shape = "4"))+ 
  geom_line(aes(y = nPerm_bm_rej,colour = "5", linetype="3")) +
  geom_point(aes(y = nPerm_bm_rej,colour = "5",shape = "5")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ylab("#permutations till rejection")+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,B,100), limits=c(0,(B+B/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15))

p4=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = nPerm_stop,colour = "3", linetype="3")) +
  geom_point(aes(y = nPerm_stop,colour = "3",shape = "3")) +
  geom_line(aes(y = nPerm_agg_stop,colour = "2", linetype="3")) +
  geom_point(aes(y = nPerm_agg_stop,colour = "2",shape = "2")) +
  geom_line(aes(y = nPerm_stop_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = nPerm_stop_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = nPerm_bm_stop,colour = "5", linetype="3")) +
  geom_point(aes(y = nPerm_bm_stop,colour = "5",shape = "5")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ylab("#permutations till futility stop")+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,B,100), limits=c(0,(B+B/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15))

combined <- p1 + p2 + p3 + p4 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("results/Plot_power_alpha001.pdf",plot=combined, width=12, height=7.5)


#####Figure 4 (Comparison randomized vs. non-randomized binomial and binomial mixture strategy)

###For alpha=0.05

load("results/power_alpha005.rda")

mus=c(0.01,0.05,0.1,0.15,0.2,0.3,0.4,0.5)
alpha=0.05

##Binomial mixture strategy
lab=c("Binomial mixture", "Binomial mixture randomized") 
col=c("orange")

results_df=data.frame(idx=mus,power_bm=power_bm, power_bm_r=power_bm_r)

p1=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = power_bm_r,colour = "2", linetype="2")) +
  geom_point(aes(y = power_bm_r,colour = "2",shape = "2")) +
  geom_line(aes(y = power_bm,colour = "1", linetype="1")) +
  geom_point(aes(y = power_bm,colour = "1",shape = "1")) +
  scale_linetype_manual(name="Strategy", values = c("1"="solid","2"="dashed"),
                        labels=c("1"=lab[1], "2"=lab[2]))+
  scale_shape_manual(name="Strategy", values=c("1"=1, "2"=1), 
                     labels=c("1"=lab[1], "2"=lab[2]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[1]), 
                      labels=c("1"=lab[1], "2"=lab[2]))+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(bquote(alpha==.(alpha)))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.1), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

##Binomial strategy
lab=c( "Binomial", "Binomial randomized") 
col=c( "limegreen")

results_df_2=data.frame(idx=mus,power_bin=power_bin, power_bin_r=power_bin_r)

p2=ggplot(results_df_2, aes(idx)) + 
  geom_line(aes(y = power_bin_r,colour = "2", linetype="2")) +
  geom_point(aes(y = power_bin_r,colour = "2",shape = "2")) +
  geom_line(aes(y = power_bin,colour = "1", linetype="1")) +
  geom_point(aes(y = power_bin,colour = "1",shape = "1")) +
  scale_linetype_manual(name="Strategy", values = c("1"="solid","2"="dashed"),
                        labels=c("1"=lab[1], "2"=lab[2]))+
  scale_shape_manual(name="Strategy", values=c("1"=1, "2"=1), 
                     labels=c("1"=lab[1], "2"=lab[2]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[1]), 
                      labels=c("1"=lab[1], "2"=lab[2]))+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(bquote(alpha==.(alpha)))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.1), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15))


###For alpha=0.01

load("results/power_alpha001.rda")

alpha=0.01

##Binomial mixture strategy
lab=c("Binomial mixture", "Binomial mixture randomized") 
col=c("orange")

results_df=data.frame(idx=mus,power_bm=power_bm, power_bm_r=power_bm_r)

p3=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = power_bm_r,colour = "2", linetype="2")) +
  geom_point(aes(y = power_bm_r,colour = "2",shape = "2")) +
  geom_line(aes(y = power_bm,colour = "1", linetype="1")) +
  geom_point(aes(y = power_bm,colour = "1",shape = "1")) +
  scale_linetype_manual(name="Strategy", values = c("1"="solid","2"="dashed"),
                        labels=c("1"=lab[1], "2"=lab[2]))+
  scale_shape_manual(name="Strategy", values=c("1"=1, "2"=1), 
                     labels=c("1"=lab[1], "2"=lab[2]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[1]), 
                      labels=c("1"=lab[1], "2"=lab[2]))+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(bquote(alpha==.(alpha)))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.1), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

##Binomial strategy
lab=c( "Binomial", "Binomial randomized") 
col=c( "limegreen")

results_df_2=data.frame(idx=mus,power_bin=power_bin, power_bin_r=power_bin_r)

p4=ggplot(results_df_2, aes(idx)) + 
  geom_line(aes(y = power_bin_r,colour = "2", linetype="2")) +
  geom_point(aes(y = power_bin_r,colour = "2",shape = "2")) +
  geom_line(aes(y = power_bin,colour = "1", linetype="1")) +
  geom_point(aes(y = power_bin,colour = "1",shape = "1")) +
  scale_linetype_manual(name="Strategy", values = c("1"="solid","2"="dashed"),
                        labels=c("1"=lab[1], "2"=lab[2]))+
  scale_shape_manual(name="Strategy", values=c("1"=1, "2"=1), 
                     labels=c("1"=lab[1], "2"=lab[2]))+
  scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[1]), 
                      labels=c("1"=lab[1], "2"=lab[2]))+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(bquote(alpha==.(alpha)))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.1), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15))


combined_b <- p2 + p4 + plot_layout(guides = "collect")  & theme(legend.position = "bottom")
combined_bm <- p1 +  p3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("results/Plot_power_randomized_b.pdf",plot=combined_b, width=12, height=4.5)
ggsave("results/Plot_power_randomized_bm.pdf",plot=combined_bm, width=12, height=4.5)


#####Figure 5 (Besag-Clifford vs. randomized strategies)

###For alpha=0.05

load("results/power_alpha005.rda")
B=1000
alpha=0.05

lab=c("Permutation p-value", "Aggressive", "Binomial randomized", "Besag-Clifford", "Binomial mixture randomized") 
col=c( "cornflowerblue","red", "limegreen", "cornflowerblue", "orange")

results_df=data.frame(idx=mus,power_bc=power_bc, power_bin_r=power_bin_r, power_agg=power_agg, power_bm_r=power_bm_r)

p1=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = power_bin_r,colour = "3", linetype="3")) +
  geom_point(aes(y = power_bin_r,colour = "3",shape = "3")) +
  geom_line(aes(y = power_agg,colour = "2", linetype="3")) +
  geom_point(aes(y = power_agg,colour = "2",shape = "2")) +
  geom_line(aes(y = power_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = power_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = power_bm_r,colour = "5", linetype="3")) +
  geom_point(aes(y = power_bm_r,colour = "5",shape = "5")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(bquote(alpha==.(alpha)))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.1), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


###For alpha=0.01
load("results/power_alpha001.rda")
B=1000
alpha=0.01

results_df=data.frame(idx=mus,power_bc=power_bc, power_bin_r=power_bin_r, power_bm_r=power_bm_r, power_agg=power_agg)

p2=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = power_bin_r,colour = "3", linetype="3")) +
  geom_point(aes(y = power_bin_r,colour = "3",shape = "3")) +
  geom_line(aes(y = power_agg,colour = "2", linetype="3")) +
  geom_point(aes(y = power_agg,colour = "2",shape = "2")) +
  geom_line(aes(y = power_bc,colour = "4", linetype="3")) +
  geom_point(aes(y = power_bc,colour = "4",shape = "4")) +
  geom_line(aes(y = power_bm_r,colour = "5", linetype="3")) +
  geom_point(aes(y = power_bm_r,colour = "5",shape = "5")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(bquote(alpha==.(alpha)))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.1), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


combined <- p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("results/Plot_power_randomized.pdf",plot=combined, width=12, height=4.5)


#####Figure 6 (randomized binomial mixture vs. Besag-Clifford with small T)


load("results/power_alpha005.rda")

lab=c("Permutation p-value", "Aggressive", "Binomial", "Besag-Clifford", "Binomial mixture randomized") 
col=c( "cornflowerblue","red", "limegreen", "cornflowerblue", "orange")

results_df=data.frame(idx=mus, power_bm_r=power_bm_r, power_bc_smallB=power_bc_smallB, 
                      nPerm_bm=nPerm_bm, nPerm_bc_smallB=nPerm_bc_smallB)

p1=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = power_bm_r,colour = "5", linetype="3")) +
  geom_point(aes(y = power_bm_r,colour = "5",shape = "5")) +
  geom_line(aes(y = power_bc_smallB,colour = "4", linetype="3")) +
  geom_point(aes(y = power_bc_smallB,colour = "4",shape = "4")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(bquote(alpha==0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.1), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p2=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = nPerm_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = nPerm_bm,colour = "5",shape = "5")) +
  geom_line(aes(y = nPerm_bc_smallB,colour = "4", linetype="3")) +
  geom_point(aes(y = nPerm_bc_smallB,colour = "4",shape = "4")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ggtitle(bquote(alpha==0.05))+
  ylab("Total #permutations")+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,200,200/5), limits=c(0,(200+200/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

load("results/power_alpha001.rda")
results_df=data.frame(idx=mus, power_bm_r=power_bm_r, power_bc_smallB=power_bc_smallB, 
                      nPerm_bm=nPerm_bm, nPerm_bc_smallB=nPerm_bc_smallB)

p3=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = power_bm_r,colour = "5", linetype="3")) +
  geom_point(aes(y = power_bm_r,colour = "5",shape = "5")) +
  geom_line(aes(y = power_bc_smallB,colour = "4", linetype="3")) +
  geom_point(aes(y = power_bc_smallB,colour = "4",shape = "4")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ggtitle(bquote(alpha==0.01))+
  ylab("Power")+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.1), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p4=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = nPerm_bm,colour = "5", linetype="3")) +
  geom_point(aes(y = nPerm_bm,colour = "5",shape = "5")) +
  geom_line(aes(y = nPerm_bc_smallB,colour = "4", linetype="3")) +
  geom_point(aes(y = nPerm_bc_smallB,colour = "4",shape = "4")) +
  scale_linetype_manual(guide="none", values = c("3"="solid","4"="dashed","5"="dotted"))+
  scale_shape_manual(name="Strategy", values=c( "3"=6, "2"=0, "4"=8, "5"=1), 
                     labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  scale_colour_manual(name="Strategy", values=c( "3"=col[3], "2"=col[2], "4"=col[4], "5"=col[5]), 
                      labels=c("3"=lab[3], "2"=lab[2], "4"=lab[4], "5"=lab[5]))+
  xlab(bquote(mu))+
  ggtitle(bquote(alpha==0.01))+
  ylab("Total #permutations")+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,400,400/5), limits=c(0,(400+400/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

combined <- p1 + p2 + p3 + p4 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("results/Plot_power_sameT.pdf",plot=combined, width=12, height=7.5)

# load("results/power_sameT.rda")
# 
# 
# col=c("orange", "cornflowerblue")
# shapes=c(1, 8)
# 
# colnames(results_df)[colnames(results_df) == "labs"] = "Strategy"
# 
# p1=ggplot(results_df[(results_df$alphas==0.05 & results_df$c_factors==0.9 & 
#                         results_df$futility_stop_powers==1 & results_df$types=="Power"),], aes(x=mus, y=values)) + 
#   geom_line(aes(colour = Strategy)) +
#   geom_point(aes(colour = Strategy, shape = Strategy)) +
#   scale_colour_manual(values=col)+
#   scale_shape_manual(values=shapes)+
#   xlab(bquote(mu))+
#   ylab("Power")+
#   ggtitle(expression(alpha == 0.05))+
#   scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
#   scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
#   theme(panel.background = element_blank(),panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(colour = "black", fill=NA, size=1), 
#         axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
#         legend.text=element_text(size=15), legend.title=element_text(size=15)) 
# 
# 
# p2=ggplot(results_df[(results_df$alphas==0.05 & results_df$c_factors==0.9 & 
#                         results_df$futility_stop_powers==1 & results_df$types=="Total #permutations"),], 
#           aes(x=mus, y=values)) + 
#   geom_line(aes(colour = Strategy)) +
#   geom_point(aes(colour = Strategy, shape = Strategy)) +
#   scale_colour_manual(values=col)+
#   scale_shape_manual(values=shapes)+
#   xlab(bquote(mu))+
#   ylab("Total #permutations")+
#   ggtitle(expression(alpha == 0.05))+
#   scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
#   scale_y_continuous(breaks = seq(0,200,200/5), limits=c(0,(200+200/20)),expand = c(0, 0))+
#   theme(panel.background = element_blank(),panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(colour = "black", fill=NA, size=1), 
#         axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
#         legend.text=element_text(size=15), legend.title=element_text(size=15)) 
# 
# 
# p3=ggplot(results_df[(results_df$alphas==0.01 & results_df$c_factors==0.9 & 
#                         results_df$futility_stop_powers==1 & results_df$types=="Power"),], aes(x=mus, y=values)) + 
#   geom_line(aes(colour = Strategy)) +
#   geom_point(aes(colour = Strategy, shape = Strategy)) +
#   scale_colour_manual(values=col)+
#   scale_shape_manual(values=shapes)+
#   xlab(bquote(mu))+
#   ylab("Power")+
#   ggtitle(expression(alpha == 0.01))+
#   scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
#   scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
#   theme(panel.background = element_blank(),panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(colour = "black", fill=NA, size=1), 
#         axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
#         legend.text=element_text(size=15), legend.title=element_text(size=15)) 
# 
# 
# p4=ggplot(results_df[(results_df$alphas==0.01 & results_df$c_factors==0.9 & 
#                         results_df$futility_stop_powers==1 & results_df$types=="Total #permutations"),], 
#           aes(x=mus, y=values)) + 
#   geom_line(aes(colour = Strategy)) +
#   geom_point(aes(colour = Strategy, shape = Strategy)) +
#   scale_colour_manual(values=col)+
#   scale_shape_manual(values=shapes)+
#   xlab(bquote(mu))+
#   ylab("Total #permutations")+
#   ggtitle(expression(alpha == 0.01))+
#   scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
#   scale_y_continuous(breaks = seq(0,400,400/5), limits=c(0,(400+400/20)),expand = c(0, 0))+
#   theme(panel.background = element_blank(),panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(colour = "black", fill=NA, size=1), 
#         axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
#         legend.text=element_text(size=15), legend.title=element_text(size=15)) 
# 
# 
# combined = p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")
# 
# ggsave("results/plot_power_sameT.pdf", plot=combined, width=12, height=7.5)


#######################Plots with logarithmic p-values

#####Figure 1

#For alpha=0.05

load("results/pval_alpha005_mu01.rda")

font_size=15
mu=0.1
alpha=0.05
m=2000

lab=c("Permutation p-value", "Binomial mixture", "Binomial", "Besag Clifford", "Aggressive" )
col=c("cornflowerblue", "orange", "limegreen", "cornflowerblue", "red")


results_df=data.frame(idx=(1:m),p_perm=sort(log(p_perm)), p_bm=sort(log(p_bm)),
                      p_bin=sort(log(p_bin)), p_agg=sort(log(p_agg)))

p1=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = p_perm,colour = "1",linewidth = "1")) +
  geom_line(aes(y = p_bin,colour = "3",linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_hline(yintercept=log(alpha),linewidth=1, colour="darkgrey")+
  scale_linewidth_manual(name  ="Strategy",values=c("1"=1,"3"=1, "5"=1, "2"=1), 
                         labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2]))+
  scale_colour_manual(name="Strategy", values=c("1"=col[1],  "3"=col[3],"5"=col[5],"2"=col[2]), 
                      labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2]))+
  xlab("Index")+
  ylab("Log-transformed p-values")+
  scale_x_continuous(breaks = seq(100,1900,200), limits=c(0,2000),expand = c(0, 0))+
  scale_y_continuous(breaks = (-6:0), limits=c(-7,0),expand = c(0, 0))+
  ggtitle(bquote(mu==.(mu)))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=font_size), axis.title.x = element_text(size=font_size), title = element_text(size=font_size),
        legend.text=element_text(size=font_size), legend.title=element_text(size=font_size))


load("results/pval_alpha005_mu1.rda")

mu=1
alpha=0.05
m=2000

lab=c("Permutation p-value", "Binomial mixture", "Binomial", "Besag Clifford", "Aggressive" )
col=c("cornflowerblue", "orange", "limegreen", "cornflowerblue", "red")


results_df=data.frame(idx=(1:m),p_perm=sort(log(p_perm)), p_bm=sort(log(p_bm)),
                      p_bin=sort(log(p_bin)), p_agg=sort(log(p_agg)))

p2=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = p_perm,colour = "1",linewidth = "1")) +
  geom_line(aes(y = p_bin,colour = "3",linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_hline(yintercept=log(alpha),linewidth=1, colour="darkgrey")+
  scale_linewidth_manual(name  ="Strategy",values=c("1"=1,"3"=1, "5"=1, "2"=1), 
                         labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2]))+
  scale_colour_manual(name="Strategy", values=c("1"=col[1],  "3"=col[3],"5"=col[5],"2"=col[2]), 
                      labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2]))+
  xlab("Index")+
  ylab("Log-transformed p-values")+
  scale_x_continuous(breaks = seq(100,1900,200), limits=c(0,2000),expand = c(0, 0))+
  scale_y_continuous(breaks = (-6:0), limits=c(-7,0),expand = c(0, 0))+
  ggtitle(bquote(mu==.(mu)))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=font_size), axis.title.x = element_text(size=font_size),title = element_text(size=font_size),
        legend.text=element_text(size=font_size), legend.title=element_text(size=font_size))


load("results/pval_alpha005_mu001.rda")

mu=0.01
alpha=0.05
m=2000

lab=c("Permutation p-value", "Binomial mixture", "Binomial",  "Besag Clifford", "Aggressive" )
col=c("cornflowerblue", "orange", "limegreen",  "green", "red")


results_df=data.frame(idx=(1:m),p_perm=sort(log(p_perm)), p_bm=sort(log(p_bm)),
                      p_bin=sort(log(p_bin)), p_agg=sort(log(p_agg)))

p3=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = p_perm,colour = "1",linewidth = "1")) +
  geom_line(aes(y = p_bin,colour = "3",linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_hline(yintercept=log(alpha),linewidth=1, colour="darkgrey")+
  scale_linewidth_manual(name  ="Strategy",values=c("1"=1,"3"=1, "5"=1, "2"=1), 
                         labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2]))+
  scale_colour_manual(name="Strategy", values=c("1"=col[1],  "3"=col[3],"5"=col[5],"2"=col[2]), 
                      labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2]))+
  xlab("Index")+
  ylab("Log-transformed p-values")+
  scale_x_continuous(breaks = seq(100,1900,200), limits=c(0,2000),expand = c(0, 0))+
  scale_y_continuous(breaks = (-6:0), limits=c(-7,0),expand = c(0, 0))+
  ggtitle(bquote(mu==.(mu)))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=font_size), axis.title.x = element_text(size=font_size),title = element_text(size=font_size),
        legend.text=element_text(size=font_size), legend.title=element_text(size=font_size))



combined <-  p3 + p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave("results/Plot_logp_alpha005.pdf", plot=combined, width=15, height=5 )


#For alpha=0.01

load("results/pval_alpha001_mu01.rda")

font_size=15
mu=0.1
alpha=0.01
m=2000

lab=c("Permutation p-value", "Binomial mixture", "Binomial", "Besag Clifford", "Aggressive" )
col=c("cornflowerblue", "orange", "limegreen", "cornflowerblue", "red")


results_df=data.frame(idx=(1:m),p_perm=sort(log(p_perm)), p_bm=sort(log(p_bm)),
                      p_bin=sort(log(p_bin)), p_agg=sort(log(p_agg)))

p1=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = p_perm,colour = "1",linewidth = "1")) +
  geom_line(aes(y = p_bin,colour = "3",linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_hline(yintercept=log(alpha),linewidth=1, colour="darkgrey")+
  scale_linewidth_manual(name  ="Strategy",values=c("1"=1,"3"=1, "5"=1, "2"=1), 
                         labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2]))+
  scale_colour_manual(name="Strategy", values=c("1"=col[1],  "3"=col[3],"5"=col[5],"2"=col[2]), 
                      labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2]))+
  xlab("Index")+
  ylab("Log-transformed p-values")+
  scale_x_continuous(breaks = seq(100,1900,200), limits=c(0,2000),expand = c(0, 0))+
  scale_y_continuous(breaks = (-6:0), limits=c(-7,0),expand = c(0, 0))+
  ggtitle(bquote(mu==.(mu)))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=font_size), axis.title.x = element_text(size=font_size), title = element_text(size=font_size),
        legend.text=element_text(size=font_size), legend.title=element_text(size=font_size))


load("results/pval_alpha001_mu1.rda")

mu=1
alpha=0.01
m=2000

lab=c("Permutation p-value", "Binomial mixture", "Binomial", "Besag Clifford", "Aggressive" )
col=c("cornflowerblue", "orange", "limegreen", "cornflowerblue", "red")


results_df=data.frame(idx=(1:m),p_perm=sort(log(p_perm)), p_bm=sort(log(p_bm)),
                      p_bin=sort(log(p_bin)), p_agg=sort(log(p_agg)))

p2=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = p_perm,colour = "1",linewidth = "1")) +
  geom_line(aes(y = p_bin,colour = "3",linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_hline(yintercept=log(alpha),linewidth=1, colour="darkgrey")+
  scale_linewidth_manual(name  ="Strategy",values=c("1"=1,"3"=1, "5"=1, "2"=1), 
                         labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2]))+
  scale_colour_manual(name="Strategy", values=c("1"=col[1],  "3"=col[3],"5"=col[5],"2"=col[2]), 
                      labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2]))+
  xlab("Index")+
  ylab("Log-transformed p-values")+
  scale_x_continuous(breaks = seq(100,1900,200), limits=c(0,2000),expand = c(0, 0))+
  scale_y_continuous(breaks = (-6:0), limits=c(-7,0),expand = c(0, 0))+
  ggtitle(bquote(mu==.(mu)))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=font_size), axis.title.x = element_text(size=font_size),title = element_text(size=font_size),
        legend.text=element_text(size=font_size), legend.title=element_text(size=font_size))


load("results/pval_alpha001_mu001.rda")

mu=0.01
alpha=0.01
m=2000

lab=c("Permutation p-value", "Binomial mixture", "Binomial",  "Besag Clifford", "Aggressive" )
col=c("cornflowerblue", "orange", "limegreen",  "green", "red")


results_df=data.frame(idx=(1:m),p_perm=sort(log(p_perm)), p_bm=sort(log(p_bm)),
                      p_bin=sort(log(p_bin)), p_agg=sort(log(p_agg)))

p3=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = p_perm,colour = "1",linewidth = "1")) +
  geom_line(aes(y = p_bin,colour = "3",linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_hline(yintercept=log(alpha),linewidth=1, colour="darkgrey")+
  scale_linewidth_manual(name  ="Strategy",values=c("1"=1,"3"=1, "5"=1, "2"=1), 
                         labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2]))+
  scale_colour_manual(name="Strategy", values=c("1"=col[1],  "3"=col[3],"5"=col[5],"2"=col[2]), 
                      labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2]))+
  xlab("Index")+
  ylab("Log-transformed p-values")+
  scale_x_continuous(breaks = seq(100,1900,200), limits=c(0,2000),expand = c(0, 0))+
  scale_y_continuous(breaks = (-6:0), limits=c(-7,0),expand = c(0, 0))+
  ggtitle(bquote(mu==.(mu)))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=font_size), axis.title.x = element_text(size=font_size),title = element_text(size=font_size),
        legend.text=element_text(size=font_size), legend.title=element_text(size=font_size))



combined <-  p3 + p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave("results/Plot_logp_alpha001.pdf", plot=combined, width=15, height=5 )

#####Figure 8 (comparison to beta prior)

load("results/pval_alpha005_mu01.rda")

b1=50
b2=100
b3=200
alpha=0.05

lab=c("Permutation p-value", "Binomial mixture (uniform prior)", "Binomial",  "Besag Clifford", "Aggressive", "Binomial mixture (beta prior)")
col=c("cornflowerblue", "orange", "limegreen", "cornflowerblue", "red", "purple")


results_df=data.frame(idx=(1:m),p_perm=sort(log(p_perm)), p_bm=sort(log(p_bm)), 
                      p_bin=sort(log(p_bin)), p_agg=sort(log(p_agg)), p_bm_beta=sort(log(p_bm_beta_b1)))

p1=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = p_perm,colour = "1",linewidth = "1")) +
  geom_line(aes(y = p_bin,colour = "3",linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_line(aes(y = p_bm_beta, colour = "6", linewidth = "6")) +
  geom_hline(yintercept=log(alpha),linewidth=1, colour="darkgrey")+
  scale_linewidth_manual(name  ="Strategy",values=c("1"=1,"3"=1, "5"=1, "2"=1, "6"=1), 
                         labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2],"6"=lab[6]))+
  scale_colour_manual(name="Strategy", values=c("1"=col[1],  "3"=col[3],"5"=col[5],"2"=col[2],"6"=col[6]), 
                      labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2],"6"=lab[6]))+
  xlab("Index")+
  ylab("Log-transformed p-values")+
  scale_x_continuous(breaks = seq(100,1900,200), limits=c(0,2000),expand = c(0, 0))+
  scale_y_continuous(breaks = (-6:0), limits=c(-7,0),expand = c(0, 0))+
  ggtitle(bquote(b==.(b1)))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


results_df=data.frame(idx=(1:m),p_perm=sort(log(p_perm)), p_bm=sort(log(p_bm)), 
                      p_bin=sort(log(p_bin)), p_agg=sort(log(p_agg)), p_bm_beta=sort(log(p_bm_beta_b2)))

p2=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = p_perm,colour = "1",linewidth = "1")) +
  geom_line(aes(y = p_bin,colour = "3",linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_line(aes(y = p_bm_beta, colour = "6", linewidth = "6")) +
  geom_hline(yintercept=log(alpha),linewidth=1, colour="darkgrey")+
  scale_linewidth_manual(name  ="Strategy",values=c("1"=1,"3"=1, "5"=1, "2"=1, "6"=1), 
                         labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2],"6"=lab[6]))+
  scale_colour_manual(name="Strategy", values=c("1"=col[1],  "3"=col[3],"5"=col[5],"2"=col[2],"6"=col[6]), 
                      labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2],"6"=lab[6]))+
  xlab("Index")+
  ylab("Log-transformed p-values")+
  scale_x_continuous(breaks = seq(100,1900,200), limits=c(0,2000),expand = c(0, 0))+
  scale_y_continuous(breaks = (-6:0), limits=c(-7,0),expand = c(0, 0))+
  ggtitle(bquote(b==.(b2)))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


results_df=data.frame(idx=(1:m),p_perm=sort(log(p_perm)), p_bm=sort(log(p_bm)), 
                      p_bin=sort(log(p_bin)), p_agg=sort(log(p_agg)), p_bm_beta=sort(log(p_bm_beta_b3)))

p3=ggplot(results_df, aes(idx)) + 
  geom_line(aes(y = p_perm,colour = "1",linewidth = "1")) +
  geom_line(aes(y = p_bin,colour = "3",linewidth = "3")) +
  geom_line(aes(y = p_agg, colour = "5", linewidth = "5")) +
  geom_line(aes(y = p_bm, colour = "2", linewidth = "2")) +
  geom_line(aes(y = p_bm_beta, colour = "6", linewidth = "6")) +
  geom_hline(yintercept=log(alpha),linewidth=1, colour="darkgrey")+
  scale_linewidth_manual(name  ="Strategy",values=c("1"=1,"3"=1, "5"=1, "2"=1, "6"=1), 
                         labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2],"6"=lab[6]))+
  scale_colour_manual(name="Strategy", values=c("1"=col[1],  "3"=col[3],"5"=col[5],"2"=col[2],"6"=col[6]), 
                      labels=c("1"=lab[1], "3"=lab[3],"5"=lab[5],"2"=lab[2],"6"=lab[6]))+
  xlab("Index")+
  ylab("Log-transformed p-values")+
  scale_x_continuous(breaks = seq(100,1900,200), limits=c(0,2000),expand = c(0, 0))+
  scale_y_continuous(breaks = (-6:0), limits=c(-7,0),expand = c(0, 0))+
  ggtitle(bquote(b==.(b3)))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

combined <-  p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave("results/Plot_alpha_005_beta.pdf", plot=combined, width=15, height=5)



