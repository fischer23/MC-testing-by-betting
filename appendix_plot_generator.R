#Generates Figures 8-14 of the paper "Sequential Monte-Carlo testing by betting"

rm(list=ls())

library(ggplot2)
library(patchwork)

load("results/appendix/power_appendix.rda")

col=c("orange", "cornflowerblue")
shapes=c(1, 8)

colnames(results_df)[colnames(results_df) == "labs"] = "Strategy"

###Plot for n=100

p1=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==100 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p2=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==100 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

p3=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==100 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p4=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==100 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


combined = p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("results/appendix/plot_n100.pdf", plot=combined, width=12, height=7.5)


###Plot for n=200

p1=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==200 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p2=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==200 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

p3=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==200 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p4=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==200 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


combined = p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("results/appendix/plot_n200.pdf", plot=combined, width=12, height=7.5)

###Plot for n=500

p1=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==500 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p2=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==500 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

p3=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==500 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p4=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==500 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


combined = p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("results/appendix/plot_n500.pdf", plot=combined, width=12, height=7.5)

###Plot for n=2000

p1=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==2000 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p2=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==2000 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

p3=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==2000 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p4=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==2000 & results_df$c_factors==0.9 & 
                        results_df$dists=="normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


combined = p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("results/appendix/plot_n2000.pdf", plot=combined, width=12, height=7.5)

###Plot for log-normal distribution

p1=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==1000 & results_df$c_factors==0.9 & 
                        results_df$dists=="log-normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p2=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==1000 & results_df$c_factors==0.9 & 
                        results_df$dists=="log-normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

p3=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==1000 & results_df$c_factors==0.9 & 
                        results_df$dists=="log-normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p4=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==1000 & results_df$c_factors==0.9 & 
                        results_df$dists=="log-normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


combined = p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("results/appendix/plot_log-normal.pdf", plot=combined, width=12, height=7.5)


###Plot for c=0.8*alpha

p1=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==1000 & results_df$c_factors==0.8 & 
                        results_df$dists=="normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p2=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==1000 & results_df$c_factors==0.8 & 
                        results_df$dists=="normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

p3=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==1000 & results_df$c_factors==0.8 & 
                        results_df$dists=="normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p4=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==1000 & results_df$c_factors==0.8 & 
                        results_df$dists=="normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


combined = p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("results/appendix/plot_c0.8.pdf", plot=combined, width=12, height=7.5)

###Plot for c=0.99*alpha

p1=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==1000 & results_df$c_factors==0.99 & 
                        results_df$dists=="normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p2=ggplot(results_df[(results_df$alphas==0.05 & results_df$ns==1000 & results_df$c_factors==0.99 & 
                        results_df$dists=="normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.05))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 

p3=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==1000 & results_df$c_factors==0.99 & 
                        results_df$dists=="normal" & results_df$types=="Power"),], aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Power")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1,0.2), limits=c(0,1.05),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


p4=ggplot(results_df[(results_df$alphas==0.01 & results_df$ns==1000 & results_df$c_factors==0.99 & 
                        results_df$dists=="normal" & results_df$types=="Total #permutations"),], 
          aes(x=mus, y=values)) + 
  geom_line(aes(colour = Strategy)) +
  geom_point(aes(colour = Strategy, shape = Strategy)) +
  scale_colour_manual(values=col)+
  scale_shape_manual(values=shapes)+
  xlab(bquote(mu))+
  ylab("Total #permutations")+
  ggtitle(expression(alpha == 0.01))+
  scale_x_continuous(breaks = seq(0.05,0.45,0.05), limits=c(0,0.5),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0,1000,1000/5), limits=c(0,(1000+1000/20)),expand = c(0, 0))+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        axis.title.y = element_text(size=15), axis.title.x = element_text(size=15),
        legend.text=element_text(size=15), legend.title=element_text(size=15)) 


combined = p1 + p2 + p3 + p4 + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")

ggsave("results/appendix/plot_c0.99.pdf", plot=combined, width=12, height=7.5)
