#Reproduces the results of Section 6.4 in the paper "Sequential Monte-Carlo testing by betting"
#In particular, generates plots for Figure 6.

rm(list=ls())
library(ggplot2)
library(patchwork)

set.seed(12345)

#Treatment vs. Control trial with binary outcomes. 18 successes among 32 treated
#observations and 5 successes among 21 control observations. The same data was
#used by Rosenbaum (2002) and Ding (2017); see below for complete reference.

treatment=c(rep(1,32),rep(0,21))
X=c(rep(1,18), rep(0,14), rep(1,5), rep(0,16))


test_stat=mean(X[treatment==1])-mean(X[treatment==0])     #Observed test statistic
B=5000                    #Number of permutations
m=1000                    #Number of runs

p_perm=rep(0,m)           #permutation p-values
p_bin=rep(0,m)            #p-values binomial strategy
p_bm=rep(0,m)             #p-values binomial mixture strategy
idx_dec=rep(B,m)          #Indices decisions binomial strategy 
dec_bin=rep(0,m)          #Decisions binomial strategy 
idx_dec_bm=rep(B,m)       #Indices decisions binomial mixture strategy      
dec_bm=rep(0,m)           #Decisions binomial mixture strategy 
dec_perm=rep(0,m)         #Decisions permutation p-value

myplots=vector('list', m) #Plots of wealth processes 

alpha=0.05                #Significance level
c=0.95*alpha              #Parameter for binomial mixture strategy
p_zero=1/ceiling(sqrt(2*pi*exp(1/6))/alpha)       #Parameter for binomial strategy


for(k in 1:m){

test_stat_perm=rep(0,B) #Permuted test statistics  

bet_bin=c()
rank=1                  #Rank of the observed test statistic
wealth_bin=c(1)
wealth_bm=c()

for(b in (1:B)){
  X_perm=sample(X)
  
  test_stat_perm[b]=mean(X_perm[treatment==1])-mean(X_perm[treatment==0])
  
  #Calculation of wealth
  
  if(test_stat_perm[b]>=test_stat){
    bet_bin[b]=p_zero*(b+1)/rank
    rank=rank+1
  }else{
    bet_bin[b]=(1-p_zero)*(b+1)/(b-rank+1)
  }
  
  wealth_bin=cumprod(bet_bin)
  if((max(wealth_bin)>=(1/alpha))&idx_dec[k]==B){
    idx_dec[k]=b
  }
  
  wealth_bm[b]=(1-pbinom(rank-1,b+1,c))/c
  if(max(wealth_bm)>=(1/alpha)&idx_dec_bm[k]==B){
    idx_dec_bm[k]=b
  }
  
}

#Calculating p-values and decisions

p_perm[k]=(1+sum(test_stat_perm[1:B]>=test_stat))/(B+1)
dec_perm[k]=ifelse(p_perm[k]<=alpha,1,0)

p_bin[k]=1/max(wealth_bin)
dec_bin[k]=ifelse(p_bin[k]<=alpha,1,0)


p_bm[k]=1/max(wealth_bm)
dec_bm[k]=ifelse(p_bm[k]<=alpha,1,0)

###Produce plot of wealth

lab=c( "Binomial", "Binomial mixture" )
col=c( "limegreen", "orange")
results_df=data.frame(idx=(1:5000), wealth_p=wealth_bin, wealth_bm=wealth_bm)

myplots[[k]] <- local({
  ggplot(results_df, aes(idx))+
    geom_line(aes(y = wealth_p,colour = "1", linetype="1")) +
    geom_line(aes(y = wealth_bm,colour = "2", linetype="1")) +
    geom_hline(yintercept=1/alpha,linewidth=1, colour="darkgrey")+
    scale_linetype_manual(guide="none", values = c("1"="solid","2"="dashed","3"="dotted"))+
    scale_colour_manual(name="Strategy", values=c( "1"=col[1], "2"=col[2]), 
                        labels=c("1"=lab[1], "2"=lab[2]))+
    xlab("Step")+
    ylab("Wealth")+
    scale_x_continuous(breaks = seq(50,450,50), limits=c(0,500),expand = c(0, 0))+
    scale_y_continuous(breaks = seq(0,60,10), limits=c(0,60),expand = c(0, 0))+
    theme(panel.background = element_blank(),panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1), 
          axis.title.y = element_text(size=20), axis.title.x = element_text(size=20),
          legend.text=element_text(size=20), legend.title=element_text(size=20))
})

}

#Calculation power and number of permutations

power_p=mean(dec_perm)

power_p_bin=mean(dec_bin>0)
mean(idx_dec)
median(idx_dec)

power_p_bm=mean(dec_bm>0)
mean(idx_dec_bm)
median(idx_dec_bm)

#Choose specific plots with different wealth behavior
p1=(myplots[[m]]+myplots[[5]] & theme(legend.position = "bottom"))+plot_layout(guides="collect")
p2=(myplots[[m-2]]+myplots[[m-4]] & theme(legend.position = "bottom"))+plot_layout(guides="collect")

###Generates plots for Figure 6 of the paper.
ggsave("results/Plot_wealth_upper.pdf",plot=p1, width=12, height=4.5)
ggsave("results/Plot_wealth_lower.pdf",plot=p2, width=12, height=4.5)


###References

#P. R. Rosenbaum. Observational Studies. Springer, 2002.
#P. Ding. A paradox from randomization-based causal inference. Statistical Science, pages 331-345, 2017.

