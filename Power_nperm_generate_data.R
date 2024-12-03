#R Code for generating the data of the Figures 2,3,4,5,6 in the paper "Sequential Monte-Carlo testing by betting"

rm(list=ls())
library(ggplot2)
library(patchwork)

#Set seed for reproducibility
set.seed(123)

#Set parameters
n=1000                                      #Number of obs. per trial
m=2000                                      #Number of simulated trials
B=1000                                      #Number of perm. per trial
smallB=60                                   #Number of perm. per trial for Besag-Clifford with small B
mus=c(0.01,0.05,0.1,0.15,0.2,0.3,0.4,0.5)   #Strength of the alternative
prop_treated=0.5                            #Probability of obs. being treated
alpha=0.05                                  #Individual significance level
c=alpha*0.90                                #Parameter for binomial mixture strategy
p_zero=1/ceiling(sqrt(2*pi*exp(1/6))/alpha) #Parameter for binomial strategy
h_bc=alpha*B                                #Parameter for Besag Clifford
h_bc_smallB=alpha*smallB                    #Parameter for Besag Clifford with small B



#Power vectors
power_bin=rep(0,length(mus))        #Binomial strategy
power_bin_r=rep(0,length(mus))      #Randomized binomial strategy
power_agg=rep(0,length(mus))        #Aggressive strategy
power_bc=rep(0,length(mus))         #Besag-Clifford strategy
power_bc_smallB=rep(0,length(mus))  #Besag-Clifford strategy with smallB
power_cal=rep(0,length(mus))        #Calibrated strategy 1
power_cal_2=rep(0,length(mus))      #Calibrated strategy 2
power_bm=rep(0,length(mus))         #Binomial mixture strategy
power_bm_r=rep(0,length(mus))       #Randomized binomial mixture strategy

#Number permutation vectors
nPerm_bc=rep(0,length(mus))
nPerm_rej_bc=rep(0,length(mus))
nPerm_stop_bc=rep(0,length(mus))

nPerm_bc_smallB=rep(0,length(mus))
nPerm_rej_bc_smallB=rep(0,length(mus))
nPerm_stop_bc_smallB=rep(0,length(mus))

nPerm_cal=rep(0,length(mus))
nPerm_rej_cal=rep(0,length(mus))
nPerm_stop_cal=rep(0,length(mus))

nPerm_cal_2=rep(0,length(mus))
nPerm_rej_cal_2=rep(0,length(mus))
nPerm_stop_cal_2=rep(0,length(mus))

nPerm=rep(0,length(mus))
nPerm_rej=rep(0,length(mus))
nPerm_stop=rep(0,length(mus))

nPerm_agg=rep(0,length(mus))
nPerm_agg_rej=rep(0,length(mus))
nPerm_agg_stop=rep(0,length(mus))

nPerm_bm=rep(0,length(mus))
nPerm_bm_rej=rep(0,length(mus))
nPerm_bm_stop=rep(0,length(mus))

counter=1
for(mu in mus){


#Initialize decision and index when decision was obtained 

idx_dec_bc=rep(B,m)
dec_bc=rep(0,m)

idx_dec_bc_smallB=rep(smallB,m)
dec_bc_smallB=rep(0,m)

idx_dec_cal=rep(B,m)
dec_cal=rep(0,m)

idx_dec_cal_2=rep(B,m)
dec_cal_2=rep(0,m)

idx_dec=rep(B,m)
dec_bin=rep(0,m)
dec_bin_r=rep(0,m)

idx_dec_agg=rep(B,m)
dec_agg=rep(0,m)

idx_dec_bm=rep(B,m)
dec_bm=rep(0,m)
dec_bm_r=rep(0,m)

for(j in 1:m){
  
X=rnorm(n)

treated=(runif(n)>=prop_treated)
X=X+mu*treated  
  

test_stat=mean(X[treated])-mean(X[!treated])    #Observed test statistic

test_stat_perm=c()                              #Permuted test statistics

#Betting vectors
bet_bin=c()
bet_agg=c()
bet_cal=c()
bet_cal_2=c()

rank=1          #Rank of the observed test statistic

#Wealth vectors
wealth_bin=c(1)
wealth_agg=c(1)
wealth_cal=c(1)
wealth_cal_2=c(1)
wealth_bm=c()
bc_count=1
bc_count_smallB=1

for(i in 1:B){
  X_perm=sample(X)                                                #Permute data
  test_stat_perm[i]=mean(X_perm[treated])-mean(X_perm[!treated])  #Calculate permuted test statistic

  #We do not check for ties as X is continuously distributed and number of observations is large
  
  if(test_stat_perm[i]>=test_stat){
    if((wealth_bin[length(wealth_bin)]*p_zero*(i+1)/rank)<=alpha){
      bet_bin[i]=0
    }else{
      bet_bin[i]=p_zero*(i+1)/rank
    }
    bet_agg[i]=0
    rank=rank+1
  }else{
    if((wealth_bin[length(wealth_bin)]*p_zero*(i+1)/rank)<=alpha){
      bet_bin[i]=(i+1)/(i-rank+1) 
    }else{
      bet_bin[i]=(1-p_zero)*(i+1)/(i-rank+1)
    }
    bet_agg[i]=(i+1)/i
  }

  #Calculate wealth for each strategy
  wealth_bin=cumprod(bet_bin)
  if((min(wealth_bin)<=alpha | max(wealth_bin)>(1/alpha))&idx_dec[j]==B){
    idx_dec[j]=i
  }
  
  wealth_agg=cumprod(bet_agg)
  if((min(wealth_agg)<=alpha | max(wealth_agg)>(1/alpha))&idx_dec_agg[j]==B){
    idx_dec_agg[j]=i
  }
  
  if((rank-1)==h_bc & bc_count==1 & i<B){
    dec_bc[j]=-1
    idx_dec_bc[j]=i
    bc_count=0
  }else if(i==B & bc_count==1){
    idx_dec_bc[j]=i
    dec_bc[j]=1
  } 
  
  if((rank-1)==h_bc_smallB & bc_count_smallB==1 & i<smallB){
    dec_bc_smallB[j]=-1
    idx_dec_bc_smallB[j]=i
    bc_count_smallB=0
  }else if(i==smallB & bc_count_smallB==1 & bc_count_smallB==1){
    idx_dec_bc_smallB[j]=i
    dec_bc_smallB[j]=1
    bc_count_smallB=0
  } 
  
  wealth_bm[i]=(1-pbinom(rank-1,i+1,c))/c
  if(((min(wealth_bm)<=alpha & i>1) | max(wealth_bm)>(1/alpha))&idx_dec_bm[j]==B){
    idx_dec_bm[j]=i
  }
  
  test_stat_comp=c(test_stat,test_stat_perm[1:(i-1)])  
  bet_cal[i]=(i+1)/(sum(1/(1:(i+1)))*(sum(test_stat_perm[i]>=test_stat_comp)+1))
  wealth_cal=cumprod(bet_cal)
  if((min(wealth_cal)<=alpha | max(wealth_cal)>(1/alpha))&idx_dec_cal[j]==B){
    idx_dec_cal[j]=i
  }
  
  bet_cal_2[i]=(i+1)/(sum(1/sqrt((1:(i+1))))*(sqrt(sum(test_stat_perm[i]>=test_stat_comp)+1)))
  wealth_cal_2=cumprod(bet_cal_2)
  if((min(wealth_cal_2)<=alpha | max(wealth_cal_2)>(1/alpha))&idx_dec_cal_2[j]==B){
    idx_dec_cal_2[j]=i
  }
}

#dec=1 -> rejected, dec=-1 -> stopped for futility, dec=0 -> accepted but did not stop earlier
if(wealth_bin[idx_dec[j]]>=(1/alpha)){
  dec_bin[j]=1
}else if(wealth_bin[idx_dec[j]]<alpha){
  dec_bin[j]=-1
}else{
  dec_bin[j]=0 
}


if(wealth_bin[idx_dec[j]]>=(runif(1)/alpha)){
  dec_bin_r[j]=1
}else if(wealth_bin[idx_dec[j]]<alpha){
  dec_bin_r[j]=-1
}else{
  dec_bin_r[j]=0 
}


if(wealth_agg[idx_dec_agg[j]]>=(1/alpha)){
  dec_agg[j]=1
}else if(wealth_agg[idx_dec_agg[j]]<alpha){
  dec_agg[j]=-1
}else{
  dec_agg[j]=0 
}


if(wealth_bm[idx_dec_bm[j]]>=(1/alpha)){
  dec_bm[j]=1
}else if(wealth_bm[idx_dec_bm[j]]<alpha){
  dec_bm[j]=-1
}else{
  dec_bm[j]=0 
}


if(wealth_bm[idx_dec_bm[j]]>=(runif(1)/alpha)){
  dec_bm_r[j]=1
}else if(wealth_bm[idx_dec_bm[j]]<alpha){
  dec_bm_r[j]=-1
}else{
  dec_bm_r[j]=0 
}


if(wealth_cal[idx_dec_cal[j]]>=(1/alpha)){
  dec_cal[j]=1
}else if(wealth_cal[idx_dec_cal[j]]<alpha){
  dec_cal[j]=-1
}else{
  dec_cal[j]=0 
}


if(wealth_cal_2[idx_dec_cal_2[j]]>=(1/alpha)){
  dec_cal_2[j]=1
}else if(wealth_cal_2[idx_dec_cal_2[j]]<alpha){
  dec_cal_2[j]=-1
}else{
  dec_cal_2[j]=0 
}

}

power_bin[counter]=mean((dec_bin>0))
power_bin_r[counter]=mean((dec_bin_r>0))
power_bc[counter]=mean((dec_bc>0))
power_bc_smallB[counter]=mean((dec_bc_smallB>0))
power_agg[counter]=mean((dec_agg>0))
power_bm[counter]=mean((dec_bm>0))
power_bm_r[counter]=mean((dec_bm_r>0))
power_cal[counter]=mean((dec_cal>0))
power_cal_2[counter]=mean((dec_cal_2>0))

nPerm[counter]=mean(idx_dec)
nPerm_rej[counter]=mean(idx_dec[which(dec_bin==1)])
nPerm_stop[counter]=mean(idx_dec[which(dec_bin==-1)])

nPerm_bc[counter]=mean(idx_dec_bc)
nPerm_rej_bc[counter]=mean(idx_dec_bc[which(dec_bc==1)])
nPerm_stop_bc[counter]=mean(idx_dec_bc[which(dec_bc==-1)])

nPerm_bc_smallB[counter]=mean(idx_dec_bc_smallB)
nPerm_rej_bc_smallB[counter]=mean(idx_dec_bc_smallB[which(dec_bc_smallB==1)])
nPerm_stop_bc_smallB[counter]=mean(idx_dec_bc_smallB[which(dec_bc_smallB==-1)])

nPerm_agg[counter]=mean(idx_dec_agg)
nPerm_agg_rej[counter]=mean(idx_dec_agg[which(dec_agg==1)])
nPerm_agg_stop[counter]=mean(idx_dec_agg[which(dec_agg==-1)])

nPerm_bm[counter]=mean(idx_dec_bm)
nPerm_bm_rej[counter]=mean(idx_dec_bm[which(dec_bm==1)])
nPerm_bm_stop[counter]=mean(idx_dec_bm[which(dec_bm==-1)])

nPerm_cal[counter]=mean(idx_dec_cal)
nPerm_rej_cal[counter]=mean(idx_dec_cal[which(dec_cal==1)])
nPerm_stop_cal[counter]=mean(idx_dec_cal[which(dec_cal==-1)])

nPerm_cal_2[counter]=mean(idx_dec_cal_2)
nPerm_rej_cal_2[counter]=mean(idx_dec_cal_2[which(dec_cal_2==1)])
nPerm_stop_cal_2[counter]=mean(idx_dec_cal_2[which(dec_cal_2==-1)])


counter=counter+1
}

save(power_bin, power_bin_r,power_bc, power_agg, power_bm,power_bm_r,power_cal, power_cal_2, power_bc_smallB,
     nPerm,nPerm_rej, nPerm_stop, nPerm_bc, nPerm_rej_bc, nPerm_stop_bc,
     nPerm_agg, nPerm_agg_rej, nPerm_agg_stop, nPerm_bm, nPerm_bm_rej, nPerm_bm_stop,
     nPerm_cal, nPerm_rej_cal, nPerm_stop_cal, nPerm_cal_2, nPerm_rej_cal_2, nPerm_stop_cal_2, 
     nPerm_bc_smallB, nPerm_rej_bc_smallB, nPerm_stop_bc_smallB,
     file = "results/power_alpha005.rda")



#########Same simulations for alpha=0.01

#Set parameters
n=1000                                      #Number of obs. per trial
m=2000                                      #Number of simulated trials
B=1000                                      #Number of perm. per trial
smallB=300                                  #Number of perm. per trial for Besag-Clifford with small B
mus=c(0.01,0.05,0.1,0.15,0.2,0.3,0.4,0.5)   #Strength of the alternative
prop_treated=0.5                            #Probability of obs. being treated
alpha=0.01                                  #Individual significance level
c=alpha*0.90                                #Parameter for binomial mixture strategy
p_zero=1/ceiling(sqrt(2*pi*exp(1/6))/alpha) #Parameter for binomial strategy
h_bc=alpha*B                                #Parameter for Besag Clifford
h_bc_smallB=alpha*smallB                    #Parameter for Besag Clifford with small B



#Power vectors
power_bin=rep(0,length(mus))        #Binomial strategy
power_bin_r=rep(0,length(mus))      #Randomized binomial strategy
power_agg=rep(0,length(mus))        #Aggressive strategy
power_bc=rep(0,length(mus))         #Besag-Clifford strategy
power_bc_smallB=rep(0,length(mus))  #Besag-Clifford strategy with smallB
power_cal=rep(0,length(mus))        #Calibrated strategy 1
power_cal_2=rep(0,length(mus))      #Calibrated strategy 2
power_bm=rep(0,length(mus))         #Binomial mixture strategy
power_bm_r=rep(0,length(mus))       #Randomized binomial mixture strategy

#Number permutation vectors
nPerm_bc=rep(0,length(mus))
nPerm_rej_bc=rep(0,length(mus))
nPerm_stop_bc=rep(0,length(mus))

nPerm_bc_smallB=rep(0,length(mus))
nPerm_rej_bc_smallB=rep(0,length(mus))
nPerm_stop_bc_smallB=rep(0,length(mus))

nPerm_cal=rep(0,length(mus))
nPerm_rej_cal=rep(0,length(mus))
nPerm_stop_cal=rep(0,length(mus))

nPerm_cal_2=rep(0,length(mus))
nPerm_rej_cal_2=rep(0,length(mus))
nPerm_stop_cal_2=rep(0,length(mus))

nPerm=rep(0,length(mus))
nPerm_rej=rep(0,length(mus))
nPerm_stop=rep(0,length(mus))

nPerm_agg=rep(0,length(mus))
nPerm_agg_rej=rep(0,length(mus))
nPerm_agg_stop=rep(0,length(mus))

nPerm_bm=rep(0,length(mus))
nPerm_bm_rej=rep(0,length(mus))
nPerm_bm_stop=rep(0,length(mus))

counter=1
for(mu in mus){
  
  
  #Initialize decision and index when decision was obtained 
  
  idx_dec_bc=rep(B,m)
  dec_bc=rep(0,m)
  
  idx_dec_bc_smallB=rep(smallB,m)
  dec_bc_smallB=rep(0,m)
  
  idx_dec_cal=rep(B,m)
  dec_cal=rep(0,m)
  
  idx_dec_cal_2=rep(B,m)
  dec_cal_2=rep(0,m)
  
  idx_dec=rep(B,m)
  dec_bin=rep(0,m)
  dec_bin_r=rep(0,m)
  
  idx_dec_agg=rep(B,m)
  dec_agg=rep(0,m)
  
  idx_dec_bm=rep(B,m)
  dec_bm=rep(0,m)
  dec_bm_r=rep(0,m)
  
  for(j in 1:m){
    
    X=rnorm(n)
    
    treated=(runif(n)>=prop_treated)
    X=X+mu*treated  
    
    
    test_stat=mean(X[treated])-mean(X[!treated])    #Observed test statistic
    
    test_stat_perm=c()                              #Permuted test statistics
    
    #Betting vectors
    bet_bin=c()
    bet_agg=c()
    bet_cal=c()
    bet_cal_2=c()
    
    rank=1          #Rank of the observed test statistic
    
    #Wealth vectors
    wealth_bin=c(1)
    wealth_agg=c(1)
    wealth_cal=c(1)
    wealth_cal_2=c(1)
    wealth_bm=c()
    bc_count=1
    bc_count_smallB=1
    
    for(i in 1:B){
      X_perm=sample(X)                                                #Permute data
      test_stat_perm[i]=mean(X_perm[treated])-mean(X_perm[!treated])  #Calculate permuted test statistic
      
      #We do not check for ties as X is continuously distributed and number of observations is large
      
      if(test_stat_perm[i]>=test_stat){
        if((wealth_bin[length(wealth_bin)]*p_zero*(i+1)/rank)<=alpha){
          bet_bin[i]=0
        }else{
          bet_bin[i]=p_zero*(i+1)/rank
        }
        bet_agg[i]=0
        rank=rank+1
      }else{
        if((wealth_bin[length(wealth_bin)]*p_zero*(i+1)/rank)<=alpha){
          bet_bin[i]=(i+1)/(i-rank+1) 
        }else{
          bet_bin[i]=(1-p_zero)*(i+1)/(i-rank+1)
        }
        bet_agg[i]=(i+1)/i
      }
      
      #Calculate wealth for each strategy
      wealth_bin=cumprod(bet_bin)
      if((min(wealth_bin)<=alpha | max(wealth_bin)>(1/alpha))&idx_dec[j]==B){
        idx_dec[j]=i
      }
      
      wealth_agg=cumprod(bet_agg)
      if((min(wealth_agg)<=alpha | max(wealth_agg)>(1/alpha))&idx_dec_agg[j]==B){
        idx_dec_agg[j]=i
      }
      
      if((rank-1)==h_bc & bc_count==1 & i<B){
        dec_bc[j]=-1
        idx_dec_bc[j]=i
        bc_count=0
      }else if(i==B & bc_count==1){
        idx_dec_bc[j]=i
        dec_bc[j]=1
      } 
      
      if((rank-1)==h_bc_smallB & bc_count_smallB==1 & i<smallB){
        dec_bc_smallB[j]=-1
        idx_dec_bc_smallB[j]=i
        bc_count_smallB=0
      }else if(i==smallB & bc_count_smallB==1 & bc_count_smallB==1){
        idx_dec_bc_smallB[j]=i
        dec_bc_smallB[j]=1
        bc_count_smallB=0
      } 
      
      wealth_bm[i]=(1-pbinom(rank-1,i+1,c))/c
      if(((min(wealth_bm)<=alpha & i>1) | max(wealth_bm)>(1/alpha))&idx_dec_bm[j]==B){
        idx_dec_bm[j]=i
      }
      
      test_stat_comp=c(test_stat,test_stat_perm[1:(i-1)])  
      bet_cal[i]=(i+1)/(sum(1/(1:(i+1)))*(sum(test_stat_perm[i]>=test_stat_comp)+1))
      wealth_cal=cumprod(bet_cal)
      if((min(wealth_cal)<=alpha | max(wealth_cal)>(1/alpha))&idx_dec_cal[j]==B){
        idx_dec_cal[j]=i
      }
      
      bet_cal_2[i]=(i+1)/(sum(1/sqrt((1:(i+1))))*(sqrt(sum(test_stat_perm[i]>=test_stat_comp)+1)))
      wealth_cal_2=cumprod(bet_cal_2)
      if((min(wealth_cal_2)<=alpha | max(wealth_cal_2)>(1/alpha))&idx_dec_cal_2[j]==B){
        idx_dec_cal_2[j]=i
      }
    }
    
    #dec=1 -> rejected, dec=-1 -> stopped for futility, dec=0 -> accepted but did not stop earlier
    if(wealth_bin[idx_dec[j]]>=(1/alpha)){
      dec_bin[j]=1
    }else if(wealth_bin[idx_dec[j]]<alpha){
      dec_bin[j]=-1
    }else{
      dec_bin[j]=0 
    }
    
    
    if(wealth_bin[idx_dec[j]]>=(runif(1)/alpha)){
      dec_bin_r[j]=1
    }else if(wealth_bin[idx_dec[j]]<alpha){
      dec_bin_r[j]=-1
    }else{
      dec_bin_r[j]=0 
    }
    
    
    if(wealth_agg[idx_dec_agg[j]]>=(1/alpha)){
      dec_agg[j]=1
    }else if(wealth_agg[idx_dec_agg[j]]<alpha){
      dec_agg[j]=-1
    }else{
      dec_agg[j]=0 
    }
    
    
    if(wealth_bm[idx_dec_bm[j]]>=(1/alpha)){
      dec_bm[j]=1
    }else if(wealth_bm[idx_dec_bm[j]]<alpha){
      dec_bm[j]=-1
    }else{
      dec_bm[j]=0 
    }
    
    
    if(wealth_bm[idx_dec_bm[j]]>=(runif(1)/alpha)){
      dec_bm_r[j]=1
    }else if(wealth_bm[idx_dec_bm[j]]<alpha){
      dec_bm_r[j]=-1
    }else{
      dec_bm_r[j]=0 
    }
    
    
    if(wealth_cal[idx_dec_cal[j]]>=(1/alpha)){
      dec_cal[j]=1
    }else if(wealth_cal[idx_dec_cal[j]]<alpha){
      dec_cal[j]=-1
    }else{
      dec_cal[j]=0 
    }
    
    
    if(wealth_cal_2[idx_dec_cal_2[j]]>=(1/alpha)){
      dec_cal_2[j]=1
    }else if(wealth_cal_2[idx_dec_cal_2[j]]<alpha){
      dec_cal_2[j]=-1
    }else{
      dec_cal_2[j]=0 
    }
    
  }
  
  power_bin[counter]=mean((dec_bin>0))
  power_bin_r[counter]=mean((dec_bin_r>0))
  power_bc[counter]=mean((dec_bc>0))
  power_bc_smallB[counter]=mean((dec_bc_smallB>0))
  power_agg[counter]=mean((dec_agg>0))
  power_bm[counter]=mean((dec_bm>0))
  power_bm_r[counter]=mean((dec_bm_r>0))
  power_cal[counter]=mean((dec_cal>0))
  power_cal_2[counter]=mean((dec_cal_2>0))
  
  nPerm[counter]=mean(idx_dec)
  nPerm_rej[counter]=mean(idx_dec[which(dec_bin==1)])
  nPerm_stop[counter]=mean(idx_dec[which(dec_bin==-1)])
  
  nPerm_bc[counter]=mean(idx_dec_bc)
  nPerm_rej_bc[counter]=mean(idx_dec_bc[which(dec_bc==1)])
  nPerm_stop_bc[counter]=mean(idx_dec_bc[which(dec_bc==-1)])
  
  nPerm_bc_smallB[counter]=mean(idx_dec_bc_smallB)
  nPerm_rej_bc_smallB[counter]=mean(idx_dec_bc_smallB[which(dec_bc_smallB==1)])
  nPerm_stop_bc_smallB[counter]=mean(idx_dec_bc_smallB[which(dec_bc_smallB==-1)])
  
  nPerm_agg[counter]=mean(idx_dec_agg)
  nPerm_agg_rej[counter]=mean(idx_dec_agg[which(dec_agg==1)])
  nPerm_agg_stop[counter]=mean(idx_dec_agg[which(dec_agg==-1)])
  
  nPerm_bm[counter]=mean(idx_dec_bm)
  nPerm_bm_rej[counter]=mean(idx_dec_bm[which(dec_bm==1)])
  nPerm_bm_stop[counter]=mean(idx_dec_bm[which(dec_bm==-1)])
  
  nPerm_cal[counter]=mean(idx_dec_cal)
  nPerm_rej_cal[counter]=mean(idx_dec_cal[which(dec_cal==1)])
  nPerm_stop_cal[counter]=mean(idx_dec_cal[which(dec_cal==-1)])
  
  nPerm_cal_2[counter]=mean(idx_dec_cal_2)
  nPerm_rej_cal_2[counter]=mean(idx_dec_cal_2[which(dec_cal_2==1)])
  nPerm_stop_cal_2[counter]=mean(idx_dec_cal_2[which(dec_cal_2==-1)])
  
  
  counter=counter+1
}

save(power_bin, power_bin_r,power_bc, power_agg, power_bm,power_bm_r,power_cal, power_cal_2, power_bc_smallB,
     nPerm,nPerm_rej, nPerm_stop, nPerm_bc, nPerm_rej_bc, nPerm_stop_bc,
     nPerm_agg, nPerm_agg_rej, nPerm_agg_stop, nPerm_bm, nPerm_bm_rej, nPerm_bm_stop,
     nPerm_cal, nPerm_rej_cal, nPerm_stop_cal, nPerm_cal_2, nPerm_rej_cal_2, nPerm_stop_cal_2, 
     nPerm_bc_smallB, nPerm_rej_bc_smallB, nPerm_stop_bc_smallB,
     file = "results/power_alpha001.rda")


