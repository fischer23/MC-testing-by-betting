#Reproduces the results of Section 6.5 in the paper "Sequential Monte-Carlo testing by betting"


rm(list=ls())

library(tidyverse)
library(lubridate)

set.seed(123)

#The following data set can be downloaded from https://ride.capitalbikeshare.com/system-data
#The same data was analysed by Berrett et al. (2020) and Grünwald et al. (2023). For complete
#references, see below.

data <- read.csv(file = "2011-capitalbikeshare-tripdata.csv")   

###NOTE: The following part of the code to generate the test statistics 
###      is copied from Grünwald et al. (2023). It is available in the supplement
###      of the paper.

# formatting and pre-selection
data <- data %>%
  mutate(
    month = month(Start.date),
    day = day(Start.date),
    minute = hour(Start.date) * 60 + minute(Start.date),
    day_of_week = 
      ((4 + day + cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))[month]) %% 7),
    holiday = (month == 9 & day == 5) |
      (month == 10 & day == 10) |
      (month == 10 & day == 31) |
      (month == 11 & (abs(day - 24L) <= 1L))
  ) %>%
  filter(day_of_week <= 4 & month >= 9 & month <= 11 & !holiday) %>%
  select(-holiday) %>%
  mutate(Duration = log(Duration))

#-------------------------------------------------------------------------------
# estimate mean and variance of log duration by kernel smoothing (and keep all
# other relevant variables)
duration_mean_by_time <- function(
    duration,
    minute,
    month,
    Member.type,
    End.date,
    Start.date
) {
  h <- 20
  K <- 
    exp(-(12 * 60 - abs(abs(outer(minute, minute, "-")) - 12 * 60))^2 / 2 / h^2)
  K[, month == 11] <- 0 
  K_weights_total <- rowSums(K)
  Duration_mean <- c(K %*% duration / K_weights_total)
  Duration_var <- c(K %*% (duration^2) / K_weights_total - Duration_mean^2)
  list(
    tibble(
      Duration_mean = Duration_mean,
      Duration_var = Duration_var,
      weights_total = K_weights_total,
      month = month,
      duration = duration,
      Member.type = Member.type,
      End.date = End.date,
      Start.date = Start.date,
      minute = minute
    )
  )
}

df <- data %>%
  group_by(Start.station.number, End.station.number) %>%
  summarise(
    duration_by_time = duration_mean_by_time(
      Duration,
      minute,
      month,
      Member.type,
      End.date,
      Start.date
    )
  ) %>% 
  unnest(duration_by_time) %>%
  ungroup()

#-------------------------------------------------------------------------------
# extract validation data and run tests
df1 <- df1_seq <- df %>%
  filter(month == 11 & weights_total >= 20) %>%
  arrange(as.POSIXct(Start.date))

start_dates <- as.POSIXct(df1$Start.date)

n0 <- 200
df_test_seq <- df_test <- data.frame(
  y = as.integer(df1$Member.type == "Member"),
  z = df1$Duration_mean,
  x = df1$duration
)

m <- nrow(df1)
e <- rep(1, m)
pred <- rep(1, m)
denom <- rep(1, m)
include_obs <- rep(TRUE, m)
n_start_sample <- 0
for (j in 1:m) {
  # remove observations which might be dependent on past ones
  start_date <- start_dates[j]
  start_diff <- as.double(start_date - start_dates[seq_len(j - 1)], units = "hours")
  potential_dependence <-  which(abs(start_diff) < 0) # replace 0 by positive number to remove potentially dependent observations
  if (length(potential_dependence) > 0) {
    start_location <- df1_seq$Start.station.number[j]
    same_location <- any((start_location == df1_seq$Start.station.number[potential_dependence]))
    if (same_location) {
      include_obs[j] <- FALSE
      df_test_seq[j, ] <- NA
      start_date[j] <- NA
      next
    }
  }
}

ninclude <- sum(include_obs)
cor_true <- abs(cor(df_test$y[include_obs], df_test$x[include_obs] - df1$Duration_mean[include_obs]))
                       
###NOTE: End of the copied code of Grünwald et al. (2023). The observed test statistic is given by cor_true.

#####Calculation of power and mean number of permutations using our sequential strategies

B=5000                #Number of permutations
m=1000                #Number of runs
p_perm=rep(0,m)       #Permutation p-values
p_bin=rep(0,m)        #p-values binomial strategy
p_bm=rep(0,m)         #p-values binomial mixture strategy
idx_dec=rep(B,m)      #Indices decisions binomial strategy
dec_bin=rep(0,m)      #Decisions binomial strategy
idx_dec_bm=rep(B,m)   #Indices decisions binomial mixture strategy
dec_bm=rep(0,m)       #Decisions binomial mixture strategy

for(k in 1:m){
  
  cor_perm=rep(0,B)   #Generated test statistics
  
  for (j in (1:B)) {
    #Generated test statistics are calculated in the same manner as by Grünwald et al. (2023):
    cor_perm[j]=abs(cor(df_test$y[include_obs], rnorm(ninclude, 0, sqrt(df1$Duration_var[include_obs]))))
  }
  
  bet_bin=c()
  
  rank=1              #Rank observed test statistic
  
  wealth_bin=c(1)
  wealth_bm=c()
  
  alpha=0.05
  c=0.95*alpha        #Parameter binomial mixture strategy
  p_zero=1/ceiling(sqrt(2*pi*exp(1/6))/alpha)         #Parameter binomial strategy
  
for (b in seq_len(B)) {
  
  if(cor_perm[b]>=cor_true){
    bet_bin[b]=p_zero*(b+1)/rank
    rank=rank+1
  }else{
    bet_bin[b]=(1-p_zero)*(b+1)/(b-rank+1)
  }
  
  wealth_bin=cumprod(bet_bin)

  if((max(wealth_bin)>(1/alpha))&idx_dec[k]==B){
    idx_dec[k]=b
  }
  
  wealth_bm[b]=(1-pbinom(rank-1,b+1,c))/c
  if(max(wealth_bm)>(1/alpha)&idx_dec_bm[k]==B){
    idx_dec_bm[k]=b
  }
}
p_perm[k]=(sum(cor_perm >= cor_true)+1)/(B + 1)
  
p_bin[k]=1/max(wealth_bin)
if(wealth_bin[idx_dec[k]]>=(1/alpha)){
  dec_bin[k]=1
}else if(wealth_bin[idx_dec[k]]<alpha){
  dec_bin[k]=-1
}else{
  dec_bin[k]=0 
}
  
p_bm[k]=1/max(wealth_bm)
if(wealth_bm[idx_dec_bm[k]]>=(1/alpha)){
  dec_bm[k]=1
}else if(wealth_bin[idx_dec_bm[k]]<alpha){
  dec_bm[k]=-1
}else{
  dec_bm[k]=0 
}
  
  
}

mean(p_perm)                #Mean permutation p-value

power_p_bin=mean(dec_bin>0) #power binomial strategy
mean(idx_dec)               #mean number of permutations
median(idx_dec)             #median number of permutations

power_p_bm=mean(dec_bm>0)   #power binomial mixture strategy
mean(idx_dec_bm)            #mean number of permutations
median(idx_dec_bm)          #median number of permutations


###Since all generated test statistics smaller than the observed one,
###we perform the same analysis but reduce the test data to 2000 obs.

include_obs[2001:length(include_obs)]=FALSE

###NOTE: Part of code by Grünwald et al. (2023).
ninclude <- sum(include_obs)
cor_true <- abs(cor(df_test$y[include_obs], df_test$x[include_obs] - df1$Duration_mean[include_obs]))

###NOTE: End of code by Grünwald et al. (2023).


#####Calculation of power and mean number of permutations using our sequential strategies

B=5000                #Number of permutations
m=1000                #Number of runs
p_perm=rep(0,m)       #Permutation p-values
p_bin=rep(0,m)        #p-values binomial strategy
p_bm=rep(0,m)         #p-values binomial mixture strategy
idx_dec=rep(B,m)      #Indices decisions binomial strategy
dec_bin=rep(0,m)      #Decisions binomial strategy
idx_dec_bm=rep(B,m)   #Indices decisions binomial mixture strategy
dec_bm=rep(0,m)       #Decisions binomial mixture strategy

for(k in 1:m){
  
  cor_perm=rep(0,B)   #Generated test statistics
  
  for (j in (1:B)) {
    #Generated test statistics are calculated in the same manner as by Grünwald et al. (2023):
    cor_perm[j]=abs(cor(df_test$y[include_obs], rnorm(ninclude, 0, sqrt(df1$Duration_var[include_obs]))))
  }
  
  bet_bin=c()
  
  rank=1              #Rank observed test statistic
  
  wealth_bin=c(1)
  wealth_bm=c()
  
  alpha=0.05
  c=0.95*alpha        #Parameter binomial mixture strategy
  p_zero=1/ceiling(sqrt(2*pi*exp(1/6))/alpha)         #Parameter binomial strategy
  
  for (b in seq_len(B)) {
    
    if(cor_perm[b]>=cor_true){
      bet_bin[b]=p_zero*(b+1)/rank
      rank=rank+1
    }else{
      bet_bin[b]=(1-p_zero)*(b+1)/(b-rank+1)
    }
    
    wealth_bin=cumprod(bet_bin)
    
    if((max(wealth_bin)>(1/alpha))&idx_dec[k]==B){
      idx_dec[k]=b
    }
    
    wealth_bm[b]=(1-pbinom(rank-1,b+1,c))/c
    if(max(wealth_bm)>(1/alpha)&idx_dec_bm[k]==B){
      idx_dec_bm[k]=b
    }
  }
  p_perm[k]=(sum(cor_perm >= cor_true)+1)/(B + 1)
  
  p_bin[k]=1/max(wealth_bin)
  if(wealth_bin[idx_dec[k]]>=(1/alpha)){
    dec_bin[k]=1
  }else if(wealth_bin[idx_dec[k]]<alpha){
    dec_bin[k]=-1
  }else{
    dec_bin[k]=0 
  }
  
  p_bm[k]=1/max(wealth_bm)
  if(wealth_bm[idx_dec_bm[k]]>=(1/alpha)){
    dec_bm[k]=1
  }else if(wealth_bin[idx_dec_bm[k]]<alpha){
    dec_bm[k]=-1
  }else{
    dec_bm[k]=0 
  }
  
  
}

mean(p_perm)                #Mean permutation p-value

power_p_bin=mean(dec_bin>0) #power binomial strategy
mean(idx_dec)               #mean number of permutations
median(idx_dec)             #median number of permutations

power_p_bm=mean(dec_bm>0)   #power binomial mixture strategy
mean(idx_dec_bm)            #mean number of permutations
median(idx_dec_bm)          #median number of permutations


###References

#T. B. Berrett, Y. Wang, R. F. Barber, and R. J. Samworth. The conditional permutation test for independence while controlling for
#confounders. Journal of the Royal Statistical Society Series B: Statistical Methodology, 82(1):175-197, 2020.

#P. Grünwald, A. Henzi, and T. Lardy. Anytime-valid tests of conditional independence under model-X. Journal of the American Statistical
#Association, pages 1-12, 2023.










