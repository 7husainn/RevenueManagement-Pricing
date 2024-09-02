# There are four possible price levels we can set for our standard product:
# 5.0, 5.5, 6.0, and 6.5
price=c(1.25,1.5,1.75,2.0)

# True probabilities at these price levels are unknown to us
# and we draw random demand from the beta probability distribution
# to compare our performance to the perfectly known demand case.
true=c(0.6,0.7,0.4,0.2)

#now we compute the best expected revenue per period
bestexprevperiod=max(price*true)


# Number of Decision Periods
T=1000

# Number of Simulations

N=2000

# Generating empty matrix and arrays for later use.

decision=matrix(NA,nrow=N,ncol=T)
Revavg=matrix(NA,nrow=N,ncol=T)
aside=array(rep( 0, len=4*N*T), dim=c(4,N,T))
Regret=matrix(NA,nrow=N,ncol=T)

# we  make an array of NA to capture 
# proportions of chosen price levels across all simulations in each period
prop1avgsim=rep( NA, T)
prop2avgsim=rep( NA, T)
prop3avgsim=rep( NA, T)
prop4avgsim=rep( NA, T)
AvgRegretTS=rep( NA, T)


#for loop will run for 1000 simulations
#In each simulation and in each period
# we draw one random variable from beta distribution for each price levels (theta)
# these random variables are the probabilities we get that demand

for(sim in 1:N){
  picked=c(0,0,0,0);
  # Initial parameters (a,b) for the beta distribution
  # Our initial belief is that they are all uniformly distributed
  a=c(1,1,1,1);
  b=c(1,1,1,1);
  
  Rev=0;
  theta=c(0,0,0,0);
  
  for(t in 1:T){
    theta[1]=rbeta(1,a[1],b[1]);
    theta[2]=rbeta(1,a[2],b[2]);
    theta[3]=rbeta(1,a[3],b[3]);
    theta[4]=rbeta(1,a[4],b[4]);
    
    #now we compare expected revenue (price*theta) across all price levels and
    #we select the price level which gives us the max exp revenue in that period
    #now we draw a random value from the uniform distribution and compare it with the 
    #true probability of the selected price level, 
    
    if(price[1]*theta[1]>=max(price[2]*theta[2],price[3]*theta[3],price[4]*theta[4])){
      picked[1]=picked[1]+1;
      aside[1,sim,t]=1
      decision[sim,t]=1
      exprevchoice=price[1]*true[1]
      random = runif(1, 0, 1);
      #if this is lesser than the true value it indicates 
      #we have demand for that price level therefore,
      #we increment parameter "a" otherwise we increment "b".
      if(random<true[1]){
        a[1]=a[1]+1;
      }
      else{
        b[1]=b[1]+1;
      }
    }
    
    if(price[2]*theta[2]>=max(price[1]*theta[1],price[3]*theta[3],price[4]*theta[4])){
      picked[2]=picked[2]+1;
      aside[2,sim,t]=1
      decision[sim,t]=2
      exprevchoice=price[2]*true[2]
      random = runif(1, 0, 1);
      if(random<true[2]){
        a[2]=a[2]+1;
      }
      else{
        b[2]=b[2]+1;
      }
    }
    
    if(price[3]*theta[3]>max(price[1]*theta[1],price[2]*theta[2],price[4]*theta[4])){
      picked[3]=picked[3]+1;
      aside[3,sim,t]=1
      decision[sim,t]=3
      exprevchoice=price[3]*true[3]
      random = runif(1, 0, 1);
      if(random<true[3]){
        a[3]=a[3]+1;
      }
      else{
        b[3]=b[3]+1;
      }
    }
    
    if(price[4]*theta[4]>max(price[1]*theta[1],price[2]*theta[2],price[3]*theta[3])){
      picked[4]=picked[4]+1;
      aside[4,sim,t]=1
      decision[sim,t]=4
      exprevchoice=price[4]*true[4]
      random = runif(1, 0, 1);
      if(random<true[4]){
        a[4]=a[4]+1;
      }
      else{
        b[4]=b[4]+1;
      }
    }
    #now we calculate regret and
    #we repeat the process for each time period and each simulation in that time period
    Regret[sim,t]=bestexprevperiod-exprevchoice
    
  }
  
}
#for each time period we compute 
#average proportion of selected price level and average regret.
for(t in 2:T){
  prop1avgsim[t]=mean(aside[1,,t]);
  prop2avgsim[t]=mean(aside[2,,t]);
  prop3avgsim[t]=mean(aside[3,,t]);
  prop4avgsim[t]=mean(aside[4,,t]);
  AvgRegretTS[t]=mean(Regret[,t])
}


# Proportion of time each price level is selected:
plot(prop1avgsim,type="l",col = "blue",
     main = "Proportion of Simulations Each Price Level is set",
     xlab = "Time", ylab = "Proportion of Simulations",ylim=c(0,1))
lines(prop2avgsim,type="l",col = "red")
lines(prop3avgsim,type="l",col = "orange")
lines(prop4avgsim,type="l",col = "green")
legend("topright",c("£1.25","£1.5", "£1.75","£2.0"),cex=.8,col=c("blue","red","orange","green"),pch=c(16,16,16,16))







