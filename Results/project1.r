#-------------------I hereby confirm this is my own work, unless stated otherwise------------------------------------#

#------------------------Expected Run time  for Full code approx 30 mins ---------------------------#








#-----------------Helper Functions-----------------------------------#
generate.gamma.data<-function(n,m,mu1=4,mu2=4,var1=8,var2=8){
  
  #Purpose:To create two samples of data from a gamma distribution
  #Intput: two samples of size n,m and means mu1,mu2, and var=var1,var2
  #output:list of sample 1 ,sample2 and the difference between means 
  
  #get the difference in means to see if under H_0 or H_1
  delta<-abs(mu2-mu1)
  
  x<-rgamma(n,shape=(mu1)^2/(var1),rate=(mu1)/(var1))
  y<-rgamma(m,shape=(mu2)^2/(var2),rate=(mu2)/(var2))
  #return as a list 
  gamma.data.list<-list(x,y,delta)
  
  return(gamma.data.list) 
}








generate.normal.data<-function(n=1,m=1,mu1=0,mu2=0,sigma1=1,sigma2=1)
{
  #Purpose : Generate data from a normal distribution
  #Intput: two samples of size n,m and means mu1,mu2, and sd's=sigma1,sigma2
  #output:list of sample 1 ,sample2 and the difference between means 
  
  #get the difference in means to see if under H_0 or H_1
  delta<-abs(mu2-mu1)
  
  x<-rnorm(n,mu1,sigma1)
  y<-rnorm(m,mu2,sigma2)
  #return as a list 
  data.list<-list(x,y,delta)
  
  return(data.list) #create two iid with a lenght 1 of normally distributed variables as default
}





calculate.t.pvalue<-function(data,alpha){
  #Purpose : calculate the pvalues for t
  #Input: A list containing the two sample sizes of data as well as a scalar (delta)
  #Output: a single p value for the ttest
  #calculate the one sided ttestt for the two sample dataset , get the pvalue
  
  
  
  t.p.value <- t.test(data[[1]],data[[2]],alternative = "less",var.equal = T,conf.level = 1-(alpha))["p.value"][[1]]
  
  return (t.p.value)}


calculate.mw.pvalue<-function(data){
  #Purpose <- calculate the pvalue for the Mann whitney test
  #Input <- A list containing (sample x,sample y,delta)
  #Output<-  A single p value for the Mann whitney test
  
  #Addiionalifo:(    #calculate the pvalue for: ""Mann whitney test"
  
  #null hypotheis : distributions of x and y differ by a location shift = mu 
  #if mu=0, x and y come from same distributions, with medians the same
  #Alternative hypotheis : if distribution differs from alocation , then the medians differ:
  #
  #)
  
  mw.p.value<- wilcox.test(data[[1]],data[[2]],alternative = "less",mu=0,paired=F)[["p.value"]]
  
  return(mw.p.value)
  
}

calculate.monte.carlo.p.value<- function(data){
  #Purpose :calculate the p-value usiing the Monte carlo Method
  #Input : data as a list : (sample1,sample2,delta)
  #Output :A single pvalue 
  
  #get data for X 
  x<-data[[1]]
  #get data for y
  y<-data[[2]]
  
  # Null hypotheis : Difference in means:0 , Alternative Hypothesis mean(x)<mean(y)
  
  
  #find the mean and SErrors for the two samples of the original dataset:
  meanx<-mean(x)
  SE.x<-sd(x)/sqrt(length(x))
  meany<-mean(y)
  SE.y<-sd(y)/sqrt(length(y))
  #get the difference in means for the original dataset:
  diff.xy<- meanx-meany
  #new.diff.xy <- diff.xy-1E-10 # take care of round for very small nos
  
  #generate 999 means for x only (under the null hypothesis) using SE fron a normal distribution
  
  estimate.meanx<-rnorm(999,meanx,SE.x)
  estimate.meany<-rnorm(999,meanx,SE.y)
  
  
  #get the difference in estimated means 
  diff.estimate.mean<- estimate.meanx-estimate.meany
  
  #add the original differenicein mean to the daataset
  diff.estimate.mean[1000]<-diff.xy
  
  
  #compare estimated difference in means to difference in sample mean
  
  values<-ifelse(diff.estimate.mean<=diff.xy,1,0)
  
  #sum the 1's and finf the propotion for one tailed test /1000
  
  monte.p.value<-sum(values)/1000
  
  return(monte.p.value)
  
  
}



Calculate.pvalue<-function(data,test,alpha){
  
  #Purpose :calculate the p value from two sample data sets
  #Input : data as a list ,the test for which the pvalue is to be  caclualted:
  #(1)t.test(2),mannwhitney or (3)montecarlo
  #delta :difference in means
  #Output: A p value
  
  if (test==1){ # calauclate pvalue for the t-test
    t.p.value<- calculate.t.pvalue(data,alpha)
    return(t.p.value)
  }
  
  else if (test==2){#calcualte pvalue for the mann-whitney test
    mw.p.value<-calculate.mw.pvalue(data)
    return(mw.p.value)
  }
  else if (test==3){ #calculate pvalue for the monte carlo test
    mc.p.value<-calculate.monte.carlo.p.value(data)
    return(mc.p.value)
  }
  
  
}


#-----------------------------------------Simulation Driver ----------------------------#
calcualte.size.power.fornsimulations<- function(nsim,n,m,alpha=0.05,test=1,mu1=0,mu2=0,data=1){
  #Purpose: produce nsimulations for the two sample data,and obtain the size and power of the test
  #Input:nsim<- no of simulations, test<- the test used , alpha<- the significance  level ,
  
  #n<-size of sample x, m<-size of sampley ,data=1 normal data ,data=2 gamma data
  #output : the size and power of the test
  
  
  #--------------Please note mu1!=0,mu2!=0 for data==2 , i.e for a gamma distribution------#
  
  #create an empty vector to store the pvalues
  pvec<-c()
  
  
  
  for (i in 1:(nsim)){
    
    if (data==1){
      mat<-generate.normal.data(n,m,mu1,mu2) #generate normal data
      
    }
    
    else if(data ==2){
      
      mat<-generate.gamma.data(n,m,mu1,mu2) # change means but hold var1=var2=8 throughout.
    }
    #get the delta for the data generated
    delta<-mat[[3]]
    
    
    p.value<- Calculate.pvalue(mat,test,alpha) #caluclate the pvalue for the generated dataset
    pvec<-c(p.value,pvec) #return them as a vector
    
  }
  if(delta==0){ # if data under null hypothesis calculate size
    
    calculate.size<-ifelse(pvec<alpha,1,0) # under null hypothies ,
    size<-(sum(calculate.size)/(nsim)) #get the proportion of pvalue has rejected null hypthesis incorrectly
    return (list("size",size))
  }
  
  else(delta!=0)
  { # if under alternative hypothesis , calculate power
  
  calculate.power<- ifelse(pvec<alpha,1,0)
  #get the propotion of time correctly rejected if null hypothesis false
  
  power<-sum(calculate.power)/(nsim)
  
  return(list("power",power))
  }
  
  
}

#------------------------------------------------------------------------------#




#----------------------------Simulation 1: Increasing n  ,effect on size-------------------#
increasenandm.size<-function(n,nfinal,by,test,nsim){
  #Purpose : increase N sequentially : assume two equal sample sizes
  #Input : n <- intial sample size ,nfinal<- final size of the sample 
  #by<- increase by length ,,nsim<-no of simulations for each sample size ,#the seed to test
  #Output : A list containing sequence of n's and corresponding sizes
  
  
  nseq<-seq(n,nfinal,by=by)
  pvec<-c() #initialize a empty vector
  for(i in 1:length(nseq)){
    #see how size  varies for increasing n  (mu1=mu2=0)
    size.forincreasingn <-calcualte.size.power.fornsimulations(nsim = nsim,n=nseq[i],m=nseq[i],test=test)[[2]]
    pvec<-c(pvec,size.forincreasingn) #append to previous results
  }
  return(list(nseq,pvec))
}

#----------------------------------------------Results for Simulation 1: PS: Takes  a bit of time to run--------------------------#
#looking at size of t-test with increasing n per 500 simulations
increasenandm.size.t<-increasenandm.size(100,10000,by=100,nsim = 100,test=1)

plot(increasenandm.size.t[[1]],increasenandm.size.t[[2]],type="l",xlab = "sample size","ylab"="size",main = "Size of t-test with increasing n")
#



#lookign at size of MC test with increasing n per 500 simulations
increasenandm.size.mc<-increasenandm.size(100,10000,by=100,nsim = 100,test=3)
plot(increasenandm.size.mc[[1]],increasenandm.size.mc[[2]],type="l",xlab = "sample size","ylab"="size",main = "Size of monte-carlo with increasing n per 100 simulations")


#looking at size of mw with increasing n per 500 simulations
increasenandm.size.mw<-increasenandm.size(100,10000,by=100,nsim = 100,test=2)

plot(increasenandm.size.mw[[1]],increasenandm.size.mw[[2]],type="l",xlab = "sample size","ylab"="size",main = "Size of Mann Whitney test with increasing n per 100 simulations")




#-----------------------------------------------------------Simulation 2: effect of increasing sample size on power ----------------#
increasenandm.power<-function(n,nfinal,by,nsim,test=1){
  #Purpose : increase N sequentially : assume two equal sample sizes and see how it affects power
  #Input : n <- intial sample size ,nfinal<- final size of the sample 
  #by<- increase by length ,,nsim<-no of simulations for each sample size ,#the seed to test
  #Output : A list of powers
  
  
  nseq<-seq(n,nfinal,by=by)
  pvec<-c() #initialize a empty vector
  for(i in 1:length(nseq)){
    #see how power  varies for increasing n  (mu2=0.1)
    power.forincreasingn <-calcualte.size.power.fornsimulations(nsim = nsim,n=nseq[i],m=nseq[i],test=test,mu2=0.1)[[2]]
    pvec<-c(pvec,power.forincreasingn) #append to previous results
  }
  return(list(nseq,pvec))
}

# ---------------------------- Results for Simulation2-------------------------#

#Inchreasing sample size and looking at  the effect on power for the ttest,montecarlo
#and the mann whitney
#looking at power of t-test with increasing n per 500 simulations
increasenandm.power.t<-increasenandm.power(100,10000,by=100,nsim =100,test=1)


#looking at  power of mann whitney with increasing n per 500 simulations

increasenandm.power.mw<-increasenandm.power(100,10000,by=100,nsim = 100,test=2)

# looking at power of MC with increasing n per 500 simulations
increasenandm.power.mc<-increasenandm.power(100,10000,by=100,nsim = 100,test=3)



plot(increasenandm.power.t[[1]],increasenandm.power.t[[2]],type="l",xlab = "sample size",ylab="power",main = "power of tests with inreasing size")
points(increasenandm.power.mc[[1]],increasenandm.power.mc[[2]],type="l",col="red")
points(increasenandm.power.mw[[1]],increasenandm.power.mw[[2]],type="l",col="green")
legend(3000,0.6,c("t-test","Monte-Carlo","Mann-Whitney"),col=c("black","red","green"),lty=c(1,1))

#---------------------------------Simulation3 ------------------------------------#
#look at inreasing mu2=0.01 (mu1=0) by 0.01 to 1 and see how affects the power

increaseeffectsize.power<-function(effinital,efffinal,by,nsim,test=1){
  #purpose :To look at the affect on power "increase in raw size"
  #Input : eddfinal: intial effect size , effinal : final effect size, by : increase by ,
  #nsim: no of simulations per increase in effect size, #test,: 1 : t.test,2:Mann Whitney 3: Monte Carlo
  # Output : A list containg "increasing raw size" with assocaited powers
  
  
  
  
  mu2seq<-seq(effinital,efffinal,by=by)
  pvec<-c() #initialize a empty vector
  for(i in 1:length(mu2seq)){
    #see how power  varies for increasing mu2 (mu1==0)
    power.forincreasingeffectsize <-calcualte.size.power.fornsimulations(nsim = nsim,n=100,m=100,test=test,mu2=mu2seq[i])[[2]]
    pvec<-c(pvec,power.forincreasingeffectsize) #append to previous results
  }
  return(list(mu2seq,pvec))
}


#------------------------------------Results for Simulations 3 ---------------------------------#
#look at inreasing mu2=0.01 (mu1=0) by 0.01 to 1 and see how affects the power

#Results for t-test

effectsize.power.t<-increaseeffectsize.power(0.01,1,by=0.01,nsim=100)

#Results for mann whitney
effectsize.power.mw<-increaseeffectsize.power(0.01,1,by=0.01,nsim=100,test=2)

#Results for Monte Carlo :
effectsize.power.mc<-increaseeffectsize.power(0.01,1,by=0.01,nsim=100,test=3)
#Plot the three tests on one plot
matplot (effectsize.power.mw[[1]], cbind (effectsize.power.t[[2]], effectsize.power.mw[[2]],effectsize.power.mc[[2]]),type = "l" ,xlab = "effect size","ylab"="power",main = "Comaprisions of power with increasing effect size")
legend(0.4,0.4,c("t-test","Mann-Whitney Test","Monte-Carlo Test"),col=c("black","red","green"),lty=c(1,1))

#------------------------------------------Simulation 4: Using samples from two gamma distribution to see the resulting power in the tests--------#


increasenandm.power.gamma<-function(n,nfinal,by,nsim,test=1,data=2){
  #Purpose : increase N sequentially : assume two equal sample sizes and see how it affects power
  #Input : n <- intial sample size ,nfinal<- final size of the sample 
  #by<- increase by length ,,nsim<-no of simulations for each sample size ,#the seed to test
  #Output : A list of powers
  
  
  
  nseq<-seq(n,nfinal,by=by)
  pvec<-c() #initialize a empty vector
  for(i in 1:length(nseq)){
    #see how power  varies for increasing n  (mu2=1.1)
    power.forincreasingn <-calcualte.size.power.fornsimulations(nsim = nsim,n=nseq[i],m=nseq[i],test=test,mu2=1.1,mu1=1,data=data)[[2]]
    pvec<-c(pvec,power.forincreasingn) #append to previous results
  }
  return(list(nseq,pvec))
}

#--------------------------------------------------------------Results for Simulation 4-------------------------------------------------#

#Simultating power for the three tests for a gamma distribution.
gamma.power.t<-increasenandm.power.gamma(100,10000,by=250,nsim = 100,test=1,data=2)
gamma.power.mw<-increasenandm.power.gamma(100,10000,by=250,nsim = 100,test=2,data=2)
gamma.power.mc<-increasenandm.power.gamma(100,10000,by=250,nsim = 100,test=3,data=2)


matplot (gamma.power.t[[1]], cbind (gamma.power.t[[2]],gamma.power.mc[[2]],gamma.power.mw[[2]]),type = "l" ,xlab = "n sample size","ylab"="power",main = "power with increasing sample size from a Gamma-distribution ")
legend(6500,0.5,c("T test","Monte-Carlo","Mann-Whitney"),col=c("black","red","green"),lty=c(1,1))


#--------------------------------------------------------------End ---------------------------------------------------------------#

