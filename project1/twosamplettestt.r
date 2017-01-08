twosample.normal.iid<-function(n,mu=0,sigma=1){
#Purpose: Generate data from two distribtuions 
#Input: no of observations to be returned ,mu sample mean for x and y , sample sigma std for x and y
  #output: A mtrix of nrows*2:from N(mu1,sigma) ,vector y : from N(mu2,sigma)
#create a empty matrix
  mat<-matrix(0,nrow=n,ncol = 2)
  
   mat[,1]<-x<-rnorm(n,mu,sigma)
  mat[,2]<-y<-rnorm(n,mu,sigma)
  
 return(mat)
}

degreesoffreedom<-function(mat){
  #purpose : Calculate df for finding the critical value
  #Input :  a matrix of the sample datset
  #output :df
  n<-nrow(mat) # obtain the no of obsecrvations of sample data 
  df<-2*n-2 # n+m-2= 2n-2 as n and m equal for iid
  
return(df)
}

#delta<-function(mu1,mu2){
  #purpose :alternative hypothesis:if means different-> calcuate the difference (delta)
  #input : mean of the population1 and mean of populaton2
  #output:delta
 # d<-mu2-mu1
  # if d=0 , then null hypothesis
#  return(d)
  
#}

critical.value <-function(d,alpha){
  #purpose: calcualte the critical value for the t-distribution
  #input: d->degrees of freedom ,alpha<- size of the test or type1error (0.05 for 1sided test)
  #output:criticalvalue
  
  cvalue<-qt(1-(alpha),d)
  return(cvalue)
}

calculate.power.t<-function(mat,alpha,delta,sigma){
 #Purpose : Calculate the power of two sample independent t test
  #input : two sample data matrix , delta<- difference between the two population means, sigma1,2
  #standard deviation between  the population
  #output : 1-beta ->power of t-test
  
  #get n
  n<-nrow(mat)
  #caclualte the SE 
  SE<-sigma*sqrt(2/(n))  
  # Calcualte the non centrality parameter
  
  ncp<-delta/SE
  #get the degrees of freedom from the sample data
 df<- degreesoffreedom(mat)
  
  #get the critical value
  criticalvalue<-critical.value(df,alpha)
# calculate the beta for a one sided t-test
  beta<-pt(criticalvalue,df,ncp)
  
  #calculate power
  
  p<-1-beta
  
  return(p)
  
  
}

power.alpha.simulation<-function(inalpha,finalpha,by,nsim,myseed){
  #purpose :To look at the affect of power with alpha
  #Input : inalpha: intial alpha, finalpha : final alpha, by :  ,
  #nsim: no of simulations per increase in alpha
  # Output : A list containg "increasing "alpha" with assocaited powers
  
  
  set.seed(myseed)
  
  mu2seq<-seq(inalpha,finalpha,by=by)
  pvec<-c() #initialize a empty vector
  for(i in 1:length(mu2seq)){
    #see how power  varies for increasing mu2 (mu1==0)
    power.forincreasingeffectsize <-calcualte.size.power.fornsimulations(nsim = nsim,n=100,m=100,test=test,mu2=mu2seq[i])[[2]]
    pvec<-c(pvec,power.forincreasingeffectsize) #append to previous results
  }
  return(list(mu2seq,pvec))
}





