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


#looking at size of t-test with increasing n per 500 simulations
increasenandm.size.t<-increasenandm.size(100,10000,by=100,nsim = 500,test=1)

plot(increasenandm.size.t[[1]],increasenandm.size.t[[2]],type="l",xlab = "sample size","ylab"="size",main = "Size of t-test with increasing n")
#chec if there is a linear relationship
abline(lm(increasenandm.size.t[[2]]~increasenandm.size.t[[1]]))



#lookign at size of MC test with increasing n per 500 simulations
increasenandm.size.mc<-increasenandm.size(100,10000,by=100,nsim = 500,test=3)
plot(increasenandm.size.mc[[1]],increasenandm.size.mc[[2]],type="l",xlab = "sample size","ylab"="size",main = "Size of monte-carlo with increasing n per 500 simulations")
abline(lm(increasenandm.size.mc[[2]]~increasenandm.size.mc[[1]]))

#looking at size of mw with increasing n per 500 simulations
increasenandm.size.mw<-increasenandm.size(100,10000,by=100,nsim = 500,test=2)

plot(increasenandm.size.mw[[1]],increasenandm.size.mw[[2]],type="l",xlab = "sample size","ylab"="size",main = "Size of Mann Whitney test with increasing n per 500 simulations")
#chec if there is a linear relationship
abline(lm(increasenandm.size.mw[[2]]~increasenandm.size.mw[[1]]))







