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



#Simultating power for the three tests for a gamma distribution.
gamma.power.t<-increasenandm.power.gamma(100,10000,by=250,nsim = 100,test=1,data=2)
gamma.power.mw<-increasenandm.power.gamma(100,10000,by=250,nsim = 100,test=2,data=2)
gamma.power.mc<-increasenandm.power.gamma(100,10000,by=250,nsim = 100,test=3,data=2)


matplot (gamma.power.t[[1]], cbind (gamma.power.t[[2]],gamma.power.mc[[2]],gamma.power.mw[[2]]),type = "l" ,xlab = "n sample size","ylab"="power",main = "power with increasing sample size from a Gamma-distribution ")
legend(6500,0.5,c("T test","Monte-Carlo","Mann-Whitney"),col=c("black","red","green"),lty=c(1,1))


