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

# ----------------------------Simulation2-------------------------#

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

