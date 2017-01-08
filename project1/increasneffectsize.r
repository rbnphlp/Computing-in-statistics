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


#---------------------------------Simulation3 ------------------------------------#
#look at inreasing mu2=0.01 (mu1=0) by 0.01 to 1 and see how affects the power

#Results for t-test

effectsize.power.t<-increaseeffectsize.power(0.01,1,by=0.01,200)

#Results for mann whitney
effectsize.power.mw<-increaseeffectsize.power(0.01,1,by=0.01,200,test=2)

#Results for Monte Carlo :
effectsize.power.mc<-increaseeffectsize.power(0.01,1,by=0.01,200,test=3)
#Plot the three tests on one plot
matplot (effectsize.power.mw[[1]], cbind (effectsize.power.t[[2]], effectsize.power.mw[[2]],effectsize.power.mc[[2]]),type = "l" ,xlab = "effect size","ylab"="power",main = "Comaprisions of power with increasing effect size")
legend(0.4,0.4,c("t-test","Mann-Whitney Test","Monte-Carlo Test"),col=c("black","red","green"),lty=c(1,1))
  
  
