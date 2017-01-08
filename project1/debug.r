calcualte.size.power.fornsimulations<- function(nsim,n,m,alpha=0.05,test=1,mu1=0,mu2=0){
  #Purpose: produce nsimulations for the two sample data,and obtain the size and power of the test
  #Input:nsim<- no of simulations, test<- the test used , alpha<- the confidence level ,
  #n<-size of sample x, m<-size of sampley
  #output : the size and power of the test
  
  #create an empty vector to store the pvalues
  pvec<-c()
  
  
  for (i in 1:(nsim)){
    
    mat<-generate.data(n,m,mu1,mu2) #generate data each time 
    #get the delta for the data generated
    delta<-mat[[3]]
    
    
    p.value<- Calculate.pvalue(mat,test) #caluclate the pvalue for the generated dataset
    pvec<-c(p.value,pvec) #return them as a vector
    
  }
  if(delta==0){ # if data under null hypothesis calculate size
    
    calculate.size<-ifelse(pvec<=alpha,1,0) # under null hypothies ,
    size<-(sum(calculate.size)/(nsim)) #get the proportion of pvalue has rejected null hypthesis incorrectly
    return (list("size",size))
  }
  
  else(delta!=0)
  { # if under alternative hypothesis , calculate power
  
  calculate.power<- ifelse(pvec<=alpha,1,0)
  #get the propotion of time correctly rejected if null hypothesis false
  
  power<-sum(calculate.power)/(nsim)
  
  return(list("power",power))
  }
  
  
}


calcualte.size.power.fornsimulations(2000,100,100,mu=50,mu2=100)