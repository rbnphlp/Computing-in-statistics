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


