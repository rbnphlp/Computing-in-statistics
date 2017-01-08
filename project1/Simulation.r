generate.data<-function(n=1,m=1,mu1=0,mu2=0,sigma1=1,sigma2=1)
{
  #Purpose : Generate data from a normal distribution
  #Intput: two samples of size n,m and means mu1,mu2, and sd's=sigma1,sigma2
  #output:list of sample 1 ,sample2 and the difference between means 
  
  #get the difference in means to see if under H_0 or H_1
  

  
  
  x<-rnorm(n,mu1,sigma1)
  y<-rnorm(m,mu2,sigma2)
  #return as a list 
  data.list<-list(x,y,delta)
  
  return(data.list) #create two iid with a lenght 1 of normally distributed variables as default
}




Calculate.pvalue<-function(data,test=c(1,2,3)){
  
  #Purpose :calculate the p value from two sample data sets
  #Input : data as amatrix (nrows*2 cols) , the test for the pvalue caclualted:t.test,mannwhitney or montecarlo
  #delta :difference in means
  #Output: A p value
  
  if (test==1){
    #calculate the one sided ttestt for the two sample dataset , get the pvalue
  t.p.value <- t.test(data[[1]],data[[2]],alternative = "less",var.equal = T)["p.value"][[1]]
    
  return (t.p.value)
  }
  

  
}

calcualte.size.power.fornsimulations<- function(nsim,alpha,test=c(1,2,3),mu1=0,mu2=0){
  #Purpose: produce nsimulations for the two sample data,and obtain the size and power of the test
  #Input:nsim<- no of simulations, test<- the test used , alpha<- the confidence level
  #output : the size and power of the test
  
  #create an empty vector to store the pvalues
  pvec<-c()
  
  
  for (i in 1:(nsim)){
    
    mat<-generate.data(15,15,mu1,mu2) #generate data each time 
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


  
calcualte.size.power.fornsimulations(5,0.05,mu1=0,mu2=100,test = 1)
  
  


