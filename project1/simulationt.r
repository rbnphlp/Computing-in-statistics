

mat<-twosample.normal.iid(1000)
calculate.power.t(mat,alpha = 0.05,delta = 0.05,sigma=1)

# work in progress need to look at the changes

# changes in alpha >larger alpha greater power?
#larger sigmas > less power

#larger delta's , greater power 
#larger n and m -> greater power
#look at repeating simulations?

#need to look at how  to scale delta;s 



calcualte.size.power.fornsimulations<- function(nsim,alpha,test=c("t.test","mann-whitney","Monte-carlo")){
  #Purpose: produce nsimulations for the two sample data,and obtain the size and power of the test
  #Input:nsim<- no of simulations, test<- the test used , alpha<- the confidence level
  #output : the size and power of the test
  
  #create an empty vector to store the pvalues
  pvec<-c()
  
  for (i in 1:(nsim)){
    
    mat<-generate.data(n,m) #generate data each time 
    
    p.value<- Calculate.pvalue(mat,test) #caluclate the pvalue for the generated dataset
    pvec<-c(p.value,pvec) #return them as a vector
    
  }
  if()
    calculate.size<-ifelse(pvec>=alpha,1,0) # under null hypothies ,
  size.pwer<-sum(size.power)/(nsim) 
  
  
  
}

