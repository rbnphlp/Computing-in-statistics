monte.carlo.p.value<- function(data){
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
 
#generate 9990 means for x only (under the null hypothesis) using SE fron a normal distribution
 
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




