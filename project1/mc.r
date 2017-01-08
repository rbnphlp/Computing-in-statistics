# Data are assumed to be in the vector data. 
mc<-function(data){
data<-data[[1]]
meandata <- mean(data) 
semean <- sd(data)/sqrt((length(data))) 
rmean <- rnorm(999,0,semean) 
rmean[1000] <- meandata 
less <- ifelse(rmean<=meandata,1,0) 
more <- ifelse(rmean>=meandata,1,0) 
nless <- sum(less) 
nmore <- sum(more)
if(nless<nmore) {nmore <- nless} 
p <- nmore/1000
return(p)
# Assume alternat
}