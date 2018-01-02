roll <- function(y,rollterm=3,piece=4,stepsize=1){
  n <- length(y)
  rollnumber<-(n-piece+1)/stepsize
  x<-as.numeric(names(y))
  for(i in 1:rollnumber){
    if(i==1){
      rollset<-data.frame(y[1:piece])
    }else{
      rollset<-cbind(rollset,y[(1+(i-1)*stepsize):(piece+(i-1)*stepsize)])
    }
  }

# training part
  rollsetf<-unlist(lapply(rollset,operator))
  target<-y[(piece+1):length(y)]
  simulation<-unlist(lapply(rollset,gmprocess))
  fitness<-mape(target,simulation[1:(length(target))])
  names(simulation) <- c(x[(piece+1):n],x[n]+1)

# extroplation part

  if(rollterm>1){
    for(j in 1:(rollterm-1)){
      rollset[[rollnumber+j]]<-c(rollset[[rollnumber+j-1]][2:piece],simulation[rollnumber+j-1])
      simulation[rollnumber+j]<-gmprocess(rollset[[rollnumber+j]])
      names(simulation)[rollnumber+j]<-x[length(x)]+j+1
    }
  }

# generating obj
  obj<-list(
    original=y,
    background=NA,
    parameter=NA,
    response=NA,
    simulation = simulation[1:(rollnumber-1)],
    forecasts=simulation[rollnumber+0:(rollterm-1)],
    term=rollterm,
    mape=fitness
  )
  class(obj)<-c("greyforecasting")
  return(obj)
}
