roll <- function(y,rollterm=3,piece=4,stepsize=1){
  n <- length(y)
  rollnumber<-(n-piece+1)/stepsize

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

# extroplation part
  x<-as.numeric(names(y))
  if(rollterm>1){
    for(j in 1:(rollterm-1)){
      rollset[[rollnumber+j]]<-c(rollset[[rollnumber+j-1]][2:piece],simulation[rollnumber+j-1])
      simulation[rollnumber+j]<-gmprocess(rollset[[rollnumber+j]])
      names(simulation)[rollnumber+j]<-x[length(y)]+j+1
    }
  }
# generating obj
  obj<-list(
    fitted = simulation

  )
  #class(obj)<-c("greyforecasting")
  return(obj)
}
