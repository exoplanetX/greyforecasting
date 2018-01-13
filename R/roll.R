roll <- function(y,buff=NA,intensity=NA,present="y",rollterm=3,piece=4,stepsize=1){
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
  if(is.function(buff)){
    rollsetf<-as.data.frame(lapply(rollset,buff,alpha=intensity))
  }else{
    rollsetf=rollset
  }
  simulation<-unlist(lapply(rollsetf,gmprocess))
  param <- lapply(rollsetf,gmprocess,pattern="parameter")
  for(i in 1:length(param)){
    if(i==1){
      paras<-data.frame(a=param[[1]]['a'],b=param[[1]]['b'])
    }else{
      paras<-rbind(paras,data.frame(a=param[[i]]['a'],b=param[[i]]['b']))
    }
  }
  row.names(paras)<-1:rollnumber
  target<-y[(piece+1):length(y)]
  fitness<-mape(target,simulation[1:(length(target))])
  names(simulation) <- c(x[(piece+1):n],x[n]+1)
  #row.names(param)<-1:rollnumber

# extroplation part

  if(rollterm>1){
    for(j in 1:(rollterm-1)){
      rollsetf[[rollnumber+j]]<-c(rollsetf[[rollnumber+j-1]][2:piece],simulation[rollnumber+j-1])
      simulation[rollnumber+j]<-gmprocess(rollsetf[[rollnumber+j]])
      names(simulation)[rollnumber+j]<-x[length(x)]+j+1
    }
  }

# generating obj
  obj<-list(
    original=y,
    description=present,
    background=NA,
    parameter=paras,
    response=NA,
    simulation = simulation[1:(rollnumber-1)],
    forecasts=simulation[rollnumber+0:(rollterm-1)],
    term=rollterm,
    mape.insample=fitness
  )
  class(obj)<-c("greyforecasting")
  return(obj)
}
