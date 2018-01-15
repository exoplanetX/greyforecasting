#' adaptive buffered grey rolling model
#' @examples
#' r<-roll(y)
#' @param y data sequence
#' @param model unit grey model used in each data piece
#' @param buff the buffer operator function adjusting data piece
#' @param rollterm forecasting terms by extroplation
#' @param piece length of data piece
#' @param stepsize  rolling step
roll <- function(y,ntest=NULL,rollterm=1,model=gm,buff=NA,intensity=NA,present=c(NA,NA),piece=4,stepsize=1){
  if(is.numeric(ntest)) {
    x<-y[1:(length(y)-trunc(ntest))]
    testvalue<-y[(length(x)+1):length(y)]
    y<-x
  }else{
    testvalue<-NULL
  }
  if(length(present)==1){
    present <- c(NA,present)
  }else{
    present <- c(present[1],present[2])
  }
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
  simulation<-unlist(lapply(rollsetf,gmprocess,model=model))
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
      rollsetf[[rollnumber+j]]<-buff(y=c(rollsetf[[rollnumber+j-1]][2:piece],simulation[rollnumber+j-1]),
                                     alpha=intensity)
      simulation[rollnumber+j]<-gmprocess(rollsetf[[rollnumber+j]])
      names(simulation)[rollnumber+j]<-x[length(x)]+j+1
    }
  }

# generating obj
  obj<-list(
    original=y,
    testvalue=testvalue,
    description=data.frame(xlab=present[1],ylab=present[2]),
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
