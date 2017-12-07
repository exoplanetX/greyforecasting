#' buffered grey rolling model
#'
demo.agrm <- function(y,x=names(y)){
  names(y)<-x
  res<-rep(0,length(y))

  for(i in 5:length(y)){
    yd<-svwbo(y[(i-4):i],0.9)
    gdata<-gmodel(yd,names(y[(i-4):i]),1)
    gdata<-gm(gdata)
    res[i+1]<-gdata$forecasts
    names(res)[i+1]<-names(y)[i+1]
  }
  options(digits=6)
  print(res[6:length(res)])
  dev.new()
  plot(x,y,pch=0,type="b",ylim=c(min(y)*0.8,max(y)*1.5))
  points(x[6:length(y)],res[6:length(y)],pch=1,type="b")
  return(res[6:length(res)])
}

