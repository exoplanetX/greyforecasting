demo.agrm <- function(){
  y=c(3126.534,3284.138,3537.416,4095.428,4765.122,5399.412,5955.199,6512.594,6651.330,7072.452,7749.038,8510.642,8666.062,9025.874,9134.896)
  x=2000:2014
  names(y)<-x
  res<-rep(0,length(y))

  for(i in 5:length(y)){
    yd<-svwbo(y[(i-4):i],0.9)
    gdata<-gmodel(yd,names(y[(i-4):i]),1)
    gdata<-gm(gdata)
    res[i+1]<-gdata$forecasts
  }
  options(digits=6)
  print(c("res=",res))
  dev.new()
  plot(x,y,pch=0,type="b",ylim=c(min(y)*0.8,max(y)*1.5))
  points(x[5:length(y)],res[5:length(y)],pch=1,type="b")
}

