res<-rep(0,length(y))
ydata<-y[(i-4):i]
for(i in 5:length(y)){
  yd<-svwbo(y[(i-4):i],0.9)
  gdata<-gmodel(yd,names(y[(i-4):i]),1)
  gdata<-gm(gdata)
  res[i+1]<-gdata$forecasts
}
