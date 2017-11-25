#MAPE<- function(ft){
#  error=mean(abs(ft$original-ft$simulation)/ft$original)
#  return(error)
#}
# grey model function ----
gm<- function(y,t){
  y1=cumsum(y)
  n=length(y)
  z<- -(y1[1:n-1]+y1[2:n])/2
  ftab<-lm(y[2:n]~z)
  p<- c(ftab$coefficients[2],ftab$coefficients[1])
  k<- 1:n
  response_y1=(y[1]-p[2]/p[1])*exp(-p[1]*(k-1))+p[2]/p[1]
  yf=(y[1]-p[2]/p[1])*(1-exp(p[1]))*exp(-p[1]*(k-1))
  resp<-function(k) ((y[1]-p[2]/p[1])*(1-exp(p[1]))*exp(-p[1]*(k-1)))
  forecasts <- resp(n+1:t)
  result <- list(original=y, simulation=yf, term=t,forecasts=forecasts, a=p[1],b=p[2],response=resp)
  return(result)
}
# extrapolation of gm model ---
gm.forecast<- function(gm,t=1){
  forecastvalues=gm$response(length(gm$original)+1:t)
  return(forecastvalues)
}
# generate gm model graph ---
gm.graph<- function(gdata,format="none",xlabel=NA,ylabel=NA,graphname=NA){
  function(){
    y=gdata$original
    x=as.numeric(names(y))
    n=length(y)
    yf=c(gdata$simulation,gdata$forecasts)
    xf<-c(x,(x[n]+1:gdata$term))
    n=length(y)
    c0<-y[1]-gdata$b/gdata$a
    eq1<-substitute(widehat(x)(k+1)==c * e^paste(a*k) ,list(c=c0,a=-gdata$a) ) #equation
    options(digits=6)

    plot(xf,yf,col="red",type="b",
       pch=1,lty=2,xlab=xlabel,
       ylab=ylabel,main=graphname,sub=eq1
       )
    points(x,y,type="b",pch=2,lty=1)
    legend("topleft",inset=0.03, c("GM(1,1)","original"),
         lty=c(2,1),pch=c(1,2),col=c("red","black"),bty="n")
    abline(v=x[n]+0.2,lty=5)
    arrows(x[n]+0.2,y[n]*1.2,x[n]-(max(x)-min(x))*0.2,y[n]*1.2,lty=5,angle=10)
    text(x[n]-1.2,y[n]*1.2,"fitting")
    arrows(x[n]+0.2,y[n]*0.7,x[n]+0.2+(max(x)-min(x))*0.2,y[n]*0.7,lty=5,angle=10)
    text(x[n]+1.6,y[n]*0.7,"forecasting")
  }
}
y=c(135.84,178.29,321.12,551.36,606.11)
x=c(2001,2002,2003,2004,2005)
names(y)<-x
ft<-gm(y,4)
print(MAPE(ft), digits=4)
p1<-gm.graph(ft)
