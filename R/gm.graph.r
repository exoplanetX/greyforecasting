gm.graph<- function(gdata,pattern="fitting"){
  if(pattern=="fitting"){
    u<-function(xlabel=NA,ylabel=NA,graphname=NA){
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
    legend("topleft",inset=0.03, c(gdata$gmname,"original"),
           lty=c(2,1),pch=c(1,2),col=c("red","black"),bty="n")
    abline(v=x[n]+0.2,lty=5)
    arrows(x[n]+0.2,y[n]*1.2,x[n]-(max(x)-min(x))*0.2,y[n]*1.2,lty=5,angle=10)
    text(x[n]-1.2,y[n]*1.2,"fitting")
    arrows(x[n]+0.2,y[n]*0.7,x[n]+0.2+(max(x)-min(x))*0.2,y[n]*0.7,lty=5,angle=10)
    text(x[n]+1.6,y[n]*0.7,"forecasting")
    }
    return(u)
  }
  if(pattern=="errors"){
    function(){
      barplot(gdata$errors)
    }
  }
}
