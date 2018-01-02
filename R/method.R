print.greyforecasting <- function(x){
  cat("<grey forecasting model>\n")
  cat("original data:\n")
  print(x$original)
  cat("fitted data<simulation>:\n")
  print(x$simulation)
  cat("forecast data<forecasts>:\n")
  print(x$forecast)
  cat("process: parameter a <p['a']> is",x$p['a'],"b <p['b']> is",x$p['b'],"\n")
  cat("prediction term:",x$term)
}

plot.greyforecasting <- function(x){
  ymax <- max(max(x$original),max(x$simulation))
  ymin <- min(min(x$original),min(x$simulation))
  xdimo <- as.numeric(names(x$original))
  xdims <- as.numeric(names(x$simulation))
  plot(xdimo,x$original,ylim = c(ymin*0.9,ymax*1.1),pch=1,col="blue",type="b",xlab="Year",ylab=x$description)
  points(xdims,x$simulation,pch=2,col="red",type="b")
  segments(xdims,x$original[(length(x$original)-length(x$simulation)+1):length(x$original)],x1=xdims,y1=x$simulation,lty=2,col="red")
  legend("topleft",legend=c("original data","fitted data"),pch=c(1,2),lty=c(1,5),col=c("blue","red"),bty="n")

}

summary.greyforecasting <- function(x){
  #if(is.na(names(y1))){return("the first argument'names contains NA")}
  y1 <- x$original
  y2 <- x$simulation
  s=1
  varname=c()
  ae=c()
  pae=c()
  n1<-length(y1)
  n2<-length(y2)
  for(i in seq_along(y2)){
    for(j in seq_along(y1)){
      if(names(y2)[i]==names(y1)[j]){
        varname[s]=names(y2)[i]
        ae[s]=abs(y2[i]-y1[j])
        pae[s]=ae[s]/y1[j]
        s<-s+1
      }
    }
  }
  print(data.frame(row.names = varname,ae=ae,pae=pae))
  cat("mean absolute pecentage error is:",mean(pae))
}

