buffplot <- function(x,buff=NULL,alpha=NA,add=FALSE,nr=4){
  #coerce(buff)
  if(is.function(buff)){
    if(is.na(alpha)){
      y<-buff(x)
    }else{
      y<-buff(x,alpha=alpha)
    }
  }else{
    stop("argument buff must be function")
  }
  nx<-as.numeric(names(x))
  n<-length(x)
  plot(nx,x,pch=1,col=1,type="b",xlab="Year",ylab="data value")
  points(nx,y,pch=2,col=2,type="b")
  location<-ifelse(gm(x)$parameter['a']>0, "topright","topleft")

  legend(location,legend = c("original data","buffered data"),
         pch=c(1,2),col=c(1,2),lty=1,bty="n")
  if(nr>=0&&nr<=n){
    arrows(nx[1:(n-nr)],x[1:(n-nr)],
           nx[1:(n-nr)],y[1:(n-nr)],
           lty=5,angle = 12,col="blue")
  }

}

segplot <- function(x){

}

writeformula <- function(x){

}
