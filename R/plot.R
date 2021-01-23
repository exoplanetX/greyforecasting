
#' generate fitting graph of grey forecasting model
#' usage:
#' g<-gm(y)
#' plot(g)
#' plot(g,forecast=TRUE)
#' @export
plot.greyforecasting <- function(x,location="topleft",add=FALSE,forecast=FALSE){
  ######### determine x and y axis range ######
  n<-length(x$simulation)
  piece<-length(x$original)-n
  if(forecast==FALSE){
    xdims <- as.numeric(names(x$simulation))
    ydims <- x$simulation
    xdimo <- as.numeric(names(x$original))
    ydimo <- x$original
  }else{
    xdims <- c(as.numeric(names(x$simulation)),as.numeric(names(x$forecast)))
    ydims <- c( x$simulation, x$forecasts )
    xdimo <- c(as.numeric(names(x$original)),as.numeric(names(x$testvalue)))
    ydimo <- c(x$original,x$testvalue)
  }
  ymax <- max(ydimo,ydims)
  ymin <- min(ydimo,ydims)
  xmax <- max(xdimo,xdims)
  xmin <- min(xdimo,xdims)
  ######## plotting fitting graph #############
  if(x$parameter['a']>0) location<-"topright"  #determine legend location
  #------- plot original data -----#
  if(add==FALSE){
    plot(xdimo,ydimo,
         ylim = c(ymin*0.9,ymax*1.1),
         xlim = c(xmin,xmax),
         pch=1,col="blue",type="b",
         xlab=x$description$xlab,ylab=x$description$ylab
    )
  }else{
    points(
      xdimo,ydimo,
      ylim = c(ymin*0.9,ymax*1.1),
      pch=1,col="blue",type="b",
      xlab="Year",ylab=x$description
    )
  }
  #----- plot fitting data -------#
  points(
    xdims,ydims,
    pch=2,col="red",type="b")
  #segments(length(x$simulation),x$original[(length(x$original)-length(x$simulation)+1):length(x$original)],
  # x1=length(x$simulation),y1=x$simulation,lty=2,col="red")
  legend(location,legend=c("original data","fitted data"),pch=c(1,2),lty=c(1,5),col=c("blue","red"),bty="n")
  if(forecast==TRUE){
    abline(v=xdimo[piece+n]+0.5,lty=5,col="blue")
    #text(xdimo[piece+n]+1.8,ymin,"out-sample",cex=0.9)
    text(xdimo[piece+n]-0.7,ymin,"in-sample",cex=0.9)
  }
  ########### plotting errors graph ###############
  #cat("do you need buffered comparison plot?  \n")
  #j<-readline("yes or no : \n")
  j="no"
  if(j=="yes"){
    barplot(x$simulation-x$original[piece+1:n],names.arg = xdims[1:n])
  }else{
    if(!(j=="no")){
      cat("sorry,plot function do not catch your words.")
    }
  }
  ############    end     #########################
}

coplot.greyforecasting <- function(...){
  gmobj <- list(...)
  for(i in 1:length(gmobj)){
    if(class(gmobj[i])!="greyforecasting"){
      stop("each argument should be class greyforecasting,stop in",i)
    }
  }
}
