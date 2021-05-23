#' generate fitting graph of grey forecasting model
#'
#' @export
#' @param md 'greyforecasting' object.
#' @param location legend position,'topleft','topright'.
#' @param forecast add the forecast data in plot.
#' @examples
#' g<-gm(y)
#' plot(g)
#' plot(g,forecast=TRUE)
plot.greyforecasting <- function(md,location="topleft",forecast=FALSE,
                                 xlab='x-axis',ylab='y-axis'){
  ######### determine x and y axis range ######
  n<-length(md$fitted)
  piece<-length(md$data)-n
  if(forecast==FALSE){
    xdims <- as.numeric(names(md$fitted))
    ydims <- md$fitted
    xdimo <- as.numeric(names(md$data))
    ydimo <- md$data
  }else{
    xdims <- c(as.numeric(names(md$fitted)),as.numeric(names(md$forecasts)))
    ydims <- c(md$fitted, md$forecasts )
    xdimo <- c(as.numeric(names(md$data)),as.numeric(names(md$test)))
    ydimo <- c(md$data,md$test)
  }
  ymax <- max(ydimo,ydims)
  ymin <- min(ydimo,ydims)
  xmax <- max(xdimo,xdims)
  xmin <- min(xdimo,xdims)
  ######## plotting fitting graph #############
  if(md$method$class=="dgm"){
    if(md$parameter[,'a'][length(md$parameter[,'a'])]>1){
      location <- "topleft"
    }else{
      location <- "topright"
    }
  }else{
    if(md$parameter[,'a'][length(md$parameter[,'a'])]>0){
      location <- "topright"
    }else{
      location <- "topleft"
    }
  }

  #----- plot original data -----#
  plot(xdimo,ydimo, ylim = c(ymin*0.9,ymax*1.1),xlab=xlab,ylab=ylab,
       xlim = c(xmin,xmax),pch=1,col="blue",type="b")
  #----- plot fitting data -------#
  points(xdims,ydims, pch=2,col="red",type="b")
  legend(location,legend=c("original data",md$method$mdname),pch=c(1,2),lty=c(1,5),col=c("blue","red"),bty="n")
  if(forecast==TRUE){
    abline(v=xdimo[piece+n]+0.5,lty=5,col="blue")
    text(xdimo[piece+n]-0.7,ymin,"in-sample",cex=0.9)
  }
}
