#' combination plot function
#'
#' plot grey models in single graph
#' @param g grey model list, a list variable containing models.
#' @param xlab label in x-axis.
#' @param ylab label in y-axis.
#' @param location legend location.
#' @param forecast present the forecasted values in plot.
#' @export
#'
conplot <- function(g,xlab='x-axis',ylab='y-axis',location='topleft',legend=NA,forecast=FALSE){
  count<-length(g)
  n=length(g[[1]]$fitted)
  piece<-length(g[[1]]$data)-n

  ydimo<-c(g[[1]]$data,g[[1]]$test)
  if(forecast==FALSE){
    xdims <- as.numeric(names(g[[1]]$fitted))
    ydims <- g[[1]]$fitted
    xdimo <- as.numeric(names(g[[1]]$data))
    ydimo <- g[[1]]$data
  }else{
    xdims <- c(as.numeric(names(g[[1]]$fitted)),as.numeric(names(g[[1]]$forecasts)))
    ydims <- c(g[[1]]$fitted, g[[1]]$forecasts )
    xdimo <- c(as.numeric(names(g[[1]]$data)),as.numeric(names(g[[1]]$test)))
    ydimo <- c(g[[1]]$data,g[[1]]$test)
  }
  ymax <- max(ydimo,ydims)
  ymin <- min(ydimo,ydims)
  xmax <- max(xdimo,xdims)
  xmin <- min(xdimo,xdims)

  if(g[[1]]$method$class=="dgm"){
    if(g[[1]]$parameter[,'a'][length(g[[1]]$parameter[,'a'])]>1){
      location <- "topleft"
    }else{
      location <- "topright"
    }
  }else{
    if(g[[1]]$parameter[,'a'][length(g[[1]]$parameter[,'a'])]>0){
      location <- "topright"
    }else{
      location <- "topleft"
    }
  }

  for(i in 1:count){
    if(i==1){
      ydims<-list(c(g[[i]]$fitted,g[[i]]$forecasts))
    }else{
      ydims[[i]]<-c(g[[i]]$fitted,g[[i]]$forecasts)
    }
  }

  plot(xdimo,ydimo,xlab=xlab,ylab=ylab,type="b",ylim=c(ymin*0.9,ymax*1.1),xlim = c(xmin,xmax))
  legendlist="origial data"
  pchlist=1
  collist='black'
  if(forecast==TRUE){
    abline(v=xdimo[piece+n]+0.5,lty=5,col="blue")
    #text(xdimo[piece+n]+1.8,ymin,"out-sample",cex=0.9)
    text(xdimo[piece+n]-0.7,ymin,"in-sample",cex=0.9)
  }

  colbase=c("red","blue","green","yellow","purple","brown","orange")
  for(i in 1:count){
    points(as.numeric(names(ydims[[i]])),ydims[[i]],
           col=colbase[i],type="b",pch=i)
    legendlist=c(legendlist,g[[i]]$method$mdname)
    pchlist=c(pchlist,i+1)
    collist=c(collist,colbase[i])
  }
  legend(location,legend=legendlist,pch=pchlist,col=collist,lty=rep(1,count+1),bty="n")
}
