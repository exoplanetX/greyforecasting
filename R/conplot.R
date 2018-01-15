#' combination plot function
#'
#' plot grey models in single graph
#' @param g grey model, a list variable containing models
#' @param xlab label in x-axis
#' @param ylab label in y-axis
#'
#'
conplot <- function(g,xlab=NA,ylab=NA,legend=NA){
  count<-length(g)
  ydimo<-c(g[[1]]$original,g[[1]]$testvalue)
  xdimo<-c(as.numeric(names(g[[1]]$original)),as.numeric(names(g[[1]]$testvalue)))
  xdims<-c(as.numeric(names(g[[1]]$simulation)),as.numeric(names(g[[1]]$forecasts)) )

  for(i in 1:count){
    if(i==1){
      ydims<-list(c(g[[i]]$simulation,g[[i]]$forecasts))
    }else{
      ydims[[i]]<-c(g[[i]]$simulation,g[[i]]$forecasts)
    }
  }
  ymax<-max(unlist(ydims),ydimo)
  ymin<-min(unlist(ydims),ydimo)
  plot(xdimo,ydimo,xlab=xlab,ylab=ylab,type="b",ylim=c(ymin*0.9,ymax*1.1))

  colbase<-c("red","blue","green","yellow")
  for(i in 1:count){
    points(as.numeric(names(ydims[[i]])),ydims[[i]],
           col=colbase[i],type="b",pch=i)
  }
}
