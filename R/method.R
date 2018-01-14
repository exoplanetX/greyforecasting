#'
print.greyforecasting <- function(x){
  cat("<grey forecasting model>\n")
  cat("original data:\n")
  print(x$original)
  cat("fitted data<simulation>:\n")
  print(x$simulation)
  cat("forecast data<forecasts>:\n")
  print(x$forecasts)
  if(is.vector(x$p)){
    cat("process: parameter a <p['a']> is",x$p['a'],"b <p['b']> is",x$p['b'],"\n")
  }else{
    if(is.data.frame(x$p)){
      cat("parameters in each section:\n")
      print(x$p)
    }
  }

  cat("prediction term:",x$term)
}
#' fetch the parameters in grey forecasting object
#' usage:
#' g<-gm(y)
#' coef(g)
coef.greyforecasting <- function(x){
  cat("parameters a and b are:\n")
  print(x$parameter)
}
#' generate fitting graph of grey forecasting model
#' usage:
#' g<-gm(y)
#' plot(g)
#' plot(g,forecast=TRUE)
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
    xdims <- c( as.numeric(names(x$simulation)),as.numeric(names(x$forecasts)) )
    ydims <- c( x$simulation, x$forecasts )
    xdimo <- c(as.numeric(names(x$original)),as.numeric(names(x$testvalue)))
    ydimo <- c(x$original,x$testvalue)
  }
  ymax <- max(ydimo,ydims)
  ymin <- min(ydimo,ydims)
  xmax <- max(xdimo,xdims)
  xmin <- min(xdimo,xdims)
######## plotting fitting graph #############
  if(gm(x$original)$parameter['a']>0) location<-"topright"  #determine legend location
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
  segments(as.numeric(names(x$simulation)),x$original[(length(x$original)-length(x$simulation)+1):length(x$original)],
           x1=as.numeric(names(x$simulation)),y1=x$simulation,lty=2,col="red")
  legend(location,legend=c("original data","fitted data"),pch=c(1,2),lty=c(1,5),col=c("blue","red"),bty="n")
  if(forecast==TRUE){
    abline(v=xdimo[piece+n]+0.5,lty=5,col="blue")
    text(xdimo[piece+n]+1.8,ymin,"out-sample",cex=0.9)
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

#' make the summary of greyforecasting object
#' usage:
#' g<-gm(y)
#' summary(g)
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
  cat("mean absolute pecentage error of insample is:",mean(pae))
}

