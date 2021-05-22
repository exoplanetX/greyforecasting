
coplot.greyforecasting <- function(...){
  gmobj <- list(...)
  for(i in 1:length(gmobj)){
    if(class(gmobj[i])!="greyforecasting"){
      stop("each argument should be class greyforecasting,stop in",i)
    }
  }
}

