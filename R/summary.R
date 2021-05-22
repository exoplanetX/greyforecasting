#' make the summary of greyforecasting object
#' @export
#' @examples
#' g<-gm(y)
#' summary(g)
summary.greyforecasting <- function(md){
  #if(is.na(names(y1))){return("the first argument'names contains NA")}
  original<-md$data
  simulation <- md$fitted
  n <- length(md$data)
  term=md$term
  ae=abs(original-simulation)
  pae=ae/original
  varname<-names(original)
  print(data.frame(row.names = varname,ae=ae,pae=pae))
  cat("MSE in -sample is:",mape(original,simulation),"\n")
  if(is.null(md$test)){
    cat("no out-sample test")
  }else{
    cat("MSE out-sample is:",mape(md$test,simulation[n+1:term-1]))
  }
}

