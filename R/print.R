#' @export
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
