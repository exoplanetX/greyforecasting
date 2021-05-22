#' @export
print.greyforecasting <- function(md){
  cat("<grey forecasting model>\n")
  cat("original data:\n")
  print(md$data)
  cat("fitted data<simulation>:\n")
  print(md$fitted)
  cat("forecast data<forecasts>:\n")
  print(md$forecasts)
  cat("parameter values<parameter>:\n")
  print(md$parameter)
  # if(is.vector(md$parameter)){
  #   cat("parameter a  is",md$parameter['a'],"b is",md$parameter['b'],"\n")
  # }else{
  #   if(is.data.frame(md$parameter)){
  #     cat("parameters in each section:\n")
  #     print(md$parameter)
  #   }
  # }
  cat("insample mape: ",md$mape.in,"\n")
  cat("outsample mape: ",md$mape.out,"\n")
  cat("prediction term:",md$term)
}
