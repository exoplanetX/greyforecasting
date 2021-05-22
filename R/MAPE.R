#' @export

mape<- function(original,simulation){
  error=mean(abs(original-simulation)/original)
  return(error)
}
