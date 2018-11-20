mape<- function(original,simulation){
  error=mean(abs(original-simulation)/original)
  return(error)
}
ape<-function(original,simulation){
  abs(original-simulation)/original
}
