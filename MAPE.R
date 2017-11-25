MAPE<- function(ft){
  error=mean(abs(ft$original-ft$simulation)/ft$original)
  return(error)
}
