#' a integral GM(1,1) process using the tool functions
#' defaut forecasting term is 1, that is term=1
#' return a integral modelset when pattern="model"
gmprocess<-function(y,pattern="forecast"){
  term=1
  if(pattern=="forecast") {
    return(gm(y,term)$forecasts)
  }
  if(pattern=="parameter") {
    return(gm(y,term)$parameter)
  }
}
