#' a integral GM(1,1) process using the tool functions
#' defaut forecasting term is 1, that is term=1
#' return a integral modelset when pattern="model"
#' usage:
#' gmprocess(y)
#' gmprocess(y,pattern="parameter")
#' @export
gmprocess<-function(y,model=gm,term=1,pattern="forecast"){
  model<-eval(substitute(model))
  if(pattern=="forecast") {
    return(model(y,term=term)$forecasts)
  }
  if(pattern=="parameter") {
    return(model(y,term=term)$parameter)
  }
}
