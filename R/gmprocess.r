#' a integral GM(1,1) process using the tool functions
#' defaut forecasting term is 1, that is term=1
#' return a integral modelset when pattern="model"
gmprocess<-function(y,term=1,pattern="forecast"){
  gdata<-gmodel(y,term)
  gdata<-gm(gdata)
  if(pattern=="forecast"){
    return(gdata$forecasts)
  }
  if(pattern=="model"){
    return(gdata)
  }
}