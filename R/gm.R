# not use
#' Evaluate paramters of GM model
#' generate background formula, response formula and forecast values
#' formate: gdata<-gm(gdata)
gm<- function(gdata){
  n=length(gdata$original)
  z<- -gdata$background(gdata$original)
  parameters<-lm(gdata$original[2:n]~z)
  gdata$a<-parameters$coefficients[2]
  gdata$b<-parameters$coefficients[1]

  gdata$response<-function(k) ((gdata$original[1]-gdata$b/gdata$a)*(1-exp(gdata$a))*exp(-gdata$a*(k-1)))
  gdata$simulation=gdata$response(1:n)

  gdata$forecasts <- gdata$response(n+1:gdata$term)
  gdata$errors<-gdata$simulation-gdata$original
  return(gdata)
}
