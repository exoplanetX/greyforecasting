gm<- function(gdata){
  y1=cumsum(gdata$original)
  n=length(y1)
  z<- -gdata$background(gdata$original)
  parameters<-lm(y[2:n]~z)
  p<- c(parameters$coefficients[2],parameters$coefficients[1])
  gdata$a<-parameters$coefficients[2]
  gdata$b<-parameters$coefficients[1]
  k<- 1:n
  gdata$simulation=(y[1]-gdata$b/gdata$a)*(1-exp(gdata$a))*exp(-gdata$a*(k-1))
  gdata$response<-function(k) ((gdata$original[1]-gdata$b/gdata$a)*(1-exp(gdata$a))*exp(-gdata$a*(k-1)))
  gdata$forecasts <- gdata$response(n+1:gdata$term)
  gdata$errors<-gdata$simulation-gdata$original
  return(gdata)
}