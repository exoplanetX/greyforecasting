#' full information buffer operator
#' @export
#' @param y original data
#' @param r weight,lambda
#' @param w weight,gamma
#' @examples smth(y,r=0.5,w=0.5)
#'
smth<-function(x){
  n=length(x)
  p=c()
  for(k in 1:n){
    p[k]=x[k]/sum(x[1:(k-1)])
  }
  print("smoothness ratio is:")
  cat(p)
  #return(y)
}
