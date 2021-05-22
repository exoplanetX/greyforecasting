#' multi-projection backaground values
#' w:以0.5为基准的左右白化方程分割参数
#' @export

background1<-function(yd,w=0.14){
  n=length(yd)
  alpha=0.5+w
  alpha*yd[2:n]+(1-alpha)*yd[1:(n-1)]
}
