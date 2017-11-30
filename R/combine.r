#' combine multi-graphs based on matrix "layoutset"
#' matrix "layoutset" may consist of dimensions or facets sequences
#'


combine<-function(layoutset,...){
  graphset<-list(...)
  n<-length(graphset)
  for(i in 1:n){
    if(!is.function(graphset[[i]])){
      return("input element must be graphic function for grey modeling")
    }
  }
  if(is.vector(layoutset)){
    par(mfrow=layoutset)
  } else {
    layout(layoutset)
  }
  for(j in 1:n){
    graphset[[j]]()
  }

}
