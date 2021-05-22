#' Progject file for grey prediction application
#'
#' @export
#' @param mdlist vector of character variable, which contains model names
project <- function(y,term=1,ntest=NULL,mdlist="gm"){
  for(i in mdlist){
    md=do.call(i,args = list(y=y,term=term,ntest=ntest))
    gssave(md,filename=i)
  }
}
