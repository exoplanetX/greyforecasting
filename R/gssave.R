#' @export
#'
gssave <- function(md,path="~/",filename=NULL){
  if(md$method$class=="roll"){
    save_roll(md,path=path,filename)
  }
  if(md$method$class=="gm"){
    save_gm(md,path=path,filename=filename)
  }
  if(md$method$class=="dgm"){
    save_gm(md,path=path,filename=filename)
  }
  if(md$method$class=="mpgm"){
    save_mpgm(md,path=path,filename=filename)
  }
}
