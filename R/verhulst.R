#' grey verhulst model
#'
#' saturated growth, S shape tendency
#' @export
#' @examples
#' g<-verhulst(y)
#' @param x data sequence.
#' @param present character vector containing xlab and ylab.
#' @param bg background formula.
#' @param buff buffer operator used for original data.
#' @param alpha coefficient in buffer operator if used.
verhulst <-  function(y, ntest = NULL, term = 1, bg = background, buff = NULL,
           alpha = NA) {
    #--原始数据截取ntest部分，生成建模序列x
    if (is.null(names(y))) {
      names(y) <- 1:length(y)
    }
    if (is.numeric(ntest)) {
      x <- y[1:(length(y) - trunc(ntest))]
      test <- y[(length(x) + 1):length(y)]
    } else{
      x <- y
      test <- NULL
    }
    ny = length(y) #原始序列长度
    n = length(x) #建模序列长度
    nf = n + term #拟合+预测序列长度
    if (nf < ny) {
      stop("ntest is too small or term is too big")
    }
    ##--缓冲处理，生成建模序列x0
    if (is.function(buff)){
      if(is.na(alpha)) x1 <- buff(x) else x1 <- buff(x, alpha = alpha)
    }else{
      x1 <- x
    }
    ##--建模处理，生成参数向量p['a']，p['b']
    x0=iago(x1)
    z1=bg(x1)
    p=LSE(x0[2:n],-z1,z1^2)
    ##--响应式
    trf_x1=function(k) p['a']*x1[1]/(p['b']*x1[1]+(p['a']-p['b']*x1[1])*exp(p['a']*(k-1)))
    fitted_x1=trf_x1(1:nf)
    fitted_x1[1]=x1[1]

    names(fitted_x1)<-names(x)
    forecasts<-fitted_x1[(n+1):nf]
    names(forecasts)<-as.numeric(names(x1)[n])+1:term

    obj<-list(
      data       = x,
      test       = test,
      parameter  = data.frame(a=p['a'],b=p['b'],ax=0.5),
      fitted     = fitted_x1[1:n],
      term       = term,
      forecasts  = forecasts,
      mape.in    = mape(x1,fitted_x1[1:n]),
      mape.out   = ifelse(is.null(ntest), NA, mape(test,forecasts[1:ntest])),
      method     = list(class="gm",mdname="Verhulst",buff=buff,alpha=alpha)
    )
    class(obj)<-"greyforecasting"
    obj
}
