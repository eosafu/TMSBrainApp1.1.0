#' @title Compute exceedance probability, credible interval, and posterior mean
#' @description Computes exceedance probability at the hotspot using the INLA output.
#' @param INLAoutput INLA output.
#' @param quantile Quantile value.
#' @param dist Data model/distribution.
#' @return Returns quantile residuals.
#' @export
#' @keywords
#' @examples
#' \dontrun{
#' ExceedanceProb(INLAoutput,dist,quantile)
#' }
#'


ExceedanceProb <-  function(INLAoutput,dist,quantile=NULL){
  aux = INLAoutput
  probb = NULL
  hpd   = NULL
  MMean = NULL
  if(dist=="gaussian"){
    MMean  = (aux$summary.fitted.values$mean)
    max.index = which.max(aux$summary.fitted.values$mean)
    t.marg = inla.tmarginal(function(x)x,aux$marginals.fitted.values[[max.index]])

    if (!is.null(quantile)) probb = mean(inla.rmarginal(10^4,t.marg)>=quantile)
    hpd = inla.hpdmarginal(0.95,t.marg)
  }else if(dist=="lognormal"){

    MMean  = exp(aux$summary.fitted.values$mean)

    max.index = which.max(aux$summary.fitted.values$mean)
    t.marg = inla.tmarginal(function(x)exp(x),aux$marginals.fitted.values[[max.index]])

    if (!is.null(quantile)) probb = mean(inla.rmarginal(10^4,t.marg) >= quantile)
    hpd = inla.hpdmarginal(0.95,t.marg)
  }else if(dist=="gamma"){

    shape=aux$summary.hyperpar$mean[1]
    rate = shape/(aux$summary.fitted.values$mean)
    Meangamma = shape/rate
    MMean = Meangamma

    max.index = which.max(Meangamma)
    t.marg = inla.tmarginal(function(x)x, aux$marginals.fitted.values[[max.index]])
    Mean =  inla.rmarginal(10^4,t.marg)

    if (!is.null(quantile)) probb = mean(Mean >=quantile)
    hpd = inla.hpdmarginal(0.95,t.marg)

  }else if(dist=="weibull"){

    shape=output$summary.hyperpar$mean[1]
    lambda = (output$summary.fitted.values$mean)
    scale = lambda^(-1/shape)
    Meanweibull = scale*gamma(1+1/shape)
    MMean = Meanweibull

    max.index = which.max(Meanweibull)
    t.marg = inla.tmarginal(function(x)x, aux$marginals.fitted.values[[max.index]])
    scale= inla.rmarginal(10^4,t.marg)^(-1/shape)
    Mean = scale*gamma(1+1/shape)
    if (!is.null(quantile)) probb = mean(Mean >= quantile)
    t.marg = inla.tmarginal(function(x)x^(-1/shape)*gamma(1+1/shape), aux$marginals.fitted.values[[max.index]])
    hpd = inla.hpdmarginal(0.95,t.marg)

  }else if(dist=="exponential"){
    Meanexp=1/output$summary.fitted.values$mean
    MMean =  Meanexp

    max.index = which.max(Meanexp)
    t.marg = inla.tmarginal(function(x)1/x, aux$marginals.fitted.values[[max.index]])
    Mean= inla.rmarginal(10^4,t.marg)
    if (!is.null(quantile)) probb = mean(Mean >=quantile)
    hpd = inla.hpdmarginal(0.95,t.marg)
  }
  return(list(probb,hpd,MMean))
}
