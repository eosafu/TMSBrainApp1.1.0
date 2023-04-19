#' @title Compute quantile residuals
#' @description Computes the quantile residual from R INLA output object.
#' @param INLAoutput INLA output.
#' @param y Target variable.
#' @param dist The likelihood model used for the INLA output. Should be "gaussian","lognormal","gamma","weibull", or "exponential".
#' @return Returns quantile residuals.
#' @export
#' @keywords
#' @examples
#' \dontrun{
#' QuantResid(INLAoutput,y,dist)
#' }
#'

QuantResid <-  function(INLAoutput,y,dist){
  output = INLAoutput

  if(dist=="gaussian"){
    resid = qnorm(pnorm(y,
                        mean= output$summary.fitted.values$mean[1:length(y)],
                        sd=sqrt(1/output$summary.hyperpar$mean[1])))
  }else if(dist=="lognormal"){

    resid =  qnorm(plnorm(y,meanlog =output$summary.fitted.values$mean[1:length(y)],
                          sdlog = sqrt(1/output$summary.hyperpar$mean[1])
    ))

  }else if(dist=="weibull"){

    alpha=output$summary.hyperpar$mean[1]
    lambda = (output$summary.fitted.values$mean[1:length(y)])
    resid = qnorm(pweibull(y,shape = alpha,
                           scale=lambda^(-1/alpha)))

  }else if(dist=="gamma"){

    shape=output$summary.hyperpar$mean[1]
    rate = shape/(output$summary.fitted.values$mean[1:length(y)])
    resid = qnorm(pgamma(y,shape = shape,
                         rate=rate))

  }else if(dist=="exponential"){
    rate=output$summary.fitted.values$mean[1:length(y)]
    resid = qnorm(pexp(y,rate=rate))
  }else{
    stop("Distribution not supported")
  }

  return(resid)
}
QuantResidGauss  <- function(INLAoutput,y){
  resid = qnorm(pnorm(y,
                      mean= output$summary.fitted.values$mean[1:length(y)],
                      sd=sqrt(1/output$summary.hyperpar$mean[1])))
  return(resid)
}
