#' @title Compute Weibull residual quantiles
#' @description Computes Weibull quantile residuals from R INLA output.
#' @param INLAoutput INLA output.
#' @param y Target variable.
#' @return Returns quantile residuals.
#' @export
#' @keywords
#' @examples
#' \dontrun{
#' QuantResidWeibull(INLAoutput,y)
#' }
#'


QuantResidWeibull <- function(INLAoutput,y){
  alpha=output$summary.hyperpar$mean[1]
  lambda = (output$summary.fitted.values$mean[1:length(y)])
  resid = qnorm(pweibull(y,shape = alpha,
                         scale=lambda^(-1/alpha)))

  return(resid)
}
