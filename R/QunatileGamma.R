#' @title Compute Gamma residual quantiles
#' @description Computes Gamma quantile residuals from R INLA output.
#' @param INLAoutput INLA output.
#' @param y Target variable.
#' @return Returns quantile residuals.
#' @export
#' @keywords
#' @examples
#' \dontrun{
#' QuantResidGamma(INLAoutput,y)
#' }
#'


QuantResidGamma <- function(INLAoutput,y){

  shape=output$summary.hyperpar$mean[1]
  rate = shape/(output$summary.fitted.values$mean[1:length(y)])
  resid = qnorm(pgamma(y,shape = shape,
                       rate=rate))
  return(resid)

}
