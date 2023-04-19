#' @title Compute Gaussian quantile
#' @description Computes Gaussian quantile residual from R INLA output.
#' @param INLAoutput INLA output.
#' @param y Target variable.
#' @return Returns quantile residuals.
#' @export
#' @keywords
#' @examples
#' \dontrun{
#' QuantResidGauss(INLAoutput,y)
#' }
#'


QuantResidGauss  <- function(INLAoutput,y){
  resid = qnorm(pnorm(y,
                      mean= output$summary.fitted.values$mean[1:length(y)],
                      sd=sqrt(1/output$summary.hyperpar$mean[1])))
  return(resid)
}
