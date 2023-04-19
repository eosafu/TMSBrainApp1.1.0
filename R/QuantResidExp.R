#' @title Compute Exponential residual quantiles
#' @description Computes Exponential quantile residuals from R INLA output.
#' @param INLAoutput INLA output.
#' @param y Target variable.
#' @return Returns quantile residuals.
#' @export
#' @keywords
#' @examples
#' \dontrun{
#' QuantResidexp(INLAoutput,y)
#' }
#'


QuantResidExp <- function(INLAoutput,y){

  rate=output$summary.fitted.values$mean[1:length(y)]
  resid = qnorm(pexp(y,rate=rate))
  return(resid)

}
