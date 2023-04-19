#' @title Compute Log Normal residual quantiles
#' @description Computes log-normal quantile residuals from R INLA output.
#' @param INLAoutput INLA output.
#' @param y Target variable.
#' @return Returns quantile residuals.
#' @export
#' @keywords
#' @examples
#' \dontrun{
#' QuantResidLNorm(INLAoutput,y)
#' }
#'


QuantResidLNorm  <- function(INLAoutput,y){

  resid =  qnorm(plnorm(y,meanlog =output$summary.fitted.values$mean[1:length(y)],
                        sdlog = sqrt(1/output$summary.hyperpar$mean[1])
  ))
  return(resid)
}
