#' claimsBoot
#' 
#' Bootstrap simulation for actuarial reserving models.
#' 
#' @docType package
#' @author Othman El Hammouchi <othman.el.hammouchi@protonmail.com>
#' @import Rcpp data.table ggplot2
#' @importFrom parallel detectCores
#' @importFrom rTRNG TRNG.Version
#' @useDynLib claimsBoot
#' @name claimsBoot
#' @keywords internal
"_PACKAGE"

#' @inherit ChainLadder::UKMotor title
#' @description Re-export of \link[ChainLadder]{UKMotor}
#' @export
UKMotor <- unclass(ChainLadder::UKMotor)
