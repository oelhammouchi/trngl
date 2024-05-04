#' claimsBoot
#'
#' Bootstrap simulation for actuarial reserving models.
#'
#' @docType package
#' @author Othman El Hammouchi <othman.el.hammouchi@protonmail.com>
#' @importFrom rTRNG TRNG.Version
#' @useDynLib claimsBoot
#' @name claimsBoot
#' @keywords internal
"_PACKAGE"

#' @inherit ChainLadder::UKMotor title
#' @description Re-export of \link[ChainLadder]{UKMotor}
#' @include trngl.R
#' @export
UKMotor <- as.trngl(ChainLadder::UKMotor)
