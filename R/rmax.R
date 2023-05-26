#' Maximum Detection Range
#'
#' This function finds the maximum detection range of a given call.
#'
#' @param a The absorption coefficeant given to you by the absorptionAir or absorptionWater formula.
#' @param sl Source level of the signal of interest, as measured in dBs.
#' @param nl Noise Level or background ambient noise in the recorder’s local environment.
#' @param dt Detection Threshold or the additional dBs the signal of interest must achieve above ambient noise conditions in order to be detected by the receiver.
#' @param d Depth
#' @param xaxis Exaggerated max distance. This gives the largest distance to evaluate in order to find the intercept of your detection threshold and propogation of the source level.
#' @return The maximum detection range in meters
#' @export
#'
#' @examples
#' # SL= 195, NL = 82.9897, DT = 10, TR = 2500
#'  rmax(sl=195,nl=82.9897,dt=10,d=2500*2, xaxis=10000000)
rmax <- function(sl,nl,dt,d,a=0, xaxis=25){

  ans <- sl-nl-dt-(10*log10(d/2))


  f1 <- function(x,ab=a){(ab*x/1000 + 10*log10(x))}
  f2 <- ans

  b <- stats::optimize(function(t0) abs(f1(t0) - f2), interval = range(1:xaxis))

  if(xaxis == b[[1]]){
    stop("Make he parameter'xaxis' larger.")
  }

  return(b[[1]])
}
