#' Snell's Law
#'
#' This function is a reformated version of Snell's law that finds reflection or refraction angle given the two mediums longitudinal
#' wave velocities.
#'
#' @param ang The known angle of relfection or refraction
#' @param v1 The longitudinal wave velocity of medium where angle (refraction or reflection) is known given in m/s.
#' @param v2 The longitudinal wave velocity of medium where angle (refraction or reflection) is unknown given in m/s.
#' @return The opposing angle of reflection or refraction, respectfully
#' @export
#'
#' @examples
#' # Calculate the reflection angle given the angle of a black-tailed godwit sound
#' #source is 64 degrees, the speed is 1564 m/s in the first medium and 1494 m/s
#' #in the second medium.
#' snell(64,1564,1494)
snell <- function(ang, v1, v2){
  #Snell's law
  ang2 <- (ang/v1)*v2
  return(ang2)
}
