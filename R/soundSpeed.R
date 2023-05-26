#' Sound Speed
#'
#' This function finds the average speed of sound in water or air (c) in m/s
#' given information on temperature, salinity, and depth.
#'
#' @param t The temperature in °C
#' @param med The medium sound is traveling through
#' @param s The salinity in parts per thousand (ppt)
#' @param z The depth in m
#' @return The speed of sound in m/s
#' @export
#'
#' @examples
#' #Speed of sound in 30°C water with a salinity of 2000 ppt and a depth of
#' # 2010 m
#' soundSpeed(30, "water", 2000, 2010)
#'

soundSpeed <- function(t, med="air", s=NULL, z=NULL){
  if(med=="air"){
    #equation for speed of sound in air
    c <- 331 + 0.6 * t
  } else if (s < 0){
    #enter different, yet correct value for s
    var = readline(prompt = "Enter value for salinity (has to be > 0) : ");
    c = soundSpeed(t, med, as.double(var), z)
  } else if (z < 0){
    #enter different, yet correct value for z
    var = readline(prompt = "Enter new value for depth (has to be > 0) : ");
    c = soundSpeed(t, med, s, as.double(var))
  } else if (med != "water"){
    #med has to be air or water
    var = readline(prompt = "Enter new value for med (has to be 'air' or 'water') : ");
    c = soundSpeed(t, var, s, z)
  } else if (med=="water" & s >= 0 & z >= 0){
    #equation for speed of sound in water
    c = 1449.2 + 4.6*t - 0.055*t^2 + 0.00029*t^3 + (1.34-0.010*t) * (s-35) + 0.0165*z
  } else {
    warning("'med' has to be 'water' or 'air'")
  }
  return(c)
}
