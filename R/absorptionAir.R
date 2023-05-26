#' absorptionAir
#'
#' This function is the standard method of calculating the absorption of sound in air (ISO 9613-1).
#'
#' @param f The frequency of the sound source in Hz
#' @param p The ambient atmospheric pressure in kPa
#' @param t The ambient atmospheric temperature in C
#' @param h The relative humidity as a percent
#' @param pr The standard pressure at mean sea level
#' @param tr The standard temperature in Celcius (293.15 in kelvin)
#' @param to The triple-point isotherm temperature
#' @return The sound attenuation rate in dB/m
#' @export
#'
#' @examples
#' #How much are Malayan tapir calls (15 kHz) absorbed in a tropical region
#' #(30 deg C) assuming a humidity of 80% and standard pressure (101.325)?
#' absorptionAir(15000, 101.325, 30, 80)

absorptionAir <- function(f, p, t, h, pr=101.325, tr=293.15, to=273.16){
  #convert C to K
  t <- t + 273.15

  v1 <- 10.79586*(1-(to/t))
  v2 <- log10(t/to)
  v3a <- -8.29692*((t/to)-1)
  v3b <- 1-(10^v3a)
  v3 <- 10^(-4) * v3b
  v4a <- 4.76955*(1-(to/t))
  v4b <- -1+(10^v4a)
  v4 <- 10^(-3) * v4b

  v <- v1 - 5.02808 * v2 + 1.50474 * v3 + 0.42873 * v4 - 2.2195983

  ps <- (pr)*10^v

  h <- h * (ps/pr) * (p/pr)^-1

  #oxygen relaxation frequency
  fo1aa <- 4.04*(10^4)*h
  fo1ab <- 0.02+h
  fo1ac <- 0.391+h
  fo1a <- fo1aa * fo1ab / fo1ac
  fo1 <- 24+ fo1a

  fo <- p/pr*fo1

  #nitrogen relaxation frequency
  fn <- ((p/pr)*((t/tr)^(-1/2))) * (9+280*h*exp(-4.170*(((t/tr)^(-1/3))-1)))

  #absorption coefficient
  a <- 8.686*(f^2)*
    ((1.84*(10^-11)*((p/pr)^-1)*(t/tr)^(1/2))+
    ((t/tr)^(-5/2))*
    (0.01275*(exp(-2239.1/t))*(fo/((f^2)+fo^2))+
       0.1068*(exp(-3352/t))*(fn/((f^2)+fn^2))))

  return(a)
}
