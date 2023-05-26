#' opDB
#'
#' This function either adds, subtracts or averages dB values specified in function. This is for dB values in re to power, intensity or pressure.
#'
#' @param dbs A vector of dB values of all the same reference.
#' @param PL Should be 10 if dB measurements were for intensity or power and 20 if dB measurements were for pressure.
#' @param op Default = 'add'. Can be 'add', 'sub', or 'avg' to add, subtract, or average, respectively.
#' @return A sinlge dB value
#' @export
#'
#' @examples
#' #Given a set of intensity values, add them.
#' opDB(c(100, 101, 127, 96), 10)


opDB <- function(dbs, PL, op="add"){

  #adding dBs
  if(op=="add"){
    dB <- PL*log10(sum(10^(dbs/PL)))
  #averaging dBs
  } else if (op=="avg"){
    dB <- PL*log10(mean(10^(dbs/PL)))
  #subtracting dBs
  } else if (op=="sub"){
    if(length(dbs>2)){

      dbs[2] <- PL*log10(10^(dbs[1]/PL) - 10^(dbs[2]/PL))
      #recursevily shorten vector after subtraction
      if (length(dbs[-1])>2){
        dB <- opDB(dbs[-1],PL, op="sub")
      } else {
        return(dbs[-1])
      }
    } else if (length(dbs==1)){
      return(dB)
    } else {
      stop("Invalid input.")
    }
  } else {
    #error when operation is invalid
    stop("Choose a valid operation: op = ('add', 'avg', 'sub').")
  }
  return(dB)

}
