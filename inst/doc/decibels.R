## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval=FALSE--------------------------------------------------------
#  #Stable - Install package from CRAN
#  install.packages("bioSNR")
#  
#  #Unstable - Install package from Github repository
#  devtools::install_github("MattyD797/bioSNR")
#  
#  #Attach package namespace to active libraries in Rstudio
#  library(bioSNR)

## ----setupR, eval=TRUE, include=FALSE-----------------------------------------
library(bioSNR)

## ----referenceTable, echo=FALSE-----------------------------------------------
ref <- data.frame(row.names = c("Air", "Water"))
ref[,1] <- c("$20\\: \\mu Pa$", "$1\\: \\mu Pa$")
ref[,2] <- c("$1*10^{-12}\\: W/m^2$", "$6.7*10^{-19}\\: W/m^2$")
ref[,3] <- c("$1*10^{-12}\\: W$", "$6.7*10^{-19}\\: W$")
colnames(ref) <- c("Pressure", "Intensity", "Power")
t <- knitr::kable(ref,align=c(rep('c',times=3)), format = "html") 
t

## ----referenceTable2, echo=FALSE----------------------------------------------
ref <- data.frame(row.names = c("1", "2", "3", "4", "5"))
ref[,1] <- c("dBs", "$+3$", "$+6$", "$+10$", "$+20$" )
ref[,2] <- c("Pressure", "$1.41*$", "$2*$", "$3.16*$", "$10*$" )
ref[,3] <- c("Intensity", "$2*$", "$4*$", "$10*$", "$100*$" )
ref[,4] <- c("Power", "$2*$", "$4*$", "$10*$", "$100*$" )
ref[,5] <- c("'Loudness'", "$1.23*$", "$1.52*$", "$2*$", "$4*$" )
t <- knitr::kable(ref,align=c(rep('c',times=5)), format = "html", col.names = NULL) 
t

## ----opDB1--------------------------------------------------------------------
opDB(c(34, 57, 48, 66, 80, 55), 20, op="avg")

## ----opDB2--------------------------------------------------------------------
opDB(c(18, 27, 12, 22, 5, 50), 10, op="add")

