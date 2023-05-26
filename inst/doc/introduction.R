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

## ----wof1---------------------------------------------------------------------
#Find the wavelength (m) given a sound level of 75 kHz (75000 Hz) in SALTWATER
wof(40000, c=15000)

## ----wof2---------------------------------------------------------------------
#Find the frequency (Hz) given a wavelength of 0.015 in AIR
wof(0.012, c=350)

## ----soundSpeed1--------------------------------------------------------------
#Speed of sound in air at 23°C 
soundSpeed(23)
#Speed of sound in air at 3°C 
soundSpeed(3)

## ----soundSpeed2--------------------------------------------------------------
#Speed of fish sound in saltwater
soundSpeed(30, "water", 35, 10)

## ----Snells1------------------------------------------------------------------
snell(64,1564,1494)

## ----Snells2------------------------------------------------------------------
snell(15.5,1493,1502)

## ----sine graph, echo=FALSE, fig.align = 'center'-----------------------------
#simulated sine sound wave
t=seq(0,4*pi,0.1)
y=2*sin(t)
s <- as.data.frame(cbind(t,y), ncol=2)

ggplot2::ggplot(s, ggplot2::aes(t,y))+
  ggplot2::geom_line()+
  ggplot2::geom_hline(yintercept=0, 
                      size=1) + 
  ggplot2::geom_segment(ggplot2::aes(x = pi/2, y = 0, xend = pi/2, yend = 2),
                  arrow = grid::arrow(length = grid::unit(0.25, "cm")), 
                  color = "blue") +
  ggplot2::geom_segment(ggplot2::aes(x = 5*pi/2, y = 0, xend = 5*pi/2, yend = 2),
                  arrow = grid::arrow(length = grid::unit(0.25, "cm")), 
                  color = "red") +
  ggplot2::geom_segment(ggplot2::aes(x = 5*pi/2, y = 0, xend = 5*pi/2, yend = -2),
                  arrow = grid::arrow(length = grid::unit(0.25, "cm")), 
                  color = "red") +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.background = ggplot2::element_blank(), 
                 axis.text.x = ggplot2::element_blank(), 
                 axis.ticks.x = ggplot2::element_blank())+
  ggplot2::ylab(expression("Pressure (" * mu ~ "Pa)"))+
  ggplot2::xlab("")
  

## ----absorption1--------------------------------------------------------------
absorptionWater(48,7.75,18,34,2)

## ----absorption2--------------------------------------------------------------
absorptionAir(10000,101.325,20,99)

## ----impedance----------------------------------------------------------------
#Assume standard impedance in water
z <- 14.8

#pressure values for example
press <- c(20, 24, 18, 34, 51, 29, 29, 15)

#pressure root mean square of pressure values
prms <- sqrt(mean(press^2))

#Formula for impedance
prms^2/z


