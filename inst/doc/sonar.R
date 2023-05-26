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

## ----wenz, echo=FALSE, fig.align = 'center', message=FALSE--------------------
freqBand <- c(0,0)
shipT <- -1
seaState <- -1
wSpeed <- 0
boolR <- T
    # This code re-traces the empirical Wenz curves, that describe Oceanic ambient noise as a function of frequency.
    freqMax <- 10000
    freq <- pracma::logspace(0,6,freqMax)


    #Geophysical contribution for Freq<10 Hz
    #N_geophys = 107 - 30log10(f)
    intervalG <- freq[1:tail(which(freq<10)+1, 1)]
    NL_geophys <- 107 - 30 * log10(intervalG)

    NL_geophys2 <- cbind(intervalG, NL_geophys)
    colnames(NL_geophys2)[1] <- "freq"

    ## Contribution of ship traffic (5 <= f <= 500 Hz)
    # - 1 - 2 low ship traffic
    # - 3-4-5 standard ship traffic
    # - 6 - 7 heavy ship traffic
    # - 8 - 9 intense ship traffic

    # N_ship = 75 -40log10(f/30) + 5(n_shiptraffic - 4)

    shipLines <- function(trafficIndex, freqI=freq, freqmax = 10000){

      interval <- freqI[head(which(freqI > 5), 1):tail(which(freqI < 500), 1)]
      NL_ship<- 76 - (20*(log10(interval/30))^2) + 5*(trafficIndex-4)
      return(NL_ship)
    }

    shipTraffic <- sapply(c(1:9), shipLines)
    shipTraffic2 <- cbind(freq[head(which(freq > 5), 1):tail(which(freq < 500), 1)], shipTraffic)
    colnames(shipTraffic2) <- c("freq",
                                "shipTraffic.1",
                                "shipTraffic.2",
                                "shipTraffic.3",
                                "shipTraffic.4",
                                "shipTraffic.5",
                                "shipTraffic.6",
                                "shipTraffic.7",
                                "shipTraffic.8",
                                "shipTraffic.9")

    ## Wind speed and sea state  (80 <= f <= 300 kHz)
    # Wind speed in knots - related to sea state as Vkn = 5 * sea state

    windSpeed <- function(windSpeed, freqI=freq, freqmax = 10000){
      #between 80 - 1000 Hz
      # < 1000 Hz
      interval1 <- head(which(freqI > 80)-1, 1):(tail(which(freqI < 1000)-1, 1))
      NL_wind <- 44 + sqrt(21*windSpeed) + 17 * (3 - log10(freqI[interval1])) * (log10(freqI[interval1])-2)
      # >= 1000 Hz
      interval2 <- head(which(freqI > 1000)-1, 1):tail(which(freqI < 300000)-1, 1)
      NL_wind <- append(NL_wind, 95 + sqrt(21*windSpeed) - 17*log10(freqI[interval2]))
      return(NL_wind)
    }

    if(seaState != -1){
      wSpeed = seaState * 5
    }

    wind <- sapply(c(0, 10, 15, 20, 25, 30, 40, 45), windSpeed)
    intervalW <- freq[head(which(freq > 80)-1, 1):tail(which(freq < 300000)-1, 1)]
    wind2 <- cbind(intervalW, wind)
    colnames(wind2) <- c("freq", "seaState.0", "seaState.10", "seaState.15", "seaState.20", "seaState.25", "seaState.30", "seaState.40", "seaState.45")

    ## Thermal noise (f > 50 kHz)
    # NLthermal = -75 + 20log10(f)
    intervalT <- freq[head(which(freq > 50000), 1):freqMax]
    NL_thermal <- -75 + 20 * log10(intervalT)
    NL_thermal2 <- cbind(intervalT, NL_thermal)
    colnames(NL_thermal2)[1] <- "freq"

    freq <- as.data.frame(freq)
    colnames(freq) <- "freq"

    #Solve for given bandwidth
    bandW <- freq[freq>=freqBand[1] & freq <= freqBand[2]]
    gpInputF <- vector()
    stInputF <- vector()
    wInputF <- vector()
    tnInputF <- vector()
    gpInput <- vector()
    stInput <- vector()
    wInput <- vector()
    tnInput <- vector()

    for(f in bandW){
      #Only geophysical SL
      if(f < 10){
        gpInput<- c(gpInput, 107 - 30 * log10(f))
        gpInputF <- c(gpInputF, f)

      }
      if (f >= 5 & f <= 500){
        if(shipT < 0 ){
          if(f <= 10){
            stop("ERROR: shipT is not specified")
          }
        } else {
          stInput <- c(stInput, 76 - (20*(log10(f/30))^2) + 5*(shipT-4))
          stInputF <- c(stInputF, f)
        }
      }
      if(f>=80 & f < 1000){
        wInput <- c(wInput, 44 + sqrt(21*wSpeed) + 17 * (3 - log10(f)) * (log10(f)-2))
        wInputF <- c(wInputF, f)
      }
      if(f>=1000 & f <= 300000){
        wInput <- c(wInput, 95 + sqrt(21*wSpeed) - 17*log10(f))
        wInputF <- c(wInputF, f)
      }
      if(f>50000){
        tnInput <- c(tnInput,-75 + 20 * log10(f))
        tnInputF <- c(tnInputF, f)
      }
    }

    gpInput2 <- cbind(gpInputF, gpInput)
    colnames(gpInput2)[1] <- "freq"
    stInput2 <- cbind(stInputF, stInput)
    colnames(stInput2)[1] <- "freq"
    wInput2 <- cbind(wInputF, wInput)
    colnames(wInput2)[1] <- "freq"
    tnInput2 <- cbind(tnInputF, tnInput)
    colnames(tnInput2)[1] <- "freq"


    df_list <- list(freq, NL_geophys2, shipTraffic2, wind2, NL_thermal2, gpInput2, stInput2, wInput2, tnInput2)
    dt <- Reduce(function(x, y) merge(x, y, by = "freq", all = TRUE), df_list)

    dt <- dplyr::mutate(dt, gpInput = as.numeric(gpInput),
             stInput = as.numeric(stInput),
             wInput = as.numeric(wInput),
             tnInput = as.numeric(tnInput))

    suppressWarnings(print(ggplot2::ggplot(dt, ggplot2::aes(freq)) +
      ggplot2::geom_line(ggplot2::aes(y=shipTraffic.1))+
      ggplot2::geom_line(ggplot2::aes(y=shipTraffic.2))+
      ggplot2::geom_line(ggplot2::aes(y=shipTraffic.3))+
      ggplot2::geom_line(ggplot2::aes(y=shipTraffic.4))+
      ggplot2::geom_line(ggplot2::aes(y=shipTraffic.5))+
      ggplot2::geom_line(ggplot2::aes(y=shipTraffic.6))+
      ggplot2::geom_line(ggplot2::aes(y=shipTraffic.7))+
      ggplot2::geom_line(ggplot2::aes(y=shipTraffic.8))+
      ggplot2::geom_line(ggplot2::aes(y=shipTraffic.9))+
      ggplot2::scale_x_continuous(trans='log10', ,
                         breaks = scales::trans_breaks("log10", function(x) 10^x),
                         labels = scales::trans_format("log10", scales::math_format(10^.x)),
                         n.breaks = 25)+
      ggplot2::geom_line(ggplot2::aes(y=NL_geophys))+
      ggplot2::geom_line(ggplot2::aes(y=seaState.0))+
      ggplot2::geom_line(ggplot2::aes(y=seaState.10))+
      ggplot2::geom_line(ggplot2::aes(y=seaState.15))+
      ggplot2::geom_line(ggplot2::aes(y=seaState.20))+
      ggplot2::geom_line(ggplot2::aes(y=seaState.25))+
      ggplot2::geom_line(ggplot2::aes(y=seaState.30))+
      ggplot2::geom_line(ggplot2::aes(y=seaState.40))+
      ggplot2::geom_line(ggplot2::aes(y=seaState.45))+
      ggplot2::geom_line(ggplot2::aes(y=NL_thermal))+
      ggplot2::ylab(as.expression(bquote("Spectrum Level (dB ref 1 \u00B5" ~ Pa^2 ~ ')')))+
      ggplot2::xlab("Frequency (Hz)") +
      ggplot2::geom_point(ggplot2::aes(y=gpInput))+
      ggplot2::geom_point(ggplot2::aes(y=stInput))+
      ggplot2::geom_point(ggplot2::aes(y=wInput))+
      ggplot2::geom_point(ggplot2::aes(y=tnInput))+
      ggplot2::geom_vline(xintercept = freqBand[1])+
      ggplot2::geom_vline(xintercept = freqBand[2])+
      ggplot2::annotation_logticks(sides = "b")))

## ----ex1, fig.align = 'center', warning=FALSE---------------------------------
specLvlGraph(c(28,33), ship=4,seaState = 1, wSpeed = 10, boolR = T)[[2]]

## ----ex2, fig.align = 'center'------------------------------------------------
#Source Level
SL <- 195
#Noise Level - from above
NL <- 82.9897
#Detection Threshold
DT <- 10
#Transition Range <- depth/2
TR <- 2500

rmax(sl=SL,nl=NL,dt=DT,d=5000, xaxis=10000000)

