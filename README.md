<p><a href="https://img.shields.io/github/issues/MattyD797/bioSNR"> <img alt="Issues" src="https://img.shields.io/github/issues/MattyD797/bioSNR"/>
  </a> <a href=" https://img.shields.io/github/r-package/v/MattyD797/bioSNR">
    <img alt="Issues" src="https://img.shields.io/github/r-package/v/MattyD797/bioSNR" />
  </a>  <a href="https://img.shields.io/github/last-commit/MattyD797/bioSNR">
    <img alt="Issues" src="https://img.shields.io/github/last-commit/MattyD797/bioSNR" />
  </a> </p>

# bioSNR: Simplifying the Physics Behind Bioacoustics and the Passive Sonar Equation

# Background

This R package is a toolbox to help those in ecology who want to deepen their understanding or utilize Bioacoustics in their work. 
The package has a number of utilization from calculating frequency from waveform, performing operations in dB, and determining acoustic range of recorders. 
The majority of this package is based on key concepts learned from the K. Lisa Yang Center for Conservation Bioacoustics at Cornell University and 
their associated course: Introduction to Bioacoustics course. More than not in the course, we utilized the R programming language to perform 
simple operations, density estimations, and sound localization, but a majority of the functions we utilize in experimental development 
have not been incorporated into R. Thus, the idea to take this as an opportunity to further the reach of the course to an open-source platform. 

A majority of these functions are changing with the developing field of bioacoustics and as of 2023, to our knowledge, this is the most up-to-date formats. A majority of these
functions that are a simulation tool to get a better idea on the number of recorders one will need to set up a bioacoustic experiment, especially in 
marine environments. As this package develops, we hope that the simulation tools developed for marine environments will grow into more terrestrial applications and as a whole be more well developed in this package. In addition
to simulation development, we aspire that this package will inspire others to take bioacoustics as an environmental parameter in interdisciplinary work.

## Installation

The bioSNR package is an open-source SONAR equation calculator. The calculator is capable of handling simple to intermediate level acoustic problems associated with bioacoustics and passive acoustic monitoring (PAM) systems. 

```
#Beta version install
devtools::install_github("MattyD797/bioSNR")
library(bioSNR)
```

## Authors

- [Matthew Duggan](https://github.com/MattyD797)
- [Marissa Garcia](https://github.com/mlynngarcia)

## Acknowledgements

We would like to thank the K. Lisa Yang Center for Conservation Bioacoustics at Cornell University for providing supporting material. 
This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE – 2139899. Any opinion, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation or the K. Lisa Yang Center for Conservation Bioacoustics.






