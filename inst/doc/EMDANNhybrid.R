## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------

##Example how the package works
library(EMDANNhybrid)

#Application
# A Random time series dataset generation
set.seed(6)
data <- rnorm(300,6.6,0.36)

#Parameter setting
k <-  0.7
l <-  1
n <- 5
r <- 20
m <- 120

#Application of EMDANN model
EMDANNhybrid(data,k,l,n,r,m)


