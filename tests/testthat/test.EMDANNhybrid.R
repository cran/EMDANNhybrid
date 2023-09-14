library(testthat)
require(EMDANNhybrid)

#Dataset generation
set.seed(6)
data <- rnorm(300,6.6,.36)

#Parameter setting
k <-  0.7
l <-  1
n <- 5
r <- 20
m <- 120

#Application of EMDANN model
EMDANNhybrid(data,k,l,n,r,m)

