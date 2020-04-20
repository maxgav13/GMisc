## ----setup, include=FALSE-----------------------------------------------------
library(GMisc)
library(tidyverse)

knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  fig.align = "center",
  out.width = "80%"
)

## ----warning=FALSE, message=FALSE---------------------------------------------
W = c(70.0,95.0,80.0,90.0,98.0,110.0,100.0,100.0,110.0)
D = c(40,55,60,50,25,60,40,70,50)
P = c(4.3,3.4,4.5,3.1,5.0,6.4,2.6,11,4.1)
UCS(W = W, D = D, P = P)

## ----echo=F, out.width='50%'--------------------------------------------------
knitr::include_graphics('../inst/GSI_general.PNG')

## ----echo=F, out.width='50%'--------------------------------------------------
knitr::include_graphics('../inst/GSI_flysch.PNG')

## -----------------------------------------------------------------------------
sig.ci = 16
GSI = 75
mi = 13
MR = 300
D = 0
height = 40
unit.weight = 18.6
HB = Hoek_Brown(sig.ci, GSI, mi, MR, D, height, unit.weight)
HB %>% 
  pluck('Results') %>% 
  as.data.frame()

## -----------------------------------------------------------------------------
Hoek_Brown_plot(HB)

## ----warning=FALSE, message=FALSE---------------------------------------------
De = 9
Q = c(.026, 1.4, .078, .56, .35)
Q_93(Q, De)

## ----echo=F, out.width='50%'--------------------------------------------------
knitr::include_graphics('../inst/JRC.png')

## -----------------------------------------------------------------------------
JRC = 10
JCS = 30
phi.r = 26
unit.weight = 18.6
depth = 40
BC = Barton_Choubey(JRC, JCS, phi.r, unit.weight, depth)
BC$parameters
Barton_Choubey_plot(BC, units = 'MPa')

