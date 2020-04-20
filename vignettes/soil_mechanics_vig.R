## ----setup, include=FALSE-----------------------------------------------------
library(GMisc)

knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  fig.align = "center",
  out.width = "80%"
)

## -----------------------------------------------------------------------------
B = seq(0.5, 2, 0.25)
D = seq(0, 2, 0.25)
L = NULL
gamma.h = 15.5
gamma.s = 18.5
tau0 = 10
phi = 30
FS = 3
wl = 1

## -----------------------------------------------------------------------------
bearing_capacity(B, D, L, gamma.h, gamma.s, tau0, phi, wl, FS)

## -----------------------------------------------------------------------------
bearing_capacity(B, D, L, gamma.h, gamma.s, tau0, phi, wl, FS, footing = "square")

## -----------------------------------------------------------------------------
bearing_capacity(B, D, L = 3, gamma.h, gamma.s, tau0, phi, wl, FS, footing = "rectangular")

## ----warning=FALSE------------------------------------------------------------
qs = 1
B = 1
L = NULL
z.end = 8
induced_stress(qs, B, L, z.end)$Plotly

## -----------------------------------------------------------------------------
qs = 1
B = c(1, 2, 4)
L = NULL
z.end = 8
induced_stress(qs, B, L, z.end, footing = "square")$Plotly

## -----------------------------------------------------------------------------
qs = 1
B = 1
L = c(1, 2, 4)
z.end = 8
induced_stress(qs, B, L, z.end, footing = "rectangular")$Plotly

## ----warning=FALSE------------------------------------------------------------
DS_plot(sign = c(80,237,395), tau = c(127,345,475))

## ----warning=FALSE, message=FALSE---------------------------------------------
DS_Mohr_plot(sign = c(80,237,395), tau = c(127,345,475))

## -----------------------------------------------------------------------------
sigx = 143.6
sigy = 100.5
tauxy = -14.4
theta = -35

## -----------------------------------------------------------------------------
Mohr_Circle(sigx, sigy, tauxy)

## -----------------------------------------------------------------------------
Mohr_Circle(sigx, sigy, tauxy, theta)

