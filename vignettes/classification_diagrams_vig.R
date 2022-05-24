## ----setup, include=FALSE-----------------------------------------------------
library(kableExtra)
library(GMisc)
library(plotly)
library(tidyverse)

knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  fig.align = "center",
  out.width = "80%"
)

## -----------------------------------------------------------------------------
ternary_folk()

## -----------------------------------------------------------------------------
ternary_qap_m(output = 'plotly')

## -----------------------------------------------------------------------------
TAS(output = 'plotly')

## -----------------------------------------------------------------------------
d = tibble(sample=c('a','b'),
           qt=c(40,30),
           f=c(40,20),
           l=c(20,10))

d2 = tibble(silica=c(58,70), 
            alkali=c(8,4))

## -----------------------------------------------------------------------------
d %>% 
  kable(caption = 'Samples for QtFL diagram') %>% 
  kable_classic(full_width=F)

## -----------------------------------------------------------------------------
d2 %>% 
  kable(caption = 'Samples for TAS diagram') %>% 
  kable_classic(full_width=F)

## -----------------------------------------------------------------------------
ternary_qtfl() + 
  geom_point(data = d,
             color = 'coral',
             size = 3,
             shape = 3,
             alpha = .7)

## -----------------------------------------------------------------------------
TAS() + 
  geom_point(aes(x=silica, y=alkali),
             data = d2,
             color = 'coral',
             size = 3,
             shape = 3,
             alpha = .7)

## -----------------------------------------------------------------------------
ternary_qtfl(output = 'plotly') %>% 
  add_trace(a = ~qt, b = ~f, c = ~l,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size = 8,
                          color = 'green',
                          symbol = 4,
                          opacity = .7),
            hovertemplate = paste0('Qt: %{a}<br>',
                                   'F: %{b}<br>',
                                   'L: %{c}'))

## -----------------------------------------------------------------------------
TAS('plotly') %>%
  add_markers(x = ~silica, y = ~alkali,
              data = d2,
              name = "My data",
              marker = list(size=8,color='orange',symbol=3,opacity=.9)) %>%
  layout(showlegend = TRUE)

## -----------------------------------------------------------------------------
diagrams.tern = tibble(
    diagram = c('afm','folk','pyroclastic','qap_g','qap_m','qap_um','qap','qmflt','qtfl','shepard'),
    x = c('a','f','lapilli','cpx','ol','opx','a','f','f','sand'),
    y = c('f','q','bb','p','p','ol','q','qm','qt','clay'),
    z = c('m','r','ash','opx','px','cpx','p','lt','l','silt'),
    a = c('~f','~q','~bb','~p','~p','~ol','~q','~qm','~qt','~clay'),
    b = c('~a','~f','~lapilli','~cpx','~ol','~opx','~a','~f','~f','~sand'),
    c = c('~m','~r','~ash','~opx','px','~cpx','~p','~lt','~l','~silt')
)

diagrams.tern %>% 
  kable(caption = 'Mapping of the data to the respective axis for both outputs, for the ternary diagrams.') %>% 
  add_header_above(c('','ggplot2'=3,'plotly'=3)) %>% 
  kable_classic()

## -----------------------------------------------------------------------------
ternary_shepard(language = 'es')

## -----------------------------------------------------------------------------
TAS(language = 'es')

