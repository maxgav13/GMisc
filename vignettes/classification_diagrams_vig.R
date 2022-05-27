## ----setup, include=FALSE-----------------------------------------------------
library(kableExtra)
library(GMisc)
library(plotly)
library(ggplot2)

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
d = data.frame(sample=c('a','b'),
               qt=c(40,30),
               f=c(40,20),
               l=c(20,10))

d2 = data.frame(silica=c(58,70), 
                alkali=c(8,4))

## -----------------------------------------------------------------------------
d %>% 
  kable(caption = 'Samples for QtFL diagram') %>% 
  kable_classic()

## -----------------------------------------------------------------------------
d2 %>% 
  kable(caption = 'Samples for TAS diagram') %>% 
  kable_classic()

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
d3 = data.frame(Group = c('A','A','B','B'),
                Ca = c(120,150,110,52.6),
                Mg = c(78,160,110,28),
                Na = c(210,590,340,51.6),
                K = c(4.2,2,3.6,2.3),
                HCO3 = c(181,181,189,151),
                CO3 = 0,
                Cl = c(220,744,476,72.2),
                SO4 = c(560,1020,584,126))

piper_data = piper_data_prep(d3)
piper_data %>% 
  kable(caption = 'Processed sample data for Piper diagram') %>% 
  kable_classic()

## -----------------------------------------------------------------------------
piper_diagram() +
   geom_point(aes(x,y,col=Group,shape=Group),
              size=3,
              data = piper_data) +
   scale_color_brewer('Group',palette = 'Dark2') +
   scale_shape_manual('Group',values = c(3,21))

## -----------------------------------------------------------------------------
diagrams.tern = data.frame(
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
  add_header_above(c('','ggplot'=3,'plotly'=3)) %>% 
  kable_classic()

## -----------------------------------------------------------------------------
ternary_shepard(language = 'es')

## -----------------------------------------------------------------------------
TAS(language = 'es')

