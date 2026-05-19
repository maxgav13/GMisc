# Shepard's Ternary Diagram for Soils

`ternary_shepard()` draws either a static or interactive ternary
diagram, in english or spanish. It is a base diagram where data can be
plotted.

## Usage

``` r
ternary_shepard(
  output = c("ggplot", "plotly"),
  language = c("en", "es"),
  opacity = 0.5
)
```

## Arguments

- output:

  The output format: "ggplot" or "plotly" (default is "ggplot")

- language:

  The language to be displayed: "en" for english or "es" for spanish
  (deafult is "en")

- opacity:

  Transparency level (default is 0.5)

## Value

Shepard's ternary diagram for soils in the desired format (object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "sand", "clay", and "silt", that way it gets mapped
automatically, if not make sure to use "aes(x=sand,y=clay,z=silt)". For
plotting on the plotly object the mapping of the new data should be as
shown in the example: `a = ~clay, b = ~sand, c = ~silt`, where `a`
refers to the top ("clay"), `b` refers to the bottom left ("sand"), and
`c` refers to the bottom right ("silt"). The examples show basic usage
and how to add data, which can be more customizable.

## See also

Other classification plots, soil mechanics:
[`Casagrande_chart()`](https://maxgav13.github.io/GMisc/reference/Casagrande_chart.md),
[`clay_activity()`](https://maxgav13.github.io/GMisc/reference/clay_activity.md),
[`soil_collapse()`](https://maxgav13.github.io/GMisc/reference/soil_collapse.md),
[`soil_expansion()`](https://maxgav13.github.io/GMisc/reference/soil_expansion.md),
[`soil_swelling()`](https://maxgav13.github.io/GMisc/reference/soil_swelling.md),
[`ternary_usda()`](https://maxgav13.github.io/GMisc/reference/ternary_usda.md)

## Examples

``` r
library(ggplot2)
library(plotly)

d = data.frame(sand=c(23,40,60),
               clay=c(27,50,30),
               silt=c(50,10,10))

# adding data to ggplot object
ternary_shepard() + geom_point(data = d)
#> Ignoring unknown labels:
#> • T : "Clay"
#> • L : "Sand"
#> • R : "Silt"


# adding data to plotly object
ternary_shepard('plotly') %>%
  add_trace(a = ~clay, b = ~sand, c = ~silt,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='blue',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('Clay: %{a}<br>',
                                   'Sand: %{b}<br>',
                                   'Silt: %{c}'))

{"x":{"visdat":{"4f743e64044c":["function () ","plotlyVisDat"],"4f7418563994":["function () ","data"],"4f7444de3007":["function () ","data"]},"cur_data":"4f7444de3007","attrs":{"4f7418563994":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":"Set3","opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","inherit":true},"4f7444de3007":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"blue","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"Clay: %{a}<br>Sand: %{b}<br>Silt: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"annotations":[{"text":"Shepard","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"Shepard","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"Shepard","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2}],"ternary":{"aaxis":{"title":"Clay","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Sand","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Silt","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Soil <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Shepard","width":864,"height":576}},"data":[{"fillcolor":"rgba(141,211,199,0.5)","a":[1,0.75,0.75],"b":[0,0.25,0],"c":[0,0,0.25],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Clay","marker":{"color":"rgba(141,211,199,1)","line":{"color":"rgba(141,211,199,1)"}},"textfont":{"color":"rgba(141,211,199,1)"},"line":{"color":"rgba(141,211,199,1)"},"frame":null,"text":""},{"fillcolor":"rgba(255,255,179,0.5)","a":[0.5,0.75,0.75,0.59999999999999998,0.40000000000000002],"b":[0.5,0.25,0.125,0.20000000000000001,0.40000000000000002],"c":[0,0,0.125,0.20000000000000001,0.20000000000000001],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Sandy Clay","marker":{"color":"rgba(255,255,179,1)","line":{"color":"rgba(255,255,179,1)"}},"textfont":{"color":"rgba(255,255,179,1)"},"line":{"color":"rgba(255,255,179,1)"},"frame":null,"text":""},{"fillcolor":"rgba(190,186,218,0.5)","a":[0.75,0.75,0.5,0.40000000000000002,0.59999999999999998],"b":[0.125,0,0,0.20000000000000001,0.20000000000000001],"c":[0.125,0.25,0.5,0.40000000000000002,0.20000000000000001],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Silty Clay","marker":{"color":"rgba(190,186,218,1)","line":{"color":"rgba(190,186,218,1)"}},"textfont":{"color":"rgba(190,186,218,1)"},"line":{"color":"rgba(190,186,218,1)"},"frame":null,"text":""},{"fillcolor":"rgba(251,128,114,0.5)","a":[0.59999999999999998,0.20000000000000001,0.20000000000000001],"b":[0.20000000000000001,0.20000000000000001,0.59999999999999998],"c":[0.20000000000000001,0.59999999999999998,0.20000000000000001],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Sand + Silt + Clay","marker":{"color":"rgba(251,128,114,1)","line":{"color":"rgba(251,128,114,1)"}},"textfont":{"color":"rgba(251,128,114,1)"},"line":{"color":"rgba(251,128,114,1)"},"frame":null,"text":""},{"fillcolor":"rgba(128,177,211,0.5)","a":[0.5,0.40000000000000002,0.20000000000000001,0.125,0.25],"b":[0.5,0.40000000000000002,0.59999999999999998,0.75,0.75],"c":[0,0.20000000000000001,0.20000000000000001,0.125,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Clayey Sand","marker":{"color":"rgba(128,177,211,1)","line":{"color":"rgba(128,177,211,1)"}},"textfont":{"color":"rgba(128,177,211,1)"},"line":{"color":"rgba(128,177,211,1)"},"frame":null,"text":""},{"fillcolor":"rgba(253,180,98,0.5)","a":[0.5,0.25,0.125,0.20000000000000001,0.40000000000000002],"b":[0,0,0.125,0.20000000000000001,0.20000000000000001],"c":[0.5,0.75,0.75,0.59999999999999998,0.40000000000000002],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Clayey Silt","marker":{"color":"rgba(253,180,98,1)","line":{"color":"rgba(253,180,98,1)"}},"textfont":{"color":"rgba(253,180,98,1)"},"line":{"color":"rgba(253,180,98,1)"},"frame":null,"text":""},{"fillcolor":"rgba(179,222,105,0.5)","a":[0,0.25,0],"b":[1,0.75,0.75],"c":[0,0,0.25],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Sand","marker":{"color":"rgba(179,222,105,1)","line":{"color":"rgba(179,222,105,1)"}},"textfont":{"color":"rgba(179,222,105,1)"},"line":{"color":"rgba(179,222,105,1)"},"frame":null,"text":""},{"fillcolor":"rgba(252,205,229,0.5)","a":[0.125,0.20000000000000001,0.20000000000000001,0,0],"b":[0.75,0.59999999999999998,0.40000000000000002,0.5,0.75],"c":[0.125,0.20000000000000001,0.40000000000000002,0.5,0.25],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Silty Sand","marker":{"color":"rgba(252,205,229,1)","line":{"color":"rgba(252,205,229,1)"}},"textfont":{"color":"rgba(252,205,229,1)"},"line":{"color":"rgba(252,205,229,1)"},"frame":null,"text":""},{"fillcolor":"rgba(217,217,217,0.5)","a":[0.20000000000000001,0.20000000000000001,0.125,0,0],"b":[0.40000000000000002,0.20000000000000001,0.125,0.25,0.5],"c":[0.40000000000000002,0.59999999999999998,0.75,0.75,0.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Sandy Silt","marker":{"color":"rgba(217,217,217,1)","line":{"color":"rgba(217,217,217,1)"}},"textfont":{"color":"rgba(217,217,217,1)"},"line":{"color":"rgba(217,217,217,1)"},"frame":null,"text":""},{"fillcolor":"rgba(188,128,189,0.5)","a":[0.25,0,0],"b":[0,0,0.25],"c":[0.75,1,0.75],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Silt","marker":{"color":"rgba(188,128,189,1)","line":{"color":"rgba(188,128,189,1)"}},"textfont":{"color":"rgba(188,128,189,1)"},"line":{"color":"rgba(188,128,189,1)"},"frame":null,"text":""},{"a":[27,50,30],"b":[23,40,60],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"blue","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(31,119,180,1)"}},"hovertemplate":["Clay: %{a}<br>Sand: %{b}<br>Silt: %{c}","Clay: %{a}<br>Sand: %{b}<br>Silt: %{c}","Clay: %{a}<br>Sand: %{b}<br>Silt: %{c}"],"line":{"color":"rgba(31,119,180,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
