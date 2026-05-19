# Ternary Diagram for Pyroclastic Rocks by Type

`ternary_pyroclastic_type()` draws either a static or interactive
ternary diagram, in english or spanish. It is a base diagram where data
can be plotted.

## Usage

``` r
ternary_pyroclastic_type(
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

Ternary diagram for pyroclastic rocks based on type in the desired
format (object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "l", "g", and "c", that way it gets mapped
automatically, if not make sure to use "aes(x=l,y=g,z=c)". For plotting
on the plotly object the mapping of the new data should be as shown in
the example: `a = ~l, b = ~g, c = ~c`, where `a` refers to the top
("l"), `b` refers to the bottom left ("g"), and `c` refers to the bottom
right ("c"). The examples show basic usage and how to add data, which
can be more customizable.

The ternary diagram for pyroclastic rocks based on type is used in
volcanology and sedimentary petrology to classify pyroclastic deposits
based on the relative proportions of different components, specifically
lithic fragments, glass, and crystals. The diagram helps to infer the
eruptive style and depositional processes of volcanic eruptions, as
different types of eruptions produce characteristic component
distributions in the resulting pyroclastic deposits.

## See also

Other classification plots, petrology:
[`TAS()`](https://maxgav13.github.io/GMisc/reference/TAS.md),
[`ternary_afm()`](https://maxgav13.github.io/GMisc/reference/ternary_afm.md),
[`ternary_dickinson_qmflt()`](https://maxgav13.github.io/GMisc/reference/ternary_dickinson_qmflt.md),
[`ternary_dickinson_qtfl()`](https://maxgav13.github.io/GMisc/reference/ternary_dickinson_qtfl.md),
[`ternary_fap()`](https://maxgav13.github.io/GMisc/reference/ternary_fap.md),
[`ternary_feldspars()`](https://maxgav13.github.io/GMisc/reference/ternary_feldspars.md),
[`ternary_folk_gsm()`](https://maxgav13.github.io/GMisc/reference/ternary_folk_gsm.md),
[`ternary_folk_qfl()`](https://maxgav13.github.io/GMisc/reference/ternary_folk_qfl.md),
[`ternary_folk_qfm()`](https://maxgav13.github.io/GMisc/reference/ternary_folk_qfm.md),
[`ternary_folk_smc()`](https://maxgav13.github.io/GMisc/reference/ternary_folk_smc.md),
[`ternary_pyroclastic_size()`](https://maxgav13.github.io/GMisc/reference/ternary_pyroclastic_size.md),
[`ternary_qap()`](https://maxgav13.github.io/GMisc/reference/ternary_qap.md),
[`ternary_qap_g()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_g.md),
[`ternary_qap_m_hbl()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_m_hbl.md),
[`ternary_qap_m_ol()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_m_ol.md),
[`ternary_qap_um()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um.md),
[`ternary_qap_um_hbl()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um_hbl.md)

## Examples

``` r
library(ggplot2)
library(plotly)

d = data.frame(l=c(23,40,60),
               g=c(27,50,30),
               c=c(50,10,10))

# adding data to ggplot object
ternary_pyroclastic_type() + geom_point(data = d)
#> Ignoring unknown labels:
#> • T : "Rock fragments"
#> • L : "Glass"
#> • R : "Crystals"


# adding data to plotly object
ternary_pyroclastic_type('plotly') %>%
  add_trace(a = ~l, b = ~g, c = ~c,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='cyan',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('Rock fragments: %{a}<br>',
                                   'Glass: %{b}<br>',
                                   'Crystals: %{c}'))

{"x":{"visdat":{"4f743cb1568d":["function () ","plotlyVisDat"],"4f741ea9621c":["function () ","data"],"4f742682bfe":["function () ","data"]},"cur_data":"4f742682bfe","attrs":{"4f741ea9621c":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":"Set1","opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","inherit":true},"4f742682bfe":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"cyan","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"Rock fragments: %{a}<br>Glass: %{b}<br>Crystals: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":35,"r":10,"autoexpand":true},"ternary":{"aaxis":{"title":"Rock fragments","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Glass","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Crystals","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Pyroclastic rock <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Pyroclastic_type","width":864,"height":576}},"data":[{"fillcolor":"rgba(228,26,28,0.5)","a":[100,50,35,50,100],"b":[0,50,35,0,0],"c":[0,0,35,50,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Lithic tuff","marker":{"color":"rgba(228,26,28,1)","line":{"color":"rgba(228,26,28,1)"}},"textfont":{"color":"rgba(228,26,28,1)"},"line":{"color":"rgba(228,26,28,1)"},"frame":null,"text":""},{"fillcolor":"rgba(55,126,184,0.5)","a":[0,50,35,0,0],"b":[100,50,35,50,100],"c":[0,0,35,50,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Vitric tuff","marker":{"color":"rgba(55,126,184,1)","line":{"color":"rgba(55,126,184,1)"}},"textfont":{"color":"rgba(55,126,184,1)"},"line":{"color":"rgba(55,126,184,1)"},"frame":null,"text":""},{"fillcolor":"rgba(77,175,74,0.5)","a":[0,50,35,0,0],"b":[0,0,35,50,0],"c":[100,50,35,50,100],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Crystal tuff","marker":{"color":"rgba(77,175,74,1)","line":{"color":"rgba(77,175,74,1)"}},"textfont":{"color":"rgba(77,175,74,1)"},"line":{"color":"rgba(77,175,74,1)"},"frame":null,"text":""},{"a":[23,40,60],"b":[27,50,30],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"cyan","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(214,39,40,1)"}},"hovertemplate":["Rock fragments: %{a}<br>Glass: %{b}<br>Crystals: %{c}","Rock fragments: %{a}<br>Glass: %{b}<br>Crystals: %{c}","Rock fragments: %{a}<br>Glass: %{b}<br>Crystals: %{c}"],"line":{"color":"rgba(214,39,40,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
