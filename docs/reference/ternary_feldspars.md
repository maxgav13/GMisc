# Feldspar Ternary Diagram

`ternary_feldspars()` draws either a static or interactive ternary
diagram, in english or spanish. It is a base diagram where data can be
plotted.

## Usage

``` r
ternary_feldspars(
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

Feldspar ternary diagram in the desired format (object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "p", "f", and "a", that way it gets mapped
automatically, if not make sure to use "aes(x=Na,y=K,z=Ca)". For
plotting on the plotly object the mapping of the new data should be as
shown in the example: `a = ~K, b = ~Na, c = ~Ca`, where `a` refers to
the top ("K"), `b` refers to the bottom left ("Na"), and `c` refers to
the bottom right ("Ca"). The examples show basic usage and how to add
data, which can be more customizable.

## See also

Other classification plots, petrology:
[`TAS()`](https://maxgav13.github.io/GMisc/reference/TAS.md),
[`ternary_afm()`](https://maxgav13.github.io/GMisc/reference/ternary_afm.md),
[`ternary_dickinson_qmflt()`](https://maxgav13.github.io/GMisc/reference/ternary_dickinson_qmflt.md),
[`ternary_dickinson_qtfl()`](https://maxgav13.github.io/GMisc/reference/ternary_dickinson_qtfl.md),
[`ternary_fap()`](https://maxgav13.github.io/GMisc/reference/ternary_fap.md),
[`ternary_folk_gsm()`](https://maxgav13.github.io/GMisc/reference/ternary_folk_gsm.md),
[`ternary_folk_qfl()`](https://maxgav13.github.io/GMisc/reference/ternary_folk_qfl.md),
[`ternary_folk_qfm()`](https://maxgav13.github.io/GMisc/reference/ternary_folk_qfm.md),
[`ternary_folk_smc()`](https://maxgav13.github.io/GMisc/reference/ternary_folk_smc.md),
[`ternary_pyroclastic_size()`](https://maxgav13.github.io/GMisc/reference/ternary_pyroclastic_size.md),
[`ternary_pyroclastic_type()`](https://maxgav13.github.io/GMisc/reference/ternary_pyroclastic_type.md),
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

d = data.frame(K=c(80,5,30),
               Na=c(15,60,65),
               Ca=c(5,35,5))

# adding data to ggplot object
ternary_feldspars() + geom_point(data = d)
#> Ignoring unknown labels:
#> • T : "K"
#> • L : "Na"
#> • R : "Ca"


# adding data to plotly object
ternary_feldspars('plotly') %>%
  add_trace(a = ~K, b = ~Na, c = ~Ca,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='coral',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('K: %{a}<br>',
                                   'Na: %{b}<br>',
                                   'Ca: %{c}'))

{"x":{"visdat":{"4f7439abcffd":["function () ","plotlyVisDat"],"4f743ee4089f":["function () ","data"],"4f746cf21efa":["function () ","data"]},"cur_data":"4f746cf21efa","attrs":{"4f743ee4089f":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#EB483D","#F6CEC9","#C0A3DC","#CAD7EF","#97B0DE","#648BCF","#2C568F","#14243F"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","inherit":true},"4f746cf21efa":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"coral","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"K: %{a}<br>Na: %{b}<br>Ca: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":35,"r":10,"autoexpand":true},"ternary":{"aaxis":{"title":"K","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Na","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Ca","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Feldspars <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Feldspars","width":864,"height":576}},"data":[{"fillcolor":"rgba(235,72,61,0.5)","a":[100,37,31,90,100],"b":[0,63,59,0,0],"c":[0,0,10,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Sanidine / Orthoclase Microcline","marker":{"color":"rgba(235,72,61,1)","line":{"color":"rgba(235,72,61,1)"}},"textfont":{"color":"rgba(235,72,61,1)"},"line":{"color":"rgba(235,72,61,1)"},"frame":null,"text":""},{"fillcolor":"rgba(246,206,201,0.5)","a":[37,31,30,28.300000000000001,26.800000000000001,25,23.699999999999999,22.5,20.899999999999999,20.100000000000001,18.399999999999999,17.399999999999999,16.399999999999999,15.6,14.6,10,10,37],"b":[63,59,60,61.399999999999999,62.5,63.5,64.200000000000003,64.700000000000003,65.200000000000003,65.400000000000006,65.799999999999997,65.900000000000006,66,65.900000000000006,65.799999999999997,80,90,63],"c":[0,10,10,10.300000000000001,10.699999999999999,11.5,12.1,12.9,13.800000000000001,14.5,15.800000000000001,16.699999999999999,17.600000000000001,18.5,19.600000000000001,10,0,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Anorthoclase","marker":{"color":"rgba(246,206,201,1)","line":{"color":"rgba(246,206,201,1)"}},"textfont":{"color":"rgba(246,206,201,1)"},"line":{"color":"rgba(246,206,201,1)"},"frame":null,"text":""},{"fillcolor":"rgba(192,163,220,0.5)","a":[0,10,10,0,0],"b":[100,90,80,90,100],"c":[0,0,10,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Albite","marker":{"color":"rgba(192,163,220,1)","line":{"color":"rgba(192,163,220,1)"}},"textfont":{"color":"rgba(192,163,220,1)"},"line":{"color":"rgba(192,163,220,1)"},"frame":null,"text":""},{"fillcolor":"rgba(202,215,239,0.5)","a":[0,10,14.6,13.9,13.1,12.6,11.800000000000001,11.300000000000001,10.800000000000001,10.4,10,0,0],"b":[90,80,65.799999999999997,65.599999999999994,65.400000000000006,65,64.700000000000003,64.299999999999997,63.899999999999999,63.5,63,70,90],"c":[10,10,19.600000000000001,20.5,21.5,22.399999999999999,23.399999999999999,24.300000000000001,25.199999999999999,26.100000000000001,27,30,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Oligoclase","marker":{"color":"rgba(202,215,239,1)","line":{"color":"rgba(202,215,239,1)"}},"textfont":{"color":"rgba(202,215,239,1)"},"line":{"color":"rgba(202,215,239,1)"},"frame":null,"text":""},{"fillcolor":"rgba(151,176,222,0.5)","a":[0,10,10,0,0],"b":[70,63,45,50,70],"c":[30,27,45,50,30],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Andesine","marker":{"color":"rgba(151,176,222,1)","line":{"color":"rgba(151,176,222,1)"}},"textfont":{"color":"rgba(151,176,222,1)"},"line":{"color":"rgba(151,176,222,1)"},"frame":null,"text":""},{"fillcolor":"rgba(100,139,207,0.5)","a":[0,10,10,0,0],"b":[50,45,27,30,50],"c":[50,45,63,70,50],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Labradorite","marker":{"color":"rgba(100,139,207,1)","line":{"color":"rgba(100,139,207,1)"}},"textfont":{"color":"rgba(100,139,207,1)"},"line":{"color":"rgba(100,139,207,1)"},"frame":null,"text":""},{"fillcolor":"rgba(44,86,143,0.5)","a":[0,10,10,0,0],"b":[30,27,10,10,30],"c":[70,63,80,90,70],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Bytownite","marker":{"color":"rgba(44,86,143,1)","line":{"color":"rgba(44,86,143,1)"}},"textfont":{"color":"rgba(44,86,143,1)"},"line":{"color":"rgba(44,86,143,1)"},"frame":null,"text":""},{"fillcolor":"rgba(20,36,63,0.5)","a":[0,0,10,10,0],"b":[0,10,10,0,0],"c":[100,90,80,90,100],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Anorthite","marker":{"color":"rgba(20,36,63,1)","line":{"color":"rgba(20,36,63,1)"}},"textfont":{"color":"rgba(20,36,63,1)"},"line":{"color":"rgba(20,36,63,1)"},"frame":null,"text":""},{"a":[80,5,30],"b":[15,60,65],"c":[5,35,5],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"coral","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(188,189,34,1)"}},"hovertemplate":["K: %{a}<br>Na: %{b}<br>Ca: %{c}","K: %{a}<br>Na: %{b}<br>Ca: %{c}","K: %{a}<br>Na: %{b}<br>Ca: %{c}"],"line":{"color":"rgba(188,189,34,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
