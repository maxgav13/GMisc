# QAP Ternary Diagram for Ultramafic Rocks with Hornblende

`ternary_qap_um_hbl()` draws either a static or interactive ternary
diagram, in english or spanish. It is a base diagram where data can be
plotted.

## Usage

``` r
ternary_qap_um_hbl(
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

QAP ternary diagram for ultramafic rocks with hornblende in the desired
format (object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "px", "ol", and "hbl", that way it gets mapped
automatically, if not make sure to use "aes(x=px,y=ol,z=hbl)". For
plotting on the plotly object the mapping of the new data should be as
shown in the example: `a = ~ol, b = ~px, c = ~hbl`, where `a` refers to
the top ("ol"), `b` refers to the bottom left ("px"), and `c` refers to
the bottom right ("hbl"). The examples show basic usage and how to add
data, which can be more customizable.

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
[`ternary_pyroclastic_type()`](https://maxgav13.github.io/GMisc/reference/ternary_pyroclastic_type.md),
[`ternary_qap()`](https://maxgav13.github.io/GMisc/reference/ternary_qap.md),
[`ternary_qap_g()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_g.md),
[`ternary_qap_m_hbl()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_m_hbl.md),
[`ternary_qap_m_ol()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_m_ol.md),
[`ternary_qap_um()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um.md)

## Examples

``` r
library(ggplot2)
library(plotly)

d = data.frame(ol=c(23,40,60),
               px=c(27,50,30),
               hbl=c(50,10,10))

# adding data to ggplot object
ternary_qap_um_hbl() + geom_point(data = d)
#> Ignoring unknown labels:
#> • T : "Ol"
#> • L : "Px"
#> • R : "Hbl"


# adding data to plotly object
ternary_qap_um_hbl('plotly') %>%
  add_trace(a = ~ol, b = ~px, c = ~hbl,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='coral',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('Ol: %{a}<br>',
                                   'Px: %{b}<br>',
                                   'Hbl: %{c}'))

{"x":{"visdat":{"4f741a9471f6":["function () ","plotlyVisDat"],"4f747c5db1c":["function () ","data"],"4f744eb40f40":["function () ","data"]},"cur_data":"4f744eb40f40","attrs":{"4f747c5db1c":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#FDE725FF","#C2DF23FF","#85D54AFF","#51C56AFF","#2BB07FFF","#1E9B8AFF","#25858EFF","#2D708EFF","#38598CFF","#433E85FF","#482173FF","#440154FF"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":"text","hoveron":"fills","inherit":true},"4f744eb40f40":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"coral","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"Ol: %{a}<br>Px: %{b}<br>Hbl: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":35,"r":10,"autoexpand":true},"ternary":{"aaxis":{"title":"Ol","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Px","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Hbl","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Ultramafic <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Ultramafic_hbl","width":864,"height":576}},"data":[{"fillcolor":"rgba(253,231,37,0.5)","a":[100,90,90,100],"b":[0,10,0,0],"c":[0,0,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Dunite","marker":{"color":"rgba(253,231,37,1)","line":{"color":"rgba(253,231,37,1)"}},"textfont":{"color":"rgba(253,231,37,1)"},"line":{"color":"rgba(253,231,37,1)"},"frame":null,"text":""},{"fillcolor":"rgba(194,223,35,0.5)","a":[90,40,40,90,90],"b":[10,60,55,5,10],"c":[0,0,5,5,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Pyroxene peridotite","marker":{"color":"rgba(194,223,35,1)","line":{"color":"rgba(194,223,35,1)"}},"textfont":{"color":"rgba(194,223,35,1)"},"line":{"color":"rgba(194,223,35,1)"},"frame":null,"text":""},{"fillcolor":"rgba(133,213,74,0.5)","a":[90,40,40,90],"b":[5,55,5,5],"c":[5,5,55,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Pyroxene hornblende peridotite","marker":{"color":"rgba(133,213,74,1)","line":{"color":"rgba(133,213,74,1)"}},"textfont":{"color":"rgba(133,213,74,1)"},"line":{"color":"rgba(133,213,74,1)"},"frame":null,"text":""},{"fillcolor":"rgba(81,197,106,0.5)","a":[90,40,40,90,90],"b":[5,5,0,0,5],"c":[5,55,60,10,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Hornblende peridotite","marker":{"color":"rgba(81,197,106,1)","line":{"color":"rgba(81,197,106,1)"}},"textfont":{"color":"rgba(81,197,106,1)"},"line":{"color":"rgba(81,197,106,1)"},"frame":null,"text":""},{"fillcolor":"rgba(43,176,127,0.5)","a":[40,10,5,40,40],"b":[60,90,90,55,60],"c":[0,0,5,5,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Olivine pyroxenite","marker":{"color":"rgba(43,176,127,1)","line":{"color":"rgba(43,176,127,1)"}},"textfont":{"color":"rgba(43,176,127,1)"},"line":{"color":"rgba(43,176,127,1)"},"frame":null,"text":""},{"fillcolor":"rgba(30,155,138,0.5)","a":[40,5,5,40,40],"b":[55,90,47.5,30,55],"c":[5,5,47.5,30,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Olivine hornblende pyroxenite","marker":{"color":"rgba(30,155,138,1)","line":{"color":"rgba(30,155,138,1)"}},"textfont":{"color":"rgba(30,155,138,1)"},"line":{"color":"rgba(30,155,138,1)"},"frame":null,"text":""},{"fillcolor":"rgba(37,133,142,0.5)","a":[40,40,5,5,40],"b":[30,5,5,47.5,30],"c":[30,55,90,47.5,30],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Olivine pyroxene hornblendite","marker":{"color":"rgba(37,133,142,1)","line":{"color":"rgba(37,133,142,1)"}},"textfont":{"color":"rgba(37,133,142,1)"},"line":{"color":"rgba(37,133,142,1)"},"frame":null,"text":""},{"fillcolor":"rgba(45,112,142,0.5)","a":[40,5,10,40,40],"b":[5,5,0,0,5],"c":[55,90,90,60,55],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Olivine hornblendite","marker":{"color":"rgba(45,112,142,1)","line":{"color":"rgba(45,112,142,1)"}},"textfont":{"color":"rgba(45,112,142,1)"},"line":{"color":"rgba(45,112,142,1)"},"frame":null,"text":""},{"fillcolor":"rgba(56,89,140,0.5)","a":[10,0,0,10],"b":[90,100,90,90],"c":[0,0,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Pyroxenite","marker":{"color":"rgba(56,89,140,1)","line":{"color":"rgba(56,89,140,1)"}},"textfont":{"color":"rgba(56,89,140,1)"},"line":{"color":"rgba(56,89,140,1)"},"frame":null,"text":""},{"fillcolor":"rgba(67,62,133,0.5)","a":[5,5,0,0,5],"b":[90,47.5,50,90,90],"c":[5,47.5,50,10,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Hornblende pyroxenite","marker":{"color":"rgba(67,62,133,1)","line":{"color":"rgba(67,62,133,1)"}},"textfont":{"color":"rgba(67,62,133,1)"},"line":{"color":"rgba(67,62,133,1)"},"frame":null,"text":""},{"fillcolor":"rgba(72,33,115,0.5)","a":[5,5,0,0,5],"b":[47.5,5,10,50,47.5],"c":[47.5,90,90,50,47.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Pyroxene hornblendite","marker":{"color":"rgba(72,33,115,1)","line":{"color":"rgba(72,33,115,1)"}},"textfont":{"color":"rgba(72,33,115,1)"},"line":{"color":"rgba(72,33,115,1)"},"frame":null,"text":""},{"fillcolor":"rgba(68,1,84,0.5)","a":[10,0,0,10],"b":[0,10,0,0],"c":[90,90,100,90],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Hornblendite","marker":{"color":"rgba(68,1,84,1)","line":{"color":"rgba(68,1,84,1)"}},"textfont":{"color":"rgba(68,1,84,1)"},"line":{"color":"rgba(68,1,84,1)"},"frame":null,"text":""},{"a":[23,40,60],"b":[27,50,30],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"coral","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(44,160,44,1)"}},"hovertemplate":["Ol: %{a}<br>Px: %{b}<br>Hbl: %{c}","Ol: %{a}<br>Px: %{b}<br>Hbl: %{c}","Ol: %{a}<br>Px: %{b}<br>Hbl: %{c}"],"line":{"color":"rgba(44,160,44,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
