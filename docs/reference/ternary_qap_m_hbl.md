# QAP Ternary Diagram for Mafic Rocks (horneblende)

`ternary_qap_m_hbl()` draws either a static or interactive ternary
diagram, in english or spanish. It is a base diagram where data can be
plotted.

## Usage

``` r
ternary_qap_m_hbl(
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

QAP ternary diagram for mafic rocks in the desired format (object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "px", "p", and "hbl", that way it gets mapped
automatically, if not make sure to use "aes(x=px,y=p,z=hbl)". For
plotting on the plotly object the mapping of the new data should be as
shown in the example: `a = ~p, b = ~px, c = ~hbl`, where `a` refers to
the top ("p"), `b` refers to the bottom left ("px"), and `c` refers to
the bottom right ("hbl"). The examples show basic usage and how to add
data, which can be more customizable. For a more complete description on
how to use this function and similar ones, see
[`vignette("classification_diagrams_vig")`](https://maxgav13.github.io/GMisc/articles/classification_diagrams_vig.md)

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
[`ternary_qap_m_ol()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_m_ol.md),
[`ternary_qap_um()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um.md),
[`ternary_qap_um_hbl()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um_hbl.md)

## Examples

``` r
library(ggplot2)
library(plotly)

d = data.frame(p=c(23,40,60),
               hbl=c(27,50,30),
               px=c(50,10,10),
               loc=c("A", "B", "C"))

# adding data to ggplot object
ternary_qap_m_hbl() + geom_point(aes(col = loc), data = d)
#> Ignoring unknown labels:
#> • T : "P"
#> • L : "Px"
#> • R : "Hbl"


# adding data to plotly object
ternary_qap_m_hbl('plotly') %>%
  add_trace(a = ~p, b = ~px, c = ~hbl,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='cyan',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('P: %{a}<br>',
                                   'Px: %{b}<br>',
                                   'Hbl: %{c}'))

{"x":{"visdat":{"814411268673":["function () ","plotlyVisDat"],"81447844f0d0":["function () ","data"],"8144761e255f":["function () ","data"]},"cur_data":"8144761e255f","attrs":{"81447844f0d0":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#FAEBDDFF","#F6BF9DFF","#F59067FF","#F05B42FF","#D82848FF","#AE1759FF","#7B1F59FF","#4C1D4BFF"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":"text","hoveron":"fills","inherit":true},"8144761e255f":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"cyan","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"P: %{a}<br>Px: %{b}<br>Hbl: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":35,"r":10,"autoexpand":true},"ternary":{"aaxis":{"title":"P","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Px","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Hbl","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Mafic <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Mafic","width":864,"height":576}},"data":[{"fillcolor":"rgba(250,235,221,0.5)","a":[100,90,90,100],"b":[0,10,0,0],"c":[0,0,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Anorthosite","marker":{"color":"rgba(250,235,221,1)","line":{"color":"rgba(250,235,221,1)"}},"textfont":{"color":"rgba(250,235,221,1)"},"line":{"color":"rgba(250,235,221,1)"},"frame":null,"text":""},{"fillcolor":"rgba(246,191,157,0.5)","a":[90,10,10,90,90],"b":[10,90,85,5,10],"c":[0,0,5,5,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Gabbro/Norite","marker":{"color":"rgba(246,191,157,1)","line":{"color":"rgba(246,191,157,1)"}},"textfont":{"color":"rgba(246,191,157,1)"},"line":{"color":"rgba(246,191,157,1)"},"frame":null,"text":""},{"fillcolor":"rgba(245,144,103,0.5)","a":[90,10,10,90],"b":[5,85,5,5],"c":[5,5,85,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Pyroxene hornblende gabbro/norite","marker":{"color":"rgba(245,144,103,1)","line":{"color":"rgba(245,144,103,1)"}},"textfont":{"color":"rgba(245,144,103,1)"},"line":{"color":"rgba(245,144,103,1)"},"frame":null,"text":""},{"fillcolor":"rgba(240,91,66,0.5)","a":[90,90,10,10,90],"b":[0,5,5,0,0],"c":[10,5,85,90,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Hornblende gabbro","marker":{"color":"rgba(240,91,66,1)","line":{"color":"rgba(240,91,66,1)"}},"textfont":{"color":"rgba(240,91,66,1)"},"line":{"color":"rgba(240,91,66,1)"},"frame":null,"text":""},{"fillcolor":"rgba(216,40,72,0.5)","a":[0,10,0,0],"b":[100,90,90,100],"c":[0,0,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Plagioclase-bearing pyroxenite","marker":{"color":"rgba(216,40,72,1)","line":{"color":"rgba(216,40,72,1)"}},"textfont":{"color":"rgba(216,40,72,1)"},"line":{"color":"rgba(216,40,72,1)"},"frame":null,"text":""},{"fillcolor":"rgba(174,23,89,0.5)","a":[10,10,0,0,10],"b":[90,45,50,90,90],"c":[0,45,50,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Plagioclase-bearing hornblende pyroxenite","marker":{"color":"rgba(174,23,89,1)","line":{"color":"rgba(174,23,89,1)"}},"textfont":{"color":"rgba(174,23,89,1)"},"line":{"color":"rgba(174,23,89,1)"},"frame":null,"text":""},{"fillcolor":"rgba(123,31,89,0.5)","a":[10,10,0,0,10],"b":[45,0,10,50,45],"c":[45,90,90,50,45],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Plagioclase-bearing pyroxene hornblendite","marker":{"color":"rgba(123,31,89,1)","line":{"color":"rgba(123,31,89,1)"}},"textfont":{"color":"rgba(123,31,89,1)"},"line":{"color":"rgba(123,31,89,1)"},"frame":null,"text":""},{"fillcolor":"rgba(76,29,75,0.5)","a":[10,0,0,10],"b":[0,0,10,0],"c":[90,100,90,90],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Plagioclase-bearing hornblendite","marker":{"color":"rgba(76,29,75,1)","line":{"color":"rgba(76,29,75,1)"}},"textfont":{"color":"rgba(76,29,75,1)"},"line":{"color":"rgba(76,29,75,1)"},"frame":null,"text":""},{"a":[23,40,60],"b":[50,10,10],"c":[27,50,30],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"cyan","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(188,189,34,1)"}},"hovertemplate":["P: %{a}<br>Px: %{b}<br>Hbl: %{c}","P: %{a}<br>Px: %{b}<br>Hbl: %{c}","P: %{a}<br>Px: %{b}<br>Hbl: %{c}"],"line":{"color":"rgba(188,189,34,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# customizing colors and symbols by location
my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")

ternary_qap_m_hbl('plotly') %>%
 add_trace(a = ~p, b = ~px, c = ~hbl,
           data = d,
           type = "scatterternary",
           mode = "markers",
           split = ~loc,
           marker = list(size=8,
                         color  = ~my_pal[loc],
                         symbol = ~my_shapes[loc],
                         opacity=.9),
           hovertemplate = paste0('P: %{a}<br>',
                                  'Px: %{b}<br>',
                                  'Hbl: %{c}'))

{"x":{"visdat":{"81443527bc8e":["function () ","plotlyVisDat"],"814443cc29e5":["function () ","data"],"814410d29c29":["function () ","data"]},"cur_data":"814410d29c29","attrs":{"814443cc29e5":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#FAEBDDFF","#F6BF9DFF","#F59067FF","#F05B42FF","#D82848FF","#AE1759FF","#7B1F59FF","#4C1D4BFF"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":"text","hoveron":"fills","inherit":true},"814410d29c29":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"type":"scatterternary","mode":"markers","split":{},"marker":{"size":8,"color":{},"symbol":{},"opacity":0.90000000000000002},"hovertemplate":"P: %{a}<br>Px: %{b}<br>Hbl: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":35,"r":10,"autoexpand":true},"ternary":{"aaxis":{"title":"P","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Px","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Hbl","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Mafic <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Mafic","width":864,"height":576}},"data":[{"fillcolor":"rgba(250,235,221,0.5)","a":[100,90,90,100],"b":[0,10,0,0],"c":[0,0,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Anorthosite","marker":{"color":"rgba(250,235,221,1)","line":{"color":"rgba(250,235,221,1)"}},"textfont":{"color":"rgba(250,235,221,1)"},"line":{"color":"rgba(250,235,221,1)"},"frame":null,"text":""},{"fillcolor":"rgba(246,191,157,0.5)","a":[90,10,10,90,90],"b":[10,90,85,5,10],"c":[0,0,5,5,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Gabbro/Norite","marker":{"color":"rgba(246,191,157,1)","line":{"color":"rgba(246,191,157,1)"}},"textfont":{"color":"rgba(246,191,157,1)"},"line":{"color":"rgba(246,191,157,1)"},"frame":null,"text":""},{"fillcolor":"rgba(245,144,103,0.5)","a":[90,10,10,90],"b":[5,85,5,5],"c":[5,5,85,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Pyroxene hornblende gabbro/norite","marker":{"color":"rgba(245,144,103,1)","line":{"color":"rgba(245,144,103,1)"}},"textfont":{"color":"rgba(245,144,103,1)"},"line":{"color":"rgba(245,144,103,1)"},"frame":null,"text":""},{"fillcolor":"rgba(240,91,66,0.5)","a":[90,90,10,10,90],"b":[0,5,5,0,0],"c":[10,5,85,90,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Hornblende gabbro","marker":{"color":"rgba(240,91,66,1)","line":{"color":"rgba(240,91,66,1)"}},"textfont":{"color":"rgba(240,91,66,1)"},"line":{"color":"rgba(240,91,66,1)"},"frame":null,"text":""},{"fillcolor":"rgba(216,40,72,0.5)","a":[0,10,0,0],"b":[100,90,90,100],"c":[0,0,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Plagioclase-bearing pyroxenite","marker":{"color":"rgba(216,40,72,1)","line":{"color":"rgba(216,40,72,1)"}},"textfont":{"color":"rgba(216,40,72,1)"},"line":{"color":"rgba(216,40,72,1)"},"frame":null,"text":""},{"fillcolor":"rgba(174,23,89,0.5)","a":[10,10,0,0,10],"b":[90,45,50,90,90],"c":[0,45,50,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Plagioclase-bearing hornblende pyroxenite","marker":{"color":"rgba(174,23,89,1)","line":{"color":"rgba(174,23,89,1)"}},"textfont":{"color":"rgba(174,23,89,1)"},"line":{"color":"rgba(174,23,89,1)"},"frame":null,"text":""},{"fillcolor":"rgba(123,31,89,0.5)","a":[10,10,0,0,10],"b":[45,0,10,50,45],"c":[45,90,90,50,45],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Plagioclase-bearing pyroxene hornblendite","marker":{"color":"rgba(123,31,89,1)","line":{"color":"rgba(123,31,89,1)"}},"textfont":{"color":"rgba(123,31,89,1)"},"line":{"color":"rgba(123,31,89,1)"},"frame":null,"text":""},{"fillcolor":"rgba(76,29,75,0.5)","a":[10,0,0,10],"b":[0,0,10,0],"c":[90,100,90,90],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Plagioclase-bearing hornblendite","marker":{"color":"rgba(76,29,75,1)","line":{"color":"rgba(76,29,75,1)"}},"textfont":{"color":"rgba(76,29,75,1)"},"line":{"color":"rgba(76,29,75,1)"},"frame":null,"text":""},{"a":[23],"b":[50],"c":[27],"type":"scatterternary","mode":"markers","marker":{"color":"red","size":8,"symbol":"circle","opacity":0.90000000000000002,"line":{"color":"rgba(188,189,34,1)"}},"hovertemplate":"P: %{a}<br>Px: %{b}<br>Hbl: %{c}","name":"A","line":{"color":"rgba(188,189,34,1)"},"frame":null},{"a":[40],"b":[10],"c":[50],"type":"scatterternary","mode":"markers","marker":{"color":"blue","size":8,"symbol":"square","opacity":0.90000000000000002,"line":{"color":"rgba(23,190,207,1)"}},"hovertemplate":"P: %{a}<br>Px: %{b}<br>Hbl: %{c}","name":"B","line":{"color":"rgba(23,190,207,1)"},"frame":null},{"a":[60],"b":[10],"c":[30],"type":"scatterternary","mode":"markers","marker":{"color":"green","size":8,"symbol":"diamond","opacity":0.90000000000000002,"line":{"color":"rgba(31,119,180,1)"}},"hovertemplate":"P: %{a}<br>Px: %{b}<br>Hbl: %{c}","name":"C","line":{"color":"rgba(31,119,180,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
