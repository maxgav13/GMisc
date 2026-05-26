# QAP Ternary Diagram for Mafic Rocks (olivine)

`ternary_qap_m_ol()` draws either a static or interactive ternary
diagram, in english or spanish. It is a base diagram where data can be
plotted.

## Usage

``` r
ternary_qap_m_ol(
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
of the dataframe are "ol", "p", and "px", that way it gets mapped
automatically, if not make sure to use "aes(x=ol,y=p,z=px)". For
plotting on the plotly object the mapping of the new data should be as
shown in the example: `a = ~p, b = ~ol, c = ~px`, where `a` refers to
the top ("p"), `b` refers to the bottom left ("ol"), and `c` refers to
the bottom right ("px"). The examples show basic usage and how to add
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
[`ternary_qap_m_hbl()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_m_hbl.md),
[`ternary_qap_um()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um.md),
[`ternary_qap_um_hbl()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um_hbl.md)

## Examples

``` r
library(ggplot2)
library(plotly)

d = data.frame(p=c(23,40,60),
               ol=c(27,50,30),
               px=c(50,10,10),
               loc=c("A", "B", "C"))

# adding data to ggplot object
ternary_qap_m_ol() + geom_point(aes(col = loc), data = d)
#> Ignoring unknown labels:
#> • T : "P"
#> • L : "Ol"
#> • R : "Px"


# adding data to plotly object
ternary_qap_m_ol('plotly') %>%
  add_trace(a = ~p, b = ~ol, c = ~px,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='cyan',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('P: %{a}<br>',
                                   'Ol: %{b}<br>',
                                   'Cpx: %{c}'))

{"x":{"visdat":{"42114d26d0e5":["function () ","plotlyVisDat"],"42112f5c91f5":["function () ","data"],"42116672841d":["function () ","data"]},"cur_data":"42116672841d","attrs":{"42112f5c91f5":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#FAEBDDFF","#F6BF9DFF","#F59067FF","#F05B42FF","#D82848FF","#AE1759FF","#7B1F59FF","#4C1D4BFF"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":"text","hoveron":"fills","inherit":true},"42116672841d":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"cyan","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"P: %{a}<br>Ol: %{b}<br>Cpx: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"ternary":{"aaxis":{"title":"P","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Ol","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Px","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Mafic <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Mafic","width":864,"height":576}},"data":[{"fillcolor":"rgba(250,235,221,0.5)","a":[100,90,90],"b":[0,10,0],"c":[0,0,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Anorthosite","marker":{"color":"rgba(250,235,221,1)","line":{"color":"rgba(250,235,221,1)"}},"textfont":{"color":"rgba(250,235,221,1)"},"frame":null,"text":""},{"fillcolor":"rgba(246,191,157,0.5)","a":[90,10,5,90],"b":[10,90,90,5],"c":[0,0,5,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Troctolite","marker":{"color":"rgba(246,191,157,1)","line":{"color":"rgba(246,191,157,1)"}},"textfont":{"color":"rgba(246,191,157,1)"},"frame":null,"text":""},{"fillcolor":"rgba(245,144,103,0.5)","a":[90,5,5],"b":[5,90,5],"c":[5,5,90],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Olivine gabbro /<br />        Olivine norite","marker":{"color":"rgba(245,144,103,1)","line":{"color":"rgba(245,144,103,1)"}},"textfont":{"color":"rgba(245,144,103,1)"},"frame":null,"text":""},{"fillcolor":"rgba(240,91,66,0.5)","a":[90,90,5,10],"b":[0,5,5,0],"c":[10,5,90,90],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Gabbro / Norite","marker":{"color":"rgba(240,91,66,1)","line":{"color":"rgba(240,91,66,1)"}},"textfont":{"color":"rgba(240,91,66,1)"},"frame":null,"text":""},{"fillcolor":"rgba(216,40,72,0.5)","a":[10,0,0],"b":[90,100,90],"c":[0,0,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Dunite","marker":{"color":"rgba(216,40,72,1)","line":{"color":"rgba(216,40,72,1)"}},"textfont":{"color":"rgba(216,40,72,1)"},"frame":null,"text":""},{"fillcolor":"rgba(174,23,89,0.5)","a":[5,0,0,5],"b":[90,90,42.5,40],"c":[5,10,57.5,55],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Wehrlite /<br />        Harzburgite","marker":{"color":"rgba(174,23,89,1)","line":{"color":"rgba(174,23,89,1)"}},"textfont":{"color":"rgba(174,23,89,1)"},"frame":null,"text":""},{"fillcolor":"rgba(123,31,89,0.5)","a":[5,0,0,5],"b":[40,42.5,10,5],"c":[55,57.5,90,90],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Olivine clino/orto-pyroxenite","marker":{"color":"rgba(123,31,89,1)","line":{"color":"rgba(123,31,89,1)"}},"textfont":{"color":"rgba(123,31,89,1)"},"frame":null,"text":""},{"fillcolor":"rgba(76,29,75,0.5)","a":[10,0,0],"b":[0,10,0],"c":[90,90,100],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Clinopyroxenite /<br />        Orthopyroxenite","marker":{"color":"rgba(76,29,75,1)","line":{"color":"rgba(76,29,75,1)"}},"textfont":{"color":"rgba(76,29,75,1)"},"frame":null,"text":""},{"a":[23,40,60],"b":[27,50,30],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"cyan","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(188,189,34,1)"}},"hovertemplate":["P: %{a}<br>Ol: %{b}<br>Cpx: %{c}","P: %{a}<br>Ol: %{b}<br>Cpx: %{c}","P: %{a}<br>Ol: %{b}<br>Cpx: %{c}"],"line":{"color":"rgba(188,189,34,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# customizing colors and symbols by location
my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")

ternary_qap_m_ol('plotly') %>%
  add_trace(a = ~p, b = ~ol, c = ~px,
            data = d,
            type = "scatterternary",
            mode = "markers",
            split = ~loc,
            marker = list(size=8,
                          color  = ~my_pal[loc],
                          symbol = ~my_shapes[loc],
                          opacity=.9),
            hovertemplate = paste0('P: %{a}<br>',
                                   'Ol: %{b}<br>',
                                   'Cpx: %{c}'))

{"x":{"visdat":{"4211683fc076":["function () ","plotlyVisDat"],"42113174b872":["function () ","data"],"421165f95dbb":["function () ","data"]},"cur_data":"421165f95dbb","attrs":{"42113174b872":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#FAEBDDFF","#F6BF9DFF","#F59067FF","#F05B42FF","#D82848FF","#AE1759FF","#7B1F59FF","#4C1D4BFF"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":"text","hoveron":"fills","inherit":true},"421165f95dbb":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"type":"scatterternary","mode":"markers","split":{},"marker":{"size":8,"color":{},"symbol":{},"opacity":0.90000000000000002},"hovertemplate":"P: %{a}<br>Ol: %{b}<br>Cpx: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"ternary":{"aaxis":{"title":"P","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Ol","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Px","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Mafic <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Mafic","width":864,"height":576}},"data":[{"fillcolor":"rgba(250,235,221,0.5)","a":[100,90,90],"b":[0,10,0],"c":[0,0,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Anorthosite","marker":{"color":"rgba(250,235,221,1)","line":{"color":"rgba(250,235,221,1)"}},"textfont":{"color":"rgba(250,235,221,1)"},"frame":null,"text":""},{"fillcolor":"rgba(246,191,157,0.5)","a":[90,10,5,90],"b":[10,90,90,5],"c":[0,0,5,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Troctolite","marker":{"color":"rgba(246,191,157,1)","line":{"color":"rgba(246,191,157,1)"}},"textfont":{"color":"rgba(246,191,157,1)"},"frame":null,"text":""},{"fillcolor":"rgba(245,144,103,0.5)","a":[90,5,5],"b":[5,90,5],"c":[5,5,90],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Olivine gabbro /<br />        Olivine norite","marker":{"color":"rgba(245,144,103,1)","line":{"color":"rgba(245,144,103,1)"}},"textfont":{"color":"rgba(245,144,103,1)"},"frame":null,"text":""},{"fillcolor":"rgba(240,91,66,0.5)","a":[90,90,5,10],"b":[0,5,5,0],"c":[10,5,90,90],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Gabbro / Norite","marker":{"color":"rgba(240,91,66,1)","line":{"color":"rgba(240,91,66,1)"}},"textfont":{"color":"rgba(240,91,66,1)"},"frame":null,"text":""},{"fillcolor":"rgba(216,40,72,0.5)","a":[10,0,0],"b":[90,100,90],"c":[0,0,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Dunite","marker":{"color":"rgba(216,40,72,1)","line":{"color":"rgba(216,40,72,1)"}},"textfont":{"color":"rgba(216,40,72,1)"},"frame":null,"text":""},{"fillcolor":"rgba(174,23,89,0.5)","a":[5,0,0,5],"b":[90,90,42.5,40],"c":[5,10,57.5,55],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Wehrlite /<br />        Harzburgite","marker":{"color":"rgba(174,23,89,1)","line":{"color":"rgba(174,23,89,1)"}},"textfont":{"color":"rgba(174,23,89,1)"},"frame":null,"text":""},{"fillcolor":"rgba(123,31,89,0.5)","a":[5,0,0,5],"b":[40,42.5,10,5],"c":[55,57.5,90,90],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Olivine clino/orto-pyroxenite","marker":{"color":"rgba(123,31,89,1)","line":{"color":"rgba(123,31,89,1)"}},"textfont":{"color":"rgba(123,31,89,1)"},"frame":null,"text":""},{"fillcolor":"rgba(76,29,75,0.5)","a":[10,0,0],"b":[0,10,0],"c":[90,90,100],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Clinopyroxenite /<br />        Orthopyroxenite","marker":{"color":"rgba(76,29,75,1)","line":{"color":"rgba(76,29,75,1)"}},"textfont":{"color":"rgba(76,29,75,1)"},"frame":null,"text":""},{"a":[23],"b":[27],"c":[50],"type":"scatterternary","mode":"markers","marker":{"color":"red","size":8,"symbol":"circle","opacity":0.90000000000000002,"line":{"color":"rgba(188,189,34,1)"}},"hovertemplate":"P: %{a}<br>Ol: %{b}<br>Cpx: %{c}","name":"A","line":{"color":"rgba(188,189,34,1)"},"frame":null},{"a":[40],"b":[50],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"blue","size":8,"symbol":"square","opacity":0.90000000000000002,"line":{"color":"rgba(23,190,207,1)"}},"hovertemplate":"P: %{a}<br>Ol: %{b}<br>Cpx: %{c}","name":"B","line":{"color":"rgba(23,190,207,1)"},"frame":null},{"a":[60],"b":[30],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"green","size":8,"symbol":"diamond","opacity":0.90000000000000002,"line":{"color":"rgba(31,119,180,1)"}},"hovertemplate":"P: %{a}<br>Ol: %{b}<br>Cpx: %{c}","name":"C","line":{"color":"rgba(31,119,180,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
