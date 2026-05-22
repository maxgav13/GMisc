# QAP Ternary Diagram for Gabbroic Rocks

`ternary_qap_g()` draws either a static or interactive ternary diagram,
in english or spanish. It is a base diagram where data can be plotted.

## Usage

``` r
ternary_qap_g(
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

QAP ternary diagram for gabbroic rocks in the desired format (object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "cpx", "p", and "opx", that way it gets mapped
automatically, if not make sure to use "aes(x=cpx,y=p,z=opx)". For
plotting on the plotly object the mapping of the new data should be as
shown in the example: `a = ~p, b = ~cpx, c = ~opx`, where `a` refers to
the top ("p"), `b` refers to the bottom left ("cpx"), and `c` refers to
the bottom right ("opx"). The examples show basic usage and how to add
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
[`ternary_qap_m_hbl()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_m_hbl.md),
[`ternary_qap_m_ol()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_m_ol.md),
[`ternary_qap_um()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um.md),
[`ternary_qap_um_hbl()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um_hbl.md)

## Examples

``` r
library(ggplot2)
library(plotly)

d = data.frame(p=c(23,40,60),
               cpx=c(27,50,30),
               opx=c(50,10,10),
               loc=c("A", "B", "C"))

# adding data to ggplot object
ternary_qap_g() + geom_point(aes(col = loc), data = d)
#> Ignoring unknown labels:
#> • T : "P"
#> • L : "Cpx"
#> • R : "Opx"


# adding data to plotly object
ternary_qap_g('plotly') %>%
  add_trace(a = ~p, b = ~cpx, c = ~opx,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='coral',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('P: %{a}<br>',
                                   'Cpx: %{b}<br>',
                                   'Opx: %{c}'))

{"x":{"visdat":{"81443a0aae52":["function () ","plotlyVisDat"],"8144133aa743":["function () ","data"],"81446fbb2991":["function () ","data"]},"cur_data":"81446fbb2991","attrs":{"8144133aa743":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#DEF5E5FF","#8AD9B1FF","#40B7ADFF","#348FA7FF","#37659EFF","#413D7BFF","#2E1E3CFF","#0B0405FF"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":"text","hoveron":"fills","inherit":true},"81446fbb2991":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"coral","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"P: %{a}<br>Cpx: %{b}<br>Opx: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"ternary":{"aaxis":{"title":"P","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Cpx","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Opx","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Gabros <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Gabbros","width":864,"height":576}},"data":[{"fillcolor":"rgba(222,245,229,0.5)","a":[100,90,90],"b":[0,10,0],"c":[0,0,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Anorthosite","marker":{"color":"rgba(222,245,229,1)","line":{"color":"rgba(222,245,229,1)"}},"textfont":{"color":"rgba(222,245,229,1)"},"frame":null,"text":""},{"fillcolor":"rgba(138,217,177,0.5)","a":[90,10,10,90],"b":[10,90,85,5],"c":[0,0,5,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Gabbro","marker":{"color":"rgba(138,217,177,1)","line":{"color":"rgba(138,217,177,1)"}},"textfont":{"color":"rgba(138,217,177,1)"},"frame":null,"text":""},{"fillcolor":"rgba(64,183,173,0.5)","a":[90,10,10],"b":[5,85,45],"c":[5,5,45],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Orthopyroxene gabbro","marker":{"color":"rgba(64,183,173,1)","line":{"color":"rgba(64,183,173,1)"}},"textfont":{"color":"rgba(64,183,173,1)"},"frame":null,"text":""},{"fillcolor":"rgba(52,143,167,0.5)","a":[90,10,10],"b":[5,45,5],"c":[5,45,85],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Clinopyroxene norite","marker":{"color":"rgba(52,143,167,1)","line":{"color":"rgba(52,143,167,1)"}},"textfont":{"color":"rgba(52,143,167,1)"},"frame":null,"text":""},{"fillcolor":"rgba(55,101,158,0.5)","a":[90,10,10,90],"b":[5,5,0,0],"c":[5,85,90,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Norite","marker":{"color":"rgba(55,101,158,1)","line":{"color":"rgba(55,101,158,1)"}},"textfont":{"color":"rgba(55,101,158,1)"},"frame":null,"text":""},{"fillcolor":"rgba(65,61,123,0.5)","a":[10,0,0],"b":[90,100,90],"c":[0,0,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Clinopyroxenite","marker":{"color":"rgba(65,61,123,1)","line":{"color":"rgba(65,61,123,1)"}},"textfont":{"color":"rgba(65,61,123,1)"},"frame":null,"text":""},{"fillcolor":"rgba(46,30,60,0.5)","a":[10,0,0,10],"b":[90,90,10,0],"c":[0,10,90,90],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Websterite","marker":{"color":"rgba(46,30,60,1)","line":{"color":"rgba(46,30,60,1)"}},"textfont":{"color":"rgba(46,30,60,1)"},"frame":null,"text":""},{"fillcolor":"rgba(11,4,5,0.5)","a":[10,0,0],"b":[0,10,0],"c":[90,90,100],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Orthopyroxenite","marker":{"color":"rgba(11,4,5,1)","line":{"color":"rgba(11,4,5,1)"}},"textfont":{"color":"rgba(11,4,5,1)"},"frame":null,"text":""},{"a":[23,40,60],"b":[27,50,30],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"coral","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(188,189,34,1)"}},"hovertemplate":["P: %{a}<br>Cpx: %{b}<br>Opx: %{c}","P: %{a}<br>Cpx: %{b}<br>Opx: %{c}","P: %{a}<br>Cpx: %{b}<br>Opx: %{c}"],"line":{"color":"rgba(188,189,34,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# adding data to plotly object with custom colors
my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")

ternary_qap_g('plotly') %>%
  add_trace(a = ~p, b = ~cpx, c = ~opx,
            data = d,
            type = "scatterternary",
            mode = "markers",
            split = ~loc,
            marker = list(size=8,
                          color  = ~my_pal[loc],
                          symbol = ~my_shapes[loc],
                          opacity=.9),
            hovertemplate = paste0('P: %{a}<br>',
                                   'Cpx: %{b}<br>',
                                   'Opx: %{c}'))

{"x":{"visdat":{"814468a627e5":["function () ","plotlyVisDat"],"81447485610f":["function () ","data"],"814460a75b8c":["function () ","data"]},"cur_data":"814460a75b8c","attrs":{"81447485610f":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#DEF5E5FF","#8AD9B1FF","#40B7ADFF","#348FA7FF","#37659EFF","#413D7BFF","#2E1E3CFF","#0B0405FF"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":"text","hoveron":"fills","inherit":true},"814460a75b8c":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"type":"scatterternary","mode":"markers","split":{},"marker":{"size":8,"color":{},"symbol":{},"opacity":0.90000000000000002},"hovertemplate":"P: %{a}<br>Cpx: %{b}<br>Opx: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"ternary":{"aaxis":{"title":"P","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Cpx","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Opx","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Gabros <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Gabbros","width":864,"height":576}},"data":[{"fillcolor":"rgba(222,245,229,0.5)","a":[100,90,90],"b":[0,10,0],"c":[0,0,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Anorthosite","marker":{"color":"rgba(222,245,229,1)","line":{"color":"rgba(222,245,229,1)"}},"textfont":{"color":"rgba(222,245,229,1)"},"frame":null,"text":""},{"fillcolor":"rgba(138,217,177,0.5)","a":[90,10,10,90],"b":[10,90,85,5],"c":[0,0,5,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Gabbro","marker":{"color":"rgba(138,217,177,1)","line":{"color":"rgba(138,217,177,1)"}},"textfont":{"color":"rgba(138,217,177,1)"},"frame":null,"text":""},{"fillcolor":"rgba(64,183,173,0.5)","a":[90,10,10],"b":[5,85,45],"c":[5,5,45],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Orthopyroxene gabbro","marker":{"color":"rgba(64,183,173,1)","line":{"color":"rgba(64,183,173,1)"}},"textfont":{"color":"rgba(64,183,173,1)"},"frame":null,"text":""},{"fillcolor":"rgba(52,143,167,0.5)","a":[90,10,10],"b":[5,45,5],"c":[5,45,85],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Clinopyroxene norite","marker":{"color":"rgba(52,143,167,1)","line":{"color":"rgba(52,143,167,1)"}},"textfont":{"color":"rgba(52,143,167,1)"},"frame":null,"text":""},{"fillcolor":"rgba(55,101,158,0.5)","a":[90,10,10,90],"b":[5,5,0,0],"c":[5,85,90,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Norite","marker":{"color":"rgba(55,101,158,1)","line":{"color":"rgba(55,101,158,1)"}},"textfont":{"color":"rgba(55,101,158,1)"},"frame":null,"text":""},{"fillcolor":"rgba(65,61,123,0.5)","a":[10,0,0],"b":[90,100,90],"c":[0,0,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Clinopyroxenite","marker":{"color":"rgba(65,61,123,1)","line":{"color":"rgba(65,61,123,1)"}},"textfont":{"color":"rgba(65,61,123,1)"},"frame":null,"text":""},{"fillcolor":"rgba(46,30,60,0.5)","a":[10,0,0,10],"b":[90,90,10,0],"c":[0,10,90,90],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Websterite","marker":{"color":"rgba(46,30,60,1)","line":{"color":"rgba(46,30,60,1)"}},"textfont":{"color":"rgba(46,30,60,1)"},"frame":null,"text":""},{"fillcolor":"rgba(11,4,5,0.5)","a":[10,0,0],"b":[0,10,0],"c":[90,90,100],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Orthopyroxenite","marker":{"color":"rgba(11,4,5,1)","line":{"color":"rgba(11,4,5,1)"}},"textfont":{"color":"rgba(11,4,5,1)"},"frame":null,"text":""},{"a":[23],"b":[27],"c":[50],"type":"scatterternary","mode":"markers","marker":{"color":"red","size":8,"symbol":"circle","opacity":0.90000000000000002,"line":{"color":"rgba(188,189,34,1)"}},"hovertemplate":"P: %{a}<br>Cpx: %{b}<br>Opx: %{c}","name":"A","line":{"color":"rgba(188,189,34,1)"},"frame":null},{"a":[40],"b":[50],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"blue","size":8,"symbol":"square","opacity":0.90000000000000002,"line":{"color":"rgba(23,190,207,1)"}},"hovertemplate":"P: %{a}<br>Cpx: %{b}<br>Opx: %{c}","name":"B","line":{"color":"rgba(23,190,207,1)"},"frame":null},{"a":[60],"b":[30],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"green","size":8,"symbol":"diamond","opacity":0.90000000000000002,"line":{"color":"rgba(31,119,180,1)"}},"hovertemplate":"P: %{a}<br>Cpx: %{b}<br>Opx: %{c}","name":"C","line":{"color":"rgba(31,119,180,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
