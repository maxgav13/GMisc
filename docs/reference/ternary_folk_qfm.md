# Folk's Ternary Diagram for Sedimentary Rocks

`ternary_folk_qfm()` draws either a static or interactive ternary
diagram, in english or spanish. It is a base diagram where data can be
plotted.

## Usage

``` r
ternary_folk_qfm(
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

Folk's ternary diagram for sandstones in the desired format (object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "f", "q", and "m", that way it gets mapped
automatically, if not make sure to use "aes(x=f,y=q,z=m)". For plotting
on the plotly object the mapping of the new data should be as shown in
the example: `a = ~q, b = ~f, c = ~m`, where `a` refers to the top
("q"), `b` refers to the bottom left ("f"), and `c` refers to the bottom
right ("m"). The examples show basic usage and how to add data, which
can be more customizable. For a more complete description on how to use
this function and similar ones, see
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

d = data.frame(q=c(23,40,60),
               f=c(27,50,30),
               m=c(50,10,10),
               loc=c("A", "B", "C"))

# adding data to ggplot object
ternary_folk_qfm() + geom_point(aes(col = loc), data = d)
#> Ignoring unknown labels:
#> • T : "Q"
#> • L : "F"
#> • R : "M"


# adding data to plotly object
ternary_folk_qfm('plotly') %>%
  add_trace(a = ~q, b = ~f, c = ~m,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='coral',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('Q: %{a}<br>',
                                   'F: %{b}<br>',
                                   'M: %{c}'))

{"x":{"visdat":{"81445ac87388":["function () ","plotlyVisDat"],"81441e191448":["function () ","data"],"8144829268":["function () ","data"]},"cur_data":"8144829268","attrs":{"81441e191448":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#777777","#AACDC9","#C0CF98","#B2CEAC","#2F4996","#D3C6E2","#EBD1C0","#564E37"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","inherit":true},"8144829268":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"coral","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"Q: %{a}<br>F: %{b}<br>M: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":35,"r":10,"autoexpand":true},"ternary":{"aaxis":{"title":"Q","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"F","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"M","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Sedimentary rock <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Folk_qfm","width":864,"height":576}},"data":[{"fillcolor":"rgba(119,119,119,0.5)","a":[100,95,91.599999999999994,95,100],"b":[0,5,4.2000000000000002,0,0],"c":[0,0,4.2000000000000002,5,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Quartzarenite","marker":{"color":"rgba(119,119,119,1)","line":{"color":"rgba(119,119,119,1)"}},"textfont":{"color":"rgba(119,119,119,1)"},"line":{"color":"rgba(119,119,119,1)"},"frame":null,"text":""},{"fillcolor":"rgba(170,205,201,0.5)","a":[95,75,66,80,91.599999999999994,95],"b":[5,25,24,10,4.2000000000000002,5],"c":[0,0,10,10,4.2000000000000002,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Subarkose","marker":{"color":"rgba(170,205,201,1)","line":{"color":"rgba(170,205,201,1)"}},"textfont":{"color":"rgba(170,205,201,1)"},"line":{"color":"rgba(170,205,201,1)"},"frame":null,"text":""},{"fillcolor":"rgba(192,207,152,0.5)","a":[95,75,66,80,91.599999999999994,95],"b":[0,0,10,10,4.2000000000000002,0],"c":[5,25,24,10,4.2000000000000002,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Subgraywacke","marker":{"color":"rgba(192,207,152,1)","line":{"color":"rgba(192,207,152,1)"}},"textfont":{"color":"rgba(192,207,152,1)"},"line":{"color":"rgba(192,207,152,1)"},"frame":null,"text":""},{"fillcolor":"rgba(178,206,172,0.5)","a":[66,80,66,52.5,66],"b":[24,10,10,23.75,24],"c":[10,10,24,23.75,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Feldspathic subgraywacke","marker":{"color":"rgba(178,206,172,1)","line":{"color":"rgba(178,206,172,1)"}},"textfont":{"color":"rgba(178,206,172,1)"},"line":{"color":"rgba(178,206,172,1)"},"frame":null,"text":""},{"fillcolor":"rgba(47,73,150,0.5)","a":[75,0,0,66,75],"b":[25,100,90,24,25],"c":[0,0,10,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Arkose","marker":{"color":"rgba(47,73,150,1)","line":{"color":"rgba(47,73,150,1)"}},"textfont":{"color":"rgba(47,73,150,1)"},"line":{"color":"rgba(47,73,150,1)"},"frame":null,"text":""},{"fillcolor":"rgba(211,198,226,0.5)","a":[0,66,52.5,0,0],"b":[90,24,23.75,50,90],"c":[10,10,23.75,50,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Impure Arkose","marker":{"color":"rgba(211,198,226,1)","line":{"color":"rgba(211,198,226,1)"}},"textfont":{"color":"rgba(211,198,226,1)"},"line":{"color":"rgba(211,198,226,1)"},"frame":null,"text":""},{"fillcolor":"rgba(235,209,192,0.5)","a":[52.5,66,0,0,52.5],"b":[23.75,10,10,50,23.75],"c":[23.75,24,90,50,23.75],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Feldspathic graywacke","marker":{"color":"rgba(235,209,192,1)","line":{"color":"rgba(235,209,192,1)"}},"textfont":{"color":"rgba(235,209,192,1)"},"line":{"color":"rgba(235,209,192,1)"},"frame":null,"text":""},{"fillcolor":"rgba(86,78,55,0.5)","a":[66,75,0,0,66],"b":[10,0,0,10,10],"c":[24,25,100,90,24],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Graywacke","marker":{"color":"rgba(86,78,55,1)","line":{"color":"rgba(86,78,55,1)"}},"textfont":{"color":"rgba(86,78,55,1)"},"line":{"color":"rgba(86,78,55,1)"},"frame":null,"text":""},{"a":[23,40,60],"b":[27,50,30],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"coral","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(188,189,34,1)"}},"hovertemplate":["Q: %{a}<br>F: %{b}<br>M: %{c}","Q: %{a}<br>F: %{b}<br>M: %{c}","Q: %{a}<br>F: %{b}<br>M: %{c}"],"line":{"color":"rgba(188,189,34,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# adding data to plotly object with custom colors
my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")

ternary_folk_qfm('plotly') %>%
  add_trace(a = ~q, b = ~f, c = ~m,
            data = d,
            type = "scatterternary",
            mode = "markers",
            split = ~loc,
            marker = list(size=8,
                          color  = ~my_pal[loc],
                          symbol = ~my_shapes[loc],
                          opacity=.9),
            hovertemplate = paste0('Q: %{a}<br>',
                                   'F: %{b}<br>',
                                   'M: %{c}'))

{"x":{"visdat":{"81447c59ea1a":["function () ","plotlyVisDat"],"8144731890bd":["function () ","data"],"814451c6a353":["function () ","data"]},"cur_data":"814451c6a353","attrs":{"8144731890bd":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#777777","#AACDC9","#C0CF98","#B2CEAC","#2F4996","#D3C6E2","#EBD1C0","#564E37"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","inherit":true},"814451c6a353":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"type":"scatterternary","mode":"markers","split":{},"marker":{"size":8,"color":{},"symbol":{},"opacity":0.90000000000000002},"hovertemplate":"Q: %{a}<br>F: %{b}<br>M: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":35,"r":10,"autoexpand":true},"ternary":{"aaxis":{"title":"Q","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"F","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"M","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Sedimentary rock <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Folk_qfm","width":864,"height":576}},"data":[{"fillcolor":"rgba(119,119,119,0.5)","a":[100,95,91.599999999999994,95,100],"b":[0,5,4.2000000000000002,0,0],"c":[0,0,4.2000000000000002,5,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Quartzarenite","marker":{"color":"rgba(119,119,119,1)","line":{"color":"rgba(119,119,119,1)"}},"textfont":{"color":"rgba(119,119,119,1)"},"line":{"color":"rgba(119,119,119,1)"},"frame":null,"text":""},{"fillcolor":"rgba(170,205,201,0.5)","a":[95,75,66,80,91.599999999999994,95],"b":[5,25,24,10,4.2000000000000002,5],"c":[0,0,10,10,4.2000000000000002,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Subarkose","marker":{"color":"rgba(170,205,201,1)","line":{"color":"rgba(170,205,201,1)"}},"textfont":{"color":"rgba(170,205,201,1)"},"line":{"color":"rgba(170,205,201,1)"},"frame":null,"text":""},{"fillcolor":"rgba(192,207,152,0.5)","a":[95,75,66,80,91.599999999999994,95],"b":[0,0,10,10,4.2000000000000002,0],"c":[5,25,24,10,4.2000000000000002,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Subgraywacke","marker":{"color":"rgba(192,207,152,1)","line":{"color":"rgba(192,207,152,1)"}},"textfont":{"color":"rgba(192,207,152,1)"},"line":{"color":"rgba(192,207,152,1)"},"frame":null,"text":""},{"fillcolor":"rgba(178,206,172,0.5)","a":[66,80,66,52.5,66],"b":[24,10,10,23.75,24],"c":[10,10,24,23.75,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Feldspathic subgraywacke","marker":{"color":"rgba(178,206,172,1)","line":{"color":"rgba(178,206,172,1)"}},"textfont":{"color":"rgba(178,206,172,1)"},"line":{"color":"rgba(178,206,172,1)"},"frame":null,"text":""},{"fillcolor":"rgba(47,73,150,0.5)","a":[75,0,0,66,75],"b":[25,100,90,24,25],"c":[0,0,10,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Arkose","marker":{"color":"rgba(47,73,150,1)","line":{"color":"rgba(47,73,150,1)"}},"textfont":{"color":"rgba(47,73,150,1)"},"line":{"color":"rgba(47,73,150,1)"},"frame":null,"text":""},{"fillcolor":"rgba(211,198,226,0.5)","a":[0,66,52.5,0,0],"b":[90,24,23.75,50,90],"c":[10,10,23.75,50,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Impure Arkose","marker":{"color":"rgba(211,198,226,1)","line":{"color":"rgba(211,198,226,1)"}},"textfont":{"color":"rgba(211,198,226,1)"},"line":{"color":"rgba(211,198,226,1)"},"frame":null,"text":""},{"fillcolor":"rgba(235,209,192,0.5)","a":[52.5,66,0,0,52.5],"b":[23.75,10,10,50,23.75],"c":[23.75,24,90,50,23.75],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Feldspathic graywacke","marker":{"color":"rgba(235,209,192,1)","line":{"color":"rgba(235,209,192,1)"}},"textfont":{"color":"rgba(235,209,192,1)"},"line":{"color":"rgba(235,209,192,1)"},"frame":null,"text":""},{"fillcolor":"rgba(86,78,55,0.5)","a":[66,75,0,0,66],"b":[10,0,0,10,10],"c":[24,25,100,90,24],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Graywacke","marker":{"color":"rgba(86,78,55,1)","line":{"color":"rgba(86,78,55,1)"}},"textfont":{"color":"rgba(86,78,55,1)"},"line":{"color":"rgba(86,78,55,1)"},"frame":null,"text":""},{"a":[23],"b":[27],"c":[50],"type":"scatterternary","mode":"markers","marker":{"color":"red","size":8,"symbol":"circle","opacity":0.90000000000000002,"line":{"color":"rgba(188,189,34,1)"}},"hovertemplate":"Q: %{a}<br>F: %{b}<br>M: %{c}","name":"A","line":{"color":"rgba(188,189,34,1)"},"frame":null},{"a":[40],"b":[50],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"blue","size":8,"symbol":"square","opacity":0.90000000000000002,"line":{"color":"rgba(23,190,207,1)"}},"hovertemplate":"Q: %{a}<br>F: %{b}<br>M: %{c}","name":"B","line":{"color":"rgba(23,190,207,1)"},"frame":null},{"a":[60],"b":[30],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"green","size":8,"symbol":"diamond","opacity":0.90000000000000002,"line":{"color":"rgba(31,119,180,1)"}},"hovertemplate":"Q: %{a}<br>F: %{b}<br>M: %{c}","name":"C","line":{"color":"rgba(31,119,180,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
