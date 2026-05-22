# QAP Ternary Diagram for Ultramafic Rocks

`ternary_qap_um()` draws either a static or interactive ternary diagram,
in english or spanish. It is a base diagram where data can be plotted.

## Usage

``` r
ternary_qap_um(
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

QAP ternary diagram for ultramafic rocks in the desired format (object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "opx", "ol", and "cpx", that way it gets mapped
automatically, if not make sure to use "aes(x=opx,y=ol,z=cpx)". For
plotting on the plotly object the mapping of the new data should be as
shown in the example: `a = ~ol, b = ~opx, c = ~cpx`, where `a` refers to
the top ("ol"), `b` refers to the bottom left ("opx"), and `c` refers to
the bottom right ("cpx"). The examples show basic usage and how to add
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
[`ternary_qap_m_ol()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_m_ol.md),
[`ternary_qap_um_hbl()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um_hbl.md)

## Examples

``` r
library(ggplot2)
library(plotly)

d = data.frame(ol=c(23,40,60),
               opx=c(27,50,30),
               cpx=c(50,10,10),
               loc=c("A", "B", "C"))

# adding data to ggplot object
ternary_qap_um() + geom_point(aes(col = loc), data = d)
#> Ignoring unknown labels:
#> • T : "Ol"
#> • L : "Opx"
#> • R : "Cpx"


# adding data to plotly object
ternary_qap_um('plotly') %>%
  add_trace(a = ~ol, b = ~opx, c = ~cpx,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='coral',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('Ol: %{a}<br>',
                                   'Opx: %{b}<br>',
                                   'Cpx: %{c}'))

{"x":{"visdat":{"81441523899e":["function () ","plotlyVisDat"],"8144501feee9":["function () ","data"],"8144607e2c17":["function () ","data"]},"cur_data":"8144607e2c17","attrs":{"8144501feee9":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":"text","hoveron":"fills","inherit":true},"8144607e2c17":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"coral","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"Ol: %{a}<br>Opx: %{b}<br>Cpx: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"ternary":{"aaxis":{"title":"Ol","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Opx","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Cpx","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Ultramafic <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Ultramafic","width":864,"height":576}},"data":[{"fillcolor":"rgba(253,231,37,0.5)","a":[100,90,90],"b":[0,10,0],"c":[0,0,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Dunite","marker":{"color":"rgba(253,231,37,1)","line":{"color":"rgba(253,231,37,1)"}},"textfont":{"color":"rgba(253,231,37,1)"},"frame":null,"text":""},{"fillcolor":"rgba(180,222,44,0.5)","a":[90,40,40,90],"b":[10,60,55,5],"c":[0,0,5,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Harzburgite","marker":{"color":"rgba(180,222,44,1)","line":{"color":"rgba(180,222,44,1)"}},"textfont":{"color":"rgba(180,222,44,1)"},"frame":null,"text":""},{"fillcolor":"rgba(109,205,89,0.5)","a":[90,40,40],"b":[5,55,5],"c":[5,5,55],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Lherzolite","marker":{"color":"rgba(109,205,89,1)","line":{"color":"rgba(109,205,89,1)"}},"textfont":{"color":"rgba(109,205,89,1)"},"frame":null,"text":""},{"fillcolor":"rgba(53,183,121,0.5)","a":[90,40,40,90],"b":[5,5,0,0],"c":[5,55,60,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Wehrlite","marker":{"color":"rgba(53,183,121,1)","line":{"color":"rgba(53,183,121,1)"}},"textfont":{"color":"rgba(53,183,121,1)"},"frame":null,"text":""},{"fillcolor":"rgba(31,158,137,0.5)","a":[40,10,5,40],"b":[60,90,90,55],"c":[0,0,5,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Olivine orthopyroxenite","marker":{"color":"rgba(31,158,137,1)","line":{"color":"rgba(31,158,137,1)"}},"textfont":{"color":"rgba(31,158,137,1)"},"frame":null,"text":""},{"fillcolor":"rgba(38,130,142,0.5)","a":[40,5,5,40],"b":[55,90,5,5],"c":[5,5,90,55],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Olivine websterite","marker":{"color":"rgba(38,130,142,1)","line":{"color":"rgba(38,130,142,1)"}},"textfont":{"color":"rgba(38,130,142,1)"},"frame":null,"text":""},{"fillcolor":"rgba(49,104,142,0.5)","a":[40,5,10,40],"b":[5,5,0,0],"c":[55,90,90,60],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Olivine clinopyroxenite","marker":{"color":"rgba(49,104,142,1)","line":{"color":"rgba(49,104,142,1)"}},"textfont":{"color":"rgba(49,104,142,1)"},"frame":null,"text":""},{"fillcolor":"rgba(62,74,137,0.5)","a":[10,0,0],"b":[90,100,90],"c":[0,0,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Orthopyroxenite","marker":{"color":"rgba(62,74,137,1)","line":{"color":"rgba(62,74,137,1)"}},"textfont":{"color":"rgba(62,74,137,1)"},"frame":null,"text":""},{"fillcolor":"rgba(72,40,120,0.5)","a":[5,0,0,5],"b":[90,90,10,5],"c":[5,10,90,90],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Websterite","marker":{"color":"rgba(72,40,120,1)","line":{"color":"rgba(72,40,120,1)"}},"textfont":{"color":"rgba(72,40,120,1)"},"frame":null,"text":""},{"fillcolor":"rgba(68,1,84,0.5)","a":[10,0,0],"b":[0,10,0],"c":[90,90,100],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Clinopyroxenite","marker":{"color":"rgba(68,1,84,1)","line":{"color":"rgba(68,1,84,1)"}},"textfont":{"color":"rgba(68,1,84,1)"},"frame":null,"text":""},{"a":[23,40,60],"b":[27,50,30],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"coral","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(31,119,180,1)"}},"hovertemplate":["Ol: %{a}<br>Opx: %{b}<br>Cpx: %{c}","Ol: %{a}<br>Opx: %{b}<br>Cpx: %{c}","Ol: %{a}<br>Opx: %{b}<br>Cpx: %{c}"],"line":{"color":"rgba(31,119,180,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# customizing colors and symbols by location
my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")

ternary_qap_um('plotly') %>%
  add_trace(a = ~ol, b = ~opx, c = ~cpx,
            data = d,
            type = "scatterternary",
            mode = "markers",
            split = ~loc,
            marker = list(size=8,
                          color  = ~my_pal[loc],
                          symbol = ~my_shapes[loc],
                          opacity=.9),
            hovertemplate = paste0('Ol: %{a}<br>',
                                   'Opx: %{b}<br>',
                                   'Cpx: %{c}'))

{"x":{"visdat":{"81447b80cb7e":["function () ","plotlyVisDat"],"814444affc8a":["function () ","data"],"81447deceb40":["function () ","data"]},"cur_data":"81447deceb40","attrs":{"814444affc8a":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":"text","hoveron":"fills","inherit":true},"81447deceb40":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"type":"scatterternary","mode":"markers","split":{},"marker":{"size":8,"color":{},"symbol":{},"opacity":0.90000000000000002},"hovertemplate":"Ol: %{a}<br>Opx: %{b}<br>Cpx: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"ternary":{"aaxis":{"title":"Ol","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Opx","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Cpx","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Ultramafic <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Ultramafic","width":864,"height":576}},"data":[{"fillcolor":"rgba(253,231,37,0.5)","a":[100,90,90],"b":[0,10,0],"c":[0,0,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Dunite","marker":{"color":"rgba(253,231,37,1)","line":{"color":"rgba(253,231,37,1)"}},"textfont":{"color":"rgba(253,231,37,1)"},"frame":null,"text":""},{"fillcolor":"rgba(180,222,44,0.5)","a":[90,40,40,90],"b":[10,60,55,5],"c":[0,0,5,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Harzburgite","marker":{"color":"rgba(180,222,44,1)","line":{"color":"rgba(180,222,44,1)"}},"textfont":{"color":"rgba(180,222,44,1)"},"frame":null,"text":""},{"fillcolor":"rgba(109,205,89,0.5)","a":[90,40,40],"b":[5,55,5],"c":[5,5,55],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Lherzolite","marker":{"color":"rgba(109,205,89,1)","line":{"color":"rgba(109,205,89,1)"}},"textfont":{"color":"rgba(109,205,89,1)"},"frame":null,"text":""},{"fillcolor":"rgba(53,183,121,0.5)","a":[90,40,40,90],"b":[5,5,0,0],"c":[5,55,60,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Wehrlite","marker":{"color":"rgba(53,183,121,1)","line":{"color":"rgba(53,183,121,1)"}},"textfont":{"color":"rgba(53,183,121,1)"},"frame":null,"text":""},{"fillcolor":"rgba(31,158,137,0.5)","a":[40,10,5,40],"b":[60,90,90,55],"c":[0,0,5,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Olivine orthopyroxenite","marker":{"color":"rgba(31,158,137,1)","line":{"color":"rgba(31,158,137,1)"}},"textfont":{"color":"rgba(31,158,137,1)"},"frame":null,"text":""},{"fillcolor":"rgba(38,130,142,0.5)","a":[40,5,5,40],"b":[55,90,5,5],"c":[5,5,90,55],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Olivine websterite","marker":{"color":"rgba(38,130,142,1)","line":{"color":"rgba(38,130,142,1)"}},"textfont":{"color":"rgba(38,130,142,1)"},"frame":null,"text":""},{"fillcolor":"rgba(49,104,142,0.5)","a":[40,5,10,40],"b":[5,5,0,0],"c":[55,90,90,60],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Olivine clinopyroxenite","marker":{"color":"rgba(49,104,142,1)","line":{"color":"rgba(49,104,142,1)"}},"textfont":{"color":"rgba(49,104,142,1)"},"frame":null,"text":""},{"fillcolor":"rgba(62,74,137,0.5)","a":[10,0,0],"b":[90,100,90],"c":[0,0,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Orthopyroxenite","marker":{"color":"rgba(62,74,137,1)","line":{"color":"rgba(62,74,137,1)"}},"textfont":{"color":"rgba(62,74,137,1)"},"frame":null,"text":""},{"fillcolor":"rgba(72,40,120,0.5)","a":[5,0,0,5],"b":[90,90,10,5],"c":[5,10,90,90],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Websterite","marker":{"color":"rgba(72,40,120,1)","line":{"color":"rgba(72,40,120,1)"}},"textfont":{"color":"rgba(72,40,120,1)"},"frame":null,"text":""},{"fillcolor":"rgba(68,1,84,0.5)","a":[10,0,0],"b":[0,10,0],"c":[90,90,100],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Clinopyroxenite","marker":{"color":"rgba(68,1,84,1)","line":{"color":"rgba(68,1,84,1)"}},"textfont":{"color":"rgba(68,1,84,1)"},"frame":null,"text":""},{"a":[23],"b":[27],"c":[50],"type":"scatterternary","mode":"markers","marker":{"color":"red","size":8,"symbol":"circle","opacity":0.90000000000000002,"line":{"color":"rgba(31,119,180,1)"}},"hovertemplate":"Ol: %{a}<br>Opx: %{b}<br>Cpx: %{c}","name":"A","line":{"color":"rgba(31,119,180,1)"},"frame":null},{"a":[40],"b":[50],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"blue","size":8,"symbol":"square","opacity":0.90000000000000002,"line":{"color":"rgba(255,127,14,1)"}},"hovertemplate":"Ol: %{a}<br>Opx: %{b}<br>Cpx: %{c}","name":"B","line":{"color":"rgba(255,127,14,1)"},"frame":null},{"a":[60],"b":[30],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"green","size":8,"symbol":"diamond","opacity":0.90000000000000002,"line":{"color":"rgba(44,160,44,1)"}},"hovertemplate":"Ol: %{a}<br>Opx: %{b}<br>Cpx: %{c}","name":"C","line":{"color":"rgba(44,160,44,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
