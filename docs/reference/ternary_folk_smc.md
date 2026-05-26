# Folk's Ternary Diagram for Clastic Fine-Grained Rocks

`ternary_folk_smc()` draws either a static or interactive ternary
diagram, in english or spanish. It is a base diagram where data can be
plotted.

## Usage

``` r
ternary_folk_smc(
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
of the dataframe are "m", "s", and "c", that way it gets mapped
automatically, if not make sure to use "aes(x=m,y=s,z=c)". For plotting
on the plotly object the mapping of the new data should be as shown in
the example: `a = ~s, b = ~m, c = ~c`, where `a` refers to the top
("s"), `b` refers to the bottom left ("m"), and `c` refers to the bottom
right ("c"). The examples show basic usage and how to add data, which
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
[`ternary_folk_qfm()`](https://maxgav13.github.io/GMisc/reference/ternary_folk_qfm.md),
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

d = data.frame(m=c(23,40,60),
               s=c(27,50,30),
               c=c(50,10,10),
               loc=c("A", "B", "C"))

# adding data to ggplot object
ternary_folk_smc() + geom_point(aes(col = loc), data = d)
#> Ignoring unknown labels:
#> • T : "Sand"
#> • L : "Silt"
#> • R : "Clay"


# adding data to plotly object
ternary_folk_smc('plotly') %>%
  add_trace(a = ~s, b = ~m, c = ~c,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='coral',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('S: %{a}<br>',
                                   'M: %{b}<br>',
                                   'C: %{c}'))

{"x":{"visdat":{"421121748cef":["function () ","plotlyVisDat"],"42116ad0b011":["function () ","data"],"42112ad762e0":["function () ","data"]},"cur_data":"42112ad762e0","attrs":{"42116ad0b011":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#FFF7EC","#FEE9CC","#FDD8A7","#FDC38C","#FCA16C","#F67B51","#E7533A","#CF2518","#AD0000","#7F0000"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":"text","hoveron":"fills","inherit":true},"42112ad762e0":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"coral","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"S: %{a}<br>M: %{b}<br>C: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":35,"r":10,"autoexpand":true},"ternary":{"aaxis":{"title":"Sand","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Silt","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Clay","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Rock (sediment) <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Folk_smc","width":864,"height":576}},"data":[{"fillcolor":"rgba(255,247,236,0.5)","a":[100,90,90,100],"b":[0,10,0,0],"c":[0,0,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Sandstone <br />(Sand)","marker":{"color":"rgba(255,247,236,1)","line":{"color":"rgba(255,247,236,1)"}},"textfont":{"color":"rgba(255,247,236,1)"},"frame":null,"text":""},{"fillcolor":"rgba(254,233,204,0.5)","a":[90,90,50,50,90],"b":[10,6.5,33.399999999999999,50,10],"c":[0,3.5,16.600000000000001,0,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Silty sandstone <br />(Silty sand)","marker":{"color":"rgba(254,233,204,1)","line":{"color":"rgba(254,233,204,1)"}},"textfont":{"color":"rgba(254,233,204,1)"},"frame":null,"text":""},{"fillcolor":"rgba(253,216,167,0.5)","a":[90,90,50,50,90],"b":[6.5,3.5,16.600000000000001,33.399999999999999,6.5],"c":[3.5,6.5,33.399999999999999,16.600000000000001,3.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Muddy sandstone <br />(Muddy sand)","marker":{"color":"rgba(253,216,167,1)","line":{"color":"rgba(253,216,167,1)"}},"textfont":{"color":"rgba(253,216,167,1)"},"frame":null,"text":""},{"fillcolor":"rgba(253,195,140,0.5)","a":[90,90,50,50,90],"b":[3.5,0,0,16.600000000000001,3.5],"c":[6.5,10,50,33.399999999999999,6.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Clayey sandstone <br />(Clayey sand)","marker":{"color":"rgba(253,195,140,1)","line":{"color":"rgba(253,195,140,1)"}},"textfont":{"color":"rgba(253,195,140,1)"},"frame":null,"text":""},{"fillcolor":"rgba(252,161,108,0.5)","a":[50,50,10,10,50],"b":[50,33.399999999999999,60,90,50],"c":[0,16.600000000000001,30,0,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Sandy siltstone <br />(Sandy silt)","marker":{"color":"rgba(252,161,108,1)","line":{"color":"rgba(252,161,108,1)"}},"textfont":{"color":"rgba(252,161,108,1)"},"frame":null,"text":""},{"fillcolor":"rgba(246,123,81,0.5)","a":[50,50,10,10,50],"b":[33.399999999999999,16.600000000000001,30,60,33.399999999999999],"c":[16.600000000000001,33.399999999999999,60,30,16.600000000000001],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Sandy mudstone <br />(Sandy mud)","marker":{"color":"rgba(246,123,81,1)","line":{"color":"rgba(246,123,81,1)"}},"textfont":{"color":"rgba(246,123,81,1)"},"frame":null,"text":""},{"fillcolor":"rgba(231,83,58,0.5)","a":[50,50,10,10,50],"b":[16.600000000000001,0,0,30,16.600000000000001],"c":[33.399999999999999,50,90,60,33.399999999999999],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Sandy claystone <br />(Sandy clay)","marker":{"color":"rgba(231,83,58,1)","line":{"color":"rgba(231,83,58,1)"}},"textfont":{"color":"rgba(231,83,58,1)"},"frame":null,"text":""},{"fillcolor":"rgba(207,37,24,0.5)","a":[10,10,0,0,10],"b":[90,60,66.700000000000003,100,90],"c":[0,30,33.299999999999997,0,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Siltstone <br />(Silt)","marker":{"color":"rgba(207,37,24,1)","line":{"color":"rgba(207,37,24,1)"}},"textfont":{"color":"rgba(207,37,24,1)"},"frame":null,"text":""},{"fillcolor":"rgba(173,0,0,0.5)","a":[10,10,0,0,10],"b":[60,30,33.299999999999997,66.700000000000003,60],"c":[30,60,66.700000000000003,33.299999999999997,30],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Mudstone <br />(Mud)","marker":{"color":"rgba(173,0,0,1)","line":{"color":"rgba(173,0,0,1)"}},"textfont":{"color":"rgba(173,0,0,1)"},"frame":null,"text":""},{"fillcolor":"rgba(127,0,0,0.5)","a":[10,10,0,0,10],"b":[30,0,0,33.299999999999997,30],"c":[60,90,100,66.700000000000003,60],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Claystone <br />(Clay)","marker":{"color":"rgba(127,0,0,1)","line":{"color":"rgba(127,0,0,1)"}},"textfont":{"color":"rgba(127,0,0,1)"},"frame":null,"text":""},{"a":[27,50,30],"b":[23,40,60],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"coral","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(31,119,180,1)"}},"hovertemplate":["S: %{a}<br>M: %{b}<br>C: %{c}","S: %{a}<br>M: %{b}<br>C: %{c}","S: %{a}<br>M: %{b}<br>C: %{c}"],"line":{"color":"rgba(31,119,180,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# adding data to plotly object with custom colors
my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")

ternary_folk_smc('plotly') %>%
  add_trace(a = ~s, b = ~m, c = ~c,
            data = d,
            type = "scatterternary",
            mode = "markers",
            split = ~loc,
            marker = list(size=8,
                          color  = ~my_pal[loc],
                          symbol = ~my_shapes[loc],
                          opacity=.9),
            hovertemplate = paste0('S: %{a}<br>',
                                   'M: %{b}<br>',
                                   'C: %{c}'))

{"x":{"visdat":{"4211229c7619":["function () ","plotlyVisDat"],"42114e0d750f":["function () ","data"],"4211558053d1":["function () ","data"]},"cur_data":"4211558053d1","attrs":{"42114e0d750f":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#FFF7EC","#FEE9CC","#FDD8A7","#FDC38C","#FCA16C","#F67B51","#E7533A","#CF2518","#AD0000","#7F0000"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":"text","hoveron":"fills","inherit":true},"4211558053d1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"type":"scatterternary","mode":"markers","split":{},"marker":{"size":8,"color":{},"symbol":{},"opacity":0.90000000000000002},"hovertemplate":"S: %{a}<br>M: %{b}<br>C: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":35,"r":10,"autoexpand":true},"ternary":{"aaxis":{"title":"Sand","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Silt","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Clay","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Rock (sediment) <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Folk_smc","width":864,"height":576}},"data":[{"fillcolor":"rgba(255,247,236,0.5)","a":[100,90,90,100],"b":[0,10,0,0],"c":[0,0,10,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Sandstone <br />(Sand)","marker":{"color":"rgba(255,247,236,1)","line":{"color":"rgba(255,247,236,1)"}},"textfont":{"color":"rgba(255,247,236,1)"},"frame":null,"text":""},{"fillcolor":"rgba(254,233,204,0.5)","a":[90,90,50,50,90],"b":[10,6.5,33.399999999999999,50,10],"c":[0,3.5,16.600000000000001,0,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Silty sandstone <br />(Silty sand)","marker":{"color":"rgba(254,233,204,1)","line":{"color":"rgba(254,233,204,1)"}},"textfont":{"color":"rgba(254,233,204,1)"},"frame":null,"text":""},{"fillcolor":"rgba(253,216,167,0.5)","a":[90,90,50,50,90],"b":[6.5,3.5,16.600000000000001,33.399999999999999,6.5],"c":[3.5,6.5,33.399999999999999,16.600000000000001,3.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Muddy sandstone <br />(Muddy sand)","marker":{"color":"rgba(253,216,167,1)","line":{"color":"rgba(253,216,167,1)"}},"textfont":{"color":"rgba(253,216,167,1)"},"frame":null,"text":""},{"fillcolor":"rgba(253,195,140,0.5)","a":[90,90,50,50,90],"b":[3.5,0,0,16.600000000000001,3.5],"c":[6.5,10,50,33.399999999999999,6.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Clayey sandstone <br />(Clayey sand)","marker":{"color":"rgba(253,195,140,1)","line":{"color":"rgba(253,195,140,1)"}},"textfont":{"color":"rgba(253,195,140,1)"},"frame":null,"text":""},{"fillcolor":"rgba(252,161,108,0.5)","a":[50,50,10,10,50],"b":[50,33.399999999999999,60,90,50],"c":[0,16.600000000000001,30,0,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Sandy siltstone <br />(Sandy silt)","marker":{"color":"rgba(252,161,108,1)","line":{"color":"rgba(252,161,108,1)"}},"textfont":{"color":"rgba(252,161,108,1)"},"frame":null,"text":""},{"fillcolor":"rgba(246,123,81,0.5)","a":[50,50,10,10,50],"b":[33.399999999999999,16.600000000000001,30,60,33.399999999999999],"c":[16.600000000000001,33.399999999999999,60,30,16.600000000000001],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Sandy mudstone <br />(Sandy mud)","marker":{"color":"rgba(246,123,81,1)","line":{"color":"rgba(246,123,81,1)"}},"textfont":{"color":"rgba(246,123,81,1)"},"frame":null,"text":""},{"fillcolor":"rgba(231,83,58,0.5)","a":[50,50,10,10,50],"b":[16.600000000000001,0,0,30,16.600000000000001],"c":[33.399999999999999,50,90,60,33.399999999999999],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Sandy claystone <br />(Sandy clay)","marker":{"color":"rgba(231,83,58,1)","line":{"color":"rgba(231,83,58,1)"}},"textfont":{"color":"rgba(231,83,58,1)"},"frame":null,"text":""},{"fillcolor":"rgba(207,37,24,0.5)","a":[10,10,0,0,10],"b":[90,60,66.700000000000003,100,90],"c":[0,30,33.299999999999997,0,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Siltstone <br />(Silt)","marker":{"color":"rgba(207,37,24,1)","line":{"color":"rgba(207,37,24,1)"}},"textfont":{"color":"rgba(207,37,24,1)"},"frame":null,"text":""},{"fillcolor":"rgba(173,0,0,0.5)","a":[10,10,0,0,10],"b":[60,30,33.299999999999997,66.700000000000003,60],"c":[30,60,66.700000000000003,33.299999999999997,30],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Mudstone <br />(Mud)","marker":{"color":"rgba(173,0,0,1)","line":{"color":"rgba(173,0,0,1)"}},"textfont":{"color":"rgba(173,0,0,1)"},"frame":null,"text":""},{"fillcolor":"rgba(127,0,0,0.5)","a":[10,10,0,0,10],"b":[30,0,0,33.299999999999997,30],"c":[60,90,100,66.700000000000003,60],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Claystone <br />(Clay)","marker":{"color":"rgba(127,0,0,1)","line":{"color":"rgba(127,0,0,1)"}},"textfont":{"color":"rgba(127,0,0,1)"},"frame":null,"text":""},{"a":[27],"b":[23],"c":[50],"type":"scatterternary","mode":"markers","marker":{"color":"red","size":8,"symbol":"circle","opacity":0.90000000000000002,"line":{"color":"rgba(31,119,180,1)"}},"hovertemplate":"S: %{a}<br>M: %{b}<br>C: %{c}","name":"A","line":{"color":"rgba(31,119,180,1)"},"frame":null},{"a":[50],"b":[40],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"blue","size":8,"symbol":"square","opacity":0.90000000000000002,"line":{"color":"rgba(255,127,14,1)"}},"hovertemplate":"S: %{a}<br>M: %{b}<br>C: %{c}","name":"B","line":{"color":"rgba(255,127,14,1)"},"frame":null},{"a":[30],"b":[60],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"green","size":8,"symbol":"diamond","opacity":0.90000000000000002,"line":{"color":"rgba(44,160,44,1)"}},"hovertemplate":"S: %{a}<br>M: %{b}<br>C: %{c}","name":"C","line":{"color":"rgba(44,160,44,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
