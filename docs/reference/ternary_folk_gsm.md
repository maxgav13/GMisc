# Folk's Ternary Diagram for Clastic Coarse-Grained Rocks

`ternary_folk_gsm()` draws either a static or interactive ternary
diagram, in english or spanish. It is a base diagram where data can be
plotted.

## Usage

``` r
ternary_folk_gsm(
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

Folk's ternary diagram for clastic rocks in the desired format (object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "s", "g", and "m", that way it gets mapped
automatically, if not make sure to use "aes(x=s,y=g,z=m)". For plotting
on the plotly object the mapping of the new data should be as shown in
the example: `a = ~g, b = ~s, c = ~m`, where `a` refers to the top
("g"), `b` refers to the bottom left ("s"), and `c` refers to the bottom
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

d = data.frame(g=c(23,40,60),
               s=c(27,50,30),
               m=c(50,10,10),
               loc=c("A", "B", "C"))

# adding data to ggplot object
ternary_folk_gsm() + geom_point(aes(col = loc), data = d)
#> Ignoring unknown labels:
#> • T : "Gravel"
#> • L : "Sand"
#> • R : "Mud"


# adding data to plotly object
ternary_folk_gsm('plotly') %>%
  add_trace(a = ~g, b = ~s, c = ~m,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='coral',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('Gravel: %{a}<br>',
                                   'Sand: %{b}<br>',
                                   'Mud: %{c}'))

{"x":{"visdat":{"4211800e2de":["function () ","plotlyVisDat"],"4211722e60ec":["function () ","data"],"421142dd6084":["function () ","data"]},"cur_data":"421142dd6084","attrs":{"4211722e60ec":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#FFF7EC","#FEEBCF","#FDDCAE","#FDCA93","#FCB17B","#FC8D59","#F16C4B","#E0452F","#C81C12","#A80000","#7F0000"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":"text","hoveron":"fills","inherit":true},"421142dd6084":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"coral","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"Gravel: %{a}<br>Sand: %{b}<br>Mud: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":35,"r":10,"autoexpand":true},"ternary":{"aaxis":{"title":"Gravel","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Sand","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Mud","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Rock (sediment) <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Folk_gsm","width":864,"height":576}},"data":[{"fillcolor":"rgba(255,247,236,0.5)","a":[100,80,80,100],"b":[0,20,0,0],"c":[0,0,20,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Conglomerate <br />(Gravel)","marker":{"color":"rgba(255,247,236,1)","line":{"color":"rgba(255,247,236,1)"}},"textfont":{"color":"rgba(255,247,236,1)"},"frame":null,"text":""},{"fillcolor":"rgba(254,235,207,0.5)","a":[80,80,30,30,80],"b":[20,18,63,70,20],"c":[0,2,7,0,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Sandy conglomerate <br />(Sandy gravel)","marker":{"color":"rgba(254,235,207,1)","line":{"color":"rgba(254,235,207,1)"}},"textfont":{"color":"rgba(254,235,207,1)"},"frame":null,"text":""},{"fillcolor":"rgba(253,220,174,0.5)","a":[80,80,30,30,80],"b":[18,10,35,63,18],"c":[2,10,35,7,2],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Muddy sandy conglomerate <br />(Muddy sandy gravel)","marker":{"color":"rgba(253,220,174,1)","line":{"color":"rgba(253,220,174,1)"}},"textfont":{"color":"rgba(253,220,174,1)"},"frame":null,"text":""},{"fillcolor":"rgba(253,202,147,0.5)","a":[80,80,30,30,80],"b":[10,0,0,35,10],"c":[10,20,70,35,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Muddy conglomerate <br />(Muddy gravel)","marker":{"color":"rgba(253,202,147,1)","line":{"color":"rgba(253,202,147,1)"}},"textfont":{"color":"rgba(253,202,147,1)"},"frame":null,"text":""},{"fillcolor":"rgba(252,177,123,0.5)","a":[30,30,5,5,30],"b":[70,63,85.5,95,70],"c":[0,7,9.5,0,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Conglomeratic sandstone <br />(Gravelly sand)","marker":{"color":"rgba(252,177,123,1)","line":{"color":"rgba(252,177,123,1)"}},"textfont":{"color":"rgba(252,177,123,1)"},"frame":null,"text":""},{"fillcolor":"rgba(252,141,89,0.5)","a":[30,30,5,5,30],"b":[63,35,47.5,85.5,63],"c":[7,35,47.5,9.5,7],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Conglomeratic muddy sandstone <br />(Gravelly muddy sand)","marker":{"color":"rgba(252,141,89,1)","line":{"color":"rgba(252,141,89,1)"}},"textfont":{"color":"rgba(252,141,89,1)"},"frame":null,"text":""},{"fillcolor":"rgba(241,108,75,0.5)","a":[30,30,5,5,30],"b":[35,0,0,47.5,35],"c":[35,70,95,47.5,35],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Conglomeratic mudstone <br />(Gravelly mud)","marker":{"color":"rgba(241,108,75,1)","line":{"color":"rgba(241,108,75,1)"}},"textfont":{"color":"rgba(241,108,75,1)"},"frame":null,"text":""},{"fillcolor":"rgba(224,69,47,0.5)","a":[5,0,0,5,5],"b":[85.5,90,100,95,85.5],"c":[9.5,10,0,0,9.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Sandstone <br />(Sand)","marker":{"color":"rgba(224,69,47,1)","line":{"color":"rgba(224,69,47,1)"}},"textfont":{"color":"rgba(224,69,47,1)"},"frame":null,"text":""},{"fillcolor":"rgba(200,28,18,0.5)","a":[5,5,0,0,5],"b":[85.5,47.5,50,90,85.5],"c":[9.5,47.5,50,10,9.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Muddy sandstone <br />(Muddy sand)","marker":{"color":"rgba(200,28,18,1)","line":{"color":"rgba(200,28,18,1)"}},"textfont":{"color":"rgba(200,28,18,1)"},"frame":null,"text":""},{"fillcolor":"rgba(168,0,0,0.5)","a":[5,5,0,0,5],"b":[47.5,9.5,10,50,47.5],"c":[47.5,85.5,90,50,47.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Sandy mudstone <br />(Sandy mud)","marker":{"color":"rgba(168,0,0,1)","line":{"color":"rgba(168,0,0,1)"}},"textfont":{"color":"rgba(168,0,0,1)"},"frame":null,"text":""},{"fillcolor":"rgba(127,0,0,0.5)","a":[5,5,0,0,5],"b":[9.5,0,0,10,9.5],"c":[85.5,95,100,90,85.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Mudstone <br />(Mud)","marker":{"color":"rgba(127,0,0,1)","line":{"color":"rgba(127,0,0,1)"}},"textfont":{"color":"rgba(127,0,0,1)"},"frame":null,"text":""},{"a":[23,40,60],"b":[27,50,30],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"coral","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(255,127,14,1)"}},"hovertemplate":["Gravel: %{a}<br>Sand: %{b}<br>Mud: %{c}","Gravel: %{a}<br>Sand: %{b}<br>Mud: %{c}","Gravel: %{a}<br>Sand: %{b}<br>Mud: %{c}"],"line":{"color":"rgba(255,127,14,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# adding data to plotly object with custom colors
my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")

ternary_folk_gsm('plotly') %>%
  add_trace(a = ~g, b = ~s, c = ~m,
            data = d,
            type = "scatterternary",
            mode = "markers",
            split = ~loc,
            marker = list(size=8,
                          color  = ~my_pal[loc],
                          symbol = ~my_shapes[loc],
                          opacity=.9),
            hovertemplate = paste0('Gravel: %{a}<br>',
                                   'Sand: %{b}<br>',
                                   'Mud: %{c}'))

{"x":{"visdat":{"421153eb9c67":["function () ","plotlyVisDat"],"42111169593c":["function () ","data"],"42111b617b12":["function () ","data"]},"cur_data":"42111b617b12","attrs":{"42111169593c":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#FFF7EC","#FEEBCF","#FDDCAE","#FDCA93","#FCB17B","#FC8D59","#F16C4B","#E0452F","#C81C12","#A80000","#7F0000"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":"text","hoveron":"fills","inherit":true},"42111b617b12":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"type":"scatterternary","mode":"markers","split":{},"marker":{"size":8,"color":{},"symbol":{},"opacity":0.90000000000000002},"hovertemplate":"Gravel: %{a}<br>Sand: %{b}<br>Mud: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":35,"r":10,"autoexpand":true},"ternary":{"aaxis":{"title":"Gravel","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Sand","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Mud","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Rock (sediment) <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Folk_gsm","width":864,"height":576}},"data":[{"fillcolor":"rgba(255,247,236,0.5)","a":[100,80,80,100],"b":[0,20,0,0],"c":[0,0,20,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Conglomerate <br />(Gravel)","marker":{"color":"rgba(255,247,236,1)","line":{"color":"rgba(255,247,236,1)"}},"textfont":{"color":"rgba(255,247,236,1)"},"frame":null,"text":""},{"fillcolor":"rgba(254,235,207,0.5)","a":[80,80,30,30,80],"b":[20,18,63,70,20],"c":[0,2,7,0,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Sandy conglomerate <br />(Sandy gravel)","marker":{"color":"rgba(254,235,207,1)","line":{"color":"rgba(254,235,207,1)"}},"textfont":{"color":"rgba(254,235,207,1)"},"frame":null,"text":""},{"fillcolor":"rgba(253,220,174,0.5)","a":[80,80,30,30,80],"b":[18,10,35,63,18],"c":[2,10,35,7,2],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Muddy sandy conglomerate <br />(Muddy sandy gravel)","marker":{"color":"rgba(253,220,174,1)","line":{"color":"rgba(253,220,174,1)"}},"textfont":{"color":"rgba(253,220,174,1)"},"frame":null,"text":""},{"fillcolor":"rgba(253,202,147,0.5)","a":[80,80,30,30,80],"b":[10,0,0,35,10],"c":[10,20,70,35,10],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Muddy conglomerate <br />(Muddy gravel)","marker":{"color":"rgba(253,202,147,1)","line":{"color":"rgba(253,202,147,1)"}},"textfont":{"color":"rgba(253,202,147,1)"},"frame":null,"text":""},{"fillcolor":"rgba(252,177,123,0.5)","a":[30,30,5,5,30],"b":[70,63,85.5,95,70],"c":[0,7,9.5,0,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Conglomeratic sandstone <br />(Gravelly sand)","marker":{"color":"rgba(252,177,123,1)","line":{"color":"rgba(252,177,123,1)"}},"textfont":{"color":"rgba(252,177,123,1)"},"frame":null,"text":""},{"fillcolor":"rgba(252,141,89,0.5)","a":[30,30,5,5,30],"b":[63,35,47.5,85.5,63],"c":[7,35,47.5,9.5,7],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Conglomeratic muddy sandstone <br />(Gravelly muddy sand)","marker":{"color":"rgba(252,141,89,1)","line":{"color":"rgba(252,141,89,1)"}},"textfont":{"color":"rgba(252,141,89,1)"},"frame":null,"text":""},{"fillcolor":"rgba(241,108,75,0.5)","a":[30,30,5,5,30],"b":[35,0,0,47.5,35],"c":[35,70,95,47.5,35],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Conglomeratic mudstone <br />(Gravelly mud)","marker":{"color":"rgba(241,108,75,1)","line":{"color":"rgba(241,108,75,1)"}},"textfont":{"color":"rgba(241,108,75,1)"},"frame":null,"text":""},{"fillcolor":"rgba(224,69,47,0.5)","a":[5,0,0,5,5],"b":[85.5,90,100,95,85.5],"c":[9.5,10,0,0,9.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Sandstone <br />(Sand)","marker":{"color":"rgba(224,69,47,1)","line":{"color":"rgba(224,69,47,1)"}},"textfont":{"color":"rgba(224,69,47,1)"},"frame":null,"text":""},{"fillcolor":"rgba(200,28,18,0.5)","a":[5,5,0,0,5],"b":[85.5,47.5,50,90,85.5],"c":[9.5,47.5,50,10,9.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Muddy sandstone <br />(Muddy sand)","marker":{"color":"rgba(200,28,18,1)","line":{"color":"rgba(200,28,18,1)"}},"textfont":{"color":"rgba(200,28,18,1)"},"frame":null,"text":""},{"fillcolor":"rgba(168,0,0,0.5)","a":[5,5,0,0,5],"b":[47.5,9.5,10,50,47.5],"c":[47.5,85.5,90,50,47.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Sandy mudstone <br />(Sandy mud)","marker":{"color":"rgba(168,0,0,1)","line":{"color":"rgba(168,0,0,1)"}},"textfont":{"color":"rgba(168,0,0,1)"},"frame":null,"text":""},{"fillcolor":"rgba(127,0,0,0.5)","a":[5,5,0,0,5],"b":[9.5,0,0,10,9.5],"c":[85.5,95,100,90,85.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Mudstone <br />(Mud)","marker":{"color":"rgba(127,0,0,1)","line":{"color":"rgba(127,0,0,1)"}},"textfont":{"color":"rgba(127,0,0,1)"},"frame":null,"text":""},{"a":[23],"b":[27],"c":[50],"type":"scatterternary","mode":"markers","marker":{"color":"red","size":8,"symbol":"circle","opacity":0.90000000000000002,"line":{"color":"rgba(255,127,14,1)"}},"hovertemplate":"Gravel: %{a}<br>Sand: %{b}<br>Mud: %{c}","name":"A","line":{"color":"rgba(255,127,14,1)"},"frame":null},{"a":[40],"b":[50],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"blue","size":8,"symbol":"square","opacity":0.90000000000000002,"line":{"color":"rgba(44,160,44,1)"}},"hovertemplate":"Gravel: %{a}<br>Sand: %{b}<br>Mud: %{c}","name":"B","line":{"color":"rgba(44,160,44,1)"},"frame":null},{"a":[60],"b":[30],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"green","size":8,"symbol":"diamond","opacity":0.90000000000000002,"line":{"color":"rgba(214,39,40,1)"}},"hovertemplate":"Gravel: %{a}<br>Sand: %{b}<br>Mud: %{c}","name":"C","line":{"color":"rgba(214,39,40,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
