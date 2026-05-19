# FAP Ternary Diagram for Plutonic and Volcanic Rocks

`ternary_fap()` draws either a static or interactive ternary diagram, in
english or spanish. It is a base diagram where data can be plotted.

## Usage

``` r
ternary_fap(
  output = c("ggplot", "plotly"),
  language = c("en", "es"),
  type = c("plutonic", "volcanic")
)
```

## Arguments

- output:

  The output format: "ggplot" or "plotly" (default is "ggplot")

- language:

  The language to be displayed: "en" for english or "es" for spanish
  (deafult is "en")

- type:

  The type of volcanic rock: "plutonic" or "volcanic" (default is
  "plutonic")

## Value

FAP ternary diagram for plutonic and volcanic rocks in the desired
format (object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "p", "f", and "a", that way it gets mapped
automatically, if not make sure to use "aes(x=p,y=f,z=a)". For plotting
on the plotly object the mapping of the new data should be as shown in
the example: `a = ~f, b = ~p, c = ~a`, where `a` refers to the top
("f"), `b` refers to the bottom left ("p"), and `c` refers to the bottom
right ("a"). The examples show basic usage and how to add data, which
can be more customizable.

## See also

Other classification plots, petrology:
[`TAS()`](https://maxgav13.github.io/GMisc/reference/TAS.md),
[`ternary_afm()`](https://maxgav13.github.io/GMisc/reference/ternary_afm.md),
[`ternary_dickinson_qmflt()`](https://maxgav13.github.io/GMisc/reference/ternary_dickinson_qmflt.md),
[`ternary_dickinson_qtfl()`](https://maxgav13.github.io/GMisc/reference/ternary_dickinson_qtfl.md),
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
[`ternary_qap_um()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um.md),
[`ternary_qap_um_hbl()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um_hbl.md)

## Examples

``` r
library(ggplot2)
library(plotly)

d = data.frame(f=c(23,40,60),
               p=c(27,50,30),
               a=c(50,10,10))

# adding data to ggplot object
ternary_fap() + geom_point(data = d)
#> Warning: Removing Layer 2 ('PositionNudge'), as it is not an approved position (for ternary plots) under the present ggtern package.
#> Ignoring unknown labels:
#> • T : "F"
#> • L : "P"
#> • R : "A"


# adding data to plotly object
ternary_fap('plotly') %>%
  add_trace(a = ~f, b = ~p, c = ~a,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='coral',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('F: %{a}<br>',
                                   'P: %{b}<br>',
                                   'A: %{c}'))

{"x":{"visdat":{"4f74686ba37f":["function () ","plotlyVisDat"],"4f7472bb1c67":["function () ","data"],"4f745a45e909":["function () ","data"],"4f7423c67c2c":["function () ","data"]},"cur_data":"4f7423c67c2c","attrs":{"4f7472bb1c67":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":"transparent","type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":"text","text":{},"hoveron":"fills","inherit":true},"4f745a45e909":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"text":{},"type":"scatterternary","mode":"text","name":"ID","hoverinfo":"none","textfont":{"family":"Arial","size":12,"color":"black"},"showlegend":true,"inherit":true},"4f7423c67c2c":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"coral","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"F: %{a}<br>P: %{b}<br>A: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"ternary":{"aaxis":{"title":"F","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"P","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"A","tickformat":".0%","tickfont":{"size":10}}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"FAP","width":864,"height":576}},"data":[{"fillcolor":"transparent","a":[0,10,10,0,0],"b":[100,90,81,90,100],"c":[0,0,9,10,0],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 10' <\/br> Foid-bearing gabbro/diorite/anorthosite","hoveron":"fills","name":"10'","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[10,10,60,60,10],"b":[0,9,4,0,0],"c":[90,81,36,40,90],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 11 <\/br> Foid syenite","hoveron":"fills","name":"11","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[10,10,60,60,10],"b":[9,45,20,4,9],"c":[81,45,20,36,81],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 12 <\/br> Foid monzosyenite","hoveron":"fills","name":"12","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[10,10,60,60,10],"b":[45,81,36,20,45],"c":[45,9,4,20,45],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 13 <\/br> Foid monzodiorite/monzogabbro","hoveron":"fills","name":"13","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[10,10,60,60,10],"b":[81,90,40,36,81],"c":[9,0,0,4,9],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 14 <\/br> Foid diorite/gabbro","hoveron":"fills","name":"14","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[100,60,60,100],"b":[0,40,0,0],"c":[0,0,40,0],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"text":"<\/br> 15 <\/br> Foidolite","hoveron":"fills","name":"15","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[0,10,10,0,0],"b":[0,0,9,10,0],"c":[100,90,81,90,100],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 6' <\/br> Foid-bearing alakali feldspar syenite","hoveron":"fills","name":"6'","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[0,10,10,0,0],"b":[10,9,31.399999999999999,35,10],"c":[90,81,58.600000000000001,65,90],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 7' <\/br> Foid-bearing syenite","hoveron":"fills","name":"7'","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[10,0,0,10,10],"b":[31.399999999999999,35,65,58.5,31.399999999999999],"c":[58.600000000000001,65,35,31.5,58.600000000000001],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 8' <\/br> Foid-bearing monzonite","hoveron":"fills","name":"8'","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[10,0,0,10,10],"b":[58.5,65,90,81,58.5],"c":[31.5,35,10,9,31.5],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 9' <\/br> Foid-bearing monzodiorite/monzogabbro","hoveron":"fills","name":"9'","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"a":[5,5,5,5,5,28,28,28,28,75],"b":[5,21,47.5,72,90,3,22,50,68,12.5],"c":[90,74,47.5,23,5,69,50,22,4,12.5],"text":["6'","7'","8'","9'","10'","11","12","13","14","15"],"type":"scatterternary","mode":"text","name":"ID","hoverinfo":["none","none","none","none","none","none","none","none","none","none"],"textfont":{"family":"Arial","size":12,"color":"black"},"showlegend":true,"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"line":{"color":"rgba(31,119,180,1)"},"frame":null},{"a":[23,40,60],"b":[27,50,30],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"coral","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(255,127,14,1)"}},"hovertemplate":["F: %{a}<br>P: %{b}<br>A: %{c}","F: %{a}<br>P: %{b}<br>A: %{c}","F: %{a}<br>P: %{b}<br>A: %{c}"],"line":{"color":"rgba(255,127,14,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
