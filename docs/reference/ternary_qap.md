# QAP Ternary Diagram for Plutonic and Volcanic Rocks

`ternary_qap()` draws either a static or interactive ternary diagram, in
english or spanish. It is a base diagram where data can be plotted.

## Usage

``` r
ternary_qap(
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

QAP ternary diagram for plutonic and volcanic rocks in the desired
format (object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "a", "q", and "p", that way it gets mapped
automatically, if not make sure to use "aes(x=a,y=q,z=p)". For plotting
on the plotly object the mapping of the new data should be as shown in
the example: `a = ~q, b = ~a, c = ~p`, where `a` refers to the top
("q"), `b` refers to the bottom left ("a"), and `c` refers to the bottom
right ("p"). The examples show basic usage and how to add data, which
can be more customizable.

The QAP ternary diagram is used in igneous petrology to classify
plutonic and volcanic rocks based on their mineralogical composition,
specifically the relative proportions of quartz (Q), alkali feldspar
(A), and plagioclase feldspar (P). The diagram helps to infer the
petrogenesis and tectonic setting of igneous rocks, as different types
of magmatic processes produce characteristic mineral assemblages in the
resulting plutonic and volcanic rocks.

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
               a=c(27,50,30),
               p=c(50,10,10))

# adding data to ggplot object
ternary_qap() + geom_point(data = d)
#> Warning: Removing Layer 2 ('PositionNudge'), as it is not an approved position (for ternary plots) under the present ggtern package.
#> Ignoring unknown labels:
#> • T : "Q"
#> • L : "A"
#> • R : "P"


# adding data to plotly object
ternary_qap('plotly') %>%
  add_trace(a = ~q, b = ~a, c = ~p,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='coral',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('Q: %{a}<br>',
                                   'A: %{b}<br>',
                                   'P: %{c}'))

{"x":{"visdat":{"4f746753950e":["function () ","plotlyVisDat"],"4f74205eff21":["function () ","data"],"4f743cbfe021":["function () ","data"],"4f745913b5af":["function () ","data"]},"cur_data":"4f745913b5af","attrs":{"4f74205eff21":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":"transparent","type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":"text","text":{},"hoveron":"fills","inherit":true},"4f743cbfe021":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"text":{},"type":"scatterternary","mode":"text","name":"ID","hoverinfo":"none","textfont":{"family":"Arial","size":12,"color":"Black"},"showlegend":true,"inherit":true},"4f745913b5af":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"coral","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"Q: %{a}<br>A: %{b}<br>P: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"annotations":[{"text":"QAP","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"QAP","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"QAP","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2}],"ternary":{"aaxis":{"title":"Q","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"A","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"P","tickformat":".0%","tickfont":{"size":10}}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"QAP","width":864,"height":576}},"data":[{"fillcolor":"transparent","a":[5,0,0,5,5],"b":[9.5,10,0,0,9.5],"c":[85.5,90,100,95,85.5],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 10 <\/br> Diorite / Gabbro / Anorthosite","hoveron":"fills","name":"10","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[20,5,5,20,20],"b":[8,9.5,0,0,8],"c":[72,85.5,95,80,72],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 10* <\/br> Quartz diorite / Quartz gabbro / Quartz anorthosite","hoveron":"fills","name":"10*","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[100,90,90,100],"b":[0,10,0,0],"c":[0,0,10,0],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text"],"text":"<\/br> 1a <\/br> Quartzolite","hoveron":"fills","name":"1a","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[90,60,60,90,90],"b":[10,40,0,0,10],"c":[0,0,40,10,0],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 1b <\/br> Quartz-rich granitoid","hoveron":"fills","name":"1b","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[60,20,20,60,60],"b":[40,80,72,36,40],"c":[0,0,8,4,0],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 2 <\/br> Alkali feldspar granite","hoveron":"fills","name":"2","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[60,20,20,60,60],"b":[36,72,52,26,36],"c":[4,8,28,14,4],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 3a <\/br> Syeno-granite","hoveron":"fills","name":"3a","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[60,20,20,60,60],"b":[26,52,28,14,26],"c":[14,28,52,26,14],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 3b <\/br> Monzo-granite","hoveron":"fills","name":"3b","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[60,20,20,60,60],"b":[14,28,8,4,14],"c":[26,52,72,36,26],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 4 <\/br> Granodiorite","hoveron":"fills","name":"4","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[60,20,20,60,60],"b":[4,8,0,0,4],"c":[36,72,80,40,36],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 5 <\/br> Tonalite","hoveron":"fills","name":"5","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[5,0,0,5,5],"b":[95,100,90,85.5,95],"c":[0,0,10,9.5,0],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 6 <\/br> Alkali feldspar syenite","hoveron":"fills","name":"6","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[20,5,5,20,20],"b":[80,95,85.5,72,80],"c":[0,0,9.5,8,0],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 6* <\/br> Quartz alkali feldspar syenite","hoveron":"fills","name":"6*","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[5,0,0,5,5],"b":[85.5,90,65,62,85.5],"c":[9.5,10,35,33,9.5],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 7 <\/br> Syenite","hoveron":"fills","name":"7","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[20,5,5,20,20],"b":[72,85.5,62,52,72],"c":[8,9.5,33,28,8],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 7* <\/br> Quartz syenite","hoveron":"fills","name":"7*","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[5,0,0,5,5],"b":[62,65,35,33,62],"c":[33,35,65,62,33],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 8 <\/br> Monzonite","hoveron":"fills","name":"8","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[20,5,5,20,20],"b":[52,62,33,28,52],"c":[28,33,62,52,28],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 8* <\/br> Quartz monzonite","hoveron":"fills","name":"8*","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[5,0,0,5,5],"b":[33,35,10,9.5,33],"c":[62,65,90,85.5,62],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 9 <\/br> Monzodiorite / Monzograbbro","hoveron":"fills","name":"9","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"fillcolor":"transparent","a":[20,5,5,20,20],"b":[28,33,9.5,8,28],"c":[52,62,85.5,72,52],"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black"},"hoverinfo":["text","text","text","text","text"],"text":"<\/br> 9* <\/br> Quartz monzodiorite / Quartz monzogabbro","hoveron":"fills","name":"9*","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"frame":null},{"a":[93.3333333333333,75,40,40,40,40,40,12.5,12.5,12.5,12.5,12.5,2.5,2.5,2.5,2.5,2.5],"b":[3.3333333333333299,12.5,57,46.5,30,13.5,3,83.125,67.875,43.75,19.625,4.375,92.625,75.625,48.75,21.875,4.875],"c":[3.3333333333333299,12.5,3,13.5,30,46.5,57,4.375,19.625,43.75,67.875,83.125,4.875,21.875,48.75,75.625,92.625],"text":["1a","1b","2","3a","3b","4","5","6*","7*","8*","9*","10*","6","7","8","9","10"],"type":"scatterternary","mode":"text","name":"ID","hoverinfo":["none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none"],"textfont":{"family":"Arial","size":12,"color":"Black"},"showlegend":true,"marker":{"color":"rgba(127,127,127,1)","line":{"color":"rgba(127,127,127,1)"}},"line":{"color":"rgba(127,127,127,1)"},"frame":null},{"a":[23,40,60],"b":[27,50,30],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"coral","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(188,189,34,1)"}},"hovertemplate":["Q: %{a}<br>A: %{b}<br>P: %{c}","Q: %{a}<br>A: %{b}<br>P: %{c}","Q: %{a}<br>A: %{b}<br>P: %{c}"],"line":{"color":"rgba(188,189,34,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
