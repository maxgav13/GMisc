# QmFLt Ternary Diagram for Provenance Analysis

`ternary_dickinson_qmflt()` draws either a static or interactive ternary
diagram, in english or spanish. It is a base diagram where data can be
plotted.

## Usage

``` r
ternary_dickinson_qmflt(
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

QmFLt ternary diagram for provenance analysis in the desired format
(object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "f", "qm", and "lt", that way it gets mapped
automatically, if not make sure to use "aes(x=f,y=qm,z=lt)". For
plotting on the plotly object the mapping of the new data should be as
shown in the example: `a = ~qm, b = ~f, c = ~lt`, where `a` refers to
the top ("qm"), `b` refers to the bottom left ("f"), and `c` refers to
the bottom right ("lt"). The examples show basic usage and how to add
data, which can be more customizable. For a more complete description on
how to use this function and similar ones, see
[`vignette("classification_diagrams_vig")`](https://maxgav13.github.io/GMisc/articles/classification_diagrams_vig.md)

The QmFLt diagram is used in sedimentary petrology to classify
sandstones based on their detrital composition, specifically the
relative proportions of monocrystalline quartz (Qm), feldspar (F), and
lithic fragments (Lt). The diagram helps to infer the tectonic setting
and provenance of the sedimentary rocks, as different source areas and
geological processes produce characteristic detrital assemblages.

## See also

Other classification plots, petrology:
[`TAS()`](https://maxgav13.github.io/GMisc/reference/TAS.md),
[`ternary_afm()`](https://maxgav13.github.io/GMisc/reference/ternary_afm.md),
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
[`ternary_qap_um()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um.md),
[`ternary_qap_um_hbl()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um_hbl.md)

## Examples

``` r
library(ggplot2)
library(plotly)

d = data.frame(qm=c(23,26.9,25.3),
               f=c(27,23.7,5.1),
               lt=c(50,49.4,59.6),
               loc=c("A", "B", "C"))

# adding data to ggplot object
ternary_dickinson_qmflt() + geom_point(aes(col = loc), data = d)
#> Ignoring unknown labels:
#> • T : "Qm"
#> • L : "F"
#> • R : "Lt"


# adding data to plotly object
ternary_dickinson_qmflt('plotly') %>%
  add_trace(a = ~qm, b = ~f, c = ~lt,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='coral',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('Qm: %{a}<br>',
                                   'F: %{b}<br>',
                                   'Lt: %{c}'))

{"x":{"visdat":{"81441c055014":["function () ","plotlyVisDat"],"814420cc5f6b":["function () ","data"],"8144498c7a9f":["function () ","data"]},"cur_data":"8144498c7a9f","attrs":{"814420cc5f6b":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#989FA7","#5A9AE1","#2F4996","#9EA76E","#6B6943","#564E37","#D4E3D0","#BFAED2","#DAB7A3","#B21C3F"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","inherit":true},"8144498c7a9f":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"coral","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"Qm: %{a}<br>F: %{b}<br>Lt: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"annotations":[{"text":"QmFLt","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"QmFLt","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"QmFLt","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2}],"ternary":{"aaxis":{"title":"Qm","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"F","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Lt","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Provenance <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Dickinson_QmFLt","width":864,"height":576}},"data":[{"fillcolor":"rgba(152,159,167,0.5)","a":[100,80,73.299999999999997,89],"b":[0,20,13.800000000000001,0],"c":[0,0,12.9,11],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Craton interior","marker":{"color":"rgba(152,159,167,1)","line":{"color":"rgba(152,159,167,1)"}},"textfont":{"color":"rgba(152,159,167,1)"},"line":{"color":"rgba(152,159,167,1)"},"frame":null,"text":""},{"fillcolor":"rgba(90,154,225,0.5)","a":[80,57,49.341000000000001,68.150999999999996,73.299999999999997],"b":[20,43,34.377000000000002,18.132999999999999,13.800000000000001],"c":[0,0,16.283000000000001,13.717000000000001,12.9],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Transitional Continental","marker":{"color":"rgba(90,154,225,1)","line":{"color":"rgba(90,154,225,1)"}},"textfont":{"color":"rgba(90,154,225,1)"},"line":{"color":"rgba(90,154,225,1)"},"frame":null,"text":""},{"fillcolor":"rgba(47,73,150,0.5)","a":[57,0,0,22.199000000000002,49.341000000000001],"b":[43,100,77,57.560000000000002,34.377000000000002],"c":[0,0,23,20.242000000000001,16.283000000000001],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Basement Uplift","marker":{"color":"rgba(47,73,150,1)","line":{"color":"rgba(47,73,150,1)"}},"textfont":{"color":"rgba(47,73,150,1)"},"line":{"color":"rgba(47,73,150,1)"},"frame":null,"text":""},{"fillcolor":"rgba(158,167,110,0.5)","a":[89,73.299999999999997,68.150999999999996,50.273000000000003,58],"b":[0,13.800000000000001,18.132999999999999,16.786000000000001,0],"c":[11,12.9,13.717000000000001,32.941000000000003,42],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Quartzose Recycled","marker":{"color":"rgba(158,167,110,1)","line":{"color":"rgba(158,167,110,1)"}},"textfont":{"color":"rgba(158,167,110,1)"},"line":{"color":"rgba(158,167,110,1)"},"frame":null,"text":""},{"fillcolor":"rgba(107,105,67,0.5)","a":[58,50.273000000000003,29.66,21.638999999999999,29],"b":[0,16.786000000000001,15.191000000000001,14.544,0],"c":[42,32.941000000000003,55.148000000000003,63.817,71],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Transitional Recycled","marker":{"color":"rgba(107,105,67,1)","line":{"color":"rgba(107,105,67,1)"}},"textfont":{"color":"rgba(107,105,67,1)"},"line":{"color":"rgba(107,105,67,1)"},"frame":null,"text":""},{"fillcolor":"rgba(86,78,55,0.5)","a":[29,21.638999999999999,12.685,0,0],"b":[0,14.544,13.932,13,0],"c":[71,63.817,73.382999999999996,87,100],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Lithic Recycled","marker":{"color":"rgba(86,78,55,1)","line":{"color":"rgba(86,78,55,1)"}},"textfont":{"color":"rgba(86,78,55,1)"},"line":{"color":"rgba(86,78,55,1)"},"frame":null,"text":""},{"fillcolor":"rgba(212,227,208,0.5)","a":[68.150999999999996,49.341000000000001,29.66,50.273000000000003],"b":[18.132999999999999,34.377000000000002,15.191000000000001,16.786000000000001],"c":[13.717000000000001,16.283000000000001,55.148000000000003,32.941000000000003],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Mixed","marker":{"color":"rgba(212,227,208,1)","line":{"color":"rgba(212,227,208,1)"}},"textfont":{"color":"rgba(212,227,208,1)"},"line":{"color":"rgba(212,227,208,1)"},"frame":null,"text":""},{"fillcolor":"rgba(191,174,210,0.5)","a":[49.341000000000001,22.199000000000002,29.66],"b":[34.377000000000002,57.560000000000002,15.191000000000001],"c":[16.283000000000001,20.242000000000001,55.148000000000003],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Dissected Arc","marker":{"color":"rgba(191,174,210,1)","line":{"color":"rgba(191,174,210,1)"}},"textfont":{"color":"rgba(191,174,210,1)"},"line":{"color":"rgba(191,174,210,1)"},"frame":null,"text":""},{"fillcolor":"rgba(218,183,163,0.5)","a":[22.199000000000002,0,0,12.685,21.638999999999999,29.66],"b":[57.560000000000002,77,47,13.932,14.544,15.191000000000001],"c":[20.242000000000001,23,53,73.382999999999996,63.817,55.148000000000003],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Transitional Arc","marker":{"color":"rgba(218,183,163,1)","line":{"color":"rgba(218,183,163,1)"}},"textfont":{"color":"rgba(218,183,163,1)"},"line":{"color":"rgba(218,183,163,1)"},"frame":null,"text":""},{"fillcolor":"rgba(178,28,63,0.5)","a":[0,0,12.685],"b":[47,13,13.932],"c":[53,87,73.382999999999996],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Undissected Arc","marker":{"color":"rgba(178,28,63,1)","line":{"color":"rgba(178,28,63,1)"}},"textfont":{"color":"rgba(178,28,63,1)"},"line":{"color":"rgba(178,28,63,1)"},"frame":null,"text":""},{"a":[23,26.899999999999999,25.300000000000001],"b":[27,23.699999999999999,5.0999999999999996],"c":[50,49.399999999999999,59.600000000000001],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"coral","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(31,119,180,1)"}},"hovertemplate":["Qm: %{a}<br>F: %{b}<br>Lt: %{c}","Qm: %{a}<br>F: %{b}<br>Lt: %{c}","Qm: %{a}<br>F: %{b}<br>Lt: %{c}"],"line":{"color":"rgba(31,119,180,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# adding data to plotly object with custom colors
my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")

ternary_dickinson_qmflt('plotly') %>%
  add_trace(a = ~qm, b = ~f, c = ~lt,
            data = d,
            type = "scatterternary",
            mode = "markers",
            split = ~loc,
            marker = list(size=8,
                          color  = ~my_pal[loc],
                          symbol = ~my_shapes[loc],
                          opacity=.9),
            hovertemplate = paste0('Qm: %{a}<br>',
                                   'F: %{b}<br>',
                                   'Lt: %{c}'))

{"x":{"visdat":{"814425c68272":["function () ","plotlyVisDat"],"8144b9e1dbe":["function () ","data"],"814439b2aae7":["function () ","data"]},"cur_data":"814439b2aae7","attrs":{"8144b9e1dbe":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#989FA7","#5A9AE1","#2F4996","#9EA76E","#6B6943","#564E37","#D4E3D0","#BFAED2","#DAB7A3","#B21C3F"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","inherit":true},"814439b2aae7":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"type":"scatterternary","mode":"markers","split":{},"marker":{"size":8,"color":{},"symbol":{},"opacity":0.90000000000000002},"hovertemplate":"Qm: %{a}<br>F: %{b}<br>Lt: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"annotations":[{"text":"QmFLt","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"QmFLt","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"QmFLt","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2}],"ternary":{"aaxis":{"title":"Qm","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"F","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Lt","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Provenance <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Dickinson_QmFLt","width":864,"height":576}},"data":[{"fillcolor":"rgba(152,159,167,0.5)","a":[100,80,73.299999999999997,89],"b":[0,20,13.800000000000001,0],"c":[0,0,12.9,11],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Craton interior","marker":{"color":"rgba(152,159,167,1)","line":{"color":"rgba(152,159,167,1)"}},"textfont":{"color":"rgba(152,159,167,1)"},"line":{"color":"rgba(152,159,167,1)"},"frame":null,"text":""},{"fillcolor":"rgba(90,154,225,0.5)","a":[80,57,49.341000000000001,68.150999999999996,73.299999999999997],"b":[20,43,34.377000000000002,18.132999999999999,13.800000000000001],"c":[0,0,16.283000000000001,13.717000000000001,12.9],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Transitional Continental","marker":{"color":"rgba(90,154,225,1)","line":{"color":"rgba(90,154,225,1)"}},"textfont":{"color":"rgba(90,154,225,1)"},"line":{"color":"rgba(90,154,225,1)"},"frame":null,"text":""},{"fillcolor":"rgba(47,73,150,0.5)","a":[57,0,0,22.199000000000002,49.341000000000001],"b":[43,100,77,57.560000000000002,34.377000000000002],"c":[0,0,23,20.242000000000001,16.283000000000001],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Basement Uplift","marker":{"color":"rgba(47,73,150,1)","line":{"color":"rgba(47,73,150,1)"}},"textfont":{"color":"rgba(47,73,150,1)"},"line":{"color":"rgba(47,73,150,1)"},"frame":null,"text":""},{"fillcolor":"rgba(158,167,110,0.5)","a":[89,73.299999999999997,68.150999999999996,50.273000000000003,58],"b":[0,13.800000000000001,18.132999999999999,16.786000000000001,0],"c":[11,12.9,13.717000000000001,32.941000000000003,42],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Quartzose Recycled","marker":{"color":"rgba(158,167,110,1)","line":{"color":"rgba(158,167,110,1)"}},"textfont":{"color":"rgba(158,167,110,1)"},"line":{"color":"rgba(158,167,110,1)"},"frame":null,"text":""},{"fillcolor":"rgba(107,105,67,0.5)","a":[58,50.273000000000003,29.66,21.638999999999999,29],"b":[0,16.786000000000001,15.191000000000001,14.544,0],"c":[42,32.941000000000003,55.148000000000003,63.817,71],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Transitional Recycled","marker":{"color":"rgba(107,105,67,1)","line":{"color":"rgba(107,105,67,1)"}},"textfont":{"color":"rgba(107,105,67,1)"},"line":{"color":"rgba(107,105,67,1)"},"frame":null,"text":""},{"fillcolor":"rgba(86,78,55,0.5)","a":[29,21.638999999999999,12.685,0,0],"b":[0,14.544,13.932,13,0],"c":[71,63.817,73.382999999999996,87,100],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Lithic Recycled","marker":{"color":"rgba(86,78,55,1)","line":{"color":"rgba(86,78,55,1)"}},"textfont":{"color":"rgba(86,78,55,1)"},"line":{"color":"rgba(86,78,55,1)"},"frame":null,"text":""},{"fillcolor":"rgba(212,227,208,0.5)","a":[68.150999999999996,49.341000000000001,29.66,50.273000000000003],"b":[18.132999999999999,34.377000000000002,15.191000000000001,16.786000000000001],"c":[13.717000000000001,16.283000000000001,55.148000000000003,32.941000000000003],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Mixed","marker":{"color":"rgba(212,227,208,1)","line":{"color":"rgba(212,227,208,1)"}},"textfont":{"color":"rgba(212,227,208,1)"},"line":{"color":"rgba(212,227,208,1)"},"frame":null,"text":""},{"fillcolor":"rgba(191,174,210,0.5)","a":[49.341000000000001,22.199000000000002,29.66],"b":[34.377000000000002,57.560000000000002,15.191000000000001],"c":[16.283000000000001,20.242000000000001,55.148000000000003],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Dissected Arc","marker":{"color":"rgba(191,174,210,1)","line":{"color":"rgba(191,174,210,1)"}},"textfont":{"color":"rgba(191,174,210,1)"},"line":{"color":"rgba(191,174,210,1)"},"frame":null,"text":""},{"fillcolor":"rgba(218,183,163,0.5)","a":[22.199000000000002,0,0,12.685,21.638999999999999,29.66],"b":[57.560000000000002,77,47,13.932,14.544,15.191000000000001],"c":[20.242000000000001,23,53,73.382999999999996,63.817,55.148000000000003],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Transitional Arc","marker":{"color":"rgba(218,183,163,1)","line":{"color":"rgba(218,183,163,1)"}},"textfont":{"color":"rgba(218,183,163,1)"},"line":{"color":"rgba(218,183,163,1)"},"frame":null,"text":""},{"fillcolor":"rgba(178,28,63,0.5)","a":[0,0,12.685],"b":[47,13,13.932],"c":[53,87,73.382999999999996],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Undissected Arc","marker":{"color":"rgba(178,28,63,1)","line":{"color":"rgba(178,28,63,1)"}},"textfont":{"color":"rgba(178,28,63,1)"},"line":{"color":"rgba(178,28,63,1)"},"frame":null,"text":""},{"a":[23],"b":[27],"c":[50],"type":"scatterternary","mode":"markers","marker":{"color":"red","size":8,"symbol":"circle","opacity":0.90000000000000002,"line":{"color":"rgba(31,119,180,1)"}},"hovertemplate":"Qm: %{a}<br>F: %{b}<br>Lt: %{c}","name":"A","line":{"color":"rgba(31,119,180,1)"},"frame":null},{"a":[26.899999999999999],"b":[23.699999999999999],"c":[49.399999999999999],"type":"scatterternary","mode":"markers","marker":{"color":"blue","size":8,"symbol":"square","opacity":0.90000000000000002,"line":{"color":"rgba(255,127,14,1)"}},"hovertemplate":"Qm: %{a}<br>F: %{b}<br>Lt: %{c}","name":"B","line":{"color":"rgba(255,127,14,1)"},"frame":null},{"a":[25.300000000000001],"b":[5.0999999999999996],"c":[59.600000000000001],"type":"scatterternary","mode":"markers","marker":{"color":"green","size":8,"symbol":"diamond","opacity":0.90000000000000002,"line":{"color":"rgba(44,160,44,1)"}},"hovertemplate":"Qm: %{a}<br>F: %{b}<br>Lt: %{c}","name":"C","line":{"color":"rgba(44,160,44,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
