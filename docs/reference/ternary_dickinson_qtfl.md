# QtFL Ternary Diagram for Provenance Analysis

`ternary_dickinson_qtfl()` draws either a static or interactive ternary
diagram, in english or spanish. It is a base diagram where data can be
plotted.

## Usage

``` r
ternary_dickinson_qtfl(
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

QtFL ternary diagram for provenance analysis in the desired format
(object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "f", "qt", and "l", that way it gets mapped
automatically, if not make sure to use "aes(x=f,y=qt,z=l)". For plotting
on the plotly object the mapping of the new data should be as shown in
the example: `a = ~qt, b = ~f, c = ~l`, where `a` refers to the top
("qt"), `b` refers to the bottom left ("f"), and `c` refers to the
bottom right ("l"). The examples show basic usage and how to add data,
which can be more customizable. For a more complete description on how
to use this function and similar ones, see
[`vignette("classification_diagrams_vig")`](https://maxgav13.github.io/GMisc/articles/classification_diagrams_vig.md)

The QtFL diagram is used in sedimentary petrology to classify sandstones
based on their detrital composition, specifically the relative
proportions of quartz (Qt), feldspar (F), and lithic fragments (L). The
diagram helps to infer the tectonic setting and provenance of the
sedimentary rocks, as different source areas and geological processes
produce characteristic detrital assemblages.

The QtFL diagram was proposed by Dickinson (1985) as a modification of
the traditional QFL diagram, which only considers quartz, feldspar, and
lithic fragments. The QtFL diagram includes a fourth component, "Qt",
which represents the total quartz content (including both
monocrystalline and polycrystalline quartz). This allows for a more
accurate classification of sandstones, especially those with high quartz
content, and provides better insights into the sedimentary processes and
source areas.

## See also

Other classification plots, petrology:
[`TAS()`](https://maxgav13.github.io/GMisc/reference/TAS.md),
[`ternary_afm()`](https://maxgav13.github.io/GMisc/reference/ternary_afm.md),
[`ternary_dickinson_qmflt()`](https://maxgav13.github.io/GMisc/reference/ternary_dickinson_qmflt.md),
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

d = data.frame(qt=c(23,40,60),
               f=c(27,50,30),
               l=c(50,10,10),
               loc=c("A", "B", "C"))

# adding data to ggplot object
ternary_dickinson_qtfl() + geom_point(aes(col = loc), data = d)
#> Ignoring unknown labels:
#> • T : "Qt"
#> • L : "F"
#> • R : "L"


# adding data to plotly object
ternary_dickinson_qtfl('plotly') %>%
  add_trace(a = ~qt, b = ~f, c = ~l,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='coral',symbol=3,opacity=.9),
            hovertemplate = paste0('Qt: %{a}<br>',
                                   'F: %{b}<br>',
                                   'L: %{c}'))

{"x":{"visdat":{"4211f7ce59d":["function () ","plotlyVisDat"],"421150c6ae5c":["function () ","data"],"421123e14372":["function () ","data"]},"cur_data":"421123e14372","attrs":{"421150c6ae5c":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#989FA7","#5A9AE1","#2F4996","#6B6943","#BFAED2","#DAB7A3","#B21C3F"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":"text","hoveron":"fills","inherit":true},"421123e14372":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"coral","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"Qt: %{a}<br>F: %{b}<br>L: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"annotations":[{"text":"QtFL","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"QtFL","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"QtFL","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2}],"ternary":{"aaxis":{"title":"Qt","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"F","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"L","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Provenance <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Dickinson_QtFL","width":864,"height":576}},"data":[{"fillcolor":"rgba(152,159,167,0.5)","a":[100,82,80,97],"b":[0,18,15,0],"c":[0,0,5,3],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Craton interior","marker":{"color":"rgba(152,159,167,1)","line":{"color":"rgba(152,159,167,1)"}},"textfont":{"color":"rgba(152,159,167,1)"},"frame":null,"text":""},{"fillcolor":"rgba(90,154,225,0.5)","a":[82,55,52,80],"b":[18,45,40,15],"c":[0,0,8,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Transitional Continental","marker":{"color":"rgba(90,154,225,1)","line":{"color":"rgba(90,154,225,1)"}},"textfont":{"color":"rgba(90,154,225,1)"},"frame":null,"text":""},{"fillcolor":"rgba(47,73,150,0.5)","a":[55,0,0,20,52],"b":[45,100,85,67.700000000000003,40],"c":[0,0,15,12.300000000000001,8],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Basement Uplift","marker":{"color":"rgba(47,73,150,1)","line":{"color":"rgba(47,73,150,1)"}},"textfont":{"color":"rgba(47,73,150,1)"},"frame":null,"text":""},{"fillcolor":"rgba(107,105,67,0.5)","a":[97,80,52,33.780999999999999,25],"b":[0,15,40,12.897,0],"c":[3,5,8,53.322000000000003,75],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Recycled Orogenic","marker":{"color":"rgba(107,105,67,1)","line":{"color":"rgba(107,105,67,1)"}},"textfont":{"color":"rgba(107,105,67,1)"},"frame":null,"text":""},{"fillcolor":"rgba(191,174,210,0.5)","a":[52,20,33.780999999999999],"b":[40,67.700000000000003,12.897],"c":[8,12.300000000000001,53.322000000000003],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Dissected Arc","marker":{"color":"rgba(191,174,210,1)","line":{"color":"rgba(191,174,210,1)"}},"textfont":{"color":"rgba(191,174,210,1)"},"frame":null,"text":""},{"fillcolor":"rgba(218,183,163,0.5)","a":[20,0,0,25,25,33.780999999999999],"b":[67.700000000000003,85,50,0,0,12.897],"c":[12.300000000000001,15,50,75,75,53.322000000000003],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text","text"],"hoveron":"fills","name":"Transitional Arc","marker":{"color":"rgba(218,183,163,1)","line":{"color":"rgba(218,183,163,1)"}},"textfont":{"color":"rgba(218,183,163,1)"},"frame":null,"text":""},{"fillcolor":"rgba(178,28,63,0.5)","a":[25,0,0],"b":[0,50,0],"c":[75,50,100],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Undissected Arc","marker":{"color":"rgba(178,28,63,1)","line":{"color":"rgba(178,28,63,1)"}},"textfont":{"color":"rgba(178,28,63,1)"},"frame":null,"text":""},{"a":[23,40,60],"b":[27,50,30],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"coral","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(127,127,127,1)"}},"hovertemplate":["Qt: %{a}<br>F: %{b}<br>L: %{c}","Qt: %{a}<br>F: %{b}<br>L: %{c}","Qt: %{a}<br>F: %{b}<br>L: %{c}"],"line":{"color":"rgba(127,127,127,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# adding data to plotly object with custom colors
my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")

ternary_dickinson_qtfl('plotly') %>%
  add_trace(a = ~qt, b = ~f, c = ~l,
            data = d,
            type = "scatterternary",
            mode = "markers",
            split = ~loc,
            marker = list(size=8,
                          color  = ~my_pal[loc],
                          symbol = ~my_shapes[loc],
                          opacity=.9),
            hovertemplate = paste0('Qt: %{a}<br>',
                                   'F: %{b}<br>',
                                   'L: %{c}'))

{"x":{"visdat":{"42111a1303c5":["function () ","plotlyVisDat"],"4211565c87e2":["function () ","data"],"421154dd32b9":["function () ","data"]},"cur_data":"421154dd32b9","attrs":{"4211565c87e2":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#989FA7","#5A9AE1","#2F4996","#6B6943","#BFAED2","#DAB7A3","#B21C3F"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":"text","hoveron":"fills","inherit":true},"421154dd32b9":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"type":"scatterternary","mode":"markers","split":{},"marker":{"size":8,"color":{},"symbol":{},"opacity":0.90000000000000002},"hovertemplate":"Qt: %{a}<br>F: %{b}<br>L: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"annotations":[{"text":"QtFL","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"QtFL","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"QtFL","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2}],"ternary":{"aaxis":{"title":"Qt","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"F","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"L","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Provenance <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Dickinson_QtFL","width":864,"height":576}},"data":[{"fillcolor":"rgba(152,159,167,0.5)","a":[100,82,80,97],"b":[0,18,15,0],"c":[0,0,5,3],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Craton interior","marker":{"color":"rgba(152,159,167,1)","line":{"color":"rgba(152,159,167,1)"}},"textfont":{"color":"rgba(152,159,167,1)"},"frame":null,"text":""},{"fillcolor":"rgba(90,154,225,0.5)","a":[82,55,52,80],"b":[18,45,40,15],"c":[0,0,8,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Transitional Continental","marker":{"color":"rgba(90,154,225,1)","line":{"color":"rgba(90,154,225,1)"}},"textfont":{"color":"rgba(90,154,225,1)"},"frame":null,"text":""},{"fillcolor":"rgba(47,73,150,0.5)","a":[55,0,0,20,52],"b":[45,100,85,67.700000000000003,40],"c":[0,0,15,12.300000000000001,8],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Basement Uplift","marker":{"color":"rgba(47,73,150,1)","line":{"color":"rgba(47,73,150,1)"}},"textfont":{"color":"rgba(47,73,150,1)"},"frame":null,"text":""},{"fillcolor":"rgba(107,105,67,0.5)","a":[97,80,52,33.780999999999999,25],"b":[0,15,40,12.897,0],"c":[3,5,8,53.322000000000003,75],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text"],"hoveron":"fills","name":"Recycled Orogenic","marker":{"color":"rgba(107,105,67,1)","line":{"color":"rgba(107,105,67,1)"}},"textfont":{"color":"rgba(107,105,67,1)"},"frame":null,"text":""},{"fillcolor":"rgba(191,174,210,0.5)","a":[52,20,33.780999999999999],"b":[40,67.700000000000003,12.897],"c":[8,12.300000000000001,53.322000000000003],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Dissected Arc","marker":{"color":"rgba(191,174,210,1)","line":{"color":"rgba(191,174,210,1)"}},"textfont":{"color":"rgba(191,174,210,1)"},"frame":null,"text":""},{"fillcolor":"rgba(218,183,163,0.5)","a":[20,0,0,25,25,33.780999999999999],"b":[67.700000000000003,85,50,0,0,12.897],"c":[12.300000000000001,15,50,75,75,53.322000000000003],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text","text","text"],"hoveron":"fills","name":"Transitional Arc","marker":{"color":"rgba(218,183,163,1)","line":{"color":"rgba(218,183,163,1)"}},"textfont":{"color":"rgba(218,183,163,1)"},"frame":null,"text":""},{"fillcolor":"rgba(178,28,63,0.5)","a":[25,0,0],"b":[0,50,0],"c":[75,50,100],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text"],"hoveron":"fills","name":"Undissected Arc","marker":{"color":"rgba(178,28,63,1)","line":{"color":"rgba(178,28,63,1)"}},"textfont":{"color":"rgba(178,28,63,1)"},"frame":null,"text":""},{"a":[23],"b":[27],"c":[50],"type":"scatterternary","mode":"markers","marker":{"color":"red","size":8,"symbol":"circle","opacity":0.90000000000000002,"line":{"color":"rgba(127,127,127,1)"}},"hovertemplate":"Qt: %{a}<br>F: %{b}<br>L: %{c}","name":"A","line":{"color":"rgba(127,127,127,1)"},"frame":null},{"a":[40],"b":[50],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"blue","size":8,"symbol":"square","opacity":0.90000000000000002,"line":{"color":"rgba(188,189,34,1)"}},"hovertemplate":"Qt: %{a}<br>F: %{b}<br>L: %{c}","name":"B","line":{"color":"rgba(188,189,34,1)"},"frame":null},{"a":[60],"b":[30],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"green","size":8,"symbol":"diamond","opacity":0.90000000000000002,"line":{"color":"rgba(23,190,207,1)"}},"hovertemplate":"Qt: %{a}<br>F: %{b}<br>L: %{c}","name":"C","line":{"color":"rgba(23,190,207,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
