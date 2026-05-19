# Ternary Diagram for Pyroclastic Rocks by Size

`ternary_pyroclastic_size()` draws either a static or interactive
ternary diagram, in english or spanish. It is a base diagram where data
can be plotted.

## Usage

``` r
ternary_pyroclastic_size(
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

Ternary diagram for pyroclastic rocks based on size in the desired
format (object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "lapilli", "bb", and "ash", that way it gets mapped
automatically, if not make sure to use "aes(x=lapilli,y=bb,z=ash)". For
plotting on the plotly object the mapping of the new data should be as
shown in the example: `a = ~bb, b = ~lapilli, c = ~ash`, where `a`
refers to the top ("bb"), `b` refers to the bottom left ("lapilli"), and
`c` refers to the bottom right ("ash"). The examples show basic usage
and how to add data, which can be more customizable.

The ternary diagram for pyroclastic rocks based on size is used in
volcanology and sedimentary petrology to classify pyroclastic deposits
based on the relative proportions of different grain sizes, specifically
blocks and bombs (\> 64 mm), lapilli (2-64 mm), and ash (\< 2 mm). The
diagram helps to infer the eruptive style and depositional processes of
volcanic eruptions, as different types of eruptions produce
characteristic grain size distributions in the resulting pyroclastic
deposits.

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

d = data.frame(lapilli=c(23,40,60),
               bb=c(27,50,30),
               ash=c(50,10,10))

# adding data to ggplot object
ternary_pyroclastic_size() + geom_point(data = d)
#> Ignoring unknown labels:
#> • T : "Blocks & Bombs (> 64 mm)"
#> • L : "Lapilli (2-64 mm)"
#> • R : "Ash (< 2 mm)"


# adding data to plotly object
ternary_pyroclastic_size('plotly') %>%
  add_trace(a = ~bb, b = ~lapilli, c = ~ash,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='cyan',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('BB: %{a}<br>',
                                   'Lapilli: %{b}<br>',
                                   'Ash: %{c}'))

{"x":{"visdat":{"4f74339899f8":["function () ","plotlyVisDat"],"4f7467a4833e":["function () ","data"],"4f7461a4909a":["function () ","data"]},"cur_data":"4f7461a4909a","attrs":{"4f7467a4833e":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":"Set1","opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","inherit":true},"4f7461a4909a":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"cyan","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"BB: %{a}<br>Lapilli: %{b}<br>Ash: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"ternary":{"aaxis":{"title":"Blocks & Bombs (> 64 mm)","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"Lapilli (2-64 mm)","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"Ash (< 2 mm)","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Pyroclastic rock <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Pyroclastic_size","width":864,"height":576}},"data":[{"fillcolor":"rgba(228,26,28,0.5)","a":[100,75,75,100],"b":[0,25,0,0],"c":[0,0,25,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Pyroclastic breccia /<br />        Agglomerate","marker":{"color":"rgba(228,26,28,1)","line":{"color":"rgba(228,26,28,1)"}},"textfont":{"color":"rgba(228,26,28,1)"},"line":{"color":"rgba(228,26,28,1)"},"frame":null,"text":""},{"fillcolor":"rgba(55,126,184,0.5)","a":[75,25,25,75,75],"b":[25,75,0,0,25],"c":[0,0,75,25,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Lapilli-tuff breccia","marker":{"color":"rgba(55,126,184,1)","line":{"color":"rgba(55,126,184,1)"}},"textfont":{"color":"rgba(55,126,184,1)"},"line":{"color":"rgba(55,126,184,1)"},"frame":null,"text":""},{"fillcolor":"rgba(77,175,74,0.5)","a":[25,0,0,25],"b":[75,100,75,75],"c":[0,0,25,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Lapilli-stone","marker":{"color":"rgba(77,175,74,1)","line":{"color":"rgba(77,175,74,1)"}},"textfont":{"color":"rgba(77,175,74,1)"},"line":{"color":"rgba(77,175,74,1)"},"frame":null,"text":""},{"fillcolor":"rgba(152,78,163,0.5)","a":[25,0,0,25,25],"b":[75,75,25,0,75],"c":[0,25,75,75,0],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Lapilli tuff","marker":{"color":"rgba(152,78,163,1)","line":{"color":"rgba(152,78,163,1)"}},"textfont":{"color":"rgba(152,78,163,1)"},"line":{"color":"rgba(152,78,163,1)"},"frame":null,"text":""},{"fillcolor":"rgba(255,127,0,0.5)","a":[25,0,0,25],"b":[0,25,0,0],"c":[75,75,100,75],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","hoveron":"fills","name":"Tuff","marker":{"color":"rgba(255,127,0,1)","line":{"color":"rgba(255,127,0,1)"}},"textfont":{"color":"rgba(255,127,0,1)"},"line":{"color":"rgba(255,127,0,1)"},"frame":null,"text":""},{"a":[27,50,30],"b":[23,40,60],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"cyan","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(140,86,75,1)"}},"hovertemplate":["BB: %{a}<br>Lapilli: %{b}<br>Ash: %{c}","BB: %{a}<br>Lapilli: %{b}<br>Ash: %{c}","BB: %{a}<br>Lapilli: %{b}<br>Ash: %{c}"],"line":{"color":"rgba(140,86,75,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
