# AFM Ternary Diagram

`ternary_afm()` draws either a static or interactive ternary diagram, in
english or spanish. It is a base diagram where data can be plotted.

## Usage

``` r
ternary_afm(output = c("ggplot", "plotly"), language = c("en", "es"))
```

## Arguments

- output:

  The output format: "ggplot" or "plotly" (default is "ggplot")

- language:

  The language to be displayed: "en" for english or "es" for spanish
  (deafult is "en")

## Value

AFM ternary diagram in the desired format (object)

## Details

For plotting data on the ggplot object it would be easier if the names
of the dataframe are "a", "f", and "m", that way it gets mapped
automatically, if not make sure to use "aes(x=a,y=f,z=m)". For plotting
on the plotly object the mapping of the new data should be as shown in
the example: `a = ~f, b = ~a, c = ~m`, where `a` refers to the top
("f"), `b` refers to the bottom left ("a"), and `c` refers to the bottom
right ("m"). The examples show basic usage and how to add data, which
can be more customizable.

The AFM diagram is used in igneous petrology to classify volcanic rocks
based on their chemical composition, specifically the relative
proportions of three major oxides: FeO (F), MgO (M), and Al2O3 (A). The
diagram helps to distinguish between tholeiitic and calc-alkaline rock
series, which have different magmatic histories and tectonic settings.

## See also

Other classification plots, petrology:
[`TAS()`](https://maxgav13.github.io/GMisc/reference/TAS.md),
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
[`ternary_qap_um()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um.md),
[`ternary_qap_um_hbl()`](https://maxgav13.github.io/GMisc/reference/ternary_qap_um_hbl.md)

## Examples

``` r
library(ggplot2)
library(plotly)

d = data.frame(f=c(23,40,60),
               a=c(27,50,30),
               m=c(50,10,10))

# adding data to ggplot object
ternary_afm() + geom_point(data = d)
#> Ignoring unknown labels:
#> • T : "F"
#> • L : "A"
#> • R : "M"


# adding data to plotly object
ternary_afm('plotly') %>%
  add_trace(a = ~f, b = ~a, c = ~m,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='coral',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('F: %{a}<br>',
                                   'A: %{b}<br>',
                                   'M: %{c}')) %>%
  layout(showlegend = TRUE)

{"x":{"visdat":{"4f749beb5fe":["function () ","plotlyVisDat"],"4f74479e3bb1":["function () ","data"],"4f7465610632":["function () ","data"]},"cur_data":"4f7465610632","attrs":{"4f74479e3bb1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"colors":"transparent","type":"scatterternary","mode":"lines","line":{"color":"darkred"},"hoverinfo":"none","showlegend":false,"inherit":true},"4f74479e3bb1.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":[70,20],"b":[20,40],"c":[15,30],"text":["Tholeiitic","Calc-Alkaline"],"type":"scatterternary","mode":"text","hoverinfo":"none","textfont":{"family":"Arial","size":12,"color":"darkred"},"showlegend":false,"inherit":true},"4f7465610632":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"coral","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"F: %{a}<br>A: %{b}<br>M: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"annotations":[{"text":"AFM","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"AFM","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"AFM","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2}],"ternary":{"aaxis":{"title":"F","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"A","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"M","tickformat":".0%","tickfont":{"size":10}}},"showlegend":true,"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"AFM","width":864,"height":576}},"data":[{"a":[31.370000000000001,33.189999999999998,35.030000000000001,36.840000000000003,38.700000000000003,40.490000000000002,42.329999999999998,44.140000000000001,45.979999999999997,47.789999999999999,49.600000000000001,51.450000000000003,53.030000000000001,54.270000000000003,55.07,55.350000000000001,55.259999999999998,54.609999999999999,53.600000000000001,52.270000000000003,50.710000000000001,48.960000000000001,47.109999999999999,45.049999999999997,42.770000000000003,41.159999999999997],"b":[64.810000000000002,61.920000000000002,59.030000000000001,56.149999999999999,53.240000000000002,50.369999999999997,47.469999999999999,44.590000000000003,41.700000000000003,38.82,35.939999999999998,33.039999999999999,30.27,27.68,25.300000000000001,23.190000000000001,21.260000000000002,19.609999999999999,18.140000000000001,16.82,15.630000000000001,14.529999999999999,13.48,12.529999999999999,11.699999999999999,11.07],"c":[3.8199999999999998,4.8899999999999997,5.9500000000000002,7.0199999999999996,8.0600000000000005,9.1400000000000006,10.199999999999999,11.27,12.32,13.390000000000001,14.460000000000001,15.51,16.699999999999999,18.050000000000001,19.629999999999999,21.460000000000001,23.48,25.780000000000001,28.260000000000002,30.899999999999999,33.659999999999997,36.509999999999998,39.409999999999997,42.409999999999997,45.530000000000001,47.770000000000003],"type":"scatterternary","mode":"lines","line":{"color":"darkred"},"hoverinfo":["none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none"],"showlegend":false,"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"frame":null},{"a":[70,20],"b":[20,40],"c":[15,30],"text":["Tholeiitic","Calc-Alkaline"],"type":"scatterternary","mode":"text","hoverinfo":["none","none"],"textfont":{"family":"Arial","size":12,"color":"darkred"},"showlegend":false,"marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"line":{"color":"rgba(255,127,14,1)"},"frame":null},{"a":[23,40,60],"b":[27,50,30],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"coral","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(44,160,44,1)"}},"hovertemplate":["F: %{a}<br>A: %{b}<br>M: %{c}","F: %{a}<br>A: %{b}<br>M: %{c}","F: %{a}<br>A: %{b}<br>M: %{c}"],"line":{"color":"rgba(44,160,44,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
