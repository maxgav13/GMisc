# Folk's Ternary Diagram for Sandstones

`ternary_folk_qfl()` draws either a static or interactive ternary
diagram, in english or spanish. It is a base diagram where data can be
plotted.

## Usage

``` r
ternary_folk_qfl(
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
of the dataframe are "f", "q", and "l", that way it gets mapped
automatically, if not make sure to use "aes(x=f,y=q,z=l)". For plotting
on the plotly object the mapping of the new data should be as shown in
the example: `a = ~q, b = ~f, c = ~l`, where `a` refers to the top
("q"), `b` refers to the bottom left ("f"), and `c` refers to the bottom
right ("l"). The examples show basic usage and how to add data, which
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

d = data.frame(q=c(23,40,60),
               f=c(27,50,30),
               l=c(50,10,10),
               loc=c("A", "B", "C"))

# adding data to ggplot object
ternary_folk_qfl() + geom_point(aes(col = loc), data = d)
#> Ignoring unknown labels:
#> • T : "Q"
#> • L : "F"
#> • R : "L"


# adding data to plotly object
ternary_folk_qfl('plotly') %>%
  add_trace(a = ~q, b = ~f, c = ~l,
            data = d,
            name = 'My data',
            type = "scatterternary",
            mode = "markers",
            marker = list(size=8,color='coral',
                          symbol=3,opacity=.9),
            hovertemplate = paste0('Q: %{a}<br>',
                                   'F: %{b}<br>',
                                   'L: %{c}'))

{"x":{"visdat":{"42114760445a":["function () ","plotlyVisDat"],"42111279552":["function () ","data"],"42114dbc3b15":["function () ","data"]},"cur_data":"42114dbc3b15","attrs":{"42111279552":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#777777","#AACDC9","#C0CF98","#2F4996","#D3C6E2","#EBD1C0","#564E37"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":"text","hoveron":"fills","inherit":true},"42114dbc3b15":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"name":"My data","type":"scatterternary","mode":"markers","marker":{"size":8,"color":"coral","symbol":3,"opacity":0.90000000000000002},"hovertemplate":"Q: %{a}<br>F: %{b}<br>L: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"annotations":[{"text":"Folk","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"Folk","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"Folk","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2}],"ternary":{"aaxis":{"title":"Q","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"F","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"L","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Sandstone <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Folk_qfl","width":864,"height":576}},"data":[{"fillcolor":"rgba(119,119,119,0.5)","a":[100,95,95,95],"b":[0,5,2.5,0],"c":[0,0,2.5,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Quartzarenite","marker":{"color":"rgba(119,119,119,1)","line":{"color":"rgba(119,119,119,1)"}},"textfont":{"color":"rgba(119,119,119,1)"},"frame":null,"text":""},{"fillcolor":"rgba(170,205,201,0.5)","a":[95,75,75,95],"b":[5,25,12.5,2.5],"c":[0,0,12.5,2.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Subarkose","marker":{"color":"rgba(170,205,201,1)","line":{"color":"rgba(170,205,201,1)"}},"textfont":{"color":"rgba(170,205,201,1)"},"frame":null,"text":""},{"fillcolor":"rgba(192,207,152,0.5)","a":[95,75,75,95],"b":[2.5,12.5,0,0],"c":[2.5,12.5,25,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Sublitharenite","marker":{"color":"rgba(192,207,152,1)","line":{"color":"rgba(192,207,152,1)"}},"textfont":{"color":"rgba(192,207,152,1)"},"frame":null,"text":""},{"fillcolor":"rgba(47,73,150,0.5)","a":[75,0,0,75],"b":[25,100,75,18.75],"c":[0,0,25,6.25],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Arkose","marker":{"color":"rgba(47,73,150,1)","line":{"color":"rgba(47,73,150,1)"}},"textfont":{"color":"rgba(47,73,150,1)"},"frame":null,"text":""},{"fillcolor":"rgba(211,198,226,0.5)","a":[75,0,0,75],"b":[18.75,75,50,12.5],"c":[6.25,25,50,12.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Lithik Arkose","marker":{"color":"rgba(211,198,226,1)","line":{"color":"rgba(211,198,226,1)"}},"textfont":{"color":"rgba(211,198,226,1)"},"frame":null,"text":""},{"fillcolor":"rgba(235,209,192,0.5)","a":[75,0,0,75],"b":[12.5,50,27,6.25],"c":[12.5,50,75,18.75],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Feldspathic Litharenite","marker":{"color":"rgba(235,209,192,1)","line":{"color":"rgba(235,209,192,1)"}},"textfont":{"color":"rgba(235,209,192,1)"},"frame":null,"text":""},{"fillcolor":"rgba(86,78,55,0.5)","a":[75,0,0,75],"b":[6.25,27,0,0],"c":[18.75,75,100,25],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Litharenite","marker":{"color":"rgba(86,78,55,1)","line":{"color":"rgba(86,78,55,1)"}},"textfont":{"color":"rgba(86,78,55,1)"},"frame":null,"text":""},{"a":[23,40,60],"b":[27,50,30],"c":[50,10,10],"name":"My data","type":"scatterternary","mode":"markers","marker":{"color":"coral","size":8,"symbol":3,"opacity":0.90000000000000002,"line":{"color":"rgba(127,127,127,1)"}},"hovertemplate":["Q: %{a}<br>F: %{b}<br>L: %{c}","Q: %{a}<br>F: %{b}<br>L: %{c}","Q: %{a}<br>F: %{b}<br>L: %{c}"],"line":{"color":"rgba(127,127,127,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# adding data to plotly object with custom colors
my_pal    <- c("A" = "red",      "B" = "blue",   "C" = "green")
my_shapes <- c("A" = "circle",   "B" = "square",  "C" = "diamond")

ternary_folk_qfl('plotly') %>%
  add_trace(a = ~q, b = ~f, c = ~l,
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
                                   'L: %{c}'))

{"x":{"visdat":{"4211cb0792":["function () ","plotlyVisDat"],"4211115e02a6":["function () ","data"],"421132ffe932":["function () ","data"]},"cur_data":"421132ffe932","attrs":{"4211115e02a6":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"color":{},"colors":["#777777","#AACDC9","#C0CF98","#2F4996","#D3C6E2","#EBD1C0","#564E37"],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":"text","hoveron":"fills","inherit":true},"421132ffe932":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"a":{},"b":{},"c":{},"type":"scatterternary","mode":"markers","split":{},"marker":{"size":8,"color":{},"symbol":{},"opacity":0.90000000000000002},"hovertemplate":"Q: %{a}<br>F: %{b}<br>L: %{c}","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"annotations":[{"text":"Folk","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"Folk","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2},{"text":"Folk","x":0.10000000000000001,"y":1,"ax":0,"ay":0,"xref":"paper","yref":"paper","align":"center","font":{"family":"serif","size":15,"color":"white"},"bgcolor":"#b3b3b3","bordercolor":"black","borderwidth":2}],"ternary":{"aaxis":{"title":"Q","tickformat":".0%","tickfont":{"size":10}},"baxis":{"title":"F","tickformat":".0%","tickfont":{"size":10}},"caxis":{"title":"L","tickformat":".0%","tickfont":{"size":10}}},"legend":{"title":{"text":"<b> Sandstone <\/b>"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"Folk_qfl","width":864,"height":576}},"data":[{"fillcolor":"rgba(119,119,119,0.5)","a":[100,95,95,95],"b":[0,5,2.5,0],"c":[0,0,2.5,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Quartzarenite","marker":{"color":"rgba(119,119,119,1)","line":{"color":"rgba(119,119,119,1)"}},"textfont":{"color":"rgba(119,119,119,1)"},"frame":null,"text":""},{"fillcolor":"rgba(170,205,201,0.5)","a":[95,75,75,95],"b":[5,25,12.5,2.5],"c":[0,0,12.5,2.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Subarkose","marker":{"color":"rgba(170,205,201,1)","line":{"color":"rgba(170,205,201,1)"}},"textfont":{"color":"rgba(170,205,201,1)"},"frame":null,"text":""},{"fillcolor":"rgba(192,207,152,0.5)","a":[95,75,75,95],"b":[2.5,12.5,0,0],"c":[2.5,12.5,25,5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Sublitharenite","marker":{"color":"rgba(192,207,152,1)","line":{"color":"rgba(192,207,152,1)"}},"textfont":{"color":"rgba(192,207,152,1)"},"frame":null,"text":""},{"fillcolor":"rgba(47,73,150,0.5)","a":[75,0,0,75],"b":[25,100,75,18.75],"c":[0,0,25,6.25],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Arkose","marker":{"color":"rgba(47,73,150,1)","line":{"color":"rgba(47,73,150,1)"}},"textfont":{"color":"rgba(47,73,150,1)"},"frame":null,"text":""},{"fillcolor":"rgba(211,198,226,0.5)","a":[75,0,0,75],"b":[18.75,75,50,12.5],"c":[6.25,25,50,12.5],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Lithik Arkose","marker":{"color":"rgba(211,198,226,1)","line":{"color":"rgba(211,198,226,1)"}},"textfont":{"color":"rgba(211,198,226,1)"},"frame":null,"text":""},{"fillcolor":"rgba(235,209,192,0.5)","a":[75,0,0,75],"b":[12.5,50,27,6.25],"c":[12.5,50,75,18.75],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Feldspathic Litharenite","marker":{"color":"rgba(235,209,192,1)","line":{"color":"rgba(235,209,192,1)"}},"textfont":{"color":"rgba(235,209,192,1)"},"frame":null,"text":""},{"fillcolor":"rgba(86,78,55,0.5)","a":[75,0,0,75],"b":[6.25,27,0,0],"c":[18.75,75,100,25],"opacity":1,"type":"scatterternary","fill":"toself","mode":"lines","line":{"color":"black","width":0.5},"hoverinfo":["text","text","text","text"],"hoveron":"fills","name":"Litharenite","marker":{"color":"rgba(86,78,55,1)","line":{"color":"rgba(86,78,55,1)"}},"textfont":{"color":"rgba(86,78,55,1)"},"frame":null,"text":""},{"a":[23],"b":[27],"c":[50],"type":"scatterternary","mode":"markers","marker":{"color":"red","size":8,"symbol":"circle","opacity":0.90000000000000002,"line":{"color":"rgba(127,127,127,1)"}},"hovertemplate":"Q: %{a}<br>F: %{b}<br>L: %{c}","name":"A","line":{"color":"rgba(127,127,127,1)"},"frame":null},{"a":[40],"b":[50],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"blue","size":8,"symbol":"square","opacity":0.90000000000000002,"line":{"color":"rgba(188,189,34,1)"}},"hovertemplate":"Q: %{a}<br>F: %{b}<br>L: %{c}","name":"B","line":{"color":"rgba(188,189,34,1)"},"frame":null},{"a":[60],"b":[30],"c":[10],"type":"scatterternary","mode":"markers","marker":{"color":"green","size":8,"symbol":"diamond","opacity":0.90000000000000002,"line":{"color":"rgba(23,190,207,1)"}},"hovertemplate":"Q: %{a}<br>F: %{b}<br>L: %{c}","name":"C","line":{"color":"rgba(23,190,207,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
