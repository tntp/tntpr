### ggplot Themes

tntpr now has two ggplot2 themes; the classic `theme_tntp()`, and the
updated `theme_tntp_2018()`.

<img src="C:/Users/dustin.pashouwer/AppData/Local/Temp/RtmpY13FVr/preview-73bc1288439b.dir/tntp-style-plots_files/figure-markdown_strict/unnamed-chunk-2-1.png" width="750px" />

<img src="C:/Users/dustin.pashouwer/AppData/Local/Temp/RtmpY13FVr/preview-73bc1288439b.dir/tntp-style-plots_files/figure-markdown_strict/unnamed-chunk-3-1.png" width="750px" />

<img src="C:/Users/dustin.pashouwer/AppData/Local/Temp/RtmpY13FVr/preview-73bc1288439b.dir/tntp-style-plots_files/figure-markdown_strict/unnamed-chunk-4-1.png" width="750px" />

### palette\_tntp() gives you access to TNTP-style colors:

You can still use palette\_tntp the way you used to…

    palette_tntp("dark_blue")
    #> [1] "#00355F"

… but now you have a larger selection of colors.

<img src="C:/Users/dustin.pashouwer/AppData/Local/Temp/RtmpY13FVr/preview-73bc1288439b.dir/tntp-style-plots_files/figure-markdown_strict/unnamed-chunk-6-1.png" width="750px" />

The `palette_tntp_scales()` function provides access to 5 TNTP color
scales:

– `"default"` (colors in the PPT and Word template),

– `"likert_4pt"`,`"likert_5pt"`, `"likert_6pt"`,and

– `"likert_orange_to_green_4pt"`, `"likert_orange_to_green_5pt"`,
`"likert_orange_to_green_6pt"`

– `colors_tntp_classic` (original `palette_tntp` colors from when this
package was created years ago).

    #> Error in `map()`:
    #> ℹ In index: 1.
    #> Caused by error in `match.arg()`:
    #> ! 'arg' should be one of "tntp_palette", "likert_4pt", "likert_5pt", "likert_6pt", "likert_orange_to_green_4pt", "likert_orange_to_green_5pt", "likert_orange_to_green_6pt"
    #> Error in eval(expr, envir, enclos): object 'palette_plots' not found

### `scale_fill_tntp()` and `scale_color_tntp()`

Supply TNTP-palette scales for filling and coloring.

    performance_data %>%
      ggplot(aes(factor(teacher_experience), fill = factor(y1_performance_quartile))) +
        geom_bar(position = position_fill()) +
        labs(title = "Title",
             subtitle = "Subtitle",
             x = "x label",
             y = "y label",
             fill = "fill",
             caption = "caption") +
      theme_tntp_2018() +
      scale_fill_tntp()

<img src="C:/Users/dustin.pashouwer/AppData/Local/Temp/RtmpY13FVr/preview-73bc1288439b.dir/tntp-style-plots_files/figure-markdown_strict/unnamed-chunk-8-1.png" width="750px" />

    performance_data %>% 
      ggplot(aes(x = y1_teacher_performance, y = y2_teacher_performance, color = factor(teacher_experience))) + 
        geom_point(size = 2) + 
        labs(title = "Title",
           subtitle = "Subtitle",
           x = "x label",
           y = "y label",
           fill = "fill",
           caption = "caption") + 
        theme_tntp_2018() + 
        scale_color_tntp()

<img src="C:/Users/dustin.pashouwer/AppData/Local/Temp/RtmpY13FVr/preview-73bc1288439b.dir/tntp-style-plots_files/figure-markdown_strict/unnamed-chunk-9-1.png" width="750px" />

You can specify which color palette you want to use.

    survey_question %>%
      ggplot(aes(factor(question), fill = factor(answer))) +
      geom_bar(position = position_fill()) +
      labs(title = "Title",
           subtitle = "Subtitle",
           x = "x label",
           y = "y label",
           caption = "caption") +
      theme_tntp_2018() + 
      scale_fill_tntp(palette = "likert_5pt")

<img src="C:/Users/dustin.pashouwer/AppData/Local/Temp/RtmpY13FVr/preview-73bc1288439b.dir/tntp-style-plots_files/figure-markdown_strict/unnamed-chunk-10-1.png" width="750px" />
