

#' @export
create_graph_code <- function(input,output)
{
  #####################################
  ####### CREATE GRAPH-CODE ###########
  #####################################

  string_code <- shiny::reactive({

    # Variable used for how to deal with x/y in ggplot
    gg_x_y <- input$Type == "Histogram" ||
      input$Type == "Density"
    # Variable used for how to deal with colour/fill
    gg_fil <- input$Type == "Histogram" ||
      input$Type == "Density" ||
      input$Type == "Dotplot"

    # Only plot jitter when graphs allow them
    if (gg_fil || input$Type == "Scatter")
      jitt <- FALSE else jitt <- input$jitter

    p <- paste(
      "ggplot2::ggplot(df, ggplot2::aes(",
      if (gg_x_y) {
        "x = input$y_var"
      } else {
        "x = input$x_var, y = input$y_var"
      },
      if (input$group != "." && gg_fil) {
        ", fill = input$group"
      } else if (input$group != "." && !gg_fil) {
        ", colour = input$group"
      },
      ")) + ",
      if (input$Type == "Histogram")
        paste(" ggplot2::geom_histogram(position = 'identity', alpha = input$alpha, ",
              "binwidth = input$binwidth)", sep = ""),
      if (input$Type == "Density")
        paste(" ggplot2::geom_density(position = 'identity', alpha = input$alpha, ",
              "adjust = input$adj_bw)", sep = ""),
      if (input$Type == "Boxplot")
        " ggplot2::geom_boxplot(notch = input$notch)",
      if (input$Type == "Violin")
        " ggplot2::geom_violin(adjust = input$adj_bw)",
      if (input$Type == "Dotplot")
        paste(" ggplot2::geom_dotplot(binaxis = 'y', binwidth = input$binwidth, ",
              "stackdir = 'input$dot_dir')", sep = ""),
      if (input$Type == "Dot + Error")
        paste(" ggplot2::geom_point(stat = 'summary', fun.y = 'mean') +\n  ",
              " ggplot2::geom_errorbar(stat = 'summary', fun.data = 'mean_se', ", "
              width=0, fun.args = list(mult = input$CI))", sep = ""),
      if (input$Type == "Scatter")
        " ggplot2::geom_point()",
      if (input$Type == "Scatter" && input$line)
        "+  ggplot2::geom_smooth(se = input$se, method = 'input$smooth')",
      if (jitt)
        paste(" +  ggplot2::geom_jitter(size = input$size_jitter, ",
              "alpha = input$opac_jitter, width = input$width_jitter, ",
              "colour = 'input$col_jitter')", sep = ""),
      sep = ""
    )

    # if at least one facet column/row is specified, add it
    facets <- paste(input$facet_row, "~", input$facet_col)
    if (facets != ". ~ .")
      p <- paste(p, "+ ggplot2::facet_grid(", facets, ")")

    # if labels specified
    if (input$label_axes)
      p <- paste(p, "+  ggplot2::labs(x = 'input$lab_x', y = 'input$lab_y')")

    # if title specified
    if (input$add_title)
      p <- paste(p, "+  ggplot2::ggtitle('input$title')")

    # if legend specified
    if (input$adj_leg == "Change legend")
      p <- paste(p, "+ ggplot2::labs(",
                 if (gg_fil) "fill" else "colour",
                 " = 'input$leg_ttl')",
                 sep = "")

    # if colour legend specified
    if (input$adj_col)
      p <- paste(p, "+ scale_",
                 if (gg_fil) "fill" else "colour",
                 "_brewer(palette = 'input$palet')",
                 sep = "")

    # If a theme specified
    p <- paste(p, "+", input$theme)

    # If theme features are specified
    if (input$adj_fnt_sz ||
        input$adj_fnt ||
        input$rot_txt ||
        input$adj_leg != "Keep legend as it is" ||
        input$adj_grd) {
      p <- paste(
        p,
        paste(
          " +  ggplot2::theme(\n    ",
          if (input$adj_fnt_sz)
            "axis.title = ggplot2::element_text(size = input$fnt_sz_ttl),\n    ",
          if (input$adj_fnt_sz)
            "axis.text = ggplot2::element_text(size = input$fnt_sz_ax),\n    ",
          if (input$adj_fnt)
            "text = ggplot2::element_text(family = 'input$font'),\n    ",
          if (input$rot_txt)
            "axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),\n    ",
          if (input$adj_leg == "Remove legend")
            "legend.position = 'none',\n    ",
          if (input$adj_leg == "Change legend")
            "legend.position = 'input$pos_leg',\n    ",
          if (input$grd_maj)
            "panel.grid.major = ggplot2::element_blank(),\n    ",
          if (input$grd_min)
            "panel.grid.minor = ggplot2::element_blank(),\n    ",
          ")",
          sep = ""
        ),
        sep = ""
      )
    }

    # Replace name of variables by values
    p <- stringr::str_replace_all(
      p,
      c("input\\$y_var" = input$y_var,
        "input\\$x_var" = input$x_var,
        "input\\$group" = input$group,
        "input\\$notch" = as.character(input$notch),
        "input\\$binwidth" = as.character(input$binwidth),
        "input\\$adj_bw" = as.character(input$adj_bw),
        "input\\$dot_dir" = as.character(input$dot_dir),
        "input\\$alpha" = as.character(input$alpha),
        "input\\$se" = as.character(input$se),
        "input\\$smooth" = as.character(input$smooth),
        "input\\$CI" = as.character(input$CI),
        "input\\$size_jitter" = as.character(input$size_jitter),
        "input\\$width_jitter" = as.character(input$width_jitter),
        "input\\$opac_jitter" = as.character(input$opac_jitter),
        "input\\$col_jitter" = as.character(input$col_jitter),
        "input\\$lab_x" = as.character(input$lab_x),
        "input\\$lab_y" = as.character(input$lab_y),
        "input\\$title" = as.character(input$title),
        "input\\$palet" = as.character(input$palet),
        "input\\$fnt_sz_ttl" = as.character(input$fnt_sz_ttl),
        "input\\$fnt_sz_ax" = as.character(input$fnt_sz_ax),
        "input\\$font" = as.character(input$font),
        "input\\$leg_ttl" = as.character(input$leg_ttl),
        "input\\$pos_leg" = as.character(input$pos_leg))
    )
    # Creates well-formatted R-code for output
    p <- stringr::str_replace_all(p, ",\n    \\)", "\n  \\)")

    p
  })
  return (string_code)
}

