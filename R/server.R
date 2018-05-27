
#' @export
shinyAppServer <- function(input, output, session) {

  #####################################
  ### GET VARIABLE NAMES FOR INPUT ####
  #####################################

  observe({
    nms <- names(df_shiny())
    # Make list of variables that are not factors
    nms_cont <- names(Filter(function(x) is.integer(x) ||
                               is.numeric(x) ||
                               is.double(x),
                             df_shiny()))

    # Make list of variables that are not factors
    nms_fact <- names(Filter(function(x) is.factor(x) ||
                               is.logical(x) ||
                               is.character(x),
                             df_shiny()))

    avail_all <- c("No groups" = ".", nms)
    avail_con <-
      if (identical(nms_cont, character(0)))
        c("No continuous vars available" = ".")
    else c(nms_cont)
    avail_fac <-
      if (identical(nms_fact, character(0)))
        c("No factors available" = ".")
    else c("No groups" = ".", nms_fact)

    updateSelectInput(session, "y_var", choices = avail_con)
    updateSelectInput(session, "x_var", choices = c("No x-var" = "' '", nms))
    updateSelectInput(session, "group", choices = avail_all)
    updateSelectInput(session, "facet_row",  choices = avail_fac)
    updateSelectInput(session, "facet_col",  choices = avail_fac)
  })


  #####################################
  ###### READ IN / GET DATA ###########
  #####################################

  df_shiny <- inputData(input,output)

  #####################################
  ####### CREATE GRAPH-CODE ###########
  #####################################

  string_code <- create_graph_code(input,output)


  #####################################
  ###### GRAPHICAL/TABLE OUTPUT #######
  #####################################

  output$out_table <- renderDataTable(
    df_shiny()
  )

  width <- shiny::reactive ({ input$fig_width })
  height <- shiny::reactive ({ input$fig_height })
  width_download <- shiny::reactive ({ input$fig_width_download })
  height_download <- shiny::reactive ({ input$fig_height_download })

  output$out_ggplot <- shiny::renderPlot(width = width,
                                         height = height, {
                                           # evaluate the string RCode as code
                                           df <- df_shiny()
                                           p <- eval(parse(text = string_code()))
                                           p
                                         })

  output$out_plotly <- plotly::renderPlotly({
    # evaluate the string RCode as code
    df <- df_shiny()
    p <- eval(parse(text = string_code()))
    plotly::ggplotly(p)
  })

  #####################################
  #### GENERATE R-CODE FOR OUTPUT #####
  #####################################

  output$out_r_code <- shiny::renderText({

    gg_code <- string_code()
    gg_code <- stringr::str_replace_all(gg_code, "\\+ ", "+\n  ")

    paste(
      "## You can use the below code to generate the graph.\n",
      "## Don't forget to replace the 'df' with the name\n",
      "## of your dataframe\n\n",
      "# You need the following package(s):\n",
      "library(\"ggplot2\")\n\n",
      "# The code below will generate the graph:\n",
      "graph <- ",
      gg_code,
      "\ngraph\n\n",
      "# If you want the plot to be interactive,\n",
      "# you need the following package(s):\n",
      "library(\"plotly\")\n",
      "ggplotly(graph)\n\n",
      "# If you would like to save your graph, you can use:\n",
      "ggsave('my_graph.pdf', graph, width = ",
      width_download(),
      ", height = ",
      height_download(),
      ", units = 'cm')",
      sep = ""
    )

  })

  #####################################
  #### GENERATE R-CODE FOR OUTPUT #####
  #####################################

  # End R-session when browser closed
  session$onSessionEnded(stopApp)
}

