initApp <- function(dataset=NA)
{

  ui<- shiny::fluidPage(
    shiny::headerPanel("Foresight"),
    shiny::sidebarPanel(width = 3,
                        shiny::conditionalPanel(
                          condition = "input.tabs=='Data upload'",
                          shiny::h4("Data upload"),
                          shiny::radioButtons(
                            "data_input", "",
                            choices = if (is.data.frame(dataset)) {
                              list("Load sample data" = 1,
                                   "Upload text file" = 2,
                                   "Paste data" = 3,
                                   "Data passed through R environment" = 4)
                            } else {
                              list("Load sample data" = 1,
                                   "Upload file" = 2,
                                   "Paste data" = 3)
                            },
                            selected = if (is.data.frame(dataset)) 4 else 1),
                          shiny::conditionalPanel(
                            condition = "input.data_input=='1'",
                            shiny::h5("dataset 'iris' from base package loaded")
                          ),
                          shiny::conditionalPanel(
                            condition = "input.data_input=='2'",
                            shiny::h5("Upload file: "),
                            shiny::fileInput("upload", "", multiple = FALSE),
                            shiny::selectInput("file_type", "Type of file:",
                                               list("text" = "text",
                                                    "csv" =  "csv"),
                                               selected = "text"),
                            shiny::conditionalPanel(
                              condition = "input.file_type=='text'",
                              shiny::selectInput("upload_delim", "Delimiter:",
                                                 list("Semicolon" = ";",
                                                      "Tab" = "\t",
                                                      "Comma" = ",",
                                                      "Space" = " "),
                                                 selected = "Semicolon")),
                            shiny::actionButton("submit_datafile_button",
                                                "Submit datafile")),
                          shiny::conditionalPanel(
                            condition = "input.data_input=='3'",
                            shiny::h5("Paste data below:"),
                            shiny::tags$textarea(id = "data_paste",
                                                 placeholder = "Add data here",
                                                 rows = 10,
                                                 cols = 20, ""),
                            shiny::actionButton("submit_data_button", "Submit data"),
                            shiny::selectInput("text_delim", "Delimiter:",
                                               list("Semicolon" = ";",
                                                    "Tab" = "\t",
                                                    "Comma" = ",",
                                                    "Space" = " "),
                                               selected = "Semicolon")
                          )
                        ),
                        shiny::conditionalPanel(
                          condition = "input.tabs=='Visualization' || input.tabs=='plotly::Plotly' ||
                          input.tabs=='R-code'",
                          shiny::h4("Create visualization"),
                          shiny::selectInput(inputId = "Type",
                                             label = "Type of graph:",
                                             choices = c("Boxplot", "Density", "Dot + Error",
                                                         "Dotplot", "Histogram", "Scatter", "Violin"),
                                             selected = "Violin"),
                          shiny::selectInput("y_var", "Y-variable", choices = ""),
                          shiny::conditionalPanel(
                            condition = "input.Type!='Density' && input.Type!='Histogram'",
                            shiny::selectInput("x_var", "X-variable", choices = "")
                          ),
                          shiny::selectInput("group", "Group (or colour)", choices = ""),
                          shiny::selectInput("facet_row", "Facet Row", choices = ""),
                          shiny::selectInput("facet_col", "Facet Column", choices = ""),
                          shiny::conditionalPanel(
                            condition = "input.Type == 'Boxplot' || input.Type == 'Violin' ||
                            input.Type == 'Dot + Error'",
                            shiny::checkboxInput(inputId = "jitter",
                                                 label = shiny::strong("Show data points (jittered)"),
                                                 value = FALSE)
                          ),
                          shiny::conditionalPanel(
                            condition = "input.Type == 'Boxplot'",
                            shiny::checkboxInput(inputId = "notch",
                                                 label = shiny::strong("Notched box plot"),
                                                 value = FALSE)
                          ),
                          shiny::conditionalPanel(
                            condition = "input.Type == 'Density' || input.Type == 'Histogram'",
                            shiny::sliderInput("alpha", "Opacity:", min = 0, max = 1, value = 0.8)
                          ),
                          shiny::conditionalPanel(
                            condition = "input.Type == 'Histogram' || input.Type=='Dotplot'",
                            shiny::numericInput("binwidth", "Binwidth:", value = 1)
                          ),
                          shiny::conditionalPanel(
                            condition = "input.Type == 'Dotplot'",
                            shiny::selectInput("dot_dir", "Direction stack:",
                                               choices = c("up", "down", "center", "centerwhole"),
                                               selected = "up")
                          ),
                          shiny::conditionalPanel(
                            condition = "input.Type == 'Density' || input.Type == 'Violin'",
                            shiny::sliderInput(inputId = "adj_bw",
                                               label = "Bandwidth adjustment:",
                                               min = 0.01, max = 2, value = 1, step = 0.1)
                          ),
                          shiny::conditionalPanel(
                            condition = "input.Type == 'Scatter'",
                            shiny::checkboxInput(inputId = "line",
                                                 label = shiny::strong("Show regression line"),
                                                 value = FALSE),
                            shiny::conditionalPanel(
                              condition = "input.line == true",
                              shiny::selectInput("smooth", "Smoothening function",
                                                 choices = c("lm", "loess", "gam"))
                            ),
                            shiny::conditionalPanel(
                              condition = "input.line == true",
                              shiny::checkboxInput(inputId = "se",
                                                   label = shiny::strong("Show confidence interval"),
                                                   value = FALSE)
                            )
                          ),
                          shiny::conditionalPanel(
                            condition = "input.Type == 'Dot + Error'",
                            shiny::selectInput("CI", "Confidence Interval:",
                                               choices = c("68% (1 SE)" = 1,
                                                           "90%" = 1.645,
                                                           "95%" = 1.96,
                                                           "99%" = 2.575),
                                               selected = 1.96)
                          )
    ),
    shiny::conditionalPanel(
      condition = "input.tabs=='Info'",
      shiny::h4("Info")
    )
    ),


    #####################################
    ########### OUPUT TABS ##############
    #####################################

    shiny::mainPanel(width = 6,
                     shiny::tabsetPanel(type = "tabs",
                                        shiny::tabPanel("Data upload", shiny::dataTableOutput("out_table")),
                                        shiny::tabPanel("Visualization",
                                                        shiny::mainPanel(

                                                          shiny::plotOutput("out_ggplot"))
                                        ),
                                        shiny::tabPanel("R-code", shiny::verbatimTextOutput("out_r_code")),
                                        id = "tabs"
                     )
    ),

    #####################################
    ######### AESTHETICS TAB ############
    #####################################

    shiny::conditionalPanel(
      condition = "input.tabs=='Visualization' || input.tabs=='Plotly' ||
      input.tabs=='R-code'",
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Change aesthetics"),
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Text",
            shiny::checkboxInput(inputId = "label_axes",
                                 label = shiny::strong("Change labels axes"),
                                 value = FALSE),
            shiny::conditionalPanel(
              condition = "input.label_axes == true",
              shiny::textInput("lab_x", "X-axis:", value = "label x-axis")
            ),
            shiny::conditionalPanel(
              condition = "input.label_axes == true",
              shiny::textInput("lab_y", "Y-axis:", value = "label y-axis")
            ),
            shiny::checkboxInput(inputId = "add_title",
                                 label = shiny::strong("Add title"),
                                 value = FALSE),
            shiny::conditionalPanel(
              condition = "input.add_title == true",
              shiny::textInput("title", "Title:", value = "Title")
            ),
            shiny::checkboxInput(inputId = "adj_fnt_sz",
                                 label = shiny::strong("Change font size"),
                                 value = FALSE),
            shiny::conditionalPanel(
              condition = "input.adj_fnt_sz == true",
              shiny::numericInput("fnt_sz_ttl",
                                  "Size axis titles:",
                                  value = 12),
              shiny::numericInput("fnt_sz_ax",
                                  "Size axis labels:",
                                  value = 10)
            ),
            shiny::checkboxInput(inputId = "rot_txt",
                                 label = shiny::strong("Rotate text x-axis"),
                                 value = FALSE),
            shiny::checkboxInput(inputId = "adj_fnt",
                                 label = shiny::strong("Change font"),
                                 value = FALSE),
            shiny::conditionalPanel(
              condition = "input.adj_fnt == true",
              shiny::selectInput("font", "Font",
                                 choices = c("Courier",
                                             "Helvetica",
                                             "Times"),
                                 selected = "Helvetica")
            )
          ),
          shiny::tabPanel(
            "Theme",
            shiny::conditionalPanel(
              condition = "input.group != '.'",
              shiny::checkboxInput(inputId = "adj_col",
                                   label = shiny::strong("Change colours"),
                                   value = FALSE),
              shiny::conditionalPanel(
                condition = "input.adj_col",
                shiny::selectInput(inputId = "palet",
                                   label = shiny::strong("Select palette"),
                                   choices = list(
                                     "Qualitative" = c("Accent",
                                                       "Dark2",
                                                       "Paired",
                                                       "Pastel1",
                                                       "Pastel2",
                                                       "Set1",
                                                       "Set2",
                                                       "Set3"),
                                     "Diverging" = c("BrBG",
                                                     "PiYG",
                                                     "PRGn",
                                                     "PuOr",
                                                     "RdBu",
                                                     "RdGy",
                                                     "RdYlBu",
                                                     "RdYlGn",
                                                     "Spectral"),
                                     "Sequential" = c("Blues",
                                                      "BuGn",
                                                      "BuPu",
                                                      "GnBu",
                                                      "Greens",
                                                      "Greys",
                                                      "Oranges",
                                                      "OrRd",
                                                      "PuBu",
                                                      "PuBuGn",
                                                      "PuRd",
                                                      "Purples",
                                                      "RdPu",
                                                      "Reds",
                                                      "YlGn",
                                                      "YlGnBu",
                                                      "YlOrBr",
                                                      "YlOrRd")),
                                   selected = "set1")
              )
            ),
            shiny::conditionalPanel(
              condition = "input.jitter",
              shiny::checkboxInput("adj_jitter",
                                   shiny::strong("Change look jitter"), FALSE),
              shiny::conditionalPanel(
                condition = "input.adj_jitter",
                shiny::textInput("col_jitter", "Colour (name or RGB):",
                                 value = "black"),
                shiny::numericInput("size_jitter", "Size:", value = 1),
                shiny::sliderInput("opac_jitter", "Opacity:",
                                   min = 0, max = 1, value = 0.5, step = 0.01),
                shiny::sliderInput("width_jitter", "Width jitter:",
                                   min = 0, max = 0.5, value = 0.25, step = 0.01)
              )
            ),
            shiny::checkboxInput("adj_grd",
                                 shiny::strong("Remove gridlines"), FALSE),
            shiny::conditionalPanel(
              condition = "input.adj_grd",
              shiny::checkboxInput("grd_maj",
                                   shiny::strong("Remove major gridlines"), FALSE),
              shiny::checkboxInput("grd_min",
                                   shiny::strong("Remove minor gridlines"), FALSE)
            ),
            shiny::selectInput("theme", "Theme",
                               choices = c("bw" = "ggplot2::theme_bw()",
                                           "classic" = "ggplot2::theme_classic()",
                                           "dark" = "ggplot2::theme_dark()",
                                           "grey" = "ggplot2::theme_grey()",
                                           "light" = "ggplot2::theme_light()",
                                           "line_draw" = "ggplot2::theme_linedraw()",
                                           "minimal" = "ggplot2::theme_minimal()"),
                               selected = "ggplot2::theme_bw()")
          ),
          shiny::tabPanel(
            "Legend",
            shiny::conditionalPanel(
              condition = "input.group != '.'",
              shiny::radioButtons(inputId = "adj_leg",
                                  label = NULL,
                                  choices = c("Keep legend as it is",
                                              "Remove legend",
                                              "Change legend"),
                                  selected = "Keep legend as it is"),
              shiny::conditionalPanel(
                condition = "input.adj_leg=='Change legend'",
                shiny::textInput("leg_ttl", "Title legend:",
                                 value = "title legend"),
                shiny::selectInput("pos_leg", "Position legend",
                                   choices = c("right",
                                               "left",
                                               "top",
                                               "bottom"))
              )
            )
          ),
          shiny::tabPanel(
            "Size",
            shiny::checkboxInput("fig_size",
                                 shiny::strong("Adjust plot size on screen"), FALSE),
            shiny::conditionalPanel(
              condition = "input.fig_size",
              shiny::numericInput("fig_height", "Plot height (# pixels): ",
                                  value = 480),
              shiny::numericInput("fig_width", "Plot width (# pixels):", value = 480)
            ),
            shiny::checkboxInput("fig_size_download",
                                 shiny::strong("Adjust plot size for download"), FALSE),
            shiny::conditionalPanel(
              condition = "input.fig_size_download",
              shiny::numericInput("fig_height_download",
                                  "Plot height (in cm):", value = 14),
              shiny::numericInput("fig_width_download",
                                  "Plot width (in cm):", value = 14)
            )
          )
        ) # Close tabsetPanel
      ) # Close sidebarPanel
    ) # Close conditionalPanel
    ) # Close fluidPage




  shiny::shinyApp(ui, server=shinyAppServer)
}
