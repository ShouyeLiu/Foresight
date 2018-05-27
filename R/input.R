
# Module server function
#' @export
inputData <- function(input,output)
{
  df_shiny <- shiny::reactive({
    if (input$data_input == 1) {
      data <- iris
    } else if (input$data_input == 2) {
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Select your datafile"))
      } else if (input$submit_datafile_button == 0) {
        return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        isolate({
          if (input$file_type == "text") {
            data <- read.table(file_in$datapath,

                               header = TRUE)
          } else if (input$file_type == "Excel") {
            data <- readxl::read_excel(file_in$datapath)
          } else if (input$file_type == "csv") {
            data <- read.csv(file_in$datapath,header=TRUE)
          }
        })
      }
    } else if (input$data_input == 3) {
      if (input$data_paste == "") {
        data <- data.frame(x = "Copy your data into the textbox,
                           select the appropriate delimiter, and
                           press 'Submit data'")
      } else {
        if (input$submit_data_button == 0) {
          return(data.frame(x = "Press 'submit data' button"))
        } else {
          isolate({
            data <- readr::read_delim(input$data_paste,
                               delim = input$text_delim,
                               col_names = TRUE)
          })
        }
      }
    } else if (input$data_input == 4){
      data <- dataset
    }
    return(data)
  })

  return (df_shiny)

}



csvFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })

  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
             header = input$heading,
             quote = input$quote,
             stringsAsFactors = stringsAsFactors)
  })

  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })

  # Return the reactive that yields the data frame
  return(dataframe)
}





