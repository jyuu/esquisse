#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom htmltools tags tagList
#' @importFrom shiny NS textInput checkboxInput verbatimTextOutput
pivotSettingsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    prettyRadioButtons(
      inputId = ns("pivot_type"),
      label = "Choose a type of pivot desired: ",
      choices = c("longer", "wider"),
      inline = TRUE,
      status = "info",
      fill = TRUE
    ),
    
    conditionalPanel(
      sprintf("input['%s'] != 'wider'", ns("pivot_type")),
      checkboxInput(ns("dropna"), "Drop NA values?")
    ),
    
    tags$br(),
    tags$div(
      style = "width:50%; float:left;",
      textInput(
        inputId = ns("names_col"),
        label = "Enter new column name to store old column data: ",
        value = "name",
        width = "50%"
      )
    ),
    tags$div(
      style = "width:50%;float:right;",
      textInput(
        inputId = ns("values_col"),
        label = "Enter new column name to store old cell values: ",
        value = "value",
        width = "50%"
      )
    ),
    tags$br()
    
  )
}

#' @importFrom shiny reactiveValues observe updateTextInput
pivotSettingsServer <- function(input, output, session) {
  
  settings <- reactiveValues()
  
  observe({
    ns <- session$ns
    
    if (input$pivot_type == "longer") {
      names_call <- "Enter new column name to store old column data: "
      values_call <- "Enter new column name to store old cell values: "
    } else {
      names_call <- "Enter old column name to get column data from: "
      values_call <- "Enter old column name to get cell values from: "
    }
    
    updateTextInput(
      session = session,
      inputId = "names_col",
      label = names_call
    )
    
    updateTextInput(
      session = session,
      inputId = "values_col",
      label = values_call
    )
    
    settings$pivot_type <- input$pivot_type
    settings$names_column <- input$names_col
    settings$values_column <- input$values_col
    settings$drop_na <- input$dropna
  })
  
  return(settings)
}
