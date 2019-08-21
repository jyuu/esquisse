#' Server associated with pivot Shiny module.
#' 
#' @param input,output,session Standards \code{shiny} server arguments.
#' @param data A \code{reactiveValues} with at least a slot \code{data} 
#'   containing a \code{data.frame} to use in the module. And a slot 
#'   \code{name} corresponding to the name of the \code{data.frame}.
#' @param dataModule Data module to use, choose between \code{"GlobalEnv"}
#'   or \code{"ImportFile"}.
#' @param sizeDataModule Size for the modal window for selecting data.
#'
#' @export
#'
#' @importFrom shiny callModule reactiveValues observeEvent renderPrint
#'   renderPlot stopApp showNotification isolate reactiveValuesToList
#' @importFrom rlang expr_deparse
pivotServer <- function(input, output, session, 
                        data = NULL, 
                        dataModule = c("GlobalEnv", "ImportFile"), 
                        sizeDataModule = "m") {
  
  observeEvent(data$data, {
    dataChart$data <- data$data
    dataChart$name <- data$name
  }, ignoreInit = FALSE)
  
  dataChart <- callModule(
    module = chooseDataServer,
    id = "choose-data",
    data = isolate(data$data),
    name = isolate(data$name),
    launchOnStart = is.null(isolate(data$data)),
    dataModule = dataModule, size = sizeDataModule
  )
  
  observeEvent(dataChart$data, {
    var_choices <- names(dataChart$data)
    updateDragulaInput(
      session = session,
      inputId = "dragvars", 
      status = NULL,
      choiceValues = var_choices,
      choiceNames = badgeType(
        col_name = var_choices,
        col_type = col_type(dataChart$data[, var_choices])
      ),
      badge = FALSE
    )
  })
  
  user_pivot_settings <- reactiveValues()
  get_settings <- callModule(pivotSettingsServer, "pivot_settings")
  
  observe({
    user_pivot_settings$pivot_type <- get_settings$pivot_type
    user_pivot_settings$names_column <- get_settings$names_column
    user_pivot_settings$values_column <- get_settings$values_column
  })

  output$pivot_settings_text <- renderText({
    paste("Desired pivot is: ", user_pivot_settings$pivot_type,
          " with cols ", user_pivot_settings$names_column,
          " and vals ", user_pivot_settings$values_column)
  })

  tidyrCall <- reactiveValues(code = "")


  output$tidyr_code <- renderText({
    tidyr_call_result <- tidyr_call(
      data = dataChart$name,
      targets_fix = input$dragvars$target[[1]],
      targets_pivot = input$dragvars$target[[2]],
      settings = get_settings
    )
    tidyrCall$code <- rlang::expr_deparse(tidyr_call_result)
    paste0(tidyrCall$code)
  })
  
  # cleaned_tidyr_code <- reactiveValues({code = NULL})
  # cleaned_tidyr_code <- callModule(
  #   module = pivotCodeServer,
  #   tidyr_code = tidyrCall 
  # )
  
  output$viewer <- DT::renderDataTable({
    # dataChart$data
    safe_tidyr_call(expr = tidyrCall$code,
                    data = dataChart$data)
  })

  observeEvent(input$close, shiny::stopApp())
  
  output_module <- reactiveValues(code_plot = NULL, code_filters = NULL, data = NULL)
  
  return(output_module)
}
