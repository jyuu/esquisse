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
  
  # get_settings <- callModule(pivotSettingsServer, "pivot_settings")
  get_settings <- reactiveValues()
  
  observe({
    get_settings$pivot_type <- input$pivot_type
    get_settings$names_column <- input$names_col
    get_settings$values_column <- input$values_col
    get_settings$drop_na <- input$dropna
  })
  
  tidyrCall <- reactiveValues(code = "")

  output$viewer <- DT::renderDataTable({
    tidyr_call_result <- tidyr_call(
      data = dataChart$name,
      targets = input$dragvars$target[[1]],
      settings = get_settings
    )
    
    tidyrCall$code <- rlang::expr_deparse(tidyr_call_result$call)
    tidyrCall$result <- tidyr_call_result$call_result
    
    out <- callModule(
      module = pivotCodeServer,
      id = "tidyr_code", 
      code_expr = tidyrCall$code
    )
    
    if (!rlang::is_string(tidyrCall$result)) {
      tidyrCall$result
    } else {
      shiny::showNotification(
        "Cannot parse pivot table with specified settings.", 
        type = "error"
      )
    }
    
  })
  
  
  
  # callModule(
  #   module = pivotCodeServer,
  #   id = "tidyr_code",
  #   # data = dataChart$name,
  #   targets = reactive(tidyrCall$code)
  #   # settings = get_settings
  #   # targets = reactive(input$dragsvars$target[[1]]),
  #   # settings = reactive(get_settings)
  # )
  
  observeEvent(input$close, shiny::stopApp())
  
  output_module <- reactiveValues(code_plot = NULL, code_filters = NULL, data = NULL)
  
  return(output_module)
}
