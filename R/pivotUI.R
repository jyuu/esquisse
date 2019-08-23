#' Pivot Shiny module.
#'
#' @param id Module id.
#' @param container Defaults to using the \code{esquisse} container as the
#'   addin display.
#' @param insert_code Logical. Displays a button to insert \pkg{tidyr} code
#'   in the current user script. Defaults to \code{TRUE}.
#'
#' @return Reactive values with 2 slots containing the code to generate
#'   the pivot table result and the resulting dataframe.
#'
#' @export
#'
#' @importFrom htmltools tags tagList singleton
#' @importFrom shiny icon NS textOutput
#' @importFrom miniUI miniPage miniTabstripPanel miniTabPanel miniContentPanel miniTitleBarButton
pivotUI <- function(id,
                    container = esquisseContainer(),
                    insert_code = TRUE) {
  ns <- NS(id)
  
  box_title <- tags$div(
    
    class="gadget-title dreamrs-title-box",
    
    tags$h1("Pivot helper", class = "dreamrs-title"),
    
    tags$div(
      class = "pull-right",
      miniTitleBarButton(inputId = ns("close"), label = "Close")
    ),
    
    tags$div(
      class = "pull-left",
      chooseDataUI(id = ns("choose-data"), class = "btn-primary")
    )
    
  )
  
  addin <- miniPage(
    
    # style sheet
    singleton(x = tagList(
      tags$link(rel="stylesheet", type="text/css", href="esquisse/styles.css"),
      tags$script(src = "esquisse/clipboard/clipboard.min.js")
    )),
    
    # header
    box_title,
    
    miniTabstripPanel(
      
      # pivot settings
      miniTabPanel("Pivot Settings", icon = icon("arrows"),
        miniContentPanel(
          
          prettyRadioButtons(
            inputId = ns("pivot_type"),
            label = "Choose a type of pivot desired: ",
            choices = c("longer", "wider"),
            inline = TRUE,
            status = "info",
            fill = TRUE
          ),
          
          conditionalPanel(
            condition = paste0("input['", ns("pivot_type"), "']=='longer'"),
            dragulaInput(
              inputId = ns("dragvars"),
              sourceLabel = "Variables",
              targetsLabels = c("Pivot Columns"),
              targetsIds = c("fvar"),
              choices = "",
              badge = FALSE,
              width = "100%",
              height = "200px",
              replace = FALSE
            )
          ), 
          
          conditionalPanel(
            sprintf("input['%s'] != 'wider'", ns("pivot_type")),
            textInputRow(
              inputId = ns("names_col"),
              label = "Enter new column name to store old column data: ",
              value = "name"
            ),
            textInputRow(
              inputId = ns("values_col"),
              label = "Enter new column name to store old cell values: ",
              value = "value"
            ),
            checkboxInput(ns("dropna"), "Drop NA values?")
          ),
          
          tags$br()
        )
      ),
      
      # viewer
      miniTabPanel("View", icon = icon("eye"), 
        miniContentPanel(
          DT::dataTableOutput(ns("viewer"))
        )
      ),
      
      # export code
      miniTabPanel("Export", icon = icon("laptop-code"),
        miniContentPanel(
          pivotCodeUI(
            ns("tidyr_code"),
            insert_code = TRUE
          )
        )
      )
      
    ) # end of miniTabstripPanel
  )
  
  if (is.function(container)) {
    addin <- container(addin)
  }
  
  return(addin)
}
