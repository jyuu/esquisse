
pivotCodeUI <- function(id, insert_code = TRUE) {
  ns <- NS(id)
  tagList(
    tags$button(
      class = "btn btn-default btn-xs pull-right btn-copy-code",
      "Copy to clipboard", `data-clipboard-target` = paste0("#", ns("codetidyr"))
    ), 
    tags$script("new Clipboard('.btn-copy-code');"),
    tags$br(),
    tags$b("Code:"),
    uiOutput(outputId = ns("code")),
    if (insert_code) {
      actionLink(
        inputId = ns("insert_code"),
        label = "Insert code in script",
        icon = icon("arrow-circle-left")
      )
    },
    tags$br()
  )
}

pivotCodeServer <- function(input, output, session,
                            code_expr = NULL) {
  ns <- session$ns
  
  observeEvent(input$insert_code, {
    context <- rstudioapi::getSourceEditorContext()
    code <- code_expr
    if (input$insert_code == 1) {
      code <- paste("library(tidyr)", code, sep = "\n\n")
    }
    rstudioapi::insertText(text = paste0("\n", code), id = context$id)
  })
  
  output$code <- renderUI({
    code <- code_expr
    htmltools::tagList(
      rCodeContainer(id = ns("codetidyr"), code)
    )
  })
}

