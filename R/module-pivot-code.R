pivotCodeUI <- function(id, insert_code = TRUE) {
  ns <- NS(id)
  
  tagList(
    tags$button(
      class = "btn btn-default btn-xs pull-right btn-copy-code",
      "Copy to clipboard", `data-clipboard-target` = paste0("#", ns("codetidyr"))
    ), tags$script("new Clipboard('.btn-copy-code');"),
    tags$br(),
    tags$b("Code:"),
    uiOutput(outputId = ns("code")),
    tags$textarea(id = ns("holderCode"), style = "display: none;"),
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
                            tidyr_code) {
  output$code <- renderUI({
    code <- tidyr_code$code
    
    htmltools::tagList(
      rCodeContainer(id = ns("codetidyr"), code)
    )
  })
}

