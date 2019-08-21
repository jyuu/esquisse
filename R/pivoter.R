#' Addin to aid with visualizing the result of pivoting with `tidyr`.
#'
#' @param data A \code{data.frame} passed by the user, or one selected from
#'   the global environment.
#' @param coerceVars Logical. Variables can be coerced to a different type
#'   when selecting data. Defaults to \code{FALSE}.
#' @return Code to reproduce the pivoted dataframe.
#'
#' @export
#'
#' @importFrom shiny dialogViewer runGadget reactiveValues
pivoter <- function(data = NULL, coerceVars = FALSE) {
  
  options("esquisse.coerceVars" = coerceVars)
  
  res_data <- get_data(data, name = deparse(substitute(data)))
  
  rv <- reactiveValues(
    data = res_data$esquisse_data,
    name = res_data$esquisse_data_name
  )
  
  runGadget(
    app = pivotUI(id = "tidygadget", container = NULL, insert_code = TRUE),
    server = function(input, output, session) {
      callModule(
        module = pivotServer,
        id = "tidygadget",
        data = rv
      )
    },
    viewer = dialogViewer("Tidy gadget", width = 700, height = 800)
  )
}
