#' Make a tidyr call
#' 
#' @param data A \code{reactiveValues} with at least a slot \code{data} 
#'   containing a \code{data.frame} to use in the module. And a slot 
#'   \code{name} corresponding to the name of the \code{data.frame}.
#' @param targets_fix Desired variables to fix in the \code{data.frame}. 
#' @param targets_pivot Desired variables to pivot on in the \code{data.frame}. 
#'   Note, if \code{targets_fix} is provided, the \code{target_pivots} 
#'   need not be. 
#' @param settings Desired pivot settings as selected by the user. 
#' 
#' @return An expression that can be parsed.
#' 
#' @export
#' 
#' @importFrom rlang syms sym is_string expr
tidyr_call <- function(data = NULL,
                       targets_fix = NULL,
                       targets_pivot = NULL,
                       settings = NULL) {
  
  if (is.null(data)) {
    return("")
  }
  
  data <- sym(data)
  
  targets_fix <- dropNulls(targets_fix)
  targets_pivot <- dropNulls(targets_pivot)
  
  cols <- expr(-c(!!!syms(targets_fix)))
  
  if (settings$pivot_type == "longer") {
    settings <- syms(settings)
    call <- expr(
      pivot_longer(
        data = !!data,
        cols = !!cols,
        names_to = paste0(expr(!!settings$names_column)),
        values_to = paste0(expr(!!settings$values_column))
      )
    )
  } else {
    settings <- syms(settings)
    call <- expr(
      pivot_wider(
        data = !!data,
        names_from = paste0(expr(!!settings$names_column)),
        values_from = paste0(expr(!!settings$values_column))
      )
    )
  }
  call
}
