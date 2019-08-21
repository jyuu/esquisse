#' Make a tidyr call
#' 
#' @param data A \code{reactiveValues} with at least a slot \code{data} 
#'   containing a \code{data.frame} to use in the module. And a slot 
#'   \code{name} corresponding to the name of the \code{data.frame}.
#' @param targets Desired variables to pivot on in the \code{data.frame}. 
#' @param settings Desired pivot settings as selected by the user. 
#' 
#' @return An expression that can be parsed.
#' 
#' @export
#' 
#' @importFrom rlang expr
#' @importFrom tidyr pivot_longer pivot_wider
tidyr_call <- function(data = NULL,
                       targets= NULL,
                       settings = NULL) {
  
  if (is.null(data)) {
    return("")
  }
  
  data <- rlang::sym(data)
  
  syms2 <- function(x) {
    if (rlang::is_string(x)) {
      return(rlang::sym(x))
    } else {
      return(x)
    }
  }
  
  targets <- dropNulls(targets)
  
  cols <- expr(c(!!!rlang::syms(targets)))
  
  if (rlang::is_null(settings)) {
    call <- "No call can be produced yet."
  } else {
    if (settings$pivot_type == "longer") {
      settings <- lapply(settings, syms2)
      call <- expr(
        pivot_longer(
          data = !!data,
          cols = !!cols,
          names_to = rlang::as_string(expr(!!settings$names_column)),
          values_to = rlang::as_string(expr(!!settings$values_column)),
          values_drop_na = !!settings$drop_na 
        )
      )
    } else if (settings$pivot_type == "wider") {
      settings <- lapply(settings, syms2)
      call <- expr(
        pivot_wider(
          data = !!data,
          names_from = rlang::as_string(expr(!!settings$names_column)),
          values_from = rlang::as_string(expr(!!settings$values_column))
        )
      )
    } else {
      call <- "No call can be produced yet."
    }
  }
  
  # Evaluate call 
  call_result <- try(eval(call), silent = TRUE)
  
  if (inherits(call_result, "try-error")) {
    call_result <- "Output transformation is not possible."
  }
  
  return(list(call = call, call_result = call_result))
}
