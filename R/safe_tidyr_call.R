
safe_tidyr_call <- function(expr, data = NULL) {
  if(!is.null(data)) {
    output <- rlang::eval_tidy(expr = expr, data = data)
  } else {
    output <- mtcars
  }
  return(output)
}