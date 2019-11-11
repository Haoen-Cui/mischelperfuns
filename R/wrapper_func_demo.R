#' @export
#' @name func_1
#' @title Dummy Function #1 To be Wrapped
#' @description A dummy function for demo of wrapper function with
#'     \code{...} arguments. This function adds two numeric values / vectors.
#' @author Haoen Cui
#'
#' @param x (numeric) a single numeric value or vector,
#'     needs to be compatible with the other argument \code{y} for addition.
#' @param y (numeric) a single numeric value or vector,
#'     needs to be compatible with the other argument \code{x} for addition.
#'
#' @return a numeric vector as addition result of inputs \code{x} and \code{y}.
#'     Its dimension is determined according to basting rules.
#'
#' @examples
#' func_1(x = 1, y = 2)
func_1 <- function(x = 1, y = 2) {
  # input validation
  stopifnot(all(is.numeric(x), is.numeric(y)))
  # some computation
  return( x + y )
}


#' @export
#' @name func_2
#' @title Dummy Function #2 To be Wrapped
#' @description A dummy function for demo of wrapper function with
#'     \code{...} arguments. This function multiplies two numeric values / vectors.
#' @author Haoen Cui
#'
#' @param a (numeric) a single numeric value or vector,
#'     needs to be compatible with the other argument \code{b} for multiplication
#' @param b (numeric) a single numeric value or vector,
#'     needs to be compatible with the other argument \code{a} for multiplication
#'
#' @return a numeric vector as multiplication result of inputs \code{a} and \code{b}.
#'     Its dimension is determined according to basting rules.
#'
#' @examples
#' func_2(a = 1, b = 2)
func_2 <- function(a = 1, b = 2) {
  # input validation
  stopifnot(all(is.numeric(a), is.numeric(b)))
  # some computation
  return( a * b )
}


#' @export
#' @name wrapper_funcs
#' @title Dummy Function #2 To be Wrapped
#' @description A dummy function for demo of wrapper function with
#'     \code{...} arguments. This function multiplies two numeric values / vectors.
#' @author Haoen Cui
#'
#' @param some_arg (numeric) a single numeric value or vector,
#'     needs to be compatible with the other argument \code{...} for arithmetics.
#' @param ... named argument value pairs to be passed to \code{func_1} and \code{func_2}
#'
#' @return a numeric vector as a result of some arithmetics using inputs.
#'     Its dimension is determined according to basting rules.
#'
#' @examples
#' # specifiy all keyword arguments 
#' wrapper_funcs(some_arg = 1, x = 2, y = 3, a = 4, b = 5)
#' # specify some (but not all) keyword arguments 
#' wrapper_funcs(some_arg = 1, x = 2, y = 3, a = 4)
#' # only keyword arguments are passed to underlying functions 
#' wrapper_funcs(some_arg = 1, x = 2, y = 3, a = 4, 5) 
#' # unrecognized keyword arguments will be ignored 
#' wrapper_funcs(some_arg = 1, x = 2, y = 3, a = 4, d = 5) 
wrapper_funcs <- function(some_arg = 1, ...) {
  # input validation
  stopifnot(is.numeric(some_arg))
  # proper dispatch of arguments
  dot_args <- list(...)
  func_1_args <- intersect(names(formals(func_1)), names(dot_args))
  func_2_args <- intersect(names(formals(func_2)), names(dot_args))
  # do something
  func_1_res <- do.call("func_1", args = dot_args[func_1_args])
  func_2_res <- do.call("func_2", args = dot_args[func_2_args])
  return( some_arg - func_1_res + func_2_res )
}
