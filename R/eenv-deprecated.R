#' @title Deprecated functions in package `een`.
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at `help("-deprecated")`.
#' @name eenv-deprecated
#' @keywords internal
NULL

# For more information see:
# <https://www.r-bloggers.com/r-deprecate-functions-with-roxygen2-2/>
#
# Add this before deprecated functions:
#
#  ## myFun.r
#  #' @title My function.
#  #' @description My function.
#  #' @param arg1 My first argument.
#  #' @param arg2 My second argument.
#  #' @return My return value.
#  #'
#  #' @name myFun-deprecated
#  #' @usage myFun(arg1, arg2)
#  #' @seealso \code{\link{ourPkg-deprecated}}
#  #' @keywords internal
#  NULL
#
#  #' @rdname ourPkg-deprecated
#  #' @section \code{myFun}:
#  #' For \code{myFun}, use \code{\link{myNewFun}}.
#  #'
#  #' @export
#  myFun <- function(arg1, arg2) {
#    .Deprecated("myNewFun")
#    "My return value"
#  }
