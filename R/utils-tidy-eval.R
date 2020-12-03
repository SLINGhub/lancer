#' Tidy eval helpers
#'
#' @description
#'
#' * \code{\link[rlang]{sym}()} creates a symbol from a string and
#'   \code{\link[rlang:sym]{syms}()} creates a list of symbols from a
#'   character vector.
#'
#' * \code{\link[rlang:nse-defuse]{enquo}()} and
#'   \code{\link[rlang:nse-defuse]{enquos}()} delay the execution of one or
#'   several function arguments. \code{enquo()} returns a single quoted
#'   expression, which is like a blueprint for the delayed computation.
#'   \code{enquos()} returns a list of such quoted expressions.
#'
#' * \code{\link[rlang:nse-defuse]{expr}()} quotes
#'   a new expression _locally_. It is mostly useful to build new
#'   expressions around arguments
#'   captured with [enquo()] or [enquos()]:
#'   \code{expr(mean(!!enquo(arg), na.rm = TRUE))}.
#'
#' * \code{\link[rlang]{as_name}()} transforms a quoted variable name
#'   into a string. Supplying something else than a quoted variable
#'   name is an error.
#'
#'   That's unlike \code{\link[rlang]{as_label}()} which also returns
#'   a single string but supports any kind of R object as input,
#'   including quoted function calls and vectors. Its purpose is to
#'   summarise that object into a single label. That label is often
#'   suitable as a default name.
#'
#'   If you don't know what a quoted expression contains (for instance
#'   expressions captured with \code{enquo()} could be a variable
#'   name, a call to a function, or an unquoted constant), then use
#'   \code{as_label()}. If you know you have quoted a simple variable
#'   name, or would like to enforce this, use \code{as_name()}.
#'
#' To learn more about tidy eval and how to use these tools, visit
#' \url{https://tidyeval.tidyverse.org} and the
#' \href{https://adv-r.hadley.nz/metaprogramming.html}{Metaprogramming
#' section} of \href{https://adv-r.hadley.nz}{Advanced R}.
#'
#' @md
#' @name tidyeval
#' @keywords internal
#' @importFrom rlang expr enquo enquos sym syms .data := as_name as_label
#' @aliases expr enquo enquos sym syms .data := as_name as_label
#' @export expr enquo enquos sym syms .data := as_name as_label
NULL


required_package <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      pkg, " package needed to be installed before using this function. ",
      "Type this in R: install.packages('", pkg, "')"
    )
  }
}

#' @title Symbol reduction
#' @description
#' An approach to reducing a vector of characters to
#' a single expression.
#' @param syms A vector of characters.
#' @param op
#' Operation used to glue the characters together, Default: '+'.
#' @return
#' A single expression that to be used for
#' non-standard evaluation like formula in `stats::lm()`.
#' @details
#' From <https://github.com/tidyverse/glue/issues/108>, it is not advisable
#' to generate code expression with strings. This function is meant to provide
#' a more robust way to generate expressions from strings
#' Code is taken from
#' <https://community.rstudio.com/t/tidy-evaluation-and-formulae/4561/12>
#' @examples
#' syms_reduce(c("a"))
#' syms_reduce(c("a", "b", "c"))
#' syms_reduce(c("1()", "`a`e", "_foo"))
#' syms_reduce(c("a", "b", "c"), op = "*")
#' syms_reduce(c("a", "b", "c"), op = "plus")
#' @export
#' @rdname syms_reduce
syms_reduce <- function(syms, op = "+") {
  exprs_reduce(rlang::syms(syms), op = op)
}

#' @title Expression reduction
#' @description
#' An approach to reducing a list of expressions
#' into one long expression
#' @param exprs A list of expression output from `rlang::exprs`
#' @param op
#' Operation used to glue the expression together, Default: '+'.
#' @return
#' A single expression that to be used for
#' non-standard evaluation like formula in `stats::lm()`.
#' @details
#' Code is taken from
#' <https://community.rstudio.com/t/tidy-evaluation-and-formulae/4561/12>
#' @examples
#' exprs <- rlang::exprs(foo(), bar(baz), quux + blip)
#' exprs_reduce(exprs, "-")
#' @export
#' @rdname exprs_reduce
exprs_reduce <- function(exprs, op = "+") {

  if (length(exprs) == 0) {
    rlang::abort("Empty list of expressions")
  }
  if (length(exprs) == 1) {
    return(exprs[[1]])
  }

  op <- rlang::sym(op)
  purrr::reduce(exprs, function(x, y) rlang::expr((!!op)(!!x, !!y)))
}
