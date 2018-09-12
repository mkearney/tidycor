#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Convert object to tibble
#'
#' @inheritParams tibble::as_tibble
#' @return A tibble, class \code{c("tbl_df", "tbl", "data.frame")}
#' @export
as_tbl <- function(x, ...) {
  tibble::as_tibble(x, ...)
}

#' Create a tibble data frame
#'
#' @inheritParams tibble::tibble
#' @return A tibble, class \code{c("tbl_df", "tbl", "data.frame")}
#' @export
tbl_frame <- function(...) {
  xs <- rlang::quos(..., .named = TRUE)
  if (length(xs) == 1L) {
    x <- eval_tidy(xs[[1]])
    if (is.data.frame(x)) {
      return(as_tbl(x))
    }
  }
  as_tbl(tibble:::lst_quos(xs, expand = TRUE))
}
