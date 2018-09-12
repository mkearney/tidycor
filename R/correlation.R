#' Correlation syntax
#'
#' Correlates with
#'
#' @param e1 Input 1
#' @param e2 Input 2
#' @param env Environment
#' @importFrom rlang !!
#' @export
`%~~%` <- function(e1, e2, env = NULL) {
  if (rlang::is_closure(e1)) {
    a <- rlang::eval_tidy(as.character(substitute(!!e1)))
  } else if (is_correlation(e1)) {
    a <- model_vars(e1)
    env <- attr(e1, "environment")
  } else if (is.character(e1)) {
    a <- e1
  } else {
    #a <- e1
    a <- as.character(substitute(e1))
  }
  if (rlang::is_closure(e2)) {
    b <- rlang::eval_tidy(as.character(substitute(!!e2)))
  } else if (is_correlation(e2)) {
    b <- model_vars(e2)
    env <- attr(e2, "environment")
  } else if (is.character(e2)) {
    b <- e2
  } else {
    #b <- e2
    b <- as.character(substitute(e2))
  }
  formula <- paste(c(a, b), collapse = " + ")
  ## environment name
  if (is.null(env)) {
    e <- paste(c("0x1", sample(letters, 3), sample(0:9, 1), sample(letters, 2), sample(0:9, 1)), collapse = "")
    ## store in global
    assign(e, new.env(), envir = .GlobalEnv)
  } else {
    e <- env
  }
  structure(
    as.list(unlist(c(a, b))),
    formula = as.character(formula),
    class = c("correlation"),
    environment = e)
}

#' Correlation method
#'
#' Conducts correlation analysis
#' @export
correlation <- function(x, ...) UseMethod("correlation")

#' @export
correlation.correlation <- function(x, ...) {
  x <- model_data(x)
  correlation(x)
}

#' @export
correlation.default <- function(x, ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    x <- as_correlation(x)
    x <- dots$data[model_vars(x)]
  }
  cor(as.matrix(x), use = "everything")
}

#' @export
as_correlation <- function(e1, e2) UseMethod("as_correlation")

#' @export
as_correlation.default <- function(e1, e2, env = NULL) {
  if (rlang::is_closure(e1)) {
    a <- rlang::eval_tidy(as.character(substitute(!!e1)))
  } else if (is_correlation(e1)) {
    a <- model_vars(e1)
    env <- attr(e1, "environment")
  } else if (is.character(e1)) {
    a <- e1
  } else {
    a <- as.character(substitute(e1))
  }
  if (rlang::is_closure(e2)) {
    b <- rlang::eval_tidy(as.character(substitute(!!e2)))
  } else if (is_correlation(e2)) {
    b <- model_vars(e2)
    env <- attr(e2, "environment")
  } else if (is.character(e2)) {
    b <- e2
  } else {
    b <- as.character(substitute(e2))
  }
  formula <- paste(c(a, b), collapse = " + ")
  ## environment name
  if (is.null(env)) {
    e <- paste(c("0x1", sample(letters, 3), sample(0:9, 1), sample(letters, 2), sample(0:9, 1)), collapse = "")
    ## store in global
    assign(e, new.env(), envir = .GlobalEnv)
  } else {
    e <- env
  }
  structure(
    as.list(unlist(c(a, b))),
    formula = as.character(formula),
    class = c("correlation"),
    environment = e)
}

#' @export
model_data <- function(x) {
  vars <- model_vars(x)
  e <- model_env(x)
  data <- lapply(x, function(.x) get(.x, envir = e))
  names(data) <- vars
  tibble::as_tibble(data)
}

#' @export
model_vars <- function(x) {
  f <- model_formula(x)
  strsplit(f, "\\s?\\+\\s?")[[1]]
}

#' @export
model_formula <- function(x) attr(x, "formula")

#' @export
model_data <- function(x) {
  formula <- lapply(list(a, b), function(.x) rlang::eval_tidy(as_formula(.x)))
}

#' @export
model_env <- function(x) get(attr(x, "environment"))

#' @export
as_formula <- function(x) UseMethod("as_formula")

#' @export
as_formula.default <- function(x) as.formula(x)

#' @export
as_formula.correlation <- function(x) {
  f <- as.formula(paste0(unclass(x), " ~ ", unclass(x)))
  structure(f, class = c("formula"),
    environment = e)
}

#' @export
print.correlation <- function(x) {
  e <- attr(x, "environment")
  x <- model_formula(x)
  f <- paste0(x, " ~~ ", x)
  cat("# A correlation:\n")
  cat(f, fill = TRUE)
  cat(paste0("<environment:", e, ">"), fill = TRUE)
}


#' @export
model_formula <- function(x) attr(x, "formula")

#' @export
`+` <- function(e1, e2) UseMethod("+")

#' @export
`+.default` <- function(e1, e2) {
  if (is_correlation(e1)) {
    as_correlation(e1, e2)
  } else {
    base::`+`(e1, e2)
  }
}

#' @export
is_correlation <- function(x) inherits(x, "correlation")

#' @export
`+.correlation` <- function(e1, e2) {
  b <- as.character(substitute(e2))
  as_correlation(e1, b)
}


`%~~~%` <- function(lhs, rhs, env = parent.frame()) {
  lhs <- rlang::enquo(lhs)
  rhs <- rlang::enquo(rhs)
  v1 <- as.character(lhs)[2]
  v2 <- as.character(rhs)[2]
  lhs <- rlang::eval_tidy(lhs)
  if (NCOL(lhs) > 1) {
    v1 <- names(lhs)
  }
  rhs <- rlang::eval_tidy(rhs)
  if (NCOL(rhs) > 1) {
    v2 <- names(rhs)
  }
  if (identical(v1, v2)) {
    x <- cor(lhs, use = "pairwise.complete.obs")
    x <- as.data.frame(x)
  } else {
    x <- cor(lhs, rhs, use = "pairwise.complete.obs")
    x <- as.data.frame(x)
  }
  cors <- unique(apply(combn(c(v1, v2), 2), 2, paste0, collapse = "~~"))
  cors <- strsplit(cors, "~~")
  xx <- sapply(cors, function(.x) .x[[1]])
  yy <- sapply(cors, function(.x) .x[[2]])
  x <- unlist(lapply(x, unlist))
  df <- data.frame(x = xx, y = yy, r = x, stringsAsFactors = FALSE,
    row.names = NULL, check.rows = FALSE, check.names = FALSE)
  tibble::as_tibble(df)
}

