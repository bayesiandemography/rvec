
## User-visible constructors --------------------------------------------------

#' Create a Blank Rvec
#'
#' Create an rvec, consisting entirely of `NAs`,
#' with a given length and number of draws.
#'
#' The type of the object is taken from `x`.
#' If `typeof(x)` is `"integer"`, for instance,
#' then `new_rvec()` returns an object
#' of class `"rvec_int"`.
#'
#' @param x Object with the intended type.
#' Default is `double()`.
#' @param length Desired length of rvec.
#' Default is `0`.
#' @param n_draw Number of draws of rvec.
#' Default is `1000`.
#'
#' @returns An rvec.
#'
#' @seealso
#' - [rvec()] [rvec_chr()], [rvec_dbl()], [rvec_int()], [rvec_lgl()]
#'   Create an rvec from data.
#' - [n_draw()] Query number of draws.
#'
#' @examples
#' new_rvec()
#' new_rvec(TRUE, length = 3, n_draw = 100)
#'
#' x <- new_rvec(length = 2)
#' x[1] <- rnorm_rvec(n = 1, n_draw = 1000)
#' x[2] <- runif_rvec(n = 1, n_draw = 1000)
#' @export
new_rvec <- function(x = double(), length = 0, n_draw = 1000) {
  check_nonneg_num_scalar(length)
  length <- as.integer(length)
  check_nonneg_num_scalar(n_draw)
  n_draw <- as.integer(n_draw)
  type <- typeof(x)
  if (type == "character")
    na <- NA_character_
  else if (type == "integer")
    na <- NA_integer_
  else if (type == "double")
    na <- NA_real_
  else if (type == "logical")
    na <- NA
  else {
    valid_types <- c("character", "integer", "double", "logical")
    cli::cli_abort(c("Invalid type.",
                     i = "{.arg x} has type {.val {type}}.",
                     i = "Valid types are: {.val {valid_types}}."))
  }
  m <- matrix(na, nrow = length, ncol = n_draw)
  rvec(m)
}


## HAS_TESTS
#' Create an Rvec from Data
#'
#' Create an object of class `"rvec"`, based
#' on input data.
#'
#' Class `"rvec"` has four subclasses, each dealing with
#' a diffent type:
#' - `"rvec_dbl"` doubles
#' - `"rvec_int"` integers
#' - `"rvec_lgl"` logical
#' - `"rvec_chr"` character
#'
#' These subclasses are analogous to [double()],
#' [integer()], [logical()], and [character()]
#' vectors.
#'
#' Function `rvec()` chooses the subclass, based on
#' `x`. Functions `rvec_dbl()`, `rvec_int()`,
#' `rvec_lgl()`, and `rvec_chr()` each create
#' objects of a particular subclass.
#' 
#' `x` can be
#' - a matrix, where each row is a set of draws
#'   for an unknown quantity;
#' - a list, where each element is a set of draws;
#' - an atomic vector, which is treated as a
#'   single-column matrix; or
#' - an rvec.
#'
#' @param x A matrix, a list of vectors,
#' an atomic vector, or an rvec.
#'
#' @returns
#' An rvec with the following class:
#' - `rvec_dbl()`: `"rvec_dbl"`
#' - `rvec_int()`: `"rvec_int"`
#' - `rvec_lgl()`: `"rvec_lgl"`
#' - `rvec_chr()`: `"rvec_chr"`
#' - `rvec()`: `"rvec_chr"`, `"rvec_dbl"`
#' `"rvec_int"`, or `"rvec_lgl"`
#'
#' @seealso
#' - [new_rvec()] Create a blank rvec.
#' - [collapse_to_rvec()] Create rvecs within
#'   a data frame.
#' - [rnorm_rvec()], [rbinom_rvec()], etc. Create rvecs
#'   representing probability distributions. 
#'   
#' @examples
#' m <- rbind(c(-1.5, 2, 0.2),
#'            c(-2.3, 3, 1.2))
#' rvec_dbl(m)
#'
#' l <- list(rpois(100, lambda = 10.2),
#'           rpois(100, lambda = 5.5))
#' rvec(l)
#'
#' rvec(letters[1:5])
#'
#' l <- list(a = c(TRUE, FALSE),
#'           b = c(FALSE, TRUE))
#' rvec(l)
#' @export
rvec <- function(x) {
  if (is_rvec(x))
    x <- vctrs::field(x, "data")
  else if (is.matrix(x) || is_Matrix(x)) {
    x <- as.matrix(x)
    check_x_has_at_least_one_col(x)
  }
  else if (is.list(x)) {
    if (length(x) == 0L)
      cli::cli_abort("If {.arg x} is a list, it must have at least one element.")
    check_lengths_nonzero(x)
    check_lengths_equal(x)
    x <- do.call(rbind, args = x)
  }
  else if (is.atomic(x) && is.vector(x)) {
    nms <- names(x)
    x <- matrix(x, ncol = 1L)
    rownames(x) <- nms
  }
  else
    cli::cli_abort(c("{.arg x} must be an rvec, a matrix, a list, or an atomic vector.",
                     i = "{.arg x} has class {.cls {class(x)}}"))
  colnames(x) <- NULL
  if (is.character(x))
    new_rvec_chr(x)
  else if (is.double(x))
    new_rvec_dbl(x)
  else if (is.integer(x))
    new_rvec_int(x)
  else if (is.logical(x))
    new_rvec_lgl(x)
  else
    cli::cli_abort(c("{.arg x} must be double, integer, logical, or character",
                     i = "{.arg x} has type {typeof(x)}"))

}

## HAS_TESTS
#' @export
#' @rdname rvec
rvec_chr <- function(x = NULL) {
  if (is_rvec(x)) {
    m <- field(x, "data")
    data_vec <- as.vector(m)
    data_vec <- as.character(data_vec)
    data <- matrix(data_vec,
                   nrow = nrow(m),
                   ncol = ncol(m))
    rownames(data) <- rownames(m)
  }
  else if (is.null(x))
    data <- matrix(character(), nrow = 0, ncol = 1L)
  else if (is.matrix(x)) {
    check_x_has_at_least_one_col(x)
    data_vec <- as.character(x)
    data <- matrix(data_vec,
                   nrow = nrow(x),
                   ncol = ncol(x))
    rownames(data) <- rownames(x)
  }
  else if (is.list(x)) {
    if (length(x) > 0L) {
      check_lengths_nonzero(x)
      check_lengths_equal(x)
      data_vec <- unlist(x)
      data_vec <- as.character(data_vec)
      data <- matrix(data_vec,
                     nrow = length(x),
                     ncol = length(x[[1L]]),
                     byrow = TRUE)
      rownames(data) <- names(x)
    }
    else
      data <- matrix(character(), nrow = 0, ncol = 1L)
  }
  else if (is.atomic(x) && is.vector(x)) {
    data <- as.character(x) ## vec_cast is too strict
    data <- matrix(data, ncol = 1L)
    rownames(data) <- names(x)
  }
  else
    cli::cli_abort(c("{.arg x} must be an rvec, a matrix, a list, an atomic vector, or NULL.",
                     i = "{.arg x} has class {.cls {class(x)}}"))
  new_rvec_chr(data)
}

## HAS_TESTS
#' @export
#' @rdname rvec
rvec_dbl <- function(x = NULL) {
  if (is_rvec(x)) {
    m <- field(x, "data")
    data_vec <- as.vector(m)
    data_vec <- vec_cast(data_vec, double())
    data <- matrix(data_vec,
                   nrow = nrow(m),
                   ncol = ncol(m))
    rownames(data) <- rownames(m)
  }
  else if (is.null(x))
    data <- matrix(double(), nrow = 0, ncol = 1L)
  else if (is.matrix(x) || is_Matrix(x)) {
    x <- as.matrix(x)
    check_x_has_at_least_one_col(x)
    data_vec <- as.vector(x)
    data_vec <- vec_cast(data_vec, double())
    data <- matrix(data_vec,
                   nrow = nrow(x),
                   ncol = ncol(x))
    rownames(data) <- rownames(x)
  }
  else if (is.list(x)) {
    if (length(x) > 0L) {
      check_lengths_nonzero(x)
      check_lengths_equal(x)
      data_vec <- unlist(x)
      data_vec <- vec_cast(data_vec, double())
      data <- matrix(data_vec,
                     nrow = length(x),
                     ncol = length(x[[1L]]),
                     byrow = TRUE)
      rownames(data) <- names(x)
    }
    else
      data <- matrix(double(), nrow = 0, ncol = 1L)
  }
  else if (is.atomic(x) && is.vector(x)) {
    data <- vec_cast(x, double())
    data <- matrix(data, ncol = 1L)
    rownames(data) <- names(x)
  }
  else
    cli::cli_abort(c("{.arg x} must be an rvec, a matrix, a list, an atomic vector, or NULL.",
                     i = "{.arg x} has class {.cls {class(x)}}"))
  new_rvec_dbl(data)
}

## HAS_TESTS
#' @export
#' @rdname rvec
rvec_int <- function(x = NULL) {
  if (is_rvec(x)) {
    m <- field(x, "data")
    data_vec <- as.vector(m)
    data_vec <- vec_cast(data_vec, integer())
    data <- matrix(data_vec,
                   nrow = nrow(m),
                   ncol = ncol(m))
    rownames(data) <- rownames(m)
    
  }
  else if (is.null(x))
    data <- matrix(integer(), nrow = 0, ncol = 1L)
  else if (is.matrix(x)) {
    check_x_has_at_least_one_col(x)
    data_vec <- as.vector(x)
    data_vec <- vec_cast(data_vec, integer())
    data <- matrix(data_vec,
                   nrow = nrow(x),
                   ncol = ncol(x))
    rownames(data) <- rownames(x)
  }
  else if (is.list(x)) {
    if (length(x) > 0L) {
      check_lengths_nonzero(x)
      check_lengths_equal(x)
      data_vec <- unlist(x)
      data_vec <- vec_cast(data_vec, integer())
      data <- matrix(data_vec,
                     nrow = length(x),
                     ncol = length(x[[1L]]),
                     byrow = TRUE)
      rownames(data) <- names(x)
    }
    else
      data <- matrix(integer(), nrow = 0, ncol = 1L)
  }
  else if (is.atomic(x) && is.vector(x)) {
    data <- vec_cast(x, integer())
    data <- matrix(data, ncol = 1L)
    rownames(data) <- names(x)
  }
  else
    cli::cli_abort(c("{.arg x} must be an rvec, a matrix, a list, an atomic vector, or NULL.",
                     i = "{.arg x} has class {.cls {class(x)}}"))
  new_rvec_int(data)
}

## HAS_TESTS
#' @export
#' @rdname rvec
rvec_lgl <- function(x = NULL) {
  if (is_rvec(x)) {
    m <- field(x, "data")
    data_vec <- as.vector(m)
    data_vec <- vec_cast(data_vec, logical())
    data <- matrix(data_vec,
                   nrow = nrow(m),
                   ncol = ncol(m))
    rownames(data) <- rownames(m)
  }
  else if (is.null(x))
    data <- matrix(logical(), nrow = 0, ncol = 1L)
  else if (is.matrix(x)) {
    check_x_has_at_least_one_col(x)
    data_vec <- as.vector(x)
    data_vec <- vec_cast(data_vec, logical())
    data <- matrix(data_vec,
                   nrow = nrow(x),
                   ncol = ncol(x))
    rownames(data) <- rownames(x)
  }
  else if (is.list(x)) {
    if (length(x) > 0L) {
      check_lengths_nonzero(x)
      check_lengths_equal(x)
      data_vec <- unlist(x)
      data_vec <- vec_cast(data_vec, logical())
      data <- matrix(data_vec,
                     nrow = length(x),
                     ncol = length(x[[1L]]),
                     byrow = TRUE)
      rownames(data) <- names(x)
    }
    else
      data <- matrix(logical(), nrow = 0, ncol = 1L)
  }
  else if (is.atomic(x) && is.vector(x)) {
    data <- vec_cast(x, logical())
    data <- matrix(data, ncol = 1L)
    rownames(data) <- names(x)
  }
  else
    cli::cli_abort(c("{.arg x} must be an rvec, a matrix, a list, an atomic vector, or NULL.",
                     i = "{.arg x} has class {.cls {class(x)}}"))
  new_rvec_lgl(data)
}


## Internal constructors ------------------------------------------------------

## HAS_TESTS
new_rvec_chr <- function(data) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_chr", "rvec"))
}

## HAS_TESTS
new_rvec_dbl <- function(data) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_dbl", "rvec"))
}

## HAS_TESTS
new_rvec_int <- function(data) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_int", "rvec"))
}

## HAS_TESTS
new_rvec_lgl <- function(data) {
    new_rcrd(fields = list(data = data),
             class = c("rvec_lgl", "rvec"))
}
