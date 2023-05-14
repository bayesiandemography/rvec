
## # Equality ----------------------------------------------------------------

## #' @export
## `==.vctrs_vctr` <- function(e1, e2) {
##   vec_equal(e1, e2)
## }

## #' @export
## `!=.vctrs_vctr` <- function(e1, e2) {
##   !vec_equal(e1, e2)
## }

## #' @export
## is.na.vctrs_vctr <- function(x) {
##   vec_detect_missing(x)
## }

## #' @importFrom stats na.fail
## #' @export
## na.fail.vctrs_vctr <- function(object, ...) {
##   if (vec_any_missing(object)) {
##     # Return the same error as `na.fail.default()`
##     abort("missing values in object")
##   }

##   object
## }

## #' @importFrom stats na.omit
## #' @export
## na.omit.vctrs_vctr <- function(object, ...) {
##   na_remove(object, "omit")
## }

## #' @importFrom stats na.exclude
## #' @export
## na.exclude.vctrs_vctr <- function(object, ...) {
##   na_remove(object, "exclude")
## }

## na_remove <- function(x, type) {
##   # The only difference between `na.omit()` and `na.exclude()` is the class
##   # of the `na.action` attribute

##   if (!vec_any_missing(x)) {
##     return(x)
##   }

##   # `na.omit/exclude()` attach the locations of the omitted values to the result
##   missing <- vec_detect_missing(x)
##   loc <- which(missing)

##   names <- vec_names(x)
##   if (!is_null(names)) {
##     # `na.omit/exclude()` retain the original names, if applicable
##     names <- vec_slice(names, loc)
##     loc <- vec_set_names(loc, names)
##   }

##   attr(loc, "class") <- type

##   out <- vec_slice(x, !missing)
##   attr(out, "na.action") <- loc
##   out
## }

## #' @export
## anyNA.vctrs_vctr <- function(x, recursive = FALSE) {
##   if (recursive && obj_is_list(x)) {
##     any(map_lgl(x, anyNA, recursive = recursive))
##   } else {
##     any(is.na(x))
##   }
## }

## #' @export
## unique.vctrs_vctr <- function(x, incomparables = FALSE, ...) {
##   vec_unique(x)
## }

## #' @export
## duplicated.vctrs_vctr <- function(x, incomparables = FALSE, ...) {
##   vec_duplicate_id(x) != seq_along(x)
## }

## #' @export
## anyDuplicated.vctrs_vctr <- function(x, incomparables = FALSE, ...) {
##   vec_duplicate_any(x)
## }

## # Comparison ----------------------------------------------------------------

## #' @export
## `<=.vctrs_vctr` <- function(e1, e2) {
##   vec_compare(e1, e2) <= 0
## }

## #' @export
## `<.vctrs_vctr` <- function(e1, e2) {
##   vec_compare(e1, e2) < 0
## }

## #' @export
## `>=.vctrs_vctr` <- function(e1, e2) {
##   vec_compare(e1, e2) >= 0
## }

## #' @export
## `>.vctrs_vctr` <- function(e1, e2) {
##   vec_compare(e1, e2) > 0
## }

## #' @export
## xtfrm.vctrs_vctr <- function(x) {
##   proxy <- vec_proxy_order(x)
##   type <- typeof(proxy)

##   if (type == "logical") {
##     proxy <- unstructure(proxy)
##     proxy <- as.integer(proxy)
##     return(proxy)
##   }

##   if (type %in% c("integer", "double")) {
##     proxy <- unstructure(proxy)
##     return(proxy)
##   }

##   vec_rank(proxy, ties = "dense", incomplete = "na")
## }

## #' @importFrom stats median
## #' @export
## median.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
##   # nocov start
##   stop_unimplemented(x, "median")
##   # nocov end
## }

## #' @importFrom stats quantile
## #' @export
## quantile.vctrs_vctr <- function(x, ..., type = 1, na.rm = FALSE) {
##   # nocov start
##   stop_unimplemented(x, "quantile")
##   # nocov end
## }

## vec_cast_or_na <- function(x, to, ...) {
##   tryCatch(
##     vctrs_error_incompatible_type = function(...) vec_init(to, length(x)),
##     vec_cast(x, to)
##   )
## }

## #' @export
## min.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
##   if (vec_is_empty(x)) {
##     return(vec_cast_or_na(Inf, x))
##   }

##   # TODO: implement to do vec_arg_min()
##   rank <- xtfrm(x)

##   if (isTRUE(na.rm)) {
##     idx <- which.min(rank)
##     if (vec_is_empty(idx)) {
##       return(vec_cast_or_na(Inf, x))
##     }
##   } else {
##     idx <- which(vec_equal(rank, min(rank), na_equal = TRUE))
##   }

##   x[[idx[[1]]]]
## }

## #' @export
## max.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
##   if (vec_is_empty(x)) {
##     return(vec_cast_or_na(-Inf, x))
##   }

##   # TODO: implement to do vec_arg_max()
##   rank <- xtfrm(x)

##   if (isTRUE(na.rm)) {
##     idx <- which.max(rank)
##     if (vec_is_empty(idx)) {
##       return(vec_cast_or_na(-Inf, x))
##     }
##   } else {
##     idx <- which(vec_equal(rank, max(rank), na_equal = TRUE))
##   }

##   x[[idx[[1]]]]
## }

## #' @export
## range.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
##   if (vec_is_empty(x)) {
##     return(vec_cast_or_na(c(Inf, -Inf), x))
##   }

##   # Inline `min()` / `max()` to only call `xtfrm()` once
##   rank <- xtfrm(x)

##   if (isTRUE(na.rm)) {
##     idx_min <- which.min(rank)
##     idx_max <- which.max(rank)
##     if (vec_is_empty(idx_min) && vec_is_empty(idx_max)) {
##       return(vec_cast_or_na(c(Inf, -Inf), x))
##     }
##   } else {
##     idx_min <- which(vec_equal(rank, min(rank), na_equal = TRUE))
##     idx_max <- which(vec_equal(rank, max(rank), na_equal = TRUE))
##   }

##   c(x[[idx_min[[1]]]], x[[idx_max[[1]]]])
## }

## # Numeric -----------------------------------------------------------------

## #' @export
## Math.vctrs_vctr <- function(x, ...) {
##   vec_math(.Generic, x, ...)
## }

## #' @export
## Summary.vctrs_vctr <- function(..., na.rm = FALSE) {
##   vec_math(.Generic, vec_c(...), na.rm = na.rm)
## }

## #' @export
## mean.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
##   vec_math("mean", x, na.rm = na.rm)
## }

## #' @export
## is.finite.vctrs_vctr <- function(x) {
##   vec_math("is.finite", x)
## }

## #' @export
## is.infinite.vctrs_vctr <- function(x) {
##   vec_math("is.infinite", x)
## }

## #' @export
## is.nan.vctrs_vctr <- function(x) {
##   vec_math("is.nan", x)
## }

## # Arithmetic --------------------------------------------------------------

## #' @export
## `+.vctrs_vctr` <- function(e1, e2) {
##   if (missing(e2)) {
##     vec_arith("+", e1, MISSING())
##   } else {
##     vec_arith("+", e1, e2)
##   }
## }

## #' @export
## `-.vctrs_vctr` <- function(e1, e2) {
##   if (missing(e2)) {
##     vec_arith("-", e1, MISSING())
##   } else {
##     vec_arith("-", e1, e2)
##   }
## }

## #' @export
## `*.vctrs_vctr` <- function(e1, e2) {
##   vec_arith("*", e1, e2)
## }

## #' @export
## `/.vctrs_vctr` <- function(e1, e2) {
##   vec_arith("/", e1, e2)
## }

## #' @export
## `^.vctrs_vctr` <- function(e1, e2) {
##   vec_arith("^", e1, e2)
## }

## #' @export
## `%%.vctrs_vctr` <- function(e1, e2) {
##   vec_arith("%%", e1, e2)
## }

## #' @export
## `%/%.vctrs_vctr` <- function(e1, e2) {
##   vec_arith("%/%", e1, e2)
## }

## #' @export
## `!.vctrs_vctr` <- function(x) {
##   vec_arith("!", x, MISSING())
## }

## #' @export
## `&.vctrs_vctr` <- function(e1, e2) {
##   vec_arith("&", e1, e2)
## }

## #' @export
## `|.vctrs_vctr` <- function(e1, e2) {
##   vec_arith("|", e1, e2)
## }

## # Unimplemented ------------------------------------------------------------

## #' @export
## summary.vctrs_vctr <- function(object, ...) {
##   # nocov start
##   stop_unimplemented(object, "summary")
##   # nocov end
## }

## # Unsupported --------------------------------------------------------------

## #' @export
## `dim<-.vctrs_vctr` <- function(x, value) {
##   stop_unsupported(x, "dim<-")
## }

## #' @export
## `dimnames<-.vctrs_vctr` <- function(x, value) {
##   stop_unsupported(x, "dimnames<-")
## }

## #' @export
## levels.vctrs_vctr <- function(x) {
##   NULL
## }

## #' @export
## `levels<-.vctrs_vctr` <- function(x, value) {
##   stop_unsupported(x, "levels<-")
## }

## #' @export
## `t.vctrs_vctr` <- function(x) {
##   stop_unsupported(x, "t")
## }

## #' @export
## `is.na<-.vctrs_vctr` <- function(x, value) {
##   vec_assign(x, value, vec_init(x))
## }

## # Helpers -----------------------------------------------------------------

## # This simple class is used for testing as defining methods inside
## # a test does not work (because the lexical scope is lost)
## # nocov start
## new_hidden <- function(x = double()) {
##   stopifnot(is.numeric(x))
##   new_vctr(vec_cast(x, double()), class = "hidden", inherit_base_type = FALSE)
## }
## format.hidden <- function(x, ...) rep("xxx", length(x))

## local_hidden <- function(frame = caller_env()) {
##   local_bindings(.env = global_env(), .frame = frame,
##     vec_ptype2.hidden.hidden  = function(x, y, ...) new_hidden(),
##     vec_ptype2.hidden.double  = function(x, y, ...) new_hidden(),
##     vec_ptype2.double.hidden  = function(x, y, ...) new_hidden(),
##     vec_ptype2.hidden.logical = function(x, y, ...) new_hidden(),
##     vec_ptype2.logical.hidden = function(x, y, ...) new_hidden(),

##     vec_cast.hidden.hidden   = function(x, to, ...) x,
##     vec_cast.hidden.double   = function(x, to, ...) new_hidden(vec_data(x)),
##     vec_cast.double.hidden   = function(x, to, ...) vec_data(x),
##     vec_cast.hidden.logical  = function(x, to, ...) new_hidden(as.double(x)),
##     vec_cast.logical.hidden  = function(x, to, ...) as.logical(vec_data(x))
##   )
## }

## # nocov end
