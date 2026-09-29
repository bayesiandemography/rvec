# Investigation only: prototypes below are not package implementations.
args <- commandArgs(TRUE)
if (length(args) != 5L)
    stop("Usage: Rscript remaining-memory.R PACKAGE_ROOT CASE VARIANT SCENARIO OUTPUT.csv")
root <- normalizePath(args[[1L]], mustWork = TRUE)
case <- match.arg(args[[2L]], c("pool", "mode", "format", "logical_format", "collapse"))
variant <- match.arg(args[[3L]], c("old", "new"))
scenario <- match.arg(args[[4L]], c("repeated", "unique"))
pkgload::load_all(root, quiet = TRUE)
pool_proto <- function(vec) {
  if (!rvec::is_rvec(vec)) cli::cli_abort('Internal error: {.arg vec} is not an rvec.')
  if (!length(vec)) return(vec)
  m <- vctrs::field(vec, 'data')
  dimnames(m) <- NULL
  dim(m) <- c(1L, length(m))
  rvec::rvec(m)
}
mode_proto <- function(x, na_rm = FALSE) {
  rvec:::check_flag(na_rm)
  m <- vctrs::field(x,'data')
  if (!nrow(m)) ans <- NA else {
    ans <- rep(NA, nrow(m))
    for (i in seq_len(nrow(m))) {
      tab <- table(m[i, ], useNA = if (na_rm) 'no' else 'ifany')
      idx <- which(tab == max(tab))
      if (length(idx) == 1L) ans[i] <- names(tab)[idx]
    }
    names(ans) <- rownames(m)
  }
  storage.mode(ans) <- storage.mode(m)
  ans
}
format_proto <- function(x) {
  out <- vector('list', nrow(x))
  for (i in seq_len(nrow(x))) {
    tab <- table(x[i,], useNA='no')
    out[i] <- list(names(tab)[[which.max(tab)]])
  }
  sprintf('.."%s"..',unlist(out))
}
collapse_proto <- rvec:::collapse_to_rvec_inner
body_text <- deparse(body(collapse_proto))
body_text <- sub('m_tmp <- matrix(nrow = nrow(ans), ncol = length(draw_ans))',
 'm_tmp <- matrix(switch(typeof(data[[values_colnums[[1L]]]]), integer=NA_integer_, double=NA_real_, character=NA_character_, NA), nrow = nrow(ans), ncol = length(draw_ans))',
 body_text, fixed=TRUE)
body(collapse_proto) <- parse(text=paste(body_text,collapse='\n'))[[1L]]

nr <- 1000L
nc <- 1000L
m <- matrix(seq_len(nr * nc) %% 97L, nr, nc)
if (scenario == "unique") m <- matrix(seq_len(nr * nc), nr, nc)
x <- rvec(m)
if (case == "format") m <- matrix(as.character(m), nr, nc)
if (case == "logical_format")
    m <- matrix(rep(c(TRUE, FALSE, NA), length.out = nr * nc), nr, nc)
if (case == "collapse")
    df <- data.frame(id = rep(seq_len(nr), nc), draw = rep(seq_len(nc), each = nr),
                     value = as.double(as.vector(m)))
current <- switch(case,
    pool = function() rvec:::pool_draws_vec(x),
    mode = function() draws_mode(x),
    format = function() rvec:::format_rvec_summaries(m),
    logical_format = function() rvec:::format_rvec_summaries(m),
    collapse = function() rvec:::collapse_to_rvec_inner(
        df, c(draw = 2L), c(value = 3L), c(id = 1L), integer(), NULL))
prototype <- switch(case,
    pool = function() pool_proto(x),
    mode = function() mode_proto(x),
    format = function() format_proto(m),
    logical_format = function()
        paste0("p=", formatC(matrixStats::rowMeans2(m, na.rm = TRUE), format = "fg")),
    collapse = function() collapse_proto(
        df, c(draw = 2L), c(value = 3L), c(id = 1L), integer(), NULL))
fun <- if (variant == "old") current else prototype
invisible(gc())
before <- gc(reset = TRUE)
elapsed <- system.time(answer <- fun())[["elapsed"]]
after <- gc()
allocation_file <- paste0(args[[5L]], ".allocations.txt")
Rprofmem(allocation_file, threshold = 1000000)
invisible(fun())
Rprofmem(NULL)
lines <- readLines(allocation_file)
bytes <- as.numeric(sub(" .*", "", lines[grepl("^[0-9]+ :", lines)]))
write.csv(data.frame(case = case, variant = variant, scenario = scenario,
                     observations = nr, draws = nc, elapsed_seconds = elapsed,
                     peak_vector_heap_growth_mb =
                         (after["Vcells", "max used"] - before["Vcells", "used"]) * 8 / 1e6,
                     allocations_over_1mb_mb = sum(bytes) / 1e6,
                     identical_to_current = identical(answer, current())),
          args[[5L]], row.names = FALSE)
writeLines(c(paste("Source:", root),
             capture.output(tools::md5sum(file.path(root, "R", c("pool_draws.R", "draws.R", "format.R", "collapse_to_rvec.R")))),
             capture.output(sessionInfo())), paste0(args[[5L]], ".session.txt"))
