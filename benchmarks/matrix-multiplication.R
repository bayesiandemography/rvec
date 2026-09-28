# Investigation only: experimental paths below are not package implementations.
# One case per fresh process. See README.md.
args <- commandArgs(TRUE)
if (length(args) != 5L)
    stop("Usage: Rscript matrix-multiplication.R PACKAGE_ROOT CASE OBSERVATIONS DRAWS OUTPUT.csv")
root <- normalizePath(args[[1L]], mustWork = TRUE)
case <- match.arg(args[[2L]], c("dot", "dot_stream", "sparse_left",
                               "sparse_left_direct", "sparse_right", "sparse_right_direct"))
size <- as.integer(args[[3L]])
n_draw <- as.integer(args[[4L]])
stopifnot(!is.na(size), size > 0L, !is.na(n_draw), n_draw > 0L)
pkgload::load_all(root, quiet = TRUE)
x <- rvec_dbl(matrix(seq_len(size * n_draw) / 1000, size, n_draw))
if (case %in% c("dot", "dot_stream")) {
    y <- rvec_dbl(matrix(seq_len(size * n_draw) / 2000, size, n_draw))
} else {
    y <- Matrix::bandSparse(size, k = c(-1L, 0L, 1L),
                           diagonals = list(rep(1, size - 1L), rep(2, size), rep(1, size - 1L)))
}
stream <- function(x, y) {
    mx <- vctrs::field(x, "data")
    my <- vctrs::field(y, "data")
    out <- numeric(ncol(mx))
    for (j in seq_len(ncol(mx))) {
        product <- mx[, j] * my[, j]
        dim(product) <- c(nrow(mx), 1L)
        out[[j]] <- matrixStats::colSums2(product)
    }
    rvec_dbl(matrix(out, nrow = 1L))
}
fun <- switch(case,
               dot = function() x %*% y,
               dot_stream = function() stream(x, y),
               sparse_left = function() y %*% x,
               sparse_left_direct = function() rvec(as.matrix(y %*% vctrs::field(x, "data"))),
               sparse_right = function() x %*% y,
               sparse_right_direct = function()
                   rvec(as.matrix(Matrix::t(Matrix::crossprod(vctrs::field(x, "data"), y)))))
invisible(gc())
before <- gc(reset = TRUE)
elapsed <- system.time(answer <- fun())[["elapsed"]]
after <- gc()
write.csv(data.frame(case = case, observations = size, draws = n_draw,
                     elapsed_seconds = elapsed,
                     output_mb = as.numeric(object.size(answer)) / 1e6,
                     peak_vector_heap_growth_mb =
                         (after["Vcells", "max used"] - before["Vcells", "used"]) * 8 / 1e6),
          args[[5L]], row.names = FALSE)
writeLines(c(paste("Source:", root),
             paste("Matrix source MD5:", tools::md5sum(file.path(root, "R", "matrixOps.R"))),
             capture.output(sessionInfo())), paste0(args[[5L]], ".session.txt"))
