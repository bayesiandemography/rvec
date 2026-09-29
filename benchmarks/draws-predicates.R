# Run each method in a fresh process:
# Rscript benchmarks/draws-predicates.R direct /tmp/direct.csv
# Rscript benchmarks/draws-predicates.R composed /tmp/composed.csv
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
method <- match.arg(args[1], c("direct", "composed"))
pkgload::load_all(".", quiet = TRUE)
x <- rvec(matrix(rep(c(1, NA, Inf, NaN, -Inf, 2), length.out = 2e6), 2000))
invisible(gc())
before <- gc(reset = TRUE)
elapsed <- system.time(answer <- if (method == "direct") draws_all_finite(x)
                       else draws_all(is.finite(x)))[["elapsed"]]
after <- gc()
stopifnot(identical(answer, rep(FALSE, 2000)))
write.csv(data.frame(method = method, elapsed_seconds = elapsed,
                     peak_vector_heap_growth_mb =
                         (after["Vcells", "max used"] - before["Vcells", "used"]) * 8 / 1e6),
          args[2], row.names = FALSE)
