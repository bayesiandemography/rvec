# One case per fresh process. See README.md.
args <- commandArgs(TRUE)
if (length(args) != 3L)
    stop("Usage: Rscript common-operations.R PACKAGE_ROOT CASE OUTPUT.csv")
root <- normalizePath(args[[1L]], mustWork = TRUE)
case <- match.arg(args[[2L]], c("constructor", "cast", "compare_single",
                               "compare_ordinary", "draws_mean", "sd"))
pkgload::load_all(root, quiet = TRUE)
m <- matrix(2, 1000L, 1000L)
x <- rvec(m)
y <- if (case == "compare_single") rvec(rep(1, 1000L)) else rep(1, 1000L)
fun <- switch(case,
              constructor = function() rvec_dbl(m),
              cast = function() vctrs::vec_cast(x, rvec_dbl(matrix(numeric(), 0L, 1000L))),
              compare_single = function() x > y,
              compare_ordinary = function() x > y,
              draws_mean = function() draws_mean(x),
              sd = function() sd(x))
invisible(gc())
before <- gc(reset = TRUE)
elapsed <- system.time(answer <- fun())[["elapsed"]]
after <- gc()
write.csv(data.frame(case = case, observations = 1000L, draws = 1000L,
                     elapsed_seconds = elapsed,
                     output_mb = as.numeric(object.size(answer)) / 1e6,
                     peak_vector_heap_growth_mb =
                         (after["Vcells", "max used"] - before["Vcells", "used"]) * 8 / 1e6),
          args[[3L]], row.names = FALSE)
writeLines(c(paste("Source:", root), capture.output(sessionInfo())),
           paste0(args[[3L]], ".session.txt"))
