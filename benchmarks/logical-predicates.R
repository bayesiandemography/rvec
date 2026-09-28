# One case per fresh process. See README.md.
args <- commandArgs(TRUE)
if (length(args) != 3L)
    stop("Usage: Rscript logical-predicates.R PACKAGE_ROOT CASE OUTPUT.csv")
root <- normalizePath(args[[1L]], mustWork = TRUE)
case <- match.arg(args[[2L]], c("is.nan", "is.finite", "is.infinite", "is.na"))
pkgload::load_all(root, quiet = TRUE)
size <- 1000L
n_draw <- 1000L
x <- rvec_lgl(matrix(rep(c(TRUE, FALSE, NA), length.out = size * n_draw),
                      size, n_draw))
fun <- get(case, envir = baseenv())
invisible(gc())
before <- gc(reset = TRUE)
elapsed <- system.time(answer <- fun(x))[["elapsed"]]
after <- gc()
# Profile a separate, warmed call so tracing does not affect peak measurement.
allocation_file <- paste0(args[[3L]], ".allocations.txt")
Rprofmem(allocation_file, threshold = 1000000)
invisible(fun(x))
Rprofmem(NULL)
allocations <- readLines(allocation_file)
bytes <- as.numeric(sub(" .*", "", allocations[grepl("^[0-9]+ :", allocations)]))
write.csv(data.frame(case = case, observations = size, draws = n_draw,
                     elapsed_seconds = elapsed,
                     output_mb = as.numeric(object.size(answer)) / 1e6,
                     peak_vector_heap_growth_mb =
                         (after["Vcells", "max used"] -
                          before["Vcells", "used"]) * 8 / 1e6,
                     allocations_over_1mb_mb = sum(bytes) / 1e6),
          args[[3L]], row.names = FALSE)
writeLines(c(paste("Source:", root),
             paste("Math source MD5:",
                   tools::md5sum(file.path(root, "R", "vec_math.R"))),
             paste("Missingness source MD5:",
                   tools::md5sum(file.path(root, "R", "missing.R"))),
             capture.output(sessionInfo())),
           paste0(args[[3L]], ".session.txt"))
