# One case per fresh process. See README.md.
args <- commandArgs(TRUE)
if (length(args) != 4L)
    stop("Usage: Rscript weighted-summaries.R PACKAGE_ROOT SUMMARY CASE OUTPUT.csv")
root <- normalizePath(args[[1L]], mustWork = TRUE)
summary <- match.arg(args[[2L]], c("mean", "median", "mad", "var", "sd"))
case <- match.arg(args[[3L]], c("ordinary", "full"))
pkgload::load_all(root, quiet = TRUE)
size <- 1000L
n_draw <- 1000L
x <- seq_len(size) / size
if (case == "full")
    x <- rvec(matrix(x, size, n_draw))
wt <- rvec(matrix((seq_len(size * n_draw) %% 17L) + 1, size, n_draw))
fun <- getExportedValue("rvec", paste0("weighted_", summary))
invisible(gc())
before <- gc(reset = TRUE)
elapsed <- system.time(answer <- fun(x, wt))[["elapsed"]]
after <- gc()
write.csv(data.frame(summary = summary, case = case,
                     observations = size, draws = n_draw,
                     elapsed_seconds = elapsed,
                     output_mb = as.numeric(object.size(answer)) / 1e6,
                     peak_vector_heap_growth_mb =
                         (after["Vcells", "max used"] -
                          before["Vcells", "used"]) * 8 / 1e6),
          args[[4L]], row.names = FALSE)
writeLines(c(paste("Source:", root),
             paste("Weighted-summary source MD5:",
                   tools::md5sum(file.path(root, "R", "weighted_mean.R"))),
             capture.output(sessionInfo())),
           paste0(args[[4L]], ".session.txt"))
