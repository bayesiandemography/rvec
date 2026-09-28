# One case per fresh process. See README.md.
args <- commandArgs(TRUE)
if (length(args) != 3L)
    stop("Usage: Rscript covariance.R PACKAGE_ROOT CASE OUTPUT.csv")
root <- normalizePath(args[[1L]], mustWork = TRUE)
case <- match.arg(args[[2L]], c("rvec_rvec", "rvec_vector"))
pkgload::load_all(root, quiet = TRUE)
size <- 1000L
n_draw <- 1000L
x <- rvec(matrix(seq_len(size * n_draw), size, n_draw))
y <- if (case == "rvec_rvec") {
    rvec(matrix(seq_len(size * n_draw) + 1, size, n_draw))
} else {
    seq_len(size) + 1
}
invisible(gc())
before <- gc(reset = TRUE)
elapsed <- system.time(answer <- var(x, y))[["elapsed"]]
after <- gc()
write.csv(data.frame(case = case, observations = size, draws = n_draw,
                     elapsed_seconds = elapsed,
                     output_mb = as.numeric(object.size(answer)) / 1e6,
                     peak_vector_heap_growth_mb =
                         (after["Vcells", "max used"] -
                          before["Vcells", "used"]) * 8 / 1e6),
          args[[3L]], row.names = FALSE)
writeLines(c(paste("Source:", root),
             paste("Covariance source MD5:",
                   tools::md5sum(file.path(root, "R", "var.R"))),
             capture.output(sessionInfo())),
           paste0(args[[3L]], ".session.txt"))
