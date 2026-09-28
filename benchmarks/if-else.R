# One case per fresh process. See README.md.
args <- commandArgs(TRUE)
if (length(args) != 3L)
    stop("Usage: Rscript if-else.R PACKAGE_ROOT LAYOUT OUTPUT.csv")
root <- normalizePath(args[[1L]], mustWork = TRUE)
layout <- match.arg(args[[2L]], c("ordinary", "single", "full"))
pkgload::load_all(root, quiet = TRUE)
size <- 1000L
n_draw <- 1000L
condition <- rvec(matrix(rep(c(TRUE, FALSE, NA),
                             length.out = size * n_draw),
                         nrow = size, ncol = n_draw))
true <- rep(1, size)
false <- switch(layout,
                ordinary = rep(2, size),
                single = rvec(rep(2, size)),
                full = rvec(matrix(2, size, n_draw)))
missing <- switch(layout,
                  ordinary = rep(3, size),
                  single = rvec(rep(3, size)),
                  full = rvec(matrix(3, size, n_draw)))
invisible(gc())
before <- gc(reset = TRUE)
elapsed <- system.time(answer <- if_else_rvec(condition, true, false,
                                               missing))[["elapsed"]]
after <- gc()
write.csv(data.frame(layout = layout, observations = size, draws = n_draw,
                     elapsed_seconds = elapsed,
                     output_mb = as.numeric(object.size(answer)) / 1e6,
                     peak_vector_heap_growth_mb =
                         (after["Vcells", "max used"] -
                          before["Vcells", "used"]) * 8 / 1e6),
          args[[3L]], row.names = FALSE)
writeLines(c(paste("Source:", root),
             paste("if_else_rvec source MD5:",
                   tools::md5sum(file.path(root, "R", "if_else_rvec.R"))),
             capture.output(sessionInfo())),
           paste0(args[[3L]], ".session.txt"))
