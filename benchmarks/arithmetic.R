# One case per fresh process; see README.md.
args <- commandArgs(TRUE)
if (length(args) != 5L)
    stop("Usage: Rscript arithmetic.R PACKAGE_ROOT LAYOUT TYPE ORDER OUTPUT.csv")
root <- normalizePath(args[[1L]], mustWork = TRUE)
layout <- match.arg(args[[2L]], c("full", "single", "ordinary", "row"))
type <- match.arg(args[[3L]], c("double", "integer", "logical"))
order <- match.arg(args[[4L]], c("forward", "reverse"))
output <- args[[5L]]
pkgload::load_all(root, quiet = TRUE)
value <- switch(type, double = 2, integer = 2L, logical = TRUE)
x <- rvec(matrix(value, 1000L, 1000L))
y <- switch(layout,
            full = rvec(matrix(value, 1000L, 1000L)),
            single = rvec(rep(value, 1000L)),
            ordinary = rep(value, 1000L),
            row = rvec(matrix(value, 1L, 1000L)))
if (order == "reverse") {
    tmp <- x; x <- y; y <- tmp
    rm(tmp)
}
before <- gc(reset = TRUE)
elapsed <- system.time(answer <- x + y)[["elapsed"]]
after <- gc()
write.csv(data.frame(layout = layout, type = type, order = order,
                     observations = 1000L, draws = 1000L,
                     elapsed_seconds = elapsed,
                     output_mb = as.numeric(object.size(answer)) / 1e6,
                     peak_vector_heap_growth_mb =
                         (after["Vcells", "max used"] - before["Vcells", "used"]) * 8 / 1e6),
          output, row.names = FALSE)
writeLines(c(paste("Source:", root),
             paste("Arithmetic source MD5:", tools::md5sum(file.path(root, "R", "vec_arith.R"))),
             capture.output(sessionInfo())), paste0(output, ".session.txt"))
