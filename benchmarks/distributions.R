# Run from any directory. See README.md for arguments and interpretation.
args <- commandArgs(trailingOnly = TRUE)
worker <- length(args) > 0L && args[[1L]] == "--worker"
if (worker) {
    root <- args[[2L]]
    case <- args[[3L]]
    layout <- args[[4L]]
    n <- as.integer(args[[5L]])
    draws <- as.integer(args[[6L]])
    output <- args[[7L]]
    pkgload::load_all(root, quiet = TRUE)
    parameter <- function(value, rows = n) {
        x <- rep(value, length.out = rows)
        switch(layout,
               shared = x,
               single = rvec(matrix(x, ncol = 1L)),
               full = rvec(matrix(x, nrow = rows, ncol = draws)))
    }
    # Keep at least one full-draw input, so layouts give identical output sizes.
    full <- rvec(matrix(2, nrow = n, ncol = draws))
    call <- switch(case,
        rgamma = list(n = n, shape = full, rate = parameter(1)),
        dgamma = list(x = full, shape = parameter(2), rate = parameter(1)),
        rpois = list(n = n, lambda = parameter(2)),
        rnbinom_mu = list(n = n, size = full, mu = parameter(2)),
        rmultinom = list(n = 1L, size = rvec(matrix(100, 1L, draws)),
                         prob = parameter(1 / n)),
        dmultinom_default = list(x = full, prob = parameter(1 / n)),
        dmultinom_explicit = list(x = full, size = sum(full), prob = parameter(1 / n)))
    # rpois has only one parameter: shared/single inputs need explicit draws.
    # Explicit n_draw requires rvec inputs already to have that many draws.
    if (case == "rpois") {
        if (layout == "single") quit(status = 0L)
        call$n_draw <- draws
    }
    name <- switch(case, rnbinom_mu = "rnbinom_rvec",
                   dmultinom_default = "dmultinom_rvec",
                   dmultinom_explicit = "dmultinom_rvec", paste0(case, "_rvec"))
    fun <- getExportedValue("rvec", name)
    rm(full, parameter)
    set.seed(42)
    before <- gc(reset = TRUE)
    elapsed <- system.time(answer <- do.call(fun, call))[["elapsed"]]
    after <- gc()
    result <- data.frame(case = case, layout = layout, observations = n,
                         draws = draws, elapsed_seconds = elapsed,
                         output_mb = as.numeric(object.size(answer)) / 1e6,
                         peak_vector_heap_growth_mb =
                             (after["Vcells", "max used"] - before["Vcells", "used"]) * 8 / 1e6)
    write.csv(result, output, row.names = FALSE)
} else {
    if (length(args) < 2L || length(args) > 4L)
        stop("Usage: Rscript benchmarks/distributions.R PACKAGE_ROOT OUTPUT_DIR [OBSERVATIONS=1000] [DRAWS=1000]")
    root <- normalizePath(args[[1L]], mustWork = TRUE)
    output <- args[[2L]]
    n <- if (length(args) >= 3L) as.integer(args[[3L]]) else 1000L
    draws <- if (length(args) >= 4L) as.integer(args[[4L]]) else 1000L
    stopifnot(!is.na(n), !is.na(draws), n > 0L, draws > 0L)
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    output <- normalizePath(output, mustWork = TRUE)
    script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
    script <- normalizePath(script, mustWork = TRUE)
    cases <- c("rgamma", "dgamma", "rpois", "rnbinom_mu", "rmultinom",
               "dmultinom_default", "dmultinom_explicit")
    results <- list()
    for (case in cases) for (layout in c("shared", "single", "full")) {
        if (case == "rpois" && layout == "single") next
        message(case, " / ", layout)
        file <- tempfile(fileext = ".csv")
        status <- system2(file.path(R.home("bin"), "Rscript"),
                          shQuote(c("--vanilla", script, "--worker", root, case,
                                    layout, n, draws, file)))
        if (status != 0L) stop("Benchmark failed: ", case, " / ", layout)
        results[[length(results) + 1L]] <- read.csv(file)
        unlink(file)
    }
    write.csv(do.call(rbind, results), file.path(output, "results.csv"), row.names = FALSE)
    metadata <- capture.output({
        cat("Package root:", root, "\n")
        cat("Run time:", format(Sys.time(), tz = "UTC"), "UTC\n")
        pkgload::load_all(root, quiet = TRUE)
        print(sessionInfo())
    })
    writeLines(metadata, file.path(output, "session.txt"))
}
