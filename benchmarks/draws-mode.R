# One case per fresh process. See README.md.
args <- commandArgs(TRUE)
if (length(args) != 4L)
    stop("Usage: Rscript draws-mode.R PACKAGE_ROOT TYPE SCENARIO OUTPUT.csv")
root <- normalizePath(args[[1L]], mustWork = TRUE)
type <- match.arg(args[[2L]], c("dbl", "int", "lgl", "chr"))
scenario <- match.arg(args[[3L]], c("repeated", "unique"))
if (type == "lgl" && scenario == "unique")
    stop("Logical inputs cannot have unique values across 1,000 draws")
pkgload::load_all(root, quiet = TRUE)
nr <- 1000L
nc <- 1000L
values <- seq_len(nr * nc)
if (scenario == "repeated") values <- values %% 97L
if (type == "lgl") values <- values %% 2L
x <- getExportedValue("rvec", paste0("rvec_", type))(matrix(values, nr, nc))
invisible(gc())
before <- gc(reset = TRUE)
elapsed <- system.time(answer <- draws_mode(x))[["elapsed"]]
after <- gc()
write.csv(data.frame(type = type, scenario = scenario,
                     observations = nr, draws = nc, elapsed_seconds = elapsed,
                     output_mb = as.numeric(object.size(answer)) / 1e6,
                     peak_vector_heap_growth_mb =
                         (after["Vcells", "max used"] - before["Vcells", "used"]) * 8 / 1e6),
          args[[4L]], row.names = FALSE)
writeLines(c(paste("Source:", root),
             paste("Draws source MD5:", tools::md5sum(file.path(root, "R", "draws.R"))),
             capture.output(sessionInfo())), paste0(args[[4L]], ".session.txt"))
