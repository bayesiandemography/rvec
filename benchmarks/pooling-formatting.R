# One case per fresh process. See README.md.
args <- commandArgs(TRUE)
if (length(args) != 4L)
    stop("Usage: Rscript pooling-formatting.R PACKAGE_ROOT CASE TYPE OUTPUT.csv")
root <- normalizePath(args[[1L]], mustWork = TRUE)
case <- match.arg(args[[2L]], c("pool", "pool_grouped", "logical_format"))
type <- match.arg(args[[3L]], c("dbl", "int", "lgl", "chr"))
if (case == "logical_format" && type != "lgl")
    stop("logical_format requires type lgl")
pkgload::load_all(root, quiet = TRUE)
nr <- 1000L
nc <- 1000L
m <- matrix(rep(c(0L, 1L, NA_integer_), length.out = nr * nc), nr, nc)
x <- getExportedValue("rvec", paste0("rvec_", type))(m)
data <- data.frame(group = rep(seq_len(10L), length.out = nr), value = x)
fun <- switch(case,
              pool = function() pool_draws(data),
              pool_grouped = function() pool_draws(data, by = group),
              logical_format = function() format(x))
invisible(gc())
before <- gc(reset = TRUE)
elapsed <- system.time(answer <- fun())[["elapsed"]]
after <- gc()
allocation_file <- paste0(args[[4L]], ".allocations.txt")
Rprofmem(allocation_file, threshold = 1000000)
invisible(fun())
Rprofmem(NULL)
lines <- readLines(allocation_file)
bytes <- as.numeric(sub(" .*", "", lines[grepl("^[0-9]+ :", lines)]))
write.csv(data.frame(case = case, type = type, observations = nr, draws = nc,
                     elapsed_seconds = elapsed,
                     peak_vector_heap_growth_mb =
                         (after["Vcells", "max used"] - before["Vcells", "used"]) * 8 / 1e6,
                     allocations_over_1mb_mb = sum(bytes) / 1e6),
          args[[4L]], row.names = FALSE)
writeLines(c(paste("Source:", root),
             capture.output(tools::md5sum(file.path(root, "R", c("pool_draws.R", "format.R")))),
             capture.output(sessionInfo())), paste0(args[[4L]], ".session.txt"))
