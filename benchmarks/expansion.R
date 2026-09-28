# One case per fresh process. See README.md.
args <- commandArgs(TRUE)
if (length(args) != 3L)
    stop("Usage: Rscript expansion.R PACKAGE_ROOT TYPE OUTPUT.csv")
root <- normalizePath(args[[1L]], mustWork = TRUE)
type <- match.arg(args[[2L]], c("dbl", "int", "lgl", "chr", "mixed"))
pkgload::load_all(root, quiet = TRUE)
size <- 1000L
n_draw <- 1000L
m <- matrix(rep(c(0L, 1L, NA_integer_), length.out = size * n_draw), size, n_draw)
data <- data.frame(id = seq_len(size))
for (kind in if (type == "mixed") c("dbl", "int", "lgl", "chr") else type)
    data[[kind]] <- getExportedValue("rvec", paste0("rvec_", kind))(m)
invisible(gc())
before <- gc(reset = TRUE)
elapsed <- system.time(answer <- expand_from_rvec(data))[["elapsed"]]
after <- gc()
allocation_file <- paste0(args[[3L]], ".allocations.txt")
Rprofmem(allocation_file, threshold = 1000000)
invisible(expand_from_rvec(data))
Rprofmem(NULL)
allocations <- readLines(allocation_file)
bytes <- as.numeric(sub(" .*", "", allocations[grepl("^[0-9]+ :", allocations)]))
write.csv(data.frame(type = type, observations = size, draws = n_draw,
                     elapsed_seconds = elapsed,
                     output_mb = as.numeric(object.size(answer)) / 1e6,
                     peak_vector_heap_growth_mb =
                         (after["Vcells", "max used"] - before["Vcells", "used"]) * 8 / 1e6,
                     allocations_over_1mb_mb = sum(bytes) / 1e6),
          args[[3L]], row.names = FALSE)
writeLines(c(paste("Source:", root),
             paste("Reshaping source MD5:", tools::md5sum(file.path(root, "R", "collapse_to_rvec.R"))),
             capture.output(sessionInfo())), paste0(args[[3L]], ".session.txt"))
