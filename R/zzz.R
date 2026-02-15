

.onLoad <- function(libname, pkgname) {                   # nocov
  if (getRversion() >= "4.3.0") {                         # nocov
    registerS3method("matrixOps", "rvec", matrixOps.rvec, # nocov
                     envir = asNamespace(pkgname))        # nocov
  }
}

.onAttach <- function(libname, pkgname) {                                      # nocov
  if (getRversion() < "4.3.0") {                                               # nocov
    packageStartupMessage(paste("rvec: matrix multiplication (%*%) for rvecs", # nocov
                                "only implemented for R >= 4.3.0."))           # nocov
  }
}
