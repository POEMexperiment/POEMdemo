scipy<-NULL
pandas<-NULL
Pillow<-NULL
numpy<-NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  scipy <<- reticulate::import("scipy", delay_load=TRUE)
  pandas <<- reticulate::import("pandas", delay_load=TRUE)
  Pillow <<- reticulate::import("Pillow", delay_load=TRUE)
  numpy <<- reticulate::import("numpy", delay_load=TRUE)
}
