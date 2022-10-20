rhizotron_explorer <- function() {
  appDir <- system.file("rhizotron_explorer", package = "POEM")
  if (appDir == "") {
    stop("Could not find rhizotron_explorer directory. Try re-installing `POEM`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
