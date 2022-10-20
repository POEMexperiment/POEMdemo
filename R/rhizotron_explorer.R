rhizotron_explorer <- function() {
  appDir <- system.file("rhizotron_explorer", package = "POEMdemo")
  if (appDir == "") {
    stop("Could not find rhizotron_explorer directory. Try re-installing `POEMdemo`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
