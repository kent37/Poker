#' Run a simple Shiny app demonstrating the poker hand parsing and ranking.
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "PokerApp", package = "Poker")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Poker`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
