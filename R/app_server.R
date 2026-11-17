#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  settings <- mod_settings_server("settings")
  game <- mod_game_server("game", settings = settings$settings, start = settings$start)
  mod_scores_server("scores", refresh = game$scores_version)
}
