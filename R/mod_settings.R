#' settings UI Function
#'
#' Player name, difficulty, game mode and the start button.
#'
#' @param id Internal parameter for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList
mod_settings_ui <- function(id) {
  ns <- NS(id)
  presets <- difficulty_presets()
  difficulty_choices <- stats::setNames(
    names(presets),
    vapply(presets, `[[`, character(1), "label")
  )
  tagList(
    textInput(ns("name"), "Player name", value = "Player 1", placeholder = "Your name"),
    radioButtons(
      ns("difficulty"), "Difficulty",
      choices = difficulty_choices, selected = "normal", inline = TRUE
    ),
    radioButtons(ns("mode"), "Mode", choices = game_modes(), selected = "campaign"),
    actionButton(
      ns("start"), "Start game",
      icon = icon("play"), class = "btn-primary w-100 mm-start"
    )
  )
}

#' settings Server Function
#'
#' @return A list with a `settings` reactive (`name`, `difficulty`, `mode`)
#'   and a `start` reactive that fires when the start button is pressed.
#' @noRd
mod_settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    settings <- reactive({
      difficulty <- input$difficulty
      mode <- input$mode
      list(
        name = clean_player_name(input$name),
        difficulty = if (isTRUE(difficulty %in% names(difficulty_presets()))) difficulty else "normal",
        mode = if (isTRUE(mode %in% game_modes())) mode else "campaign"
      )
    })
    list(settings = settings, start = reactive(input$start))
  })
}
