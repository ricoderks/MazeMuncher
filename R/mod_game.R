#' game UI Function
#'
#' The canvas the JavaScript engine draws on, plus a small HUD that the
#' engine updates directly (no server round trip per frame).
#'
#' @param id Internal parameter for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList
mod_game_ui <- function(id) {
  ns <- NS(id)
  hud_item <- function(label, key, value) {
    span(class = "mm-hud-item", label, strong(id = ns(key), value))
  }
  div(
    class = "mm-game",
    div(
      class = "mm-hud",
      hud_item("Score", "score", "0"),
      hud_item("Level", "level", "-"),
      hud_item("Lives", "lives", "-"),
      span(class = "mm-hud-item mm-hud-maze", strong(id = ns("maze"), ""))
    ),
    div(
      class = "mm-stage",
      tags$canvas(
        id = ns("canvas"),
        class = "mm-canvas",
        tabindex = "0",
        `aria-label` = "Maze Muncher game board"
      )
    ),
    div(
      class = "mm-help",
      "Arrow keys or WASD to move \u00b7 P to pause \u00b7 swipe on touch screens"
    )
  )
}

#' game Server Function
#'
#' Drives level progression: sends the first level on start, the next level
#' each time the client reports `level_cleared`, and stores the score when it
#' reports `game_over`.
#'
#' @param settings Reactive returning `list(name, difficulty, mode)`.
#' @param start Reactive that changes whenever a new game should start.
#' @return A list of reactives: `level`, `scores_version` (increments after
#'   each stored score) and `last_result`.
#' @noRd
mod_game_server <- function(id, settings, start) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    state <- reactiveValues(
      active = FALSE,
      level = 0L,
      seed = 1L,
      settings = NULL,
      scores_version = 0L,
      last_result = NULL
    )

    level_payload <- function() {
      s <- state$settings
      level_config(state$level, s$difficulty, s$mode, state$seed)
    }

    begin_game <- function() {
      state$settings <- settings()
      state$level <- 1L
      state$seed <- sample.int(1e6, 1L)
      state$active <- TRUE
      session$sendCustomMessage("mazemuncher:start", list(
        canvas = ns("canvas"),
        input = ns("event"),
        hud = list(
          score = ns("score"),
          level = ns("level"),
          lives = ns("lives"),
          maze = ns("maze")
        ),
        lives = 3L,
        config = level_payload()
      ))
    }

    observeEvent(start(), begin_game(), ignoreInit = TRUE)

    observeEvent(input$again, {
      removeModal()
      begin_game()
    })

    observeEvent(input$event, {
      ev <- input$event
      if (!isTRUE(state$active) || is.null(ev$type)) {
        return()
      }
      if (identical(ev$type, "level_cleared")) {
        state$level <- state$level + 1L
        session$sendCustomMessage("mazemuncher:level", list(
          canvas = ns("canvas"),
          config = level_payload()
        ))
      } else if (identical(ev$type, "game_over")) {
        state$active <- FALSE
        s <- state$settings
        score <- suppressWarnings(as.numeric(ev$score))
        if (length(score) != 1 || !is.finite(score) || score < 0) score <- 0
        saved <- add_score(s$name, score, state$level, s$difficulty, s$mode)
        state$last_result <- list(
          name = s$name,
          score = as.integer(round(score)),
          level = state$level,
          rank = saved$rank
        )
        state$scores_version <- state$scores_version + 1L
        showModal(game_over_modal(state$last_result, ns))
      }
    })

    list(
      level = reactive(state$level),
      scores_version = reactive(state$scores_version),
      last_result = reactive(state$last_result)
    )
  })
}

#' Modal shown at the end of a game
#' @noRd
game_over_modal <- function(result, ns) {
  modalDialog(
    title = "Game over",
    tags$p(sprintf(
      "%s scored %s points and reached level %d.",
      result$name,
      format(result$score, big.mark = ","),
      result$level
    )),
    if (!is.na(result$rank) && result$rank <= 10) {
      tags$p(class = "mm-rank", sprintf("New top-10 entry: #%d!", result$rank))
    },
    footer = tagList(
      modalButton("Close"),
      actionButton(ns("again"), "Play again", class = "btn-primary", icon = icon("rotate-right"))
    ),
    easyClose = TRUE
  )
}
