#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bslib::page_sidebar(
      title = tags$span(class = "mm-title", "Maze Muncher"),
      theme = app_theme(),
      fillable = FALSE,
      sidebar = bslib::sidebar(
        width = 290,
        mod_settings_ui("settings"),
        tags$hr(),
        mod_scores_ui("scores")
      ),
      bslib::card(
        class = "mm-card",
        bslib::card_body(mod_game_ui("game"))
      )
    )
  )
}

#' Dark neon theme
#' @noRd
app_theme <- function() {
  system_fonts <- bslib::font_collection(
    "system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"
  )
  bslib::bs_theme(
    version = 5,
    bg = "#140b24",
    fg = "#ece8ff",
    primary = "#2de2c4",
    secondary = "#ff5fa2",
    base_font = system_fonts,
    heading_font = system_fonts
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Maze Muncher"
    )
  )
}
