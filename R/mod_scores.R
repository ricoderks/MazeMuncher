#' scores UI Function
#'
#' @param id Internal parameter for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList
mod_scores_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "mm-scores",
    tags$h6("High scores"),
    uiOutput(ns("table"))
  )
}

#' scores Server Function
#'
#' @param refresh Reactive that changes whenever a new score was stored.
#' @noRd
mod_scores_server <- function(id, refresh) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderUI({
      refresh()
      scores_table(top_scores(10L))
    })
  })
}

#' Render a score table as HTML
#' @noRd
scores_table <- function(scores) {
  if (nrow(scores) == 0) {
    return(tags$p(class = "mm-empty", "No scores yet. Be the first!"))
  }
  header <- tags$tr(tags$th("#"), tags$th("Name"), tags$th("Score"), tags$th("Lvl"))
  rows <- lapply(seq_len(nrow(scores)), function(i) {
    tags$tr(
      tags$td(i),
      tags$td(scores$name[i], tags$small(class = "mm-diff", substr(scores$difficulty[i], 1, 1))),
      tags$td(class = "text-end", format(scores$score[i], big.mark = ",")),
      tags$td(class = "text-end", scores$level[i])
    )
  })
  tags$table(class = "table table-sm mm-score-table", tags$thead(header), tags$tbody(rows))
}
