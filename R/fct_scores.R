#' Location of the high-score file
#'
#' Resolution order: the `mazemuncher.scores_path` option, the `scores_path`
#' golem option (`run_app(scores_path = ...)`), then a file in the user data
#' directory returned by [tools::R_user_dir()].
#'
#' @noRd
scores_path <- function() {
  path <- getOption("mazemuncher.scores_path")
  if (is.null(path)) {
    path <- tryCatch(
      golem::get_golem_options("scores_path"),
      error = function(e) NULL
    )
  }
  if (is.null(path)) {
    path <- file.path(tools::R_user_dir("mazemuncher", "data"), "scores.rds")
  }
  path
}

#' Empty score table
#' @noRd
empty_scores <- function() {
  data.frame(
    name = character(),
    score = integer(),
    level = integer(),
    difficulty = character(),
    mode = character(),
    date = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )
}

#' Clean a player name for display and storage
#' @noRd
clean_player_name <- function(name) {
  if (is.null(name) || length(name) == 0 || is.na(name[1])) {
    return("Anonymous")
  }
  name <- gsub("[^[:alnum:] _.-]", "", as.character(name[1]))
  name <- trimws(substr(trimws(name), 1, 16))
  if (!nzchar(name)) "Anonymous" else name
}

#' Read all stored scores, best first
#' @noRd
read_scores <- function(path = scores_path()) {
  if (!file.exists(path)) {
    return(empty_scores())
  }
  out <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.data.frame(out) || !all(names(empty_scores()) %in% names(out))) {
    return(empty_scores())
  }
  out[, names(empty_scores()), drop = FALSE]
}

#' Record a finished game
#'
#' Ties keep the older entry first. Only the best `keep` scores are stored.
#'
#' @return Invisibly, a list with the new entry's `rank` (`NA` if it did not
#'   make the table) and the updated `scores`.
#' @noRd
add_score <- function(name, score, level, difficulty, mode = "campaign",
                      path = scores_path(), keep = 100L) {
  score <- suppressWarnings(as.numeric(score))
  if (length(score) != 1 || !is.finite(score) || score < 0) {
    stop("`score` must be a single non-negative number.", call. = FALSE)
  }
  score <- as.integer(min(round(score), 1e8))
  level <- max(1L, as.integer(level))
  old <- read_scores(path)
  rank <- sum(old$score >= score) + 1L

  entry <- data.frame(
    name = clean_player_name(name),
    score = score,
    level = level,
    difficulty = as.character(difficulty),
    mode = as.character(mode),
    date = Sys.time(),
    stringsAsFactors = FALSE
  )
  scores <- rbind(old, entry)
  scores <- scores[order(-scores$score, scores$date), , drop = FALSE]
  scores <- utils::head(scores, keep)
  rownames(scores) <- NULL

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(scores, path)
  invisible(list(rank = if (rank <= keep) rank else NA_integer_, scores = scores))
}

#' The best `n` scores
#' @noRd
top_scores <- function(n = 10L, path = scores_path()) {
  utils::head(read_scores(path), n)
}
