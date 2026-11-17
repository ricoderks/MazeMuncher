# Point the score store at a throwaway file for the duration of a test.
local_scores_file <- function(env = parent.frame()) {
  path <- tempfile("scores-", fileext = ".rds")
  old <- options(mazemuncher.scores_path = path)
  withr_defer <- function() {
    options(old)
    unlink(path)
  }
  do.call(on.exit, list(substitute(withr_defer()), add = TRUE), envir = env)
  path
}
