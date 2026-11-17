test_that("game module advances levels and stores the score", {
  local_scores_file()
  settings <- reactiveVal(list(name = "Tester", difficulty = "normal", mode = "campaign"))
  start <- reactiveVal(0)

  testServer(mod_game_server, args = list(settings = settings, start = start), {
    out <- session$returned
    # Events before a game starts are ignored.
    session$setInputs(event = list(type = "level_cleared", level = 1, score = 10))
    expect_equal(out$level(), 0L)

    start(1)
    session$flushReact()
    expect_equal(out$level(), 1L)

    session$setInputs(event = list(type = "level_cleared", level = 1, score = 1500, nonce = 1))
    expect_equal(out$level(), 2L)

    session$setInputs(event = list(type = "game_over", level = 2, score = 2345, nonce = 2))
    expect_equal(out$scores_version(), 1L)
    expect_equal(out$last_result()$score, 2345L)
    expect_equal(out$last_result()$level, 2L)
    expect_equal(out$last_result()$rank, 1L)

    s <- top_scores()
    expect_equal(s$name, "Tester")
    expect_equal(s$score, 2345L)

    # A duplicate game_over after the game ended is ignored.
    session$setInputs(event = list(type = "game_over", level = 2, score = 9999, nonce = 3))
    expect_equal(nrow(top_scores()), 1)
  })
})

test_that("game module treats a garbage score as zero", {
  local_scores_file()
  settings <- reactiveVal(list(name = "X", difficulty = "easy", mode = "endless"))
  start <- reactiveVal(0)
  testServer(mod_game_server, args = list(settings = settings, start = start), {
    session$flushReact() # let the start observer initialise (ignoreInit)
    start(1)
    session$flushReact()
    session$setInputs(event = list(type = "game_over", level = 1, score = "lots", nonce = 1))
    expect_equal(session$returned$last_result()$score, 0L)
  })
})

test_that("game UI contains the canvas and HUD", {
  html <- as.character(mod_game_ui("g"))
  expect_match(html, "<canvas id=\"g-canvas\"")
  expect_match(html, "id=\"g-score\"")
})

test_that("scores table renders empty and filled states", {
  expect_match(as.character(scores_table(empty_scores())), "No scores yet")
  local_scores_file()
  add_score("A", 10, 1, "easy")
  expect_match(as.character(scores_table(top_scores())), "<table")
})

test_that("app UI and server build", {
  local_scores_file()
  ui <- app_ui(NULL)
  expect_s3_class(ui, "shiny.tag.list")
  testServer(app_server, {
    expect_true(TRUE)
  })
})
