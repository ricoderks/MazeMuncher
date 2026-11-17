test_that("settings module returns cleaned settings", {
  testServer(mod_settings_server, {
    session$setInputs(name = "  Zed!! ", difficulty = "hard", mode = "endless", start = 1)
    out <- session$returned
    expect_equal(out$settings(), list(name = "Zed", difficulty = "hard", mode = "endless"))
    expect_equal(out$start(), 1)

    session$setInputs(difficulty = "bogus", mode = "bogus")
    expect_equal(out$settings()$difficulty, "normal")
    expect_equal(out$settings()$mode, "campaign")
  })
})

test_that("settings UI renders", {
  ui <- mod_settings_ui("x")
  expect_s3_class(ui, "shiny.tag.list")
  expect_match(as.character(ui), "x-start")
})
