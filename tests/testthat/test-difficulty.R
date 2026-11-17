test_that("presets are complete", {
  fields <- c(
    "label", "player_speed", "sentry_speed", "speed_step", "sentry_speed_cap",
    "frightened", "frightened_step", "frightened_min", "sentries", "release", "schedule"
  )
  presets <- difficulty_presets()
  expect_named(presets, c("easy", "normal", "hard"))
  for (p in presets) {
    expect_true(all(fields %in% names(p)))
    expect_length(p$release, 4)
    expect_true(p$sentries >= 1 && p$sentries <= 4)
  }
  expect_lt(presets$easy$sentry_speed, presets$hard$sentry_speed)
})

test_that("level_config scales with level and respects caps", {
  l1 <- level_config(1, "normal")
  l5 <- level_config(5, "normal")
  l50 <- level_config(50, "normal")
  expect_gt(l5$sentry_speed, l1$sentry_speed)
  expect_lt(l5$frightened, l1$frightened)
  expect_equal(l50$sentry_speed, difficulty_presets()$normal$sentry_speed_cap)
  expect_equal(l50$frightened, difficulty_presets()$normal$frightened_min)
  expect_equal(l50$gem_value, 1000L)
})

test_that("campaign cycles through the hand-made mazes", {
  names <- vapply(1:4, function(l) level_config(l)$name, character(1))
  expect_equal(names, c("Greenhouse", "Circuit", "Lattice", "Greenhouse"))
})

test_that("endless mode generates a maze per level", {
  a <- level_config(1, "hard", "endless", seed = 10)
  b <- level_config(2, "hard", "endless", seed = 10)
  expect_equal(a$name, "Endless #1")
  expect_true(is_valid_maze(a$rows))
  expect_false(identical(a$rows, b$rows))
})

test_that("level_config validates arguments", {
  expect_error(level_config(0), "positive")
  expect_error(level_config(1, "insane"), "Unknown difficulty")
  expect_error(level_config(1, "easy", "zen"), "Unknown mode")
})
