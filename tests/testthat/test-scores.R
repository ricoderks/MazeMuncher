test_that("scores start empty and are stored best first", {
  path <- local_scores_file()
  expect_equal(nrow(read_scores()), 0)

  r1 <- add_score("Ann", 500, 2, "normal")
  r2 <- add_score("Bob", 1200, 3, "hard")
  r3 <- add_score("Cy", 500, 1, "easy")

  expect_equal(r1$rank, 1)
  expect_equal(r2$rank, 1)
  expect_equal(r3$rank, 3) # tie goes after the older entry
  s <- read_scores()
  expect_equal(s$name, c("Bob", "Ann", "Cy"))
  expect_equal(s$score, c(1200L, 500L, 500L))
  expect_true(file.exists(path))
})

test_that("score table is truncated to `keep` entries", {
  local_scores_file()
  for (i in 1:12) add_score(paste("P", i), i * 10, 1, "normal", keep = 10)
  s <- read_scores()
  expect_equal(nrow(s), 10)
  expect_equal(max(s$score), 120L)
  expect_equal(min(s$score), 30L)
  expect_true(is.na(add_score("Low", 1, 1, "normal", keep = 10)$rank))
  expect_equal(nrow(top_scores(3)), 3)
})

test_that("names are sanitised and bad scores rejected", {
  local_scores_file()
  expect_equal(clean_player_name("  <b>Rico</b>  "), "bRicob")
  expect_equal(clean_player_name(""), "Anonymous")
  expect_equal(clean_player_name(NULL), "Anonymous")
  expect_equal(nchar(clean_player_name(strrep("x", 40))), 16)
  expect_error(add_score("A", -5, 1, "easy"), "non-negative")
  expect_error(add_score("A", "abc", 1, "easy"), "non-negative")
})

test_that("a corrupt score file is treated as empty", {
  path <- local_scores_file()
  writeLines("not an rds", path)
  expect_equal(nrow(read_scores()), 0)
})
