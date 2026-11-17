test_that("generated mazes are valid and symmetric across seeds", {
  for (seed in 1:50) {
    rows <- generate_maze(seed)
    expect_length(rows, 21)
    expect_true(all(nchar(rows) == 23))
    expect_equal(validate_maze(rows), character(0), info = paste("seed", seed))
    tiles <- parse_maze(rows)$tiles
    expect_equal(tiles, tiles[, rev(seq_len(ncol(tiles)))])
  }
})

test_that("generation is reproducible and leaves the global RNG alone", {
  set.seed(99)
  before <- .Random.seed
  a <- generate_maze(7)
  expect_identical(.Random.seed, before)
  expect_identical(a, generate_maze(7))
  expect_false(identical(a, generate_maze(8)))
})

test_that("other sizes work and bad sizes are rejected", {
  expect_true(is_valid_maze(generate_maze(3, height = 25, width = 27)))
  expect_error(generate_maze(1, width = 24), "width")
  expect_error(generate_maze(1, height = 20), "height")
})
