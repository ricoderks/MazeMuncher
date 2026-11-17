test_that("mirror_layout produces symmetric rows", {
  rows <- mirror_layout(c("#.ab", "#..c"))
  expect_equal(rows, c("#.aba.#", "#..c..#"))
})

test_that("built-in mazes parse and are valid", {
  layouts <- maze_layouts()
  expect_length(layouts, 3)
  for (m in layouts) {
    expect_true(nzchar(m$name))
    expect_equal(validate_maze(m$rows), character(0), info = m$name)
    parsed <- parse_maze(m$rows)
    expect_equal(nrow(parsed$player), 1)
    expect_gte(nrow(parsed$den), 4)
    expect_gt(parsed$pellet_count, 100)
    expect_equal(parsed$star_count, 4)
    # Symmetric left/right.
    expect_equal(parsed$tiles, parsed$tiles[, rev(seq_len(parsed$width))], info = m$name)
  }
})

test_that("parse_maze rejects malformed input", {
  expect_error(parse_maze(character()), "non-empty")
  expect_error(parse_maze(c("###", "##")), "same width")
  expect_error(parse_maze(c("#x#")), "Unknown maze symbol")
})

test_that("validate_maze reports broken layouts", {
  good <- maze_layouts()$greenhouse$rows

  two_players <- good
  substr(two_players[2], 2, 2) <- "P"
  expect_match(validate_maze(two_players), "exactly one player start", all = FALSE)

  # Wall off the top-left corridor so its pellets become unreachable.
  sealed <- good
  substr(sealed[3], 2, 2) <- "#"
  substr(sealed[2], 6, 6) <- "#"
  substr(sealed[2], 7, 7) <- "#"
  expect_true(any(grepl("cannot be reached|dead end", validate_maze(sealed))))

  open_border <- good
  substr(open_border[1], 5, 5) <- "."
  expect_match(validate_maze(open_border), "top and bottom", all = FALSE)

  expect_false(is_valid_maze(c("#####", "#P..#", "#####")))
})
