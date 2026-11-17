#' Maze symbols understood by Maze Muncher
#'
#' * `#` wall
#' * `.` seed (regular pellet)
#' * `*` power star
#' * `P` player start
#' * `S` sentry den tile
#' * `=` den gate (sentries only)
#' * ` ` empty corridor
#' * `~` wrap tunnel (must sit on the left/right border)
#'
#' @noRd
maze_symbols <- function() {
  c("#", ".", "*", "P", "S", "=", " ", "~")
}

#' Symbols the player can walk on
#' @noRd
player_walkable_symbols <- function() {
  c(".", "*", "P", " ", "~")
}

#' Mirror a half-width layout into a symmetric full-width layout
#'
#' Each row holds the left half *including* the centre column; the centre
#' column is kept once and the rest is mirrored to the right.
#'
#' @param half Character vector of half rows.
#' @return Character vector of full rows (width `2 * nchar(half) - 1`).
#' @noRd
mirror_layout <- function(half) {
  vapply(
    half,
    function(row) {
      ch <- strsplit(row, "", fixed = TRUE)[[1]]
      paste0(c(ch, rev(ch[-length(ch)])), collapse = "")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' The hand-made campaign mazes
#'
#' All three layouts are original designs. They are stored as left halves
#' and mirrored, which guarantees left/right symmetry.
#'
#' @return A named list; each element has `name` and `rows`.
#' @noRd
maze_layouts <- function() {
  list(
    greenhouse = list(
      name = "Greenhouse",
      rows = mirror_layout(c(
        "############",
        "#*...#.....#",
        "#.##.#.###.#",
        "#...........",
        "#.##.###.###",
        "#....#...#..",
        "####.#.#.#.#",
        "~~~~...     ",
        "####.## ###=",
        "#....## #SSS",
        "#.##.## ####",
        "#.##...     ",
        "#.##.###.###",
        "#....#.....P",
        "###.##.###.#",
        "#*.........#",
        "#.##.#.#.###",
        "#....#.#....",
        "#.####.#.##.",
        "#...........",
        "############"
      ))
    ),
    circuit = list(
      name = "Circuit",
      rows = mirror_layout(c(
        "############",
        "#*..#.......",
        "#.#.#.####.#",
        "#.#...#....#",
        "#.###.#.##.#",
        "#.....#..#..",
        "###.#.##.#.#",
        "#...#.      ",
        "#.###.# ###=",
        "#.....# #SSS",
        "###.#.# ####",
        "~~~.#.      ",
        "###.#.####.#",
        "#.....#....P",
        "#.###.#.##.#",
        "#*..#......#",
        "###.#.####.#",
        "#......#....",
        "#.####.#.##.",
        "#...........",
        "############"
      ))
    ),
    lattice = list(
      name = "Lattice",
      rows = mirror_layout(c(
        "############",
        "#*..##.....#",
        "#.#.##.#.#.#",
        "#.#....#.#..",
        "#.####.#.#.#",
        "#......#...#",
        "#.#.####.#.#",
        "#.#.#.      ",
        "~.....# ###=",
        "#.###.# #SSS",
        "#.....# ####",
        "###.#.      ",
        "#...#.###.##",
        "#.#.#...#..P",
        "#.#.###.#.#.",
        "#*........#.",
        "###.#.###.#.",
        "#...#...#...",
        "#.#####.#.#.",
        "#...........",
        "############"
      ))
    )
  )
}

#' Parse a maze layout
#'
#' @param rows Character vector, one string per maze row.
#' @return A list with `width`, `height`, the `tiles` character matrix, the
#'   positions (row/col matrices) of the `player`, `den` and `gate`, and
#'   `pellet_count` / `star_count`.
#' @noRd
parse_maze <- function(rows) {
  if (!is.character(rows) || length(rows) == 0) {
    stop("A maze must be a non-empty character vector.", call. = FALSE)
  }
  widths <- nchar(rows)
  if (length(unique(widths)) != 1) {
    stop("All maze rows must have the same width.", call. = FALSE)
  }
  tiles <- do.call(rbind, strsplit(rows, "", fixed = TRUE))
  unknown <- setdiff(unique(as.vector(tiles)), maze_symbols())
  if (length(unknown) > 0) {
    stop(
      "Unknown maze symbol(s): ",
      paste0("'", unknown, "'", collapse = ", "),
      call. = FALSE
    )
  }
  locate <- function(sym) which(tiles == sym, arr.ind = TRUE)
  list(
    width = ncol(tiles),
    height = nrow(tiles),
    tiles = tiles,
    player = locate("P"),
    den = locate("S"),
    gate = locate("="),
    pellet_count = sum(tiles %in% c(".", "*")),
    star_count = sum(tiles == "*")
  )
}

#' Orthogonal neighbours of a tile, wrapping horizontally
#'
#' Horizontal wrapping only matters for tunnel rows, because
#' [validate_maze()] demands that every other border tile is a wall.
#'
#' @noRd
maze_neighbours <- function(r, c, height, width) {
  cand <- rbind(
    c(r - 1L, c),
    c(r + 1L, c),
    c(r, if (c == 1L) width else c - 1L),
    c(r, if (c == width) 1L else c + 1L)
  )
  cand[cand[, 1] >= 1L & cand[, 1] <= height, , drop = FALSE]
}

#' Breadth-first reachability over a logical walkability matrix
#'
#' @param walk Logical matrix: `TRUE` where movement is allowed.
#' @param start Integer vector `c(row, col)`.
#' @return Logical matrix of tiles reachable from `start`.
#' @noRd
maze_reachable <- function(walk, start) {
  h <- nrow(walk)
  w <- ncol(walk)
  seen <- matrix(FALSE, h, w)
  if (!walk[start[1], start[2]]) {
    return(seen)
  }
  queue <- matrix(0L, h * w, 2)
  queue[1, ] <- start
  head <- 1L
  tail <- 1L
  seen[start[1], start[2]] <- TRUE
  while (head <= tail) {
    cur <- queue[head, ]
    head <- head + 1L
    nb <- maze_neighbours(cur[1], cur[2], h, w)
    for (i in seq_len(nrow(nb))) {
      r <- nb[i, 1]
      c <- nb[i, 2]
      if (walk[r, c] && !seen[r, c]) {
        seen[r, c] <- TRUE
        tail <- tail + 1L
        queue[tail, ] <- c(r, c)
      }
    }
  }
  seen
}

#' Check a maze for playability
#'
#' Rules:
#' * rectangular, known symbols only, at least 7 x 7;
#' * exactly one player start, exactly one gate, at least 4 den tiles;
#' * the gate has walkable corridor above and a den tile below;
#' * the border is wall, except tunnels, which come in left/right pairs;
#' * every pellet is reachable from the player start;
#' * every den tile can reach the corridor above the gate;
#' * no dead ends and no 2x2 open areas in the player's corridors.
#'
#' @param rows Character vector of maze rows.
#' @return Character vector of problems; empty when the maze is valid.
#' @noRd
validate_maze <- function(rows) {
  maze <- tryCatch(parse_maze(rows), error = function(e) conditionMessage(e))
  if (is.character(maze)) {
    return(maze)
  }
  problems <- character()
  add <- function(...) problems <<- c(problems, paste0(...))

  h <- maze$height
  w <- maze$width
  tiles <- maze$tiles
  if (h < 7 || w < 7) {
    return("Maze must be at least 7 x 7 tiles.")
  }
  if (nrow(maze$player) != 1) {
    add("Maze needs exactly one player start 'P' (found ", nrow(maze$player), ").")
  }
  if (nrow(maze$den) < 4) {
    add("Maze needs at least 4 den tiles 'S' (found ", nrow(maze$den), ").")
  }
  if (nrow(maze$gate) != 1) {
    add("Maze needs exactly one gate '=' (found ", nrow(maze$gate), ").")
  }

  # Border: walls, plus paired tunnels on the left/right edges only.
  top_bottom <- c(tiles[1, ], tiles[h, ])
  if (any(top_bottom != "#")) {
    add("The top and bottom rows must be solid wall.")
  }
  left <- tiles[, 1]
  right <- tiles[, w]
  if (any(!left %in% c("#", "~")) || any(!right %in% c("#", "~"))) {
    add("The left and right borders may only hold walls or tunnels.")
  }
  if (any((left == "~") != (right == "~"))) {
    add("Every tunnel on one side needs a matching tunnel on the other side.")
  }
  if (length(problems) > 0) {
    return(problems)
  }

  # Gate geometry.
  g <- maze$gate[1, ]
  above <- tiles[g[1] - 1, g[2]]
  below <- tiles[g[1] + 1, g[2]]
  if (!above %in% player_walkable_symbols()) {
    add("The tile above the gate must be a corridor.")
  }
  if (below != "S") {
    add("The tile below the gate must be a den tile.")
  }

  # Player reachability.
  walk <- matrix(tiles %in% player_walkable_symbols(), h, w)
  reach <- maze_reachable(walk, maze$player[1, ])
  pellets <- tiles %in% c(".", "*")
  if (any(pellets & !reach)) {
    add(sum(pellets & !reach), " pellet(s) cannot be reached from the player start.")
  }
  if (above %in% player_walkable_symbols() && !reach[g[1] - 1, g[2]]) {
    add("The corridor above the gate cannot be reached from the player start.")
  }

  # Sentries must be able to leave the den.
  sentry_walk <- walk | tiles %in% c("S", "=")
  den_reach <- maze_reachable(sentry_walk, c(g[1] - 1, g[2]))
  if (any(tiles == "S" & !den_reach)) {
    add("Some den tiles are sealed off from the gate.")
  }

  # Dead ends.
  dead <- 0L
  for (r in seq_len(h)) {
    for (c in seq_len(w)) {
      if (!walk[r, c]) next
      nb <- maze_neighbours(r, c, h, w)
      if (sum(walk[nb]) < 2) dead <- dead + 1L
    }
  }
  if (dead > 0) {
    add("Maze has ", dead, " dead end(s).")
  }

  # Wide-open 2x2 areas make chases messy.
  open_blocks <- walk[-h, -w] & walk[-1, -w] & walk[-h, -1] & walk[-1, -1]
  if (any(open_blocks)) {
    add("Maze has ", sum(open_blocks), " 2x2 open area(s).")
  }

  problems
}

#' @rdname validate_maze
#' @noRd
is_valid_maze <- function(rows) {
  length(validate_maze(rows)) == 0
}
