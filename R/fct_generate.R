#' Generate a random, symmetric maze for Endless mode
#'
#' The left half (including the centre column) is carved as a perfect maze
#' on a cell grid (cells on odd rows/columns) with a randomised depth-first
#' search. The sentry den with its surrounding ring corridor is stamped into
#' the centre, every dead end is then "braided" open so there are loops
#' everywhere, and the half is mirrored. Candidates are checked with
#' [validate_maze()] and regenerated if they fail.
#'
#' The global RNG state is left untouched.
#'
#' @param seed Integer seed; the same seed always gives the same maze.
#' @param height Odd number of rows, at least 15.
#' @param width Number of columns; must satisfy `width %% 4 == 3`
#'   and be at least 19 (so the centre column lies on the cell grid).
#' @param max_tries Number of candidates before giving up.
#' @return Character vector of maze rows.
#' @noRd
generate_maze <- function(seed, height = 21L, width = 23L, max_tries = 50L) {
  height <- as.integer(height)
  width <- as.integer(width)
  if (height < 15L || height %% 2L != 1L) {
    stop("`height` must be an odd number >= 15.", call. = FALSE)
  }
  if (width < 19L || width %% 4L != 3L) {
    stop("`width` must be >= 19 and satisfy width %% 4 == 3.", call. = FALSE)
  }

  with_seed(seed, {
    for (try in seq_len(max_tries)) {
      rows <- generate_candidate(height, width)
      if (is_valid_maze(rows)) {
        return(rows)
      }
    }
    stop("Could not generate a valid maze in ", max_tries, " tries.", call. = FALSE)
  })
}

#' Evaluate an expression with a temporary RNG seed
#' @noRd
with_seed <- function(seed, expr) {
  genv <- globalenv()
  had_seed <- exists(".Random.seed", envir = genv, inherits = FALSE)
  if (had_seed) {
    old <- get(".Random.seed", envir = genv, inherits = FALSE)
    on.exit(assign(".Random.seed", old, envir = genv), add = TRUE)
  } else {
    on.exit(
      if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
        rm(".Random.seed", envir = genv)
      },
      add = TRUE
    )
  }
  set.seed(seed)
  expr
}

#' Build one maze candidate (0-based coordinates internally)
#' @noRd
generate_candidate <- function(h, w) {
  cc <- (w - 1L) %/% 2L # centre column (odd)
  dr <- (h - 1L) %/% 2L # den centre row, forced odd
  if (dr %% 2L == 0L) dr <- dr - 1L

  g <- matrix("#", h, cc + 1L) # left half incl. centre column
  get <- function(r, c) g[r + 1L, c + 1L]
  set <- function(r, c, v) g[r + 1L, c + 1L] <<- v

  cell_rows <- seq(1L, h - 2L, by = 2L)
  cell_cols <- seq(1L, cc, by = 2L)
  excluded <- function(r, c) r == dr && c >= cc - 2L
  valid_cell <- function(r, c) {
    r >= 1L && r <= h - 2L && c >= 1L && c <= cc && !excluded(r, c)
  }
  steps <- list(c(-2L, 0L), c(2L, 0L), c(0L, -2L), c(0L, 2L))

  # 1. Perfect maze via iterative randomised DFS.
  visited <- matrix(FALSE, h, cc + 1L)
  stack <- list(c(1L, 1L))
  visited[2L, 2L] <- TRUE
  set(1L, 1L, ".")
  while (length(stack) > 0) {
    cur <- stack[[length(stack)]]
    opts <- Filter(
      function(s) {
        nr <- cur[1] + s[1]
        nc <- cur[2] + s[2]
        valid_cell(nr, nc) && !visited[nr + 1L, nc + 1L]
      },
      steps
    )
    if (length(opts) == 0) {
      stack[[length(stack)]] <- NULL
      next
    }
    s <- opts[[sample.int(length(opts), 1L)]]
    nr <- cur[1] + s[1]
    nc <- cur[2] + s[2]
    set(cur[1] + s[1] %/% 2L, cur[2] + s[2] %/% 2L, ".")
    set(nr, nc, ".")
    visited[nr + 1L, nc + 1L] <- TRUE
    stack[[length(stack) + 1L]] <- c(nr, nc)
  }

  # 2. Stamp the den: ring corridor, box walls, gate and den tiles.
  for (c in (cc - 4L):cc) {
    set(dr - 2L, c, " ")
    set(dr + 2L, c, " ")
  }
  for (r in (dr - 2L):(dr + 2L)) set(r, cc - 4L, " ")
  for (r in (dr - 1L):(dr + 1L)) {
    for (c in (cc - 3L):cc) set(r, c, "#")
  }
  set(dr - 1L, cc, "=")
  for (c in (cc - 2L):cc) set(dr, c, "S")

  # 3. Braid: open walls until no cell is a dead end.
  open_tile <- function(r, c) get(r, c) %in% c(".", " ")
  degree <- function(r, c) {
    d <- open_tile(r - 1L, c) + open_tile(r + 1L, c) + open_tile(r, c - 1L)
    # The centre column's right-hand wall mirrors its left-hand wall.
    d + if (c == cc) open_tile(r, c - 1L) else open_tile(r, c + 1L)
  }
  cells <- expand.grid(r = cell_rows, c = cell_cols)
  cells <- cells[sample.int(nrow(cells)), ]
  for (i in seq_len(nrow(cells))) {
    r <- cells$r[i]
    c <- cells$c[i]
    if (excluded(r, c) || degree(r, c) >= 2L) next
    opts <- Filter(
      function(s) {
        if (c == cc && s[2] > 0L) {
          return(FALSE)
        }
        valid_cell(r + s[1], c + s[2]) &&
          get(r + s[1] %/% 2L, c + s[2] %/% 2L) == "#"
      },
      steps
    )
    if (length(opts) == 0) next
    s <- opts[[sample.int(length(opts), 1L)]]
    set(r + s[1] %/% 2L, c + s[2] %/% 2L, ".")
  }

  # 4. Tunnel, power stars and player start.
  tunnel_rows <- cell_rows[cell_rows >= 3L & cell_rows <= h - 4L]
  set(tunnel_rows[sample.int(length(tunnel_rows), 1L)], 0L, "~")
  set(3L, 1L, "*")
  set(h - 4L, 1L, "*")
  pr <- min(dr + 6L, h - 2L)
  set(pr, cc, "P")

  # 5. Mirror into the full width.
  full <- cbind(g, g[, rev(seq_len(cc)), drop = FALSE])
  apply(full, 1, paste0, collapse = "")
}
