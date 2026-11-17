#' Difficulty presets
#'
#' Speeds are in tiles per second, durations in seconds.
#'
#' * `player_speed`: base speed of the blob.
#' * `sentry_speed`, `speed_step`, `sentry_speed_cap`: base sentry speed,
#'   its increase per level, and the cap it never exceeds.
#' * `frightened`, `frightened_step`, `frightened_min`: how long sentries stay
#'   stunned after a power star, how much shorter that gets per level, and
#'   the floor.
#' * `sentries`: number of sentries (1-4).
#' * `release`: seconds before each sentry leaves the den.
#' * `schedule`: alternating scatter/chase durations; after the last entry
#'   the sentries chase for good.
#'
#' @return Named list of presets (`easy`, `normal`, `hard`).
#' @noRd
difficulty_presets <- function() {
  list(
    easy = list(
      label = "Easy",
      player_speed = 7.5,
      sentry_speed = 5.6,
      speed_step = 0.25,
      sentry_speed_cap = 7.6,
      frightened = 9,
      frightened_step = 0.5,
      frightened_min = 4,
      sentries = 3L,
      release = c(0, 4, 8, 12),
      schedule = c(9, 18, 9, 18, 7, 22, 7)
    ),
    normal = list(
      label = "Normal",
      player_speed = 8,
      sentry_speed = 6.6,
      speed_step = 0.3,
      sentry_speed_cap = 8.4,
      frightened = 7,
      frightened_step = 0.6,
      frightened_min = 2.5,
      sentries = 4L,
      release = c(0, 2, 5, 9),
      schedule = c(7, 20, 7, 20, 5, 20, 5)
    ),
    hard = list(
      label = "Hard",
      player_speed = 8.5,
      sentry_speed = 7.6,
      speed_step = 0.3,
      sentry_speed_cap = 9.2,
      frightened = 4.5,
      frightened_step = 0.5,
      frightened_min = 1.5,
      sentries = 4L,
      release = c(0, 1, 3, 5),
      schedule = c(5, 25, 5, 25, 3, 30)
    )
  )
}

#' Game modes
#' @noRd
game_modes <- function() {
  c("Campaign (3 mazes)" = "campaign", "Endless (generated)" = "endless")
}

#' Build the full configuration for one level
#'
#' This is the payload the JavaScript engine receives for each level.
#'
#' @param level Level number (1-based).
#' @param difficulty One of `names(difficulty_presets())`.
#' @param mode `"campaign"` cycles through the hand-made mazes; `"endless"`
#'   generates a fresh maze from `seed + level`.
#' @param seed Integer seed used in endless mode.
#' @return A list that is serialised to JSON for the client.
#' @noRd
level_config <- function(level, difficulty = "normal", mode = "campaign", seed = 1L) {
  level <- as.integer(level)
  if (length(level) != 1 || is.na(level) || level < 1L) {
    stop("`level` must be a positive integer.", call. = FALSE)
  }
  presets <- difficulty_presets()
  if (!difficulty %in% names(presets)) {
    stop("Unknown difficulty: ", difficulty, call. = FALSE)
  }
  if (!mode %in% game_modes()) {
    stop("Unknown mode: ", mode, call. = FALSE)
  }
  p <- presets[[difficulty]]
  step <- level - 1L

  if (mode == "campaign") {
    layouts <- maze_layouts()
    maze <- layouts[[(step %% length(layouts)) + 1L]]
    name <- maze$name
    rows <- maze$rows
  } else {
    rows <- generate_maze(as.integer(seed) + level)
    name <- paste0("Endless #", level)
  }

  list(
    level = level,
    name = name,
    rows = rows,
    difficulty = difficulty,
    player_speed = min(p$player_speed + 0.1 * step, p$player_speed + 1),
    sentry_speed = min(p$sentry_speed + p$speed_step * step, p$sentry_speed_cap),
    frightened = max(p$frightened - p$frightened_step * step, p$frightened_min),
    sentries = p$sentries,
    release = round(p$release / (1 + 0.15 * step), 2),
    schedule = p$schedule,
    gem_value = min(100L * level, 1000L),
    tunnel_factor = 0.5,
    frightened_factor = 0.55
  )
}
