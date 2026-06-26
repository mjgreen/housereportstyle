default_condition_levels <- function() {
  c("everything_fixed", "only_trees_fixed", "nothing_fixed")
}

default_condition_palette <- function() {
  c(
    "everything_fixed" = "#4477AA",
    "only_trees_fixed" = "#CC6677",
    "nothing_fixed" = "#228833"
  )
}

default_click_levels <- function() {
  c(
    "consume high",
    "consume low",
    "reject high",
    "reject low",
    "visit barren",
    "revisit high",
    "revisit low",
    "revisit barren"
  )
}

default_click_palette <- function() {
  c(
    "consume high" = "#C51B29",
    "consume low" = "#2C7FB8",
    "reject high" = "#FDAE6B",
    "reject low" = "#9ECAE1",
    "visit barren" = "#6B6B6B",
    "revisit high" = "#7F0000",
    "revisit low" = "#084081",
    "revisit barren" = "#252525"
  )
}

default_click_shapes <- function() {
  c(
    "consume high" = 21,
    "consume low" = 21,
    "reject high" = 4,
    "reject low" = 4,
    "visit barren" = 16,
    "revisit high" = 24,
    "revisit low" = 24,
    "revisit barren" = 17
  )
}

