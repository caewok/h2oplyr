step_set <- function(x, y, style) {
  stopifnot(is_step(x))
  stopifnot(is_step(y))
  stopifnot(is.character(style))

  new_step(
    parent = x,
    parent2 = y,
    style = style,
    class = "h2oplyr_step_set"
  )
}

dt_sources.h2oplyr_step_set <- function(x) {
  dt_sources.h2oplyr_step_join(x)
}

dt_call.h2oplyr_step_set <- function(x, needs_copy = x$needs_copy) {
  lhs <- dt_call(x$parent, needs_copy)
  rhs <- dt_call(x$parent2)

  call <- switch(x$style,
    intersect = call2("fintersect", lhs, rhs),
    union = call2("funion", lhs, rhs),
    union_all = call2("funion", lhs, rhs, all = TRUE),
    setdiff = call2("fsetdiff", lhs, rhs),
  )

  call
}

# dplyr verbs -------------------------------------------------------------

#' @importFrom dplyr intersect
# Exported onload
intersect.h2oplyr_step <- function(x, y, ...) {
  step_set(x, y, style = "intersect")
}

#' @importFrom dplyr union
# Exported onload
union.h2oplyr_step <- function(x, y, ...) {
  step_set(x, y, style = "union")
}

#' @importFrom dplyr union_all
#' @export
union_all.h2oplyr_step <- function(x, y, ...) {
  step_set(x, y, style = "union_all")
}

#' @importFrom dplyr setdiff
# Exported onload
setdiff.h2oplyr_step <- function(x, y, ...) {
  step_set(x, y, style = "setdiff")
}
