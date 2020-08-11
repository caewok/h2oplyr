step_call <- function(parent, fun, args = list(), vars = parent$vars, in_place = FALSE) {

  stopifnot(is_step(parent))
  stopifnot(is.character(fun))
  stopifnot(is.list(args))

  new_step(
    parent = parent,
    vars = vars,
    groups = parent$groups,
    implicit_copy = !in_place,
    needs_copy = in_place || parent$needs_copy,
    fun = fun,
    args = args,
    class = "h2oplyr_step_call"
  )
}

dt_call.h2oplyr_step_call <- function(x, needs_copy = x$needs_copy) {
  call2(x$fun, dt_call(x$parent, needs_copy), !!!x$args)
}

# dplyr verbs -------------------------------------------------------------

#' @importFrom utils head
#' @export
head.h2oplyr_step <- function(x, n = 6L, ...) {
  step_call(x, "head", args = list(n = n))
}

#' @importFrom utils tail
#' @export
tail.h2oplyr_step <- function(x, n = 6L, ...) {
  step_call(x, "tail", args = list(n = n))
}

#' @importFrom dplyr rename
#' @export
rename.h2oplyr_step <- function(.data, ...) {
  vars <- tidyselect::vars_rename(.data$vars, ...)
  new_vars <- names(vars)
  vars <- vars[vars != names(vars)]

  if (length(vars) == 0) {
    return(.data)
  }

  out <- step_call(.data,
    "setnames",
    args = list(unname(vars), names(vars)),
    vars = new_vars,
    in_place = TRUE
  )

  groups <- rename_groups(.data$groups, vars)
  step_group(out, groups)
}


#' @importFrom dplyr distinct
#' @importFrom h2o h2o.unique
#' @export
distinct.h2oplyr_step <- function(.data, ..., .keep_all = FALSE) {
  dots <- capture_dots(.data, ...)
  # message("Dot names: ", names(dots))

  if (length(dots) > 0) {
    only_syms <- all(vapply(dots, is_symbol, logical(1)))

    if (.keep_all) {
      if (only_syms) {
        # message("only syms, keep all")
        columns <- union(.data$groups, names(dots))
      } else {
        # message("keep all")
        .data <- mutate(.data, !!!dots)
        columns <- unique(names(dots))
      }
    } else {
      if (only_syms) {
        # message('only syms')
        .data <- select(.data, !!!dots)
        columns <- unique(names(dots))
      } else {
        # message("none")
        .data <- transmute(.data, !!!dots)
        columns <- unique(names(dots))
      }
    }
  } else {
    # message("no dots")
    columns <- NULL
  }

  args <- list()
  args$columns <- columns

  step_call(.data, "h2o.drop_duplicates", args = args)
}


# #' @export
# unique.h2oplyr_step <- function(x, incomparables = FALSE, ...) {
#   if (!missing(incomparables)) {
#     abort("`incomparables` not supported by `unique.h2oplyr_step()`")
#   }
#   distinct(x)
# }
