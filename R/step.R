# We use a hybrid approach where most of the computation is done on
# construction. This avoids the deeply recursive approach of dbplyr, which
# should improve performance because we're not repeatedly re-computing the
# same values.
#
# dt_call() is managed separately because it involves much more code (which
# which dilute the intent of the constructor), and should only be called
# relatively few times.

new_step <- function(parent,
                     vars = parent$vars,
                     groups = parent$groups,
                     implicit_copy = parent$implicit_copy,
                     needs_copy = parent$needs_copy,
                     env = parent$env,
                     ...,
                     class = character()) {

  stopifnot(h2o::is.h2o(parent) || is_step(parent))
  stopifnot(is.character(vars))
  stopifnot(is.character(groups))

  structure(
    list(
      parent = parent,
      vars = vars,
      groups = groups,
      implicit_copy = implicit_copy,
      needs_copy = needs_copy,
      env = env,
      ...
    ),
    class = c(class, "h2oplyr_step")
  )
}


#' @export
dim.h2oplyr_step <- function(x) {
  c(NA, length(x$vars))
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.h2oplyr_step <- function(x) {
  x$vars
}

#' @importFrom dplyr groups
#' @export
groups.h2oplyr_step <- function(x) {
  syms(x$groups)
}

#' @importFrom dplyr group_vars
#' @export
group_vars.h2oplyr_step <- function(x) {
  x$groups
}

#' @importFrom dplyr group_size
#' @export
group_size.h2oplyr_step <- function(x) {
  if(length(x$groups) == 0) return(nrow(x))
  collect(summarise(x, n = .N))$n
}

#' @importFrom dplyr n_groups
#' @export
n_groups.h2oplyr_step <- function(x) {
  if(length(x$groups) == 0) return(1)
  length(group_size(x)) # will be wrong if no groups b/c h2o does not trim
}

#' Force computation of a lazy data.table
#'
#' * `collect()` returns a tibble, grouped if needed
#' * `compute()` returns a new [lazy_dt]
#' * `as.data.table()` returns a data.table
#' * `as.data.frame()` returns a data frame
#' * `as_tibble()` returns a tibble
#'
#' @export
#' @param x A [lazy_dt]
#' @param ... Arguments used by other methods.
#' @importFrom dplyr collect
#' @rdname collect
collect.h2oplyr_step <- function(x, ...) {
  # for consistency with dbplyr::collect()
  out <- as_tibble(x)

  if (length(x$groups) > 0) {
    out <- group_by(out, !!!syms(x$groups))
  }

  out
}

#' @rdname collect
#' @export
#' @importFrom dplyr compute
compute.h2oplyr_step <- function(x, name = unique_name(), ...) {
  tbl.hex <- dt_eval(x)
  if(h2o::h2o.getId(tbl.hex) != name) tbl.hex <- h2o::h2o.assign(tbl.hex, name)
  out <- lazy_dt(tbl.hex)

  if (length(x$groups) > 0) {
    out <- step_group(out, x$groups)
  }

  out
}

#' @rdname collect
#' @export
#' @param keep.rownames Ignored as dplyr never preserves rownames.
as.data.table.h2oplyr_step <- function(x, keep.rownames = FALSE, ...) {
  as.data.table(as.data.frame(x))
}

#' @rdname collect
#' @export
as.data.frame.h2oplyr_step <- function(x, ...) {
  as.data.frame(dt_eval(x))
}

#' @rdname collect
#' @export
#' @importFrom tibble as_tibble
as_tibble.h2oplyr_step <- function(x, ...) {
  as_tibble(dt_eval(x))
}

#' @export
#' @importFrom dplyr pull
pull.h2oplyr_step <- function(.data, var = -1) {
  expr <- enquo(var)
  var <- dplyr:::find_var(expr, .data$vars)

  .data <- ungroup(.data)
  .data <- select(.data, !! sym(var))
  .data <- collect(.data)
  .data[[1]]
}

#' @export
print.h2oplyr_step <- function(x, ...) {
  cat_line(crayon::bold("Source: "), "h2o frame ", dplyr::dim_desc(x))
  cat_line(crayon::bold("Call:   "), expr_text(dt_call(x)))
  cat_line()
  cat_line(format(as_tibble(head(x)))[-1]) # Hack to remove "A tibble" line
  cat_line()
  cat_line(crayon::silver(
    "# Use as.data.table()/as.data.frame()/as_tibble() to access results"
  ))

  invisible(x)
}

#' @importFrom dplyr show_query
#' @export
show_query.h2oplyr_step <- function(x) {
  dt_call(x)
}

is_step <- function(x) inherits(x, "h2oplyr_step")


# Returns a named list of data.tables: most just dispatch to their
# parent. The only exceptions are dt_step_first() and the two-table verbs.
dt_sources <- function(x) {
  UseMethod("dt_sources")
}
dt_sources.h2oplyr_step <- function(x) {
  dt_sources(x$parent)
}

dt_call <- function(x, needs_copy = x$needs_copy) {
  UseMethod("dt_call")
}
dt_call.h2oplyr_step <- function(x, needs_copy = x$needs_copy) {
  dt_call(x$parent, needs_copy)
}
