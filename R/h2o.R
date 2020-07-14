#' Tidyeval h2o selection
#'
# #' @importFrom h2o `[[<-.H2OFrame`
# #' @importFrom rlang as_string ensym
# `[[<-.H2OFrame` <- function(data, name, value) {
#   message("In [[<-.H2OFrame")
#   name <- rlang::as_string(rlang::ensym(name))
#   # message("name is ", name)
#   h2o:::`[[<-.H2OFrame`(data, name = name, value = value)
# }

# #' @importFrom h2o `[[.H2OFrame`
# #' @importFrom rlang as_string ensym
# `[[.H2OFrame` <- function(x, i, exact = TRUE) {
#   message("In [[.H2OFrame")
#   i <- rlang::as_string(rlang::ensym(i)) # really should be the only possiblity -- a single variable?
#   # message("i is ", i)
#   h2o:::`[[.H2OFrame`(x, i = i, exact = exact)
# }

#' Extract or replace H2OFrame elements.
#'
#' Operators to extract or replace parts of H2OFrame objects.
#' Uses tidyverse non-standard evaluation; replaces underlying H2O operators.
#' Refer to column names without strings (see examples).
#'
#' @param data H2OFrame
#' @param row Row or an expression that evaluates to a row mask.
#' @param col One or more column names, joined using either list(), c(), or datatable's '.()' pronoun.
#'
#'
#' @examples
#' \dontrun{
#' h2o.init()
#' mtcars.hex <- h2o::as.h2o()
#' mtcars.hex[, .(mpg, cyl)]
#' mtcars.hex[, list(x = mpg, y = cyl)]
#' mtcars.hex[1:5,]
#' mtcars.hex[cyl == 4 & mpg > 20, c(mpg, cyl, wt)]
#' }
#' @export
`[<-.H2OFrame` <- function(data, row, col, ..., value) {
  message("In [<-.H2OFrame")
  new_colnames <- NULL
  # based on data.table `[` function
  if(!missing(col)) {
    new_colnames <- NULL
    colsub <- replace_dot_alias(substitute(col))
    root <- if (is.call(colsub)) as.character(colsub[[1L]])[1L] else ""
    if(root == "list" | root == "c") {
      stopifnot(length(colsub) > 1)
      colvars <- as.character(colsub[-1])
      new_colnames <- names(colsub)[-1]
    } else if(root == "") {
      colvars <- as.character(colsub)
    } else stop("Root not recognized.")
    names(colvars) <- new_colnames
    col <- colvars
  }

  if(!missing(row)) {
    # replace all the variable names with data[[name]] and evaluate
    rowsub <- substitute(row)
    tmp <- subdatanames(rowsub, data_name = "data")
    row <- eval(tmp)
  }

  out <- h2o:::`[<-.H2OFrame`(data, row = row, col = col, ..., value = value)
  if(!is.null(new_colnames)) h2o::colnames(out) <- new_colnames
  return(out)
}



#' @importFrom rlang as_string ensym
#' @export
`[.H2OFrame` <- function(data, row, col, drop = TRUE) {
  message("In [.H2OFrame")
  # data.table evaluates the row filter first. e.g. mtcars.dt[cyl == 4, mean(mpg)]; mtcars.dt[cyl == 8, mean(mpg)]
  if(!missing(row)) {
    # replace all the variable names with data[[name]] and evaluate
    rowsub <- substitute(row)
    tmp <- subdatanames(rowsub, data_name = "data")
    row <- eval(tmp)
    data <- h2o:::`[.H2OFrame`(data, row = row, drop = drop)
  }

  new_colnames <- NULL
  # based on data.table `[` function
  if(!missing(col)) {
    colsub <- replace_dot_alias(substitute(col))
    res <- eval_columns(data = data, colsub = colsub)
    col <- res$col
    data <- res$data
    data <- h2o:::`[.H2OFrame`(data, col = col, drop = drop)
  }

  # update column names if passed a named list or named vector
  if(!is.null(new_colnames)) h2o::colnames(data) <- new_colnames

  return(data)
}

eval_columns <- function(data, colsub) {
  message("evaluating columns: ", as.character(colsub))
  new_colnames <- NULL

  root <- if (is.call(colsub)) as.character(colsub[[1L]])[1L] else ""
  if(root == "list" | root == "c") {
    stopifnot(length(colsub) > 1)

    # check for calls, if so, apply the calls in turn
    for(i in 2:length(colsub)) {
      if(is.call(colsub[[i]])) {
        # apply the call to the H2OFrame
        col_mod <- subdatanames(colsub[[i]], data_name = "data")
        col_mod <- eval(col_mod)
        new_col_name = names(colsub)[[i]]
        if(is.null(new_col_name) || new_col_name == "") {
          new_col_name <- paste0("V", i - 1)
        }
        data <- h2o:::`[<-.H2OFrame`(data, col = new_col_name, drop = FALSE, value = col_mod)

        # replace the colsub entry with the new name, so that it is included in the final result
        colsub[[i]] <- as.name(new_col_name)
      }
    }

    colvars <- as.character(colsub[-1])
    new_colnames <- names(colsub)[-1]
  } else if(root == "") {
    colvars <- as.character(colsub)
  } else stop("Root not recognized.")
  names(colvars) <- new_colnames
  return(list(data = data, col = colvars))
}


subdatanames <- function(rowsub, data_name) {
  for(i in 2:length(rowsub)) { # first is a function name; skip
    if(class(rowsub[[i]]) == "name") {
      rowsub[[i]] <- call("[[", as.name(data_name), as.character(rowsub[[i]]))

    } else if(class(rowsub[[i]]) == "call") {
      rowsub[[i]] <- subdatanames(rowsub[[i]], data_name = data_name)
    }
  }
  return(rowsub)
}

# from data.table; copied here to avoid importing internal data.table function
replace_dot_alias = function(e) {
  # we don't just simply alias .=list because i) list is a primitive (faster to iterate) and ii) we test for use
  # of "list" in several places so it saves having to remember to write "." || "list" in those places
  if (is.call(e) && !is.function(e[[1L]])) {
    # . alias also used within bquote, #1912
    if (e[[1L]] == 'bquote') return(e)
    if (e[[1L]] == ".") e[[1L]] = quote(list)
    for (i in seq_along(e)[-1L]) if (!is.null(e[[i]])) e[[i]] = replace_dot_alias(e[[i]])
  }
  e
}

# fn_col <- function(var) {
#   var = replace_dot_alias(substitute(var))
#   return(var)
# }
#
# fn_row <- function(var) substitute(var)
