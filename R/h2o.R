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
`[.H2OFrame` <- function(data, row, col, drop = TRUE, by, keyby) {
  message("In [.H2OFrame")

  column_is_grouped <- FALSE
  if(!missing(by) && !missing(keyby)) {
    stop("Only one of 'by' or 'keyby' should be specified")
  } else if(!missing(by)) {
    if(missing(col)) stop("Applying a group to an H2O Frame requires specification of an aggregate function for the col variable.")
    keyby <- dtplyr:::replace_dot_alias(substitute(by))
    column_is_grouped <- TRUE
  } else if(!missing(keyby)) {
    if(missing(col)) stop("Applying a group to an H2O Frame requires specification of an aggregate function for the col variable.")
    keyby <- dtplyr:::replace_dot_alias(substitute(keyby))
    column_is_grouped <- TRUE
  }


  # data.table evaluates the row filter first. e.g. mtcars.dt[cyl == 4, mean(mpg)]; mtcars.dt[cyl == 8, mean(mpg)]
  if(!missing(row)) {
    # replace all the variable names with data[[name]] and evaluate
    rowsub <- substitute(row)
    if(class(rowsub[[1]]) == "name" & as.character(rowsub[[1]]) == "order") {
      stopifnot(length(rowsub) > 1)
      # translate, e.g., order(mpg, cyl, desc(hp)), into h2o.arrange
      args <- c(list(x = data),
                as.list(as.character(rowsub[2:length(rowsub)])))
      data <- do.call(h2o.arrange, args)

    } else {
      tmp <- subdatanames(rowsub, data_name = "data")
      row <- rlang::eval_tidy(tmp)
      data <- h2o:::`[.H2OFrame`(data, row = row, drop = drop)
    }


  }

  new_colnames <- NULL
  # based on data.table `[` function
  if(!missing(col)) {
    colsub <- replace_dot_alias(substitute(col))
    colsub <- replace_N_alias(colsub, col = colnames(data)[[1]])

    if(column_is_grouped) {
      data <- eval_grouping(data = data, colsub = colsub, keyby = keyby)

    } else {
      res <- eval_columns(data = data, colsub = colsub)
      col <- res$col
      data <- res$data
      data <- h2o:::`[.H2OFrame`(data, col = col, drop = drop)
    }
  }

  # update column names if passed a named list or named vector
  # if(!is.null(new_colnames)) h2o::colnames(data) <- new_colnames

  return(data)
}

#' @importFrom dplyr group_data
#' @importFrom vctrs new_list_of new_data_frame
#' @export
group_data.H2OFrame <- function(.data) {
  # same as dplyr::group_data.data.frame
  rows <- vctrs::new_list_of(list(seq_len(nrow(.data))), ptype = integer())
  vctrs::new_data_frame(list(.rows = rows), n = 1L)
}

#' Copy an H2OFrame
#'
#' Simply returns the H2OFrame; used only for consistency with data.table.
copy

#' Set column names of H2OFrame
#'
#' Mimics functionality of \code{\link[data.table]{setnames}} from the data.table package.
#' Changes the column names of the provided H2OFrame.
#'
#' @param x H2OFrame
#' @param old When new is provided, character names or numeric positions of column names to change. When new is not provided, a function or the new column names. If a function, it will be called with the current column names and is supposed to return the new column names. The new column names must be the same length as the number of columns. See examples.
#' @param new Optional. It can be a function or the new column names. If a function, it will be called with old and expected to return the new column names. The new column names must be the same length as columns provided to old argument.
#' @param skip_absent Skip items in old that are missing (i.e. absent) in 'names(x)'. Default FALSE halts with error if any are missing.
#' @return The renamed H2OFrame.
#' @export
setnames <- function(x, ...) UseMethod("setnames")

#' @importFrom data.table setnames
#' @export
setnames.default <- function(x, ...) data.table::setnames(x, ...)

#' @export
setnames.data.table <- function(x, ...) NextMethod()


#' @importFrom h2o `colnames<-` colnames
#' @export
setnames.H2OFrame <- function(x, old, new, skip_absent = FALSE) {
  # see setnames from the data.table package
  existing_cols <- h2o::colnames(x)
  if(missing(new)) {
    if(is.function(old)) {
      new <- old(existing_cols)
    } else {
      new <- old
    }

  } else {
    # not missing new
    stopifnot(is.character(old),
              old %in% existing_cols)

    if(is.function(new)) {
      new <- new(old)
    }

    # create a full new list of column names
    names(existing_cols) <- existing_cols
    existing_cols[old] <- new
    new <- unname(existing_cols) # probably don't need unname, but just in case

  }

  stopifnot(length(new) == ncol(x),
            is.character(new))

  h2o::colnames(x) <- new
  return(x)
}

#' @importFrom h2o colnames h2o.group_by
eval_grouping <- function(data, colsub, keyby) {
  message("evaluating columns with keyby: ", as.character(colsub), " ", as.character(keyby))

  # according to data.table, by can be:
  # • single unquoted name: by = x
  # • list of expressions with column names: by = .(x = x > 0, y)
  # • single character string: by = "x,y,z"
  # • character vector: by = c("x", "y")
  # • startcol:endcol: by = x:z
  # for now, check for all but character string. Accept .() for dtplyr but no expressions.
  root <- if (is.call(keyby)) as.character(keyby[[1L]])[1L] else ""
  if(root == "list" || root == "c") {
    keyby <- as.character(keyby[-1])
  } else if(root == ":") {
    stopifnot(length(keyby) == 3,
              is.name(keyby[[2]]),
              is.name(keyby[[3]]))

    cols <- colnames(data)
    start_col <- as.character(keyby[[2]])
    end_col <- as.character(keyby[[3]])
    start_i <- which(cols == start_col)
    end_i <- which(cols == end_col)
    stopifnot(length(start_i) == 1,
              length(end_i) == 1,
              end_i >= start_i)
    keyby <- cols[start_i:end_i]
  } else if(root == "") {
    stopifnot(is.name(keyby) | class(keyby) == "character")
    keyby <- as.character(keyby)
  } else stop("Keyby: root not recognized.")

  # https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-munging/groupby.html
  # h2o.groupby can only use the following:
  # nrow
  # min
  # max
  # mean
  # mode -- categorical columns only
  # median -- not listed but works
  # count fails -- use nrow instead
  # sd
  # ss (sum of squares) -- fails
  # sum
  # var
  # must include at least one aggregate function

  # h2o::h2o.group_by(data = data, by = keyby, nrow("cyl"), mean("mpg"))

  root <- if (is.call(colsub)) as.character(colsub[[1L]])[1L] else ""

  if(root == "list" | root == "c") {
    stopifnot(length(colsub) > 1)

    # each call is a separate aggregation argument
    stopifnot(sapply(colsub[-1], is.call))
    args <- as.list(colsub[-1])

  } else if(root == "") {
    stop("Keyby: An aggregate function is required.")

  } else if(root == ":=") {
    # := indicates a mutate function, such that the underlying non-subsetted frame is modified using subset data
    # to accomplish this with h2o, need to create a separate frame to analyze the subset and then join them
    colsub[[1]] <- as.name(".")
    args <- list(data = data, col = colsub, by = as.name(keyby))

    subdata <- do.call(dtplyr:::`[.H2OFrame`, args)
    newcols <- setdiff(colnames(subdata), keyby)
    cols_to_keep <- setdiff(colnames(data), newcols)

    data <- h2o::h2o.merge(x = h2o:::`[.H2OFrame`(data = data, row = 1:nrow(data), col = cols_to_keep),
              y = subdata,
              by = keyby,
              all.x = TRUE)

    return(data)

  } else if(root == "[" && as.character(colsub[[2]]) == ".SD") {
    # .SD indicates a filter on a subset.
    # e.g., .SD[mpg < mean(mpg)] in by_cyl %>% filter(mpg < mean(mpg))
    # or .SD[mpg < mean(mpg), .(hp = mean(hp))]
    # treat similarly to :=
    # here, each subset should be filtered separately and then combined

    # first, handle the row filter
    stopifnot(length(colsub) == 3 | length(colsub) == 4)
    sd_row <- colsub[[3]]

    parts <- sublogicals_fn(sd_row)
    parts <- parts[sapply(parts, is.call)]
    stopifnot(length(parts) > 0)

    col_call <- as.call(c(list(as.name("list")), parts))

    args <- list(data = data, col = col_call, by = as.name(keyby))
    subdata <- do.call(dtplyr:::`[.H2OFrame`, args)
    newcols <- setdiff(colnames(subdata), keyby)
    cols_to_keep <- setdiff(colnames(data), newcols)

    joined_data <- h2o::h2o.merge(x = h2o:::`[.H2OFrame`(data = data, row = 1:nrow(data), col = cols_to_keep),
                             y = subdata,
                             by = keyby,
                             all.x = TRUE)

    # modify the aggregation calls to be the names of the newcols
    # each new column is the function_column, like mean_mpg
    for(i in seq_along(parts)) {
      new_colsub <- substitute_call_element(sd_row,
                                            element = parts[[i]],
                                            replacement = as.name(paste(as.character(parts[[i]]), collapse = "_")))
    }

    # run the selection on the joined data, using the new columns, and remove the new columns
    col_call <- as.call(c(as.name("list"), lapply(colnames(data), as.name)))
    args <- list(data = joined_data, row = new_colsub, col = col_call)
    filtered_data <- do.call(dtplyr:::`[.H2OFrame`, args)

    # if a column argument provided, apply it to the filtered data
    if(length(colsub) == 4) {
      sd_col <- colsub[[4]]
      args <- list(data = filtered_data, col = sd_col, keyby = as.name(keyby))
      filtered_data <- do.call(dtplyr:::`[.H2OFrame`, args)

    }

    return(filtered_data)

  } else {
    args <- as.list(colsub)
  }

  # add "" to column names of the aggregate arguments
  # switch out count for nrow
  for(i in seq_len(length(args))) {
    arg <- args[[i]]
    stopifnot(length(arg) == 2,
              is.name(arg[[1]]),
              is.name(arg[[2]])
    ) # all are of form mean(col)
    if(as.character(arg[[1]]) == "count") arg[[1]] <- as.name("nrow")
    arg[[2]] <- as.character(arg[[2]])
    args[[i]] <- arg
  }

  args$data <- data
  args$by <- keyby
  data <- do.call(h2o::h2o.group_by, args)

  new_names <- names(colsub)[-1]
  if(!is.null(new_names) && any(new_names != "")) {
    # rename aggregated columns
    stopifnot(length(new_names) == length(args) - 2) # minus data and key
    # keyed columns are returned as normal; only change the end columns
    cols <- colnames(data)
    new_names <- c(cols[1:length(args$by)], new_names)
    blank_idx <- new_names == ""
    new_names[blank_idx] <- cols[blank_idx]

    h2o::colnames(data) <- new_names

  }

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
        col_mod <- rlang::eval_tidy(col_mod)
        new_col_name = names(colsub)[[i]]
        if(is.null(new_col_name) || new_col_name == "") {
          new_col_name <- paste0("V", i - 1)
        }
        data <- h2o:::`[<-.H2OFrame`(data, col = new_col_name, drop = FALSE, value = col_mod)

        # replace the colsub entry with the new name, so that it is included in the final result
        colsub[[i]] <- as.name(new_col_name)
      } else if(!is.name(colsub[[i]])) {
        # integer or some other scalar
        col_mod <- colsub[[i]]
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
  } else if(root == ":=") {
    # internal mutate of columns.
    # first argument are the column names
    # second argument is what to apply to the H2OFrame
    # the returned data set should be the full set with the mutations
    # arguably, should re-assign to the original name using h2o.assign, but that is probably not necessary
    # instead, can use compute() for re-assignment
    if(length(colsub) == 2) {
      stopifnot(names(colsub)[[2]] != "")
      colsub[[3]] <- colsub[[2]] # e.g., `:=`(cyl2 = cyl * 2); cyl2 is name
      colsub[[2]] <- as.name(names(colsub)[[2]])
    }
    stopifnot(length(colsub) == 3)

    if(class(colsub[[2]]) == "call") {
      mutated_col_names <- rlang::eval_tidy(colsub[[2]])
    } else if(class(colsub[[2]]) == "name") {
      mutated_col_names <- as.character(colsub[[2]])
    } else stop("Class for the LHS of ':=' is not recognized.")

    col_mod <- subdatanames(colsub[[3]], data_name = "data")
    mutated_values <- rlang::eval_tidy(col_mod) # may return a list

    stopifnot(length(mutated_values) == length(mutated_col_names))
    if(length(mutated_values) > 1) {
      for(i in seq_along(mutated_values)) {
        data <- h2o:::`[<-.H2OFrame`(data, col = mutated_col_names[[i]], drop = FALSE, value = mutated_values[[i]])
      }
    } else {
      data <- h2o:::`[<-.H2OFrame`(data, col = mutated_col_names, drop = FALSE, value = mutated_values)
    }

    colvars <- h2o::colnames(data)

  } else if(root == ":") {
    colvars <- rlang::eval_tidy(colsub)

  } else if(root == "") {
    stopifnot(length(colsub) == 1)
    if(class(colsub) == "name") {
      colvars <- as.character(colsub)
    } else {
      colvars <- rlang::eval_tidy(colsub)
    }

  } else stop("Root not recognized.")
  names(colvars) <- new_colnames
  return(list(data = data, col = colvars))
}

subdatanames <- function(sub, data_name) {
  if(length(sub) == 1) return(sub)

  # check for h2o functions and replace
  if(class(sub[[1]]) == "name") {
    sub[[1]] <- as.name(switch(as.character(sub[[1]]),
                               mean = "h2o.mean",
                               median = "h2o.median",
                               abs = "h2o.abs",
                               acos = "h2o.acos",
                               all = "h2o.all",
                               any = "h2o.any",
                               cor = "h2o.cor",
                               cos = "h2o.cos",
                               cosh = "h2o.cosh",
                               cummax = "h2o.cummax",
                               cummin = "h2o.cummin",
                               cumprod = "h2o.cumprod",
                               cumsum = "h2o.cumsum",
                               exp = "h2o.exp",
                               floor = "h2o.floor",
                               gsub = "h2o.gsub",
                               ifelse = "h2o.ifelse",
                               log = "h2o.log",
                               log10 = "h2o.log10",
                               log2 = "h2o.log2",
                               match = "h2o.match",
                               max = "h2o.max",
                               mean = "h2o.mean",
                               median = "h2o.median",
                               min = "h2o.min",
                               nchar = "h2o.nchar",
                               prod = "h2o.prod",
                               quantile = "h2o.quantile",
                               range = "h2o.range",
                               sd = "h2o.sd",
                               sin = "h2o.sin",
                               sqrt = "h2o.sqrt",
                               strsplit = "h2o.strsplit",
                               sub = "h2o.sub",
                               substr = "h2o.substr",
                               substring = "h2o.substring",
                               tan = "h2o.tan",
                               tanh = "h2o.tanh",
                               toupper = "h2o.toupper",
                               trim = "h2o.trim",
                               trunc = "h2o.trunc",
                               unique = "h2o.unique",
                               var = "h2o.var",
                               which = "h2o.which",
                               which.max = "h2o.which_max",
                               which.min = "h2o.which_min",
                               as.character(sub[[1]])))
  }

  for(i in 2:length(sub)) { # first is a function name; skip
    if(class(sub[[i]]) == "name") {
      sub[[i]] <- call("[[", as.name(data_name), as.character(sub[[i]]))

    } else if(class(sub[[i]]) %in% c("call", "<-", "(")) {
      sub[[i]] <- subdatanames(sub[[i]], data_name = data_name)
    }
  }
  return(sub)
}

sublogicals_fn <- function(sub) {
 unique(unlist(sublogicals(sub)))
}

sublogicals <- function(sub) {
  print(sub)

  if(is.name(sub)) return(sub)

  LOGICAL_NAMES <- c("[", "(", "&", "|", "!", "==", "!=", "<", "<=", ">=", ">")
  IGNORE_NAMES <- c(".SD")

  ignore_idx <- as.character(sub) %in% IGNORE_NAMES
  sub <- sub[!ignore_idx]

  if(as.character(sub[[1]]) %in% LOGICAL_NAMES) {
    return(lapply(sub[-1], sublogicals))

  } else {
    return(sub)
  }
}

substitute_call_element <- function(the_call, element, replacement) {
  for(i in seq_along(the_call)) {
    if(identical(the_call[[i]], element)) the_call[[i]] <- replacement
    if(is.call(the_call[[i]])) the_call[[i]] <- substitute_call_element(the_call = the_call[[i]], element = element, replacement = replacement)
  }
  return(the_call)
}

replace_N_alias <- function(sub, col) {
  N_idx <- as.character(sub) == ".N"
  if(any(N_idx)) {
    for(i in which(N_idx)) {
      sub[[i]] <- call("nrow", as.name(col))
      if(names(sub)[[i]] == "") names(sub)[[i]] <- "N"
    }
  }

  # recurse through the various calls
  call_idx <- sapply(sub, is.call)
  if(any(call_idx)) {
    for(i in which(call_idx)) {
      sub[[i]] <- replace_N_alias(sub = sub[[i]], col)
    }
  }

  return(sub)
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
#   var = dtplyr:::replace_dot_alias(substitute(var))
#   return(var)
# }
#
# fn_row <- function(var) substitute(var)

# typical dtplyr mutate
# colsub <- fn_col(`:=`(c("cyl2", "cyl4"), {
#   cyl2 <- cyl * 2
#   cyl4 <- cyl2 * 2
#   .(cyl2, cyl4)
# }))

# copy(dth2o_2)[, `:=`(cyl2 = cyl * 2)]
# colsub <- fn_col(`:=`(cyl2 = cyl * 2))

# typical dtplyr transmute
# `_DT1`[, .(cyl2 = cyl * 2, vs2 = vs * 2)]
# colsub <- fn_col(.(cyl2 = cyl * 2, vs2 = vs * 2))

# dtplyr summarize
# mean works; median doesn't
# Call:   dth2o_2[, .(a = median(mpg))]
# colsub <- fn_col(.(a = median(mpg)))

# typical DT mutates
# colsub <- fn_col(cyl := 8)
# colsub <- fn_col(mpg := mean(mpg))
# colsub <- fn_col(am := NULL)

# group_size calls: dth2o_2[, .(n = .N), keyby = .(cyl)]
# may or may not be in a group
# colsub <- fn_col(.(n = .N))
