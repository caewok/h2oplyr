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
#'
#' @importFrom rlang enexpr set_names
#' @importFrom h2o is.h2o colnames `colnames<-`
#' @importFrom dplyr `%>%`
#' @export
`[<-.H2OFrame` <- function(data, row, col, ..., value) {
  # message("In [<-.H2OFrame")

  # create an evaluation environment for interpreting row and column on data
  parent_env <- parent.frame()
  eval_env <- new.env(parent = parent_env)
  assign("data", data, envir = eval_env)

  # based on data.table `[` function
  if(!missing(row)) {
    # replace all the variable names with data[[name]] and evaluate
    rowsub_expr <- rlang::enexpr(row)
    row_value <- eval_rows(rowsub_expr, eval_env)
  }

  if(!missing(col)) {
    new_colnames <- NULL
    colsub_expr <- replace_dot_alias(rlang::enexpr(col))

    res <- eval_columns(colsub_expr, eval_env)
    eval_env <- res$eval_env
    colvars <- res$colvars
  }

  if(length(value) > 1 && !h2o::is.h2o(value)) value <- as.h2o(value)

  if(!missing(col) && !missing(row)) {
    out <- h2o:::`[<-.H2OFrame`(get("data", envir = eval_env), row = row_value, col = colvars, ..., value = value)
  } else if(!missing(row)) {
    out <- h2o:::`[<-.H2OFrame`(get("data", envir = eval_env), row = row_value, ..., value = value)
  } else if(!missing(col)) {
    out <- h2o:::`[<-.H2OFrame`(get("data", envir = eval_env), col = colvars, ..., value = value)
  } else {
    out <- get("data", envir = eval_env)
  }


  # rename columns
  data_mask <- h2o::colnames(out) %>% rlang::set_names()
  colvars <- colvars[names(colvars) != ""]
  if(length(colvars) > 0 && !identical(colvars, data_mask)) {
    data_mask[colvars] <- names(colvars)
    h2o::colnames(out) <- data_mask
  }

  return(out)
}


#' Extract parts of the H2OFrame object.
#'
#' Operator to extract parts of the H2OFrame object. This implementation modifies the
#' \code{\link[h2o]{`[.H2OFrame`}} version to more closely approximate data.table usage.
#'
#' @details
#' This implementation modifies the
#' \code{\link[h2o]{`[.H2OFrame`}} version to more closely approximate data.table usage in the following ways.
#'
#' @section For column specifications:
#' - Permits use of a list() or .() as the column entry.
#' - When using list() or .(), references can be directly to column names. For example, \code{mtcars.hex[, .(cyl, mpg)]}.
#' - When using list() or .(), common h2o aggregation functions can be specified without using the "h2o." prefix.
#'   For example, \code{mtcars.hex[, .(mean(cyl))]} instead of \code{mtcars.hex[, .(h2o.mean(cyl))]}.
#' - To use a variable, specify it outside the list or use a .env pronoun.
#'   For example, \code{mtcars.hex[, "cyl"]} or \code{mtcars.hex[, c("cyl", "hp")]} or
#'   \code{mtcars.hex[, .(.env$var)]} or \code{mtcars.hex[, var]}.
#' - Permits use of :=. See \code{\link[data.table]{.SD}}.
#' - Permits renaming columns by passing a named list. For example, \code{mtcars.hex[, .(cyl2 = cyl * 2)]}.
#'
#' @section For row specifications:
#' - Uses references to columns directly. For example, \code{mtcars.hex[cyl == 4, ]}.
#' - To use a variable, use a .env pronoun or !! or !!!.
#'
#' @section For group specifications:
#' - Permits use of .SD. See \code{\link[data.table]{.SD}}.
#' - Permits use of .N. See \code{\link[data.table]{.N}}.
#'
#' @importFrom rlang as_string ensym
#' @export
`[.H2OFrame` <- function(data, row, col, drop = TRUE, by, keyby) {
  # message("In [.H2OFrame")

  # create an evaluation environment for interpreting row and column on data
  parent_env <- parent.frame()
  eval_env <- new.env(parent = parent_env)
  assign("data", data, envir = eval_env)

  column_is_grouped <- FALSE
  if(!missing(by) && !missing(keyby)) {
    stop("Only one of 'by' or 'keyby' should be specified")
  } else if(!missing(by)) {
    if(missing(col)) stop("Applying a group to an H2O Frame requires specification of an aggregate function for the col variable.")
    # keyby <- h2oplyr:::replace_dot_alias(rlang::enexpr(by))
    keyby_expr <- h2oplyr:::replace_dot_alias(rlang::enexpr(by))
    column_is_grouped <- TRUE
  } else if(!missing(keyby)) {
    if(missing(col)) stop("Applying a group to an H2O Frame requires specification of an aggregate function for the col variable.")
    keyby_expr <- h2oplyr:::replace_dot_alias(rlang::enexpr(keyby))
    # keyby <- h2oplyr:::replace_dot_alias(rlang::enexpr(keyby))
    column_is_grouped <- TRUE
  }


  # data.table evaluates the row filter first. e.g. mtcars.dt[cyl == 4, mean(mpg)]; mtcars.dt[cyl == 8, mean(mpg)]
  if(!missing(row)) {
    data_mask <- with(eval_env, h2o::colnames(data)) %>% rlang::set_names()
    # replace all the variable names with data[[name]] and evaluate
    rowsub_expr <- rlang::enexpr(row)
    # replace .N with nrow applied to the first column.
    rowsub_expr <- replace_N_alias(rowsub_expr, col = data_mask[[1]])

    row_value <- eval_rows(rowsub_expr, eval_env)
    if(is.numeric(row_value)) row_value <- sort(row_value) # H2O will not re-order rows

    assign("data",
           value = h2o:::`[.H2OFrame`(get("data", envir = eval_env), row = row_value, drop = drop),
           envir = eval_env)
  }

  # based on data.table `[` function
  if(!missing(col)) {
    data_mask <- with(eval_env, h2o::colnames(data)) %>% rlang::set_names()
    # colsub <- replace_dot_alias(rlang::enexpr(col))
    colsub_expr <- replace_dot_alias(rlang::enexpr(col))

    # replace .N with nrow applied to the first column.
    colsub_expr <- replace_N_alias(colsub_expr, col = data_mask[[1]])

    if(column_is_grouped) {
      eval_env <- eval_grouping(colsub_expr, eval_env, keyby_expr)

    } else {
      res <- eval_columns(colsub_expr, eval_env)
      eval_env <- res$eval_env
      colvars <- res$colvars

      # limit to only the chosen columns
      data_mask <- with(eval_env, h2o::colnames(data)) %>% rlang::set_names()
      if(!identical(colvars, data_mask)) {
        assign("data",
               value = h2o:::`[.H2OFrame`(get("data", envir = eval_env), col = colvars, drop = FALSE),
               envir = eval_env)
        data_mask <- with(eval_env, h2o::colnames(data)) %>% rlang::set_names()
      }

      # rename columns
      colvars <- colvars[names(colvars) != ""]
      if(length(colvars) > 0 && !identical(colvars, data_mask)) {
        data_mask[colvars] <- names(colvars)
        h2o::colnames(eval_env$data) <- data_mask
      }
    }
  }

  return(get("data", envir = eval_env))
}

#' @importFrom dplyr group_data
#' @importFrom vctrs new_list_of new_data_frame
#' @export
group_data.H2OFrame <- function(.data) {
  # same as dplyr::group_data.data.frame
  rows <- vctrs::new_list_of(list(seq_len(nrow(.data))), ptype = integer())
  vctrs::new_data_frame(list(.rows = rows), n = 1L)
}

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

#' @importFrom h2o as.h2o is.h2o
#' @importFrom lubridate with_tz
#' @importFrom H2OUtilities h2o.moment
#' @export
as.h2o <- function(x, ...) {
  if(h2o::is.h2o(x)) return(h2o::as.h2o(x, ...))

  # as.h2o does not handle POSIXct, so temporarily convert to character
  posix_idx <- sapply(x, inherits, what = "POSIXt")
  if(any(posix_idx)) {
    x[, posix_idx] <- lapply(x[, posix_idx], lubridate::with_tz, tzone = "UTC")
    x[, posix_idx] <- lapply(x[, posix_idx], format, format = "%Y-%m-%dT%H:%M:%SZ")

    # h2o.as_date does not understand %OS3, so remove if present .000
    # if milliseconds are present, this will fail
    x[, posix_idx] <- lapply(x[, posix_idx], gsub, pattern = "[.]000Z", replacement = "Z")
  }

  # convert booleans to integer if option is set
  # otherwise, the default is enumerated
  if(getOption("h2oplyr.translate_booleans") == "int") x <- x %>% dplyr::mutate_if(is.logical, as.integer)

  out <- h2o::as.h2o(x, ...)

  # convert time strings back to time
  if(any(posix_idx)) {
    for(col in names(posix_idx)[posix_idx]) {
      out[, col] <- H2OUtilities::h2o.moment(date = out[, col], format = "%Y-%m-%dT%H:%M:%SZ")
    }
  }

  return(out)
}

#' @importFrom h2o is.h2o h2o.describe
h2o_schema <- function(dat) {
  stopifnot(h2o::is.h2o(dat))
  description <- h2o::h2o.describe(dat)

  # convert to named vector of types
  if(ncol(dat) == 1) {
    schema <- as.character(description$Type)[[1]]
    names(schema) <- as.character(description$Label[[1]])
  } else {

    schema <- as.character(description$Type)
    names(schema) <- as.character(description$Label)
  }
  schema
}

#' @importFrom h2o is.h2o h2o.hour
#' @importFrom H2OUtilities h2o.minute h2o.second
h2o_column_is_date <- function(dat, column_name) {
  stopifnot(h2o::is.h2o(dat))
  h2o::h2o.all(h2o::h2o.hour(dat[, column_name]) == 0 |
                 is.na(h2o::h2o.hour(dat[, column_name]))) &
    h2o::h2o.all(H2OUtilities::h2o.minute(dat[, column_name]) == 0 |
                   is.na(H2OUtilities::h2o.minute(dat[, column_name]))) &
    h2o::h2o.all(H2OUtilities::h2o.second(dat[,column_name]) == 0 |
                   is.na(H2OUtilities::h2o.second(dat[,column_name])))
}

#' @importFrom h2o is.h2o
#' @importFrom H2OUtilities h2o.format_date
#' @importFrom fasttime fastPOSIXct
#' @export
as.data.frame.H2OFrame <- function(x, ...) {
  if(!h2o::is.h2o(x)) return(h2o:::as.data.frame.H2OFrame(x, ...))

  # convert time columns
  schema <- h2o_schema(x)
  time_cols <- names(schema)[schema == "time"]
  date_cols <- NULL
  if(length(time_cols) > 0) {
    # check times for dates
    date_cols <- sapply(time_cols, h2o_column_is_date, dat = x)
    date_cols <- names(date_cols)[date_cols]
    time_cols <- setdiff(time_cols, date_cols)

    # correct time specifications to be strings instead of integers
    # use data.table::fread ISO format, with dateTtimeZ
    for(time_col in time_cols) {
      x[, time_col] <- H2OUtilities::h2o.format_date(x[, time_col], format = "%Y-%m-%dT%H:%M:%SZ")
    }

    for(date_col in date_cols) {
      x[, date_col] <- H2OUtilities::h2o.format_date(x[, date_col], format = "%Y-%m-%d")
    }
  }

  out <- h2o:::as.data.frame.H2OFrame(x, ...)

  time_date_cols <- union(time_cols, date_cols)
  if(length(time_date_cols) > 0) {
    out <- out %>%
      # dplyr::mutate_at(date_cols, ~ as.Date(., tz = "UTC")) %>%
      # dplyr::mutate_at(time_cols, ~ as.POSIXct(., tz = "UTC"))
      dplyr::mutate_at(time_date_cols, ~ fasttime::fastPOSIXct(.))

    if(length(date_cols) > 0) {
      out <- out %>% dplyr::mutate_at(date_cols, as.Date)
    }
  }

  # apparent bug in h2o returns a character vector data frame if the data frame has only one column
  # revert to correct schema
  if(ncol(out) == 1 && !(schema[[1]] %in% c("time", "string"))) {
    if(schema[[1]] == "int") out[[1]] <- as.integer(out[[1]])
    if(schema[[1]] == "real") out[[1]] <- as.numeric(out[[1]])
    if(schema[[1]] == "enum") out[[1]] <- factor(out[[1]], levels = h2o::h2o.levels(x, 1))
  }

  if(getOption("h2oplyr.translate_booleans") == "int") {
    bool_cols <- names(schema)[schema == "int"]
    if(length(bool_cols) > 0) {
      bool_cols <- bool_cols[sapply(out[, bool_cols, drop = FALSE], function(vec) all(na.omit(vec) %in% c(0, 1)))]
    }
    if(length(bool_cols) > 0) {
      out <- out %>%
        dplyr::mutate_at(bool_cols, as.logical)
    }

  } else if(getOption("h2oplyr.translate_booleans") == "enum") {
    bool_cols <- names(schema)[schema == "enum"]
    if(length(bool_cols) > 0) {
      bool_cols <- bool_cols[sapply(out[, bool_cols, drop = FALSE], function(vec) all(na.omit(tolower(vec)) %in% c("true", "false")))]
    }
    if(length(bool_cols) > 0) {
      out <- out %>%
        dplyr::mutate_at(bool_cols, as.character) %>%
        dplyr::mutate_at(bool_cols, as.logical)
    }
  }

  out
}


#' @importFrom h2o colnames h2o.group_by
#' @importFrom rlang eval_tidy call2
eval_grouping <- function(colsub_expr, eval_env, keyby_expr) {
  # message(sprintf("evaluating columns: %s\n        with keyby: %s",
  #                 paste(as.character(colsub_expr), collapse = " "),
  #                 paste(as.character(keyby_expr), collapse = " ")))
  data_mask <- with(eval_env, h2o::colnames(data)) %>% rlang::set_names()

  # according to data.table, by can be:
  # • single unquoted name: by = x
  # • list of expressions with column names: by = .(x = x > 0, y)
  # • single character string: by = "x,y,z"
  # • character vector: by = c("x", "y")
  # • startcol:endcol: by = x:z
  # for now, check for all but character string. Accept .() for h2oplyr but no expressions.

  root <- if (is.call(keyby_expr)) as.character(keyby_expr[[1L]])[1L] else ""
  if(root == "list") {
    stopifnot(sapply(keyby_expr, class) == "name")
    keyby_value <- rlang::eval_tidy(keyby_expr, data = data_mask, env = eval_env) %>% as.character()
  } else if(root == ":") {
    stopifnot(length(keyby_expr) == 3,
              is.name(keyby_expr[[2]]),
              is.name(keyby_expr[[3]]))

    start_col <- as.character(keyby_expr[[2]])
    end_col <- as.character(keyby_expr[[3]])
    start_i <- which(data_mask == start_col)
    end_i <- which(data_mask == end_col)
    stopifnot(length(start_i) == 1,
              length(end_i) == 1,
              end_i >= start_i)
    keyby_value <- data_mask[start_i:end_i] %>% unname()
  } else {
    keyby_value <- rlang::eval_tidy(keyby_expr, data = data_mask, env = eval_env)
  }

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

  root <- if(is.call(colsub_expr)) as.character(colsub_expr[[1L]])[1L] else ""

  if(root == "list") {
    stopifnot(length(colsub_expr) > 1)

    # each call is a separate aggregation argument
    stopifnot(sapply(colsub_expr[-1], is.call))
    args <- as.list(colsub_expr[-1])

  } else if(root == "") {
    stop("Keyby: An aggregate function is required.")

  } else if(root == ":=") {
    # := indicates a mutate function, such that the underlying non-subsetted frame is modified using subset data
    # to accomplish this with h2o, need to create a separate frame to analyze the subset and then join them
    colsub_expr[[1]] <- as.name(".")
    args <- list(data = eval_env$data, col = colsub_expr, by = keyby_expr)

    subdata <- do.call(h2oplyr:::`[.H2OFrame`, args)
    newcols <- setdiff(colnames(subdata), keyby_value)
    cols_to_keep <- setdiff(data_mask, newcols)

    assign("data",
           value = h2o::h2o.merge(x = h2o:::`[.H2OFrame`(data = eval_env$data, row = 1:nrow(eval_env$data), col = cols_to_keep), # throws error if row not specified
                                  y = subdata,
                                  by = keyby_value,
                                  all.x = TRUE),
           envir = eval_env)

    return(eval_env)

  } else if(root == "[" && as.character(colsub_expr[[2]]) == ".SD") {
    # .SD indicates a filter on a subset.
    # e.g., .SD[mpg < mean(mpg)] in by_cyl %>% filter(mpg < mean(mpg))
    # or .SD[mpg < mean(mpg), .(hp = mean(hp))]
    # treat similarly to :=
    # here, each subset should be filtered separately and then combined

    # first, handle the row filter
    stopifnot(length(colsub_expr) == 3 | length(colsub_expr) == 4)
    sd_row <- colsub_expr[[3]]

    parts <- sublogicals_fn(sd_row)
    parts <- parts[sapply(parts, is.call)]
    stopifnot(length(parts) > 0)

    col_call <- as.call(c(list(as.name(":=")), parts)) # add the filtered row(s) results
    args <- list(data = eval_env$data, col = col_call, by = keyby_expr)
    joined_data <- do.call(h2oplyr:::`[.H2OFrame`, args)


    # modify the aggregation calls to be the names of the newcols
    # each new column is the function_column, like mean_mpg
    for(i in seq_along(parts)) {
      new_colsub <- substitute_call_element(sd_row,
                                            element = parts[[i]],
                                            replacement = as.name(paste(as.character(parts[[i]]), collapse = "_")))
    }

    # run the selection on the joined data, using the new columns, and then remove the new columns
    args <- list(data = joined_data, row = new_colsub, col = data_mask %>% unname)
    assign("data",
           value = do.call(h2oplyr:::`[.H2OFrame`, args),
           env = eval_env)

    # if a column argument provided, apply it to the filtered data
    if(length(colsub_expr) == 4) {
      sd_col <- colsub_expr[[4]]
      args <- list(data = eval_env$data, col = sd_col, keyby = keyby_value)
      assign("data",
             value = do.call(h2oplyr:::`[.H2OFrame`, args),
             env = eval_env)
    }

    return(eval_env)

  } else {
    args <- as.list(colsub_expr)
  }

  # switch out count for nrow
  args <- lapply(args, function(arg) {
    stopifnot(length(arg) == 2,
              is.name(arg[[1]]),
              is.name(arg[[2]])
    ) # all are of form mean(col)
    if(as.character(arg[[1]]) == "count") arg[[1]] <- as.name("nrow")
    arg
  })

  grouping_call <- do.call(rlang::call2,
                           args = c(list(data = as.name("data"),
                                         .fn = "h2o.group_by",
                                         by = keyby_value,
                                         .ns = "h2o"),
                                    args %>% unname()),
                           quote = TRUE)

  assign("data",
         value = rlang::eval_tidy(grouping_call, data = data_mask, env = eval_env),
         envir = eval_env)

  new_names <- names(colsub_expr)[-1]
  if(!is.null(new_names) && any(new_names != "")) {
    # rename aggregated columns
    stopifnot(length(new_names) == length(args))
    # add keyed columns, which are returned as normal; only change the end columns
    data_mask <- with(eval_env, h2o::colnames(data)) %>% rlang::set_names()
    new_names <- c(data_mask[1:length(keyby_value)], new_names) %>% unname()

    # check for blank names
    blank_idx <- new_names == ""
    new_names[blank_idx] <- data_mask[blank_idx]

    h2o::colnames(eval_env$data) <- new_names
  }

  return(eval_env)
}

# modify_h2o_names_in_environment <- function(e, new_names) {
#   h2o::colnames(e$data) <- new_names
#   invisible()
# }

# select_h2o_columns_in_environment <- function(e, colvars) {
#   e$data <- h2o:::`[.H2OFrame`(e$data, col = colvars, drop = FALSE)
#   invisible()
# }
#
# modify_h2o_data_in_environment <- function(e, row, col, value, drop = FALSE) {
#   e$data <- h2o:::`[<-.H2OFrame`(e$data, row = row, col = col, drop = drop, value = value)
#   invisible()
# }

#' @importFrom h2o colnames
#' @importFrom rlang set_names eval_tidy
eval_columns <- function(colsub_expr, eval_env) {
  # message("evaluating columns: ", as.character(colsub_expr))
  data_mask <- with(eval_env, h2o::colnames(data)) %>% rlang::set_names()

  root <- if(is.call(colsub_expr)) as.character(colsub_expr[[1L]])[1L] else ""
  if(root == "list") {
    stopifnot(length(colsub_expr) > 1)

    # check for calls, if so, apply the calls in turn
    for(i in seq_along(colsub_expr)) {
      if(i == 1) next; # first is the list; ignore

      if(expr_is_dot_data(colsub_expr[[i]]) | expr_is_dot_env(colsub_expr[[i]])) {
       # leave it for evaluation at the end

      } else if(is.call(colsub_expr[[i]])) {
        # apply the call to the H2OFrame
        colmod_expr <- sub_variable_names(colsub_expr[[i]], eval_env) %>% sub_h2o_data_names(data_mask = data_mask)
        value <- rlang::eval_tidy(colmod_expr, data = data_mask, env = eval_env)

        new_col_name = names(colsub_expr)[[i]]
        if(is.null(new_col_name) || new_col_name == "") {
          new_col_name <- paste0("V", i - 1)
        }

        assign("data",
               value = h2o:::`[<-.H2OFrame`(get("data", envir = eval_env), col = new_col_name, drop = FALSE, value = value),
               envir = eval_env)

        # replace the colsub entry with the new name, so that it is included in the final result
        colsub_expr[[i]] <- as.name(new_col_name)

        # update data mask
        data_mask <- with(eval_env, h2o::colnames(data)) %>% rlang::set_names()

      } else if(!is.name(colsub_expr[[i]])) {
        # integer or some other scalar
        col_mod <- colsub_expr[[i]]
        new_col_name = names(colsub_expr)[[i]]
        if(is.null(new_col_name) || new_col_name == "") {
          new_col_name <- paste0("V", i - 1)
        }

        # fix error when modifying multiple items
        if(length(col_mod) > 1 && !h2o::is.h2o(col_mod)) col_mod <- as.h2o(col_mod)
        assign("data",
               value = h2o:::`[<-.H2OFrame`(get("data", envir = eval_env), col = new_col_name, drop = FALSE, value = col_mod),
               envir = eval_env)

        # replace the colsub entry with the new name, so that it is included in the final result
        colsub_expr[[i]] <- as.name(new_col_name)

        # update data mask
        data_mask <- with(eval_env, h2o::colnames(data)) %>% rlang::set_names()
      }
    }

    colsub_expr[[1]] <- as.name("c") # remaining items in the list should be evaluated as a named vector
    colvars <- rlang::eval_tidy(colsub_expr, data = data_mask, env = eval_env)

    # colvars <- as.character(colsub_expr[-1])
  } else if(root == ":=") {
    # internal mutate of columns.
    # names are the new columns
    # the expression is the new value for each new (or existing) column
    # the returned data set should be the full set with the mutations
    # arguably, should re-assign to the original name using h2o.assign, but that is probably not necessary b/c h2o makes internal temp copies
    # instead, user can use compute() for re-assignment
    # differing forms:
    # simple: cyl := 8 or mpg := mean(mpg) --> `:=`(cyl, 8) or `:=`(mpg, mean(mpg)), length 3, not named
    # functional: `:=`(cyl2 = cyl * 2, cyl4 = cyl2 * 2, .(cyl2, cyl4)) --> length varies, named
    # h2oplyr using {}: `:=`(c("cyl2", "cyl4"), {
    #   cyl2 <- cyl * 2
    #   cyl4 <- cyl2 * 2
    #   .(cyl2, cyl4)
    # })  --> length 3 call with `:=`, c(), and {}
    stopifnot(length(colsub_expr) >= 2)

    if(is.null(names(colsub_expr))) {
      expr_name <- try(rlang::eval_tidy(colsub_expr[[2]], data = data_mask, env = eval_env), silent = TRUE)
      if(inherits(expr_name, "try-error")) expr_name <- as.character(colsub_expr[[2]])
      expr_name <- c("", expr_name)
      colsub_expr <- colsub_expr[-2]
    } else {
      expr_name <- names(colsub_expr)
    }

    if(is.call(colsub_expr[[2]]) && colsub_expr[[2]][[1]] == "{") {
      old_sub <- colsub_expr[[2]]

      for(i in seq_along(old_sub)) {
        if(i == 1) next;

        # if <- or =, simplify
        if(old_sub[[i]][[1]] == "=" || old_sub[[i]][[1]] == "<-") {
          # change cyl2 <- cyl * 2 to cyl * 2
          colsub_expr[[i]] <- old_sub[[i]][[3]]
        } else {
          colsub_expr[[i]] <- old_sub[[i]]
        }
      }
    }

    for(i in seq_along(colsub_expr)) {
      if(i == 1) next; # function

      if(length(colsub_expr[[i]]) > 0 && colsub_expr[[i]][[1]] == "list") next; # unclear why passing the unnamed list is necessary;
      # something specific to DT b/c DT does not apply the modifications to columns correctly otherwise.

      col_mod <- sub_variable_names(colsub_expr[[i]], eval_env) %>% sub_h2o_data_names(data_mask = data_mask)
      mutated_value <- rlang::eval_tidy(col_mod, data = data_mask, env = eval_env)

      assign("data",
             value = `[<-.H2OFrame`(get("data", envir = eval_env), col = expr_name[[i]], drop = FALSE, value = mutated_value),
             envir = eval_env)

      # update data mask to account for the possible additional column
      data_mask <- with(eval_env, h2o::colnames(data)) %>% rlang::set_names()
    }

    colvars <- data_mask

  } else {
    colvars <- rlang::eval_tidy(colsub_expr, data = data_mask, env = eval_env)
  }

  return(list(eval_env = eval_env, colvars = colvars))
}


#' @importFrom h2o colnames
#' @importFrom rlang eval_tidy set_names
eval_rows <- function(rowsub_expr, eval_env) {
  if(class(rowsub_expr[[1]]) == "name" & as.character(rowsub_expr[[1]]) == "order") {
    stopifnot(length(rowsub_expr) > 1)
    # translate, e.g., order(mpg, cyl, desc(hp)), into h2o.arrange
    args <- c(list(x = data),
              as.list(as.character(rowsub_expr[2:length(rowsub_expr)])))
    # data <- do.call(h2o.arrange, args)
    row_value <- do.call(h2o.arrange, args)
    # assign("data",
    #        value = do.call(h2o.arrange, args),
    #        envir = eval_env)
  } else {
    data_mask <- with(eval_env, h2o::colnames(data)) %>% rlang::set_names()
    rowsub_expr <- sub_variable_names(rowsub_expr, eval_env) %>% sub_h2o_data_names(data_mask = data_mask)
    row_value <- rlang::eval_tidy(rowsub_expr, data = data_mask, env = eval_env)
    # assign("data",
    #        value = h2o:::`[.H2OFrame`(get("data", envir = eval_env), row = row_value, drop = drop),
    #        envir = eval_env)
  }

  # return(eval_env)
  return(row_value)
}


h2o_function_replacement <- function(fn_name) {
  switch(as.character(fn_name),
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
         as.character(fn_name))
}

# walk through sub expression
# replace aggregation function names like "mean" with "h2o.mean"
# replace references to data columns with data[[columns]]
#' @importFrom rlang call2
sub_h2o_data_names <- function(sub, data_mask) {

  if(length(sub) == 1) {
    if(class(sub) == "character" && sub %in% data_mask) {
      sub <- call("[[", as.name("data"), sub)
    } else if(class(sub) == "name" && as.character(sub) %in% data_mask) {
      sub <- call("[[", as.name("data"), sub)
    }
    return(sub)
  }

  if(class(sub[[1]]) == "name") {
    sub[[1]] <- as.name(h2o_function_replacement(sub[[1]]))
  }


  for(i in seq_along(sub)) {
    if(length(sub[[i]]) > 1) {
      # run through children of sub[[i]]
      sub[[i]] <- sub_h2o_data_names(sub[[i]], data_mask = data_mask)
    } else if(class(sub[[i]]) == "character" && sub[[i]] %in% data_mask) {
      sub[[i]] <- rlang::call2("[[", as.name("data"), sub[[i]])
    } else if(class(sub) == "name" && as.character(sub[[i]]) %in% data_mask) {
      sub[[i]] <- call("[[", as.name("data"), sub[[i]])
    }
  }
  return(sub)
}

# test for .env$var
expr_is_dot_env <- function(expr) {
  if(length(expr) != 3) return(FALSE)
  all(sapply(expr, class) == "name") && expr[[1]] == "$" && expr[[2]] == ".env"
}

# test for .data$var
expr_is_dot_data <- function(expr) {
  if(length(expr) != 3) return(FALSE)
  all(sapply(expr, class) == "name") && expr[[1]] == "$" && expr[[2]] == ".data"
}

#' @importFrom h2o colnames
#' @importFrom rlang set_names eval_tidy
sub_variable_names <- function(sub, eval_env) {
  if(length(sub) == 0) return(sub) # usually setting NULL value

  data_mask <- with(eval_env, h2o::colnames(data)) %>% rlang::set_names()

  if(length(sub) == 1) {
    if(class(sub) == "name") sub <- rlang::eval_tidy(sub, data = data_mask, env = eval_env)
    return(sub)
  }

  if(expr_is_dot_data(sub) | expr_is_dot_env(sub)) {
    sub <- rlang::eval_tidy(sub, data = data_mask, env = eval_env)
  } else {
    # evaluate any names
    for(i in seq_along(sub)) {
      if(i == 1) next; # first is a function name; skip

      if(length(sub[[i]]) > 1) {
        sub[[i]] <- sub_variable_names(sub[[i]], eval_env = eval_env)
      } else if(class(sub[[i]]) == "name") {
        sub[[i]] <- rlang::eval_tidy(sub[[i]], data = data_mask, env = eval_env)
      }
    }
  }

  return(sub)
}

sublogicals_fn <- function(sub) {
 unique(unlist(sublogicals(sub)))
}

sublogicals <- function(sub) {
  # print(sub)

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
      # if(is.null(names(sub[[i]])) || names(sub)[[i]] == "") names(sub)[[i]] <- "N"
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

# fn_col <- function(var) h2oplyr:::replace_dot_alias(rlang::enexpr(var))
# fn_row <- function(var) rlang::enexpr(var)

# typical h2oplyr mutate
# colsub <- fn_col(`:=`(c("cyl2", "cyl4"), {
#   cyl2 <- cyl * 2
#   cyl4 <- cyl2 * 2
#   .(cyl2, cyl4)
# }))

# copy(dth2o_2)[, `:=`(cyl2 = cyl * 2)]
# colsub <- fn_col(`:=`(cyl2 = cyl * 2))

# typical h2oplyr transmute
# `_DT1`[, .(cyl2 = cyl * 2, vs2 = vs * 2)]
# colsub <- fn_col(.(cyl2 = cyl * 2, vs2 = vs * 2))

# h2oplyr summarize
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


# h2oplyr calls
# > mtcars2 %>% select(mpg:cyl)
# Source: local data table [?? x 2]
# Call:   `_DT1`[, .(mpg, cyl)]

# > mtcars2 %>% select(x = mpg, y = cyl)
# Source: local data table [?? x 2]
# Call:   `_DT1`[, .(x = mpg, y = cyl)]

# > mtcars2 %>% filter(cyl == 4) %>% select(mpg)
# Source: local data table [?? x 1]
# Call:   `_DT1`[cyl == 4, .(mpg)]

# > mtcars2 %>% select(mpg, cyl) %>% filter(cyl == 4)
# Source: local data table [?? x 2]
# Call:   `_DT1`[, .(mpg, cyl)][cyl == 4]

# > mtcars2 %>% mutate(cyl2 = cyl * 2, cyl4 = cyl2 * 2)
# Source: local data table [?? x 13]
# Call:   copy(`_DT1`)[, `:=`(c("cyl2", "cyl4"), {
#   cyl2 <- cyl * 2
#   cyl4 <- cyl2 * 2
#   .(cyl2, cyl4)
# })]

# > mtcars2 %>% transmute(cyl2 = cyl * 2, vs2 = vs * 2)
# Source: local data table [?? x 2]
# Call:   `_DT1`[, .(cyl2 = cyl * 2, vs2 = vs * 2)]

# > mtcars2 %>% filter(cyl == 8) %>% mutate(cyl2 = cyl * 2)
# Source: local data table [?? x 12]
# Call:   `_DT1`[cyl == 8][, `:=`(cyl2 = cyl * 2)]

# > by_cyl %>% summarise(mpg = mean(mpg))
# Source: local data table [?? x 2]
# Call:   `_DT1`[, .(mpg = mean(mpg)), keyby = .(cyl)]

# > by_cyl %>% mutate(mpg = mean(mpg))
# Source: local data table [?? x 11]
# Call:   copy(`_DT1`)[, `:=`(mpg = mean(mpg)), keyby = .(cyl)]

# > by_cyl %>% filter(mpg < mean(mpg)) %>% summarise(hp = mean(hp))
# Source: local data table [?? x 2]
# Call:   `_DT1`[, .SD[mpg < mean(mpg), .(hp = mean(hp))], keyby = .(cyl)]

# sampling
# sample_frac: Call:   dth2o_1[sample(.N, .N * 0.5)]
# sample_n: Call:   dth2o_1[sample(.N, 10)]

# fn1 <- function(data, arg) {
#   data2 <- data
#
#   vars <- c("cyl", "mpg")
#
#   fn2(data2, arg)
# }
#
# fn2 <- function(data, arg) {
#   arg_sub <- h2oplyr:::replace_dot_alias(rlang::enquo(arg))
#   arg_expr <- rlang::get_expr(arg_sub)
#   print(as.character(arg_expr))
#
#   data_mask <- rlang::as_data_mask(h2o::colnames(data) %>% rlang::set_names())
#
#   # out <- rlang::eval_tidy(arg_expr, data = data_mask)
#
#   # out <- rlang::eval_tidy(arg_expr, data = h2o::colnames(data) %>% rlang::set_names())
#
#   arg_expr <- h2oplyr:::subdatanames(arg_expr, data_name = "data")
#   parent_env <- parent.frame()
#   env <- new.env(parent = parent_env)
#   assign("data", data, envir = env)
#
#   # arg_renamed <- rlang::as_quosure(arg_expr, env = env)
#   # out <- rlang::eval_tidy(arg_renamed)
#
#   out <- rlang::eval_tidy(arg_expr, data = data_mask, env = env)
#
#   return(out)
# }
# fn2(mtcars.hex,  mean(hp) + mean(cyl))
# fn2(mtcars.hex, .(hp, cyl))
# fn1(mtcars.hex,  mean(hp) + mean(cyl))
