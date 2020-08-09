# nocov start
# http://r-pkgs.had.co.nz/namespace.html

.onLoad <- function(...) {
  # stopifnot(requireNamespace("h2o")) # need to load first, so h2oplyr can override `[<-.H2OFrame` and `[.H2OFrame`

  register_s3_method("dplyr", "filter", "data.table")

  register_s3_method("dplyr", "filter", "h2oplyr_step")
  register_s3_method("dplyr", "intersect", "h2oplyr_step")
  register_s3_method("dplyr", "setdiff", "h2oplyr_step")
  register_s3_method("dplyr", "union", "h2oplyr_step")

  op <- options()
  toset <- !(names(h2oplyr_default_options) %in% names(op))
  if (any(toset)) options(h2oplyr_default_options[toset])

  invisible()
}

h2oplyr_default_options <- list(
  h2oplyr.translate_booleans = "no" # alternatives "enum" or "int"
)


register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
