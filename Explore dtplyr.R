library(dtplyr)
library(dplyr)

mtcars2 <- lazy_dt(mtcars)
mtcars2

class(mtcars2)
# [1] "dtplyr_step_first" "dtplyr_step"

names(mtcars2)
# [1] "parent"        "vars"          "groups"        "implicit_copy" "needs_copy"    "env"
# [7] "name"

mtcars2$parent # points to a data table
class(mtcars2$parent) # "data.table" "data.frame"

mtcars2$vars
# [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear" "carb"

mtcars2$groups
# character(0)

mtcars2$implicit_copy
# [1] FALSE

mtcars2$needs_copy
# [1] FALSE

mtcars2$env
# <environment: R_GlobalEnv>

mtcars2$name
# `_DT1`

attributes(mtcars2)
# $names
# [1] "parent"        "vars"          "groups"        "implicit_copy" "needs_copy"    "env"
# [7] "name"
#
# $class
# [1] "dtplyr_step_first" "dtplyr_step"


basic_select_mtcars <- mtcars2 %>% select(mpg:cyl)
class(basic_select_mtcars)
# [1] "dtplyr_step_group" "dtplyr_step"

basic_select_mtcars$parent
# Source: local data table [?? x 2]
# Call:   `_DT1`[, .(mpg, cyl)]
#
# mpg   cyl
# <dbl> <dbl>
#   1  21       6
# 2  21       6
# 3  22.8     4
# 4  21.4     6
# 5  18.7     8
# 6  18.1     6

# Use as.data.table()/as.data.frame()/as_tibble() to access results
class(basic_select_mtcars$parent)
# [1] "dtplyr_step_subset" "dtplyr_step"

basic_select_mtcars$vars
# [1] "mpg" "cyl"

basic_select_mtcars$groups
# character(0)

basic_select_mtcars$implicit_copy
# [1] TRUE

basic_select_mtcars$needs_copy
# [1] FALSE

basic_select_mtcars$env
# <environment: R_GlobalEnv>



mtcars2 <- lazy_dt(mtcars)

# In step-first.R:
# lazy_dt(x, name = NULL, immutable = TRUE, key_by = NULL)
#   calls step_first(x, name = name, immutable = immutable, env = caller_env())
#   rlang::caller_env is the environment of the function that called the current environment
#   here, used to locate x

# step_first(x, name = name, immutable = immutable, env = caller_env())
#   simple constructor for a new_step
#   new_step(parent, vars = names(parent), groups = character(), implicit_copy = !immutable,
#            needs_copy = FALSE, name = sym(name), env = env, class = "dtplyr_step_first")
#

# new_step <- function(parent, vars = parent$vars, groups = parent$groups,
#                      implicit_copy = parent$implicit_copy,
#                      needs_copy = parent$needs_copy, env = parent$env,
#                      ...,
#                      class = character())
#   creates a structure to hold these variables

basic_select_mtcars <- mtcars2 %>% select(mpg:cyl)

# In step-subset.R
# select.dtplyr_step(.data, ...)
#   calls vars <- tidyselect::vars_select(.data$vars, ..., .include = .data$groups)
#   always includes grouping vars
#   calls rename_groups
#   calls simplify_names
#   creates j variable used for selection (see data.table j selection) using rlang::call2
#      https://www.datacamp.com/community/tutorials/data-table-r-tutorial
#      .(mpg, cyl)
#      class(j) is "call"
#   calls step_subset_j(.data, vars = new_vars, groups = character(), j = j)
#      attempts to merge when only j, and calls step_subset(parent, vars = parent$vars,
#                                                           groups = parent$groups, i = NULL,
#                                                           j = NULL, on = character()
#      That creates a basic structure with "dtplyr_step_subset" class by calling new_step()
#   calls step_group(out, groups)

out <- basic_select_mtcars %>% collect
# returns "tbl_df"     "tbl"        "data.frame"
# calls collect.dtplyr_step(x, ...)
#   calls as_tibble.dtplyr_step(x, ...), calls as_tibble(dt_eval(x))
# dt_eval(x)
#  env <- as_environment(dt_sources(x), x$env) # dt_sources in step.R
#  dt_sources just moves up the parents to the initial data frame
#    here, chain is:
#       class(x): dtplyr_step_group
#       class(x$parent): dtplyr_step_subset
#       class(x$parent$parent): dtplyr_step_first
#       class(x$parent$parent$parent): data.table
#  dt_sources.dtplyr_step_first <- function(x) {
#    stats::setNames(list(x$parent), as.character(x$name))
#   }
#  tmp <- dtplyr:::dt_sources.dtplyr_step_first(x$parent$parent) # named list of length 1, containing the data frame
#  env <- rlang::as_environment(tmp, x$env) # creates an environment with the names of the tmp list ("_DT1")
#
# add_dt_wrappers(env)
#   adds 3 data.table functions to the environment: copy, setkeyv, setname
# quo <- new_quosure(dt_call(x), env)
#  dt_call(x) moves up the parent chain, identifying if each parent needs a copy
#
#  full_x <- x
#  dt_call.dtplyr_step_group: does nothing
#  dt_call.dtplyr_step_subset: x <- x$parent
#    does nothing if no x$i nor x$j
#    checks for additional steps in x$i and executes those calls, if any
#    calls parent
#  dt_call.dtplyr_step_first:  x <- x$parent
#     makes copy if necessary
#     gets name of the table x$name
#  back to dt_call.dtplyr_step_subset: x <- full_x$parent
#     parent <- `_DT1`
#     constructs a call out <- `_DT1`[, .(mpg, cyl)] using rlang::call2
#
# eval_tidy(quo)
