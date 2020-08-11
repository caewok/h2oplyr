
# head and tail -------------------------------------------------------------

test_that("simple calls generate expected results", {
  dt <- lazy_dt(data.frame(x = 1), "DT")

  expect_equal(
    dt %>% head() %>% show_query(),
    expr(head(DT, n = 6L))
  )
  expect_equal(
    dt %>% tail() %>% show_query(),
    expr(tail(DT, n = 6L))
  )
})

test_that("vars set correctly", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))
  expect_equal(dt %>% head() %>% .$vars, c("x", "y"))
})


# rename ------------------------------------------------------------------

test_that("simple calls generate expected translations", {
  dt <- lazy_dt(data.frame(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% rename(b = y) %>% show_query(),
    expr(setnames(copy(DT), "y", "b"))
  )
})

test_that("vars set correctly", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))
  expect_equal(dt %>% rename(a = x) %>% .$vars, c("a", "y"))
})

test_that("empty rename returns original", {
  dt <- data.frame(x = 1, y = 1, z = 1)
  lz <- lazy_dt(dt, "DT")

  expect_equal(lz %>% rename() %>% show_query(), expr(DT))
})

test_that("renames grouping vars", {
  dt <- lazy_dt(data.frame(x = 1, y = 1, z = 1))
  gt <- group_by(dt, x)
  expect_equal(rename(gt, y = x)$groups, "y")
})

# distinct ----------------------------------------------------------------

test_that("no input uses all variables", {
  dt <- lazy_dt(data.frame(x = c(1, 1), y = c(1, 2)), "dt")

  expect_equal(
    dt %>% distinct() %>% show_query(),
    expr(h2o.drop_duplicates(dt))
  )

  expect_equal(dt %>% distinct() %>% .$vars, c("x", "y"))
})

test_that("uses supplied variables", {
  dt <- lazy_dt(data.frame(x = c(1, 1), y = c(1, 2)), "dt")

  expect_equal(
    dt %>% distinct(y) %>% show_query(),
    expr(h2o.drop_duplicates(dt[, .(y)], columns = "y"))
  )
  expect_equal(dt %>% distinct(y) %>% .$vars, "y")

  expect_equal(
    dt %>% group_by(x) %>% distinct(y) %>% show_query(),
    expr(h2o.drop_duplicates(dt[, .(x, y)], columns = "y"))
  )
})

test_that("doesn't duplicate variables", {
  dt <- lazy_dt(data.frame(x = c(1, 1), y = c(1, 2)), "dt")

  expect_equal(
    dt %>% distinct(x, x) %>% show_query(),
    expr(h2o.drop_duplicates(dt[, .(x)], columns = "x"))
  )

  expect_equal(dt %>% distinct(x, x) %>% .$vars, "x")

  expect_equal(
    dt %>% group_by(x) %>% distinct(x) %>% show_query(),
    expr(h2o.drop_duplicates(dt[, .(x)], columns = "x"))
  )
})
test_that("keeps all variables if requested", {
  dt <- lazy_dt(data.frame(x = 1, y = 1, z = 1), "dt")

  expect_equal(
    dt %>% distinct(y, .keep_all = TRUE) %>% show_query(),
    expr(h2o.drop_duplicates(dt, columns = "y"))
  )
  expect_equal(dt %>% distinct(y, .keep_all = TRUE) %>% .$vars, c("x", "y", "z"))

  expect_equal(
    dt %>% group_by(x) %>% distinct(y, .keep_all = TRUE) %>% show_query(),
    expr(h2o.drop_duplicates(dt, columns = c("x", "y")))
  )
})

test_that("can compute distinct computed variables", {
  dt <- lazy_dt(data.frame(x = c(1, 1), y = c(1, 2)), "dt")

  expect_equal(
    dt %>% distinct(z = x + y) %>% show_query(),
    expr(h2o.drop_duplicates(dt[, .(z = x + y)], columns = "z"))
  )

  expect_equal(
    dt %>% distinct(z = x + y, .keep_all = TRUE) %>% show_query(),
    expr(h2o.drop_duplicates(copy(dt)[, `:=`(z = x + y)], columns = "z"))
  )
})


# unique ------------------------------------------------------------------
# probably don't want unique as an alias for distinct. h2o.unique is a separate function, and unique is not a dplyr function.
# test_that("unique is an alias for distinct", {
#   dt <- lazy_dt(data.frame(x = c(1, 1)))
#   expect_equal(unique(dt, x), distinct(dt, x))
# })
