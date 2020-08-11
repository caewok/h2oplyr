testthat::setup({
  # Databases::GetH2OConnection()
  h2o::h2o.init()
  h2o::h2o.no_progress()
  h2o::h2o.removeAll()
})
