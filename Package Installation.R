# renv::activate()
renv::install("tidyselect")
renv::install("rmarkdown")
renv::install("knitr")
renv::install("data.table")
renv::install("dplyr")
renv::install("roxygen2")
renv::install("bit64")
renv::install("testthat")

# Sys.setenv(AUTH_HEADER = "Authorization: Basic caewok:$apr1$ALMkVuFN$f4RXCnMoqFW6P18mgTAbm/")

# h2o package installation
# https://www.h2o.ai/download/#h2o
# prereqs:
renv::install(c("RCurl", "jsonlite"))

# 3.30.06
# http://h2o-release.s3.amazonaws.com/h2o/rel-zahradnik/6/index.html
h2o_release_name <- "rel-zahradnik"
h2o_patch_number <- 4
h2o_repository <- sprintf("http://h2o-release.s3.amazonaws.com/h2o/%s/%s/R",
                          h2o_release_name,
                          h2o_patch_number)
install.packages("h2o", type = "source", repos = h2o_repository)

# My packages
renv::install("H2OUtilities")
renv::install("Databases")
renv::install("qs")

renv::snapshot()
