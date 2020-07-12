# renv::activate()


# h2o package installation
# https://www.h2o.ai/download/#h2o
# prereqs:
renv::install(c("RCurl", "jsonlite"))

# 3.30.06
# http://h2o-release.s3.amazonaws.com/h2o/rel-zahradnik/6/index.html
h2o_release_name <- "zahradnik"
h2o_patch_number <- 6
h2o_repository <- sprintf("http://h2o-release.s3.amazonaws.com/h2o/%s/%s/R",
                          h2o_release_name,
                          h2o_patch_number)
install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-yule/1/R")

