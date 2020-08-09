# fn_col <- function(var) dtplyr:::replace_dot_alias(rlang::enexpr(var))
# fn_row <- function(var) rlang::enexpr(var)
# parent_env <- parent.frame()
# eval_env <- new.env(parent = parent_env)
# assign("data", mtcars.hex, envir = eval_env)

Databases::GetH2OConnection()
mtcars.hex <- as.h2o(mtcars, "mtcars")
mtcars_local <- as.data.frame(mtcars.hex)

var <- "hp"
vars <- c("cyl", "mpg")
named_vars <- c(x = "cyl", y = "mpg", "hp")

# selection
mtcars.hex[, .(cyl, hp)]
mtcars.hex[, .(cyl, hp, .env$var)]
mtcars.hex[, .(.env$vars)]
#mtcars.hex[, .(!!!vars)]
mtcars.hex[, vars]
mtcars.hex[, named_vars]
mtcars.hex[, .env$vars]
mtcars.hex[, .data$cyl]

mtcars.hex[, .(x = cyl, y = hp)]
mtcars.hex[, .(x = cyl, y = hp, mpg)]

# transmute
mtcars.hex[, .(cyl2 = cyl * 2, vs2 = vs * 2)]
mtcars.hex[, .(cyl2 = cyl * 2, cyl4 = cyl2 * 2)]
mtcars.hex[, .(cyl2 = 2, vs2 = "a")]

mtcars.hex[, .(cyl2 = .env$var * 2, vs2 = vs * 2)]

# mutate
mtcars.hex[, `:=`(cyl2 = cyl * 2)]
mtcars.hex[, `:=`(cyl2 = cyl * 2, cyl4 = cyl2 * 2, vs = vs * 2)]
mtcars.hex[, `:=`(c("cyl2", "cyl4"), {
  cyl2 <- cyl * 2
  cyl4 <- cyl2 * 2
  .(cyl2, cyl4)
})]
mtcars.hex[, cyl := 8]
mtcars.hex[, mpg := mean(mpg)]
mtcars.hex[, am := NULL]

mtcars.hex[, samp := sample(as.vector(cyl, mode = "integer"), size = .N, replace = TRUE)] # cannot just take cyl b/c sample will attempt to re-order it
mtcars.hex[, samp := sample(.env$LETTERS, size = .N, replace = TRUE)]
mtcars.hex[, char := sample(.env$LETTERS, size = nrow(.env$mtcars.hex), replace = TRUE)]

mtcars.hex[, `:=`(cyl = 8, mpg = mean(mpg), am = NULL)]

# summarize
mtcars.hex[, .(a = mean(mpg))]
mtcars.hex[, .(a = median(mpg))]
mtcars.hex[, .(n = .N)]
mtcars.hex[, .(.N)]

# filter
mtcars.hex[cyl == 4, ]
mtcars.hex[cyl == 4 & hp > 100,]

mtcars.hex["cyl" == 4, ]
mtcars.hex[cyl == 4 & var > 100, ]
mtcars.hex[.env$var > 100,]

# filter and mutate
mtcars.hex[cyl == 4, mpg := mean(mpg)]

tmp <- mtcars.hex[, char := "A"]
tmp[cyl == 4 & char == "A",]

mtcars.hex[, `:=`(c("cyl2", "cyl4"), {
  cyl2 <- cyl * 2
  cyl4 <- cyl2 * 2
  .(cyl2, cyl4)
})]

# group
mtcars.hex[, .(mpg = mean(mpg)), keyby = .(cyl)]
mtcars.hex[, .(mpg = mean(mpg)), keyby = .(cyl, gear)]
mtcars.hex[, .(mpg = mean(mpg)), keyby = "cyl"]
mtcars.hex[, .(mpg = mean(mpg)), keyby = c("cyl", "gear")]

mtcars.hex[, `:=`(mpg = mean(mpg)), keyby = .(cyl)]

mtcars.hex[, .SD[mpg < mean(mpg), .(hp = mean(hp))], keyby = .(cyl)]
