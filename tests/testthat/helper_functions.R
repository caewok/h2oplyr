setup_test_data_frame <- function() {
  time_char <- c("2017-12-31 23:59:59",
                 "2018-01-01 00:00:00",
                 "2018-01-01 00:00:01",
                 "2018-01-01 23:59:59",
                 "2018-02-28 10:00:00",
                 "2018-02-28 09:59:59",
                 "2018-03-01 01:01:01",
                 "2018-03-31 23:50:50",
                 "2018-11-30 12:00:00",
                 "2018-12-01 00:00:00")

  N <- length(time_char)

  tbl <- data.frame(char = LETTERS[1:N],
                    factor = as.factor(LETTERS[1:N]),
                    int = 1:N,
                    num = seq(-1.5, -1.5+N-1, by = 1.0),
                    bool = sample(c(TRUE, FALSE), size = N, replace = TRUE),
                    date = seq.Date(from = as.Date("2018-01-01"), length.out = N, by = "month"),
                    num_nan = rep(NaN, times = N),
                    time_Pacific = as.POSIXct(time_char, tz = "America/Los_Angeles"),
                    time_UTC = as.POSIXct(time_char, tz = "UTC"),
                    time_Eastern = as.POSIXct(time_char, tz = "America/New_York"),
                    blanks = rep("", times = N), # should be treated as NA according to .csv spec
                    na_quotes = rep("NA", times = N),
                    stringsAsFactors = FALSE)

  # add NAs at beginning, end, middle to catch edge cases
  rbind(NA, tbl, NA, tbl, NA)

}
#
# fn <- function(dat) {
#   char_matches <- c("int" = 7, "num" = 6)
#   for(i in seq_along(char_matches)) {
#     format_value <- char_matches[[i]]
#     col_name <- names(char_matches)[[i]]
#
#     if(format_value == 7)
#       dat[, col_name] <- format_value
#
#     # if(grepl(format_value, x = format)) {
#     #   split_time.hex[, col_name] <- h2o::h2o.ascharacter(format_value)
#     # }
#   }
#   return(dat)
# }
