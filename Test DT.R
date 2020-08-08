DT = data.table(a = LETTERS[c(3L,1:3)], b = 4:7)
DT[, c := 8]
DT[, d := 9L]

DT[, `:=`(c("b2", "b4"), {
  b2 = b * 2
  b4 = b2 * 2
})]

DT[, `:=`(c("b2", "b4"), {
  b2 <- b * 2
  b4 <- b2 * 2
})]

DT[, `:=`(c("b2", "b4"), {
  b2 <- b * 2
  b4 <- b2 * 2
  .(b2, b4)
 })]

DT[, .(b2, b4)]

rm(DT)
