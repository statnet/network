td <- data.frame(
  lettres = letters[1:10],
  values = 1:10,
  stringsAsFactors = FALSE
)

# Correct output
correct <-
  structure(
    c(
      "Length:10         ",
      "Class :character  ",
      "Mode  :character  ",
      NA,
      NA,
      NA,
      "Min.   : 1.00  ",
      "1st Qu.: 3.25  ",
      "Median : 5.50  ",
      "Mean   : 5.50  ",
      "3rd Qu.: 7.75  ",
      "Max.   :10.00  "
    ),
    .Dim = c(6L,  2L),
    .Dimnames = list(c("", "", "", "", "", ""), c("  lettres",  "    values")),
    class = "table"
  )


actual <- summary(td)


expect_identical(actual, correct)
