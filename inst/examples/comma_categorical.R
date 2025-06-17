#Function to format categorical variables with commas
comma_categorical <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
    sprintf("%s (%s%%)",
            prettyNum(FREQ, big.mark = ","),
            format(round(PCT, 1), nsmall = 1)))))
}