#' @title Count NAs in dataset

#' @description Counts NAs

#' @param  dataframe NAs by column

#' @return dataframe adding NAs by column

#' @examples
#' dat <- head(airquality, 12)
#' dat <- as.data.frame(dat)
#' countNA(dat)

#' @export
countNA <- function(dataframe) {
colSums(is.na(dataframe))
}

