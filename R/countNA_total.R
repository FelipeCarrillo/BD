#' @title  Sum total NAs in dataset
#'
#' @description Counts total NAs in dataframe
#'
#' @param dataframe A dataset with NAs
#'
#' @return dataframe with total NAs
#'
#' @examples
#' dat <- head(airquality, 12)
#' dat <- as.data.frame(dat)
#' countNA_total(dat)
#'
#' @export
countNA_total <- function(dataframe) {
sum(colSums(is.na(dataframe)))
}
