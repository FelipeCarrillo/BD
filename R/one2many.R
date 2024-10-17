
#' @title Deaggregate rows with rownames as integers instead of decimals
#'
#' @param data A dataset with multiple rows
#' @param var The variable that the user want to split in multiple rows
#' @return An aggregated dataset larger than the original
#' @import tidyverse tidyr magrittr ggplot2 dplyr

#' @name one2many
#' @examples
#' library(tidyverse)
#' library(BD)
#' data(RBsalmon)
#' RBsalmon <- arrange(RBsalmon, SampleDate)
#' RBsalmon <- RBsalmon[1:20,]
#' RBsalmon <- data.frame(RBsalmon)
#' RBsalmon

#' one2many(RBsalmon, RBsalmon$Count)

#' @export
library(tidyr)
library(tidyverse)
one2many <- function(data, var,...){
data %>% mutate(rows = row_number()) |>
uncount({{var}},...)
}
