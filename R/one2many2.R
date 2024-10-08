
#' @title Deaggregate rows with rownames as integers instead of decimals
#'
#' @param data A dataset with multiple rows
#' @param var The variable that the user want to split in multiple rows
#' @return An aggregated dataset larger than the original
#' @import tidyverse tidyr magrittr ggplot2 dplyr

#' @name one2many2
#' @examples
#' library(tidyverse)
#' library(BD)
#' a <- data.frame(name= c("Maria", "Tina"), age= c(5,3))
#' a
#' one2many2(a, age)

#' @export
library(tidyr)
library(tidyverse)
library(dplyr)
one2many2 <- function(data, var,...){
data %>% mutate(rows = row_number()) |>
uncount({{var}},...)
}
