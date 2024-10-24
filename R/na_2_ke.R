#' @title Convert NA to a number or character
#'
#' @param dat data.frame of numerical or mixed formats
#' @param replace replace NA in complete data.frame with '.', '-', '0', '?', 'hello'...etc
#' @import dplyr
#' @return data.frame with replaced NA to any number or character
#'
#' @name na_2_ke
#' @examples
#'
#' library(BD)
#' library(dplyr)
#' data(fish)
#' juv <- round(data.frame(head(fish)),5)
#' juv
#'
#' #Convert NA in data.frame to any desirable value
#' na_2_ke(juv, '?')
#' na_2_ke(juv, '_')
#' na_2_ke(juv, replace = '.')
#' na_2_ke(juv, 0)
#' na_2_ke(juv, replace = 'hola')
#'
#' #To operate only on some columns
#' juv %>%
#' mutate(xd = ifelse(is.na(xd), '_', xd),
#'        pd = ifelse(is.na(pd), 'hola', pd))
#'
#' @export

library(dplyr)
na_2_ke <- function(dat, replace = "_") {
               dat[is.na(dat)] <- replace
               dat
                }
