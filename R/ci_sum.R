
#' @title Compute traditional confidence intervals
#'
#' @param dat Dataset of juvenile or adult salmon
#' @param by Break dat by week, month or year
#' @param var Numeric variable to compute to estimate sum and confidence intervals
#' @param sdv Standard deviation
#' @param se Standard error
#' @param total Total fish passage
#' @param conf.int Confidence intervals such as 68, 80, 90, 95, 99
#' @return Summed fish passage and confidence intervals on total fish passage
#' @name ci_sum
#' @import tidyverse dplyr
#'
#' @examples
#' library(BD)
#' library(tidyverse)
#'
#' data(fish)
#' salmon <- dplyr::filter(fish, wk < 6)
#' salmon
#'
#' #Estimate daily mean passage for weeks that contain NA
#'
#' salmon <- salmon %>% group_by(wk) %>% mutate(wk_days = length(wk))
#' salmon <- salmon %>% group_by(wk_days, year, month, wk) %>%
#' reframe(pd = round(if_else(is.na(pd),mean(pd, na.rm=TRUE),pd))) %>%
#' arrange(wk) %>% data.frame()
#'
#' #' ci_sum(salmon, month, pd, conf.int = 0.90)
#' ci_sum(salmon, wk, pd, conf.int = 0.99)
#'
#' @export
library(tidyverse)

ci_sum <- function(dat, by, var, mult = qt((1 + conf.int)/2, n - 1), conf.int = 0.95) {
   n <- nrow(dat)
  dat %>% na.omit() %>%
    group_by({{by}}) %>%
    dplyr::summarise(total = sum({{var}}), sdev = sd({{var}})) %>%
     mutate(se = sdev/sqrt(n), lower = total - mult*se, upper = total + mult*se)
}
