#' @title Compute mean bootstrapped confidence intervals
#'
#' @param dat Dataset of juvenile or adult salmon.
#' @param by Break dat by week, month or year. These columns must exist in dataset.
#' @param var Numeric variable (catch) to compute to estimate mean and confidence intervals.
#' @param conf.int Confidence intervals such as 68, 80, 90, 95, 99.
#' @return Mean fish passage and confidence intervals on the mean.
#' @import Hmisc dplyr conflicted
#' @name ci_boot
#'
#' @examples
#' library(BD)
#' library(tidyverse)
#' library(dplyr)
#' library(Hmisc)
#' library(conflicted)
#' conflicts_prefer(dplyr::summarise)
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
#'salmon
#'
#' ci_boot(dat = salmon, by = wk, var = pd, conf.int = 0.90)
#' ci_boot(salmon, month, pd)
#'
#' @export
#THIS FUCTION IS TO BE USED WITH ANY SPECIES OR ANY DATASET THAT CONTAINS A CATCH COLUMN
#BE SURE TO SUPPLY YOUR CATCH COLUMN AS var = yourcatchcolumnname inside the ci_boot function
library(Hmisc)
library(tidyverse)
library(dplyr)
library(conflicted)
conflicts_prefer(dplyr::summarise)

ci_boot <- function(dat, by, var, conf.int = 0.90,...) {
  v <- pull(dat, {{var}})
  if (missing(v)) stop("Error: 'var' is missing.")
  if(!is.numeric(v)) {stop("'var' must be numeric")}
  dat %>%
    group_by({{by}}) %>%
    dplyr::summarise(mean=list(smean.cl.boot({{var}}, conf.int=conf.int))) %>%
    unnest_wider(mean)
}
