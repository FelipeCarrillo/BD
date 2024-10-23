#' @title Compute summed bootstrapped confidence intervals
#'
#' @param dat Dataset of juvenile or adult salmon
#' @param by Break dat by week, month or year
#' @param var Numeric variable to compute to estimate sum and confidence intervals
#' @param conf.int Confidence intervals such as 68, 80, 90, 95, 99
#' @return Summed fish passage and confidence intervals on the sum
#' @name ci_boot_sum
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
#'  salmon
#'
#' ci_boot_sum(dat = salmon, by = wk, var = pd)
#' ci_boot_sum(salmon, month, pd)
#'
#' @export

library(tidyverse)
my_boot = function(x, times = 1000, conf.int = 0.95) {
  cis = quantile(replicate(times, sum(sample(x, replace = TRUE))), probs = c((1 - conf.int)/2, (1 + conf.int)/2))
  data.frame(sum = sum(x), lower.ci = cis[1], upper.ci = cis[2])
}

#THEN APPLY THE DEVELOPED FUNCTION ABOVE INSIDE MY FUNCTION BELOW INSIDE summarise:
ci_boot_sum <- function(dat, by, var, conf.int = 0.95,...) {
  #if(!is.numeric(var)) {stop("'var' must be numeric")}  #CAUSES ERRORS SO USE 'pull' INSTEAD TO AVOID ERRORS
  dat <- na.omit(dat)
  v <- pull(dat, {{var}})
  if(!is.numeric(v)) {stop("'var' must be numeric")}
  dat %>%
    group_by({{by}}) %>%
    dplyr::summarise(sum = list(my_boot({{var}}, conf.int = conf.int))) %>%
    unnest_wider(sum)
}

