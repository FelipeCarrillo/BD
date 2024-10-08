#' @title Compute intervals by week
#'
#' @param dataset Data with 6 columns
#' @param by Compute fish passage by week
#' @param model Trap efficiency data
#' @import stats
#' @return Confidence intervals by week
#'
#' @name intervals
#' @examples
#' library(BD)
#' library(tidyverse)
#' data(fish)
#' salmon <- dplyr::filter(fish, wk < 6)
#' salmon
#'
#' data(model)
#' intervals(salmon,'wk',model)
#'
#library(plyr)

#' @export

library(stats)
intervals <- function(dataset, by, model){
  SEValues <- lm(Efficiency ~ PercQ, data = model) # Regression equation values and std Error
  MSValue <- anova(lm(Efficiency ~ PercQ, data = model))    # Mean-Square value

  Beta <- coef(summary(SEValues))
  StdE = Beta[,"Std. Error"]
  # Intercept or Upper Std. Error value
  varA <- StdE[1]^2
  #  Variable(PercQ) or lower Std. Error value
  varB <- StdE[2]^2
  # Extract the Mean sq after estimating ANOVA
  msquare <- MSValue$"Mean Sq"[2]
  # Estimate the Mean Standard Error of Percent Q
  meanQ <- mean(model$PercQ)
  n <- nrow(model)
  variance <- var(model$PercQ)
  covAB <- msquare*(- meanQ)/(var(model$PercQ)*n - 1)

  plyr::ddply(dataset, by, function(x) {

    NumDays <- nrow(x)
    Week <- unique(x$wk)         #get the week number
    Month <- unique(x$month)
    Year <- unique(x$year)      #get the year number
    # Make a dataframe with the sampled days only
    sampled <- subset(x, xd >= 0, select = c(xd, pd, td))
    # Count the days sampled only (I could have used length to count the sampled days)
    daysSamp <- nrow(sampled)
    NonSampDays <- NumDays - daysSamp
    # Estimate and add the varPD column to the dataframe
    x$varPD <- with(x, (pd * (1 - td))/td + msquare * ((pd*(1 - td) + pd^2*td))/td^3)
    # Convert the NA's to "0" to calculate the weekly covariance.
    x <- data.frame(lapply(x, function(x) replace(x,is.na(x),0)))
    #Subset the x dataset to extract xd,td,pd and transpose it to estimate daily covariance

    # use the 'signif' function to apply scientific decimals to the values(or 'round' to limit the decimals)
    xd <- x$xd
    pd <- x$pd
    td <- x$td
    mydf <- data.frame(xd, pd, td)
    transData <- t(mydf)

    Csequence <- lapply(seq(ncol(transData) - 1), function(x) x:(ncol(transData) - 1))
    CovSummary <- lapply(Csequence, function(.col){
      # compute the 3 columns of data
      + cbind(xd=varA + transData[1, .col[1]] * covAB + transData[1, .col + 1] *
                covAB + transData[1, .col[1]] * transData[1, .col + 1] * varB,
              pd=transData[2, .col[1]] * transData[2, .col + 1],td=transData[3, .col[1]] * transData[3, .col + 1])
    })

    # rbind for the output
    WCovariance <- do.call(rbind, CovSummary)
    # add the covariance
    WCovariance <- cbind(WCovariance, WeekCov=WCovariance[, 'xd'] * WCovariance[, 'pd'] / WCovariance[, 'td'])
    SummedCovPiPj <- sum(WCovariance[, 'WeekCov'],na.rm=T)
    pdmean <- sum(x$pd)/daysSamp   # Mean weekly passage
    Weekvariance <- var(x$pd)  #Weekly pd variance
    SummedVarPD <- sum(x$varPD)  #Summed variance of pd

    TotalPD<- pdmean*NumDays    # Mean based on number of days of the week
    varTotalPD <- ((1-daysSamp/NumDays)*NumDays^2/daysSamp*Weekvariance) + ((NumDays/daysSamp)*(SummedVarPD + 2*SummedCovPiPj))
    T <- qt((1-0.10/2),NumDays)  # Inverse t-distribution for 90% confidence intervals
    PlusMin <-T*varTotalPD^.5
    Lower <- TotalPD-PlusMin   # Lower Confidence
    Upper <- TotalPD+PlusMin  # Upper Confidence
    Final <- data.frame(Year,Week,NumDays,daysSamp,NonSampDays,pdmean,Weekvariance,SummedVarPD,SummedCovPiPj,TotalPD,varTotalPD,T,PlusMin,Lower,Upper)
    # Replace NA's with 0
    Final <- data.frame(lapply(Final,function(x) replace(x,is.na(x),0)))
    # Another way for replacing NA's
    #x <- data.frame(lapply(x_NA,function(x){x[is.na(x)] <- 0 ; x}))
    FinalData <- data.frame(Year,Week,daysSamp,NonSampDays,Lower,TotalPD,Upper,varTotalPD)
    FinalData <- data.frame(lapply(FinalData,function(x) replace(x,is.na(x),0)))
  })}
