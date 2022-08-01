#' Simplified Data for generating life tables.
#'
#' Data extracted and processed from The Health and Retirement Study (HRS). 
#' 
#' Each subject has 3 status in the cohort: 1: health; 2: unhealthiness; 3: death. 
#' Thus we will have 6 kind of possible transitions: 1:health to health; 2:health to unhealthiness; 3: health to death; 4: unhealthiness to health; 5: unhealthiness to unhealthiness; 6: unhealthiness to death. To check the transition status for each subject, please use \code{lifedata[,1]}.
#' 
#' To use this package with your data, please make sure your data have a vector for transitions.
#'
#' @format A data frame with 8198 rows and 16 variables:
#' \describe{
#'   \item{trans}{Transitions that recorded in the original data. In this data, we have 6 kinds of transtions in total.}
#'   \item{age}{Age for each subject.}
#'   \item{male}{Sex for each subject. male=1, female=0.}
#'   \item{black,hispanic}{Dummy variables for race.}
#'   \item{mar}{Marital status.}
#'   \item{educc,educg}{Dummy variables for education level.}
#'   \item{cohort}{Birth cohort, which is birth year minus 1900. }
#'   \item{neb,mwb,wb}{Dummy variables for birth regions.}
#'   \item{nen,mwn,wn}{Dummy variables for residential regions.}
#' }
#' @source \url{https://hrsdata.isr.umich.edu/data-products/rand?_ga=2.225225498.1006069885.1653670364-1014684070.1647264850}
"lifedata"
