#' Simplified Data for generating life tables.
#'
#' Data extracted and processed from The Health and Retirement Study (HRS). 
#' 
#' @details
#' To use this package with your data, please make sure your data have a vector for transitions. The transitions can be manually created following the example below: 
#' 
#' In \code{lifedata}, Each subject has 3 states in the cohort: 1: health; 2: unhealthiness; 3: death. 
#' Thus we will have 6 kind of possible transitions: 1:health to health; 2:health to unhealthiness; 3: health to death; 4: unhealthiness to health; 5: unhealthiness to unhealthiness; 6: unhealthiness to death. To check the transition for each subject, please use \code{lifedata[,1]}.
#' 
#' When creating transitions by yourself, please follow the orders as below:
#' 
#' |               | Health        | Unhealthiness  |      Death     |
#' | :-----------: |:-------------:|:--------------:|:--------------:|
#' | Health        |       1       |       2        |       3        |
#' | Unhealthiness |       4       |       5        |       6        |
#' | Death         |       7       |       8        |       9        |
#' 
#' where the first column indicates the previous state of subjects and the first row indicates the current state that subjects are in. The numbers indicates the index of our transitions. 
#' For impossible transitions like death to death, you can also label them following the above order, which won't change the results. If transitions are not created in this order, the computation may encounter an error. One can also use \code{CreateTrans()} to create the transition vector. 
#' 
#' @md
#' @seealso \code{\link{CreateTrans}}
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
#' 

"lifedata"

