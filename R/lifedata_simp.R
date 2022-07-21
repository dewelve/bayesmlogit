#' Simplified Data for generating life tables.
#'
#' Data extracted and processed from The Health and Retirement Study (HRS). 
#' 
#' Each subject has 3 status in the cohort: 1: health; 2: unhealthiness; 3: death. 
#' Thus we will have 4 kind of possible transitions: 1:health to unhealthiness; 2: health to death; 3: unhealthiness to health; 4: unhealthiness to death. To check the transition status for each subject, please use \code{lifedata[,1]}.
#' 
#' To use this package on your data, please make sure your data have a transition status vector.
#'
#' @format A data frame with 7321 rows and 13 variables:
#' \describe{
#'   \item{trans}{Transition status was recorded in the original data. In this data, we have 4 kinds of transtions in total.}
#'   \item{age}{Age for each subject.}
#'   \item{male}{Sex for each subject. male=1, female=0}
#'   \item{black,other,hispanic}{Dummy variables for Race.}
#'   \item{edu}{Education level. Education is measured as below high school (less than 12 years of education) (edu=1), high school or some college (12-15 years of education) (edu=2), and college or higher (at least 16 years of education) (edu=3).}
#'   \item{region}{Region of Birth and residence.}
#'   \item{immigrant}{Immigrant status. Immigrant = 1, Not Immigrant = 0.}
#'   \item{cohort}{Cohort. Birth cohort, which is birth year minus 1900. }
#'   \item{norcg, cogcg, demcg}{Dummy variables for Caregiver Type.}
#' }
#' @source \url{https://hrsdata.isr.umich.edu/data-products/rand?_ga=2.225225498.1006069885.1653670364-1014684070.1647264850}
"lifedata_simp"
