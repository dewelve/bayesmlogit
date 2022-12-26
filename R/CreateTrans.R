#' @title Create Transition Vector
#' @description  A function used to create transition vectors with data in long format, which requires \code{dplyr} package. The rules for creating transitions can be found with \code{?lifedata}
#' @param ID A vector that specifies the ID for each subject.
#' @param Age A vector that indicates each subject's age at this visit.
#' @param State A vector or a factor that indicates the state for each subject at this visit.
#' @param Death A vector that indicates whether the subject died or not at this visit.
#' @param states The total number of states in our data.
#' @seealso \code{\link{lifedata}}
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#' @return  
#' A vector that contains all transitions.
#' @examples
#' ID <- rep(1:50, each = 5) 
#' Age <- rep(31:35, times = 50) 
#' State <- sample(1:5,size=250,replace=TRUE)  
#' Death <- rep(c(0,0,0,0,1),times=50)
#' 
#' Example <- data.frame(ID,Age,State,Death)
#' 
#' Example$trans <- CreateTrans(Example$ID,Example$Age, Example$State,Example$Death,states=6)
#' 
#' 
CreateTrans <- function(ID,Age,State,Death,states){
  UseMethod("CreateTrans")
}
#' @export
CreateTrans.default <- function(ID,Age,State,Death,states){
  
  
  temp <- data.frame(id=ID,age = Age,state = as.numeric(factor(State)),death = Death)
  trans <- temp %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(trans = CreatTrans2(age,state,death,states)) %>%
    as.data.frame()
  trans <- trans$trans
  
  return(trans)
  
}

CreatTrans2 <- function(Age,State,Death,states){
  reps <- states
  temp <- data.frame(age = Age,state = State,death = Death) %>% 
    dplyr::arrange(age)
  
  trans <- rep(NA,length(temp$state))
  index <- NULL
  
  for(i in 1:length(temp$state)){
    
    if(temp$death[i] ==1){
      index[i] <- reps
    }
    else{
      index[i] <- temp$state[i]
    }
  }
  
  for(i in 1:(length(temp$state)-1)){
    trans[i+1] <- (index[i]-1)*reps+(index[i+1]-1)
  }
  trans <- trans+1
  return(trans)
}
