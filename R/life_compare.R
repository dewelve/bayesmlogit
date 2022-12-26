#' @title Compare life expectancies
#' @description  A function for comparing the life expectancies of subgroups. This function will, by default, calculate the percentage of samples in your reference group with a higher (or lower) life expectancy (or proportion of total life expectancy) than other groups.
#' @param state.include The status we aim to compare. It can be a number or a vector. Default is 0, which means we'll consider all states. It can be inherited from \code{mlifetable_plot()}.
#' @param ref.var A vector containing all covariates used as comparison factors for each subgroup.
#' @param file_path The file path for data reading. It can be inherited from \code{mlifetable_plot()}.
#' @param file The file path for outputs. Default is \code{paste(file_path,"/mplotResults",sep='')}.
#' @param states The total number of states in data. It can be inherited from \code{mlifetable_plot()}.
#' @param ref.level A vector that declares the reference value of each reference variable.
#' @param index.matrix A matrix that generated in \code{mlifeTable_plot()}. You don't need to specify it when using \code{mlifeTable_plot()}.
#' @param state.names A vector used to specify names of each state except death. It can be inherited from \code{mlifetable_plot()}.
#' @param prop If TRUE, this function will output the comparision reulsts of life expectancy proportions in addition to orginal comparison results. Default is TRUE. It can be inherited from \code{mlifetable_plot()}.
#' @param criterion The criterion for comparison, which can be either ">" or "<". Default is ">".
#' @seealso \code{\link{mlifeTable_plot}}
#' @import utils
#' @export
#' @return A \code{.csv} file with comparison results.
#' @examples
#' \dontrun{
#' 
#' #By setting the parameter 'compare' in mlifeTable_plot() to TRUE. 
#' #We can directly employ this function.
#' 
#' mlifeTable_plot(X=lifedata[,-1],state.include = 3,
#'       groupby = c("male","black","hispanic"), 
#'       cred = 0.84, 
#'       states = 3,
#'       file_path = ".",
#'       compare = TRUE,
#'       ref.var = c("black","hispanic"),
#'       ref.level = c(0,0))
#' 
#'}

life_compare <- function(file_path,
                         file=paste(file_path,"/mplotResults",sep=''),
                         state.include = 0 , states,
                         ref.var, ref.level, index.matrix,
                         prop = TRUE,
                         criterion = ">",
                         state.names = NA){
  UseMethod("life_compare")
}

#' @export
##Construct a function for comparsion
life_compare <- function(file_path,
                         file=paste(file_path,"/mplotResults",sep=''),
                         state.include = 0 , states,
                         ref.var, ref.level, index.matrix,
                         prop = TRUE,
                         criterion = ">",
                         state.names = NA){
  #Initialize
  cri <- criterion
  other.var <- colnames(index.matrix)[which(!(colnames(index.matrix) %in% ref.var))]
  if(length(other.var) == 0){Index.other <- matrix("",nrow=1, ncol=1)}
  else{Index.other <- unique(as.matrix(index.matrix[,other.var]))}

  Index.ref <- unique(as.matrix(index.matrix[,ref.var]))
  
  if(state.include[1] ==0 | is.na(state.include[1])){
    
    if(prop == TRUE){
      table <- matrix(nrow =2*states-1,ncol=dim(Index.other)[1]*(dim(Index.ref)[1]-1))
      if(is.na(state.names[1])){
        row.names <- c("Total Life", paste("Status=",1:(states-1),sep=''),paste("Status=",1:(states-1),"_prop",sep=''))
      }
      else{
        row.names <- c("Total Life", c(state.names),paste(c(state.names),"_prop",sep=''))
      }
    }
    else{
      table <- matrix(nrow =states,ncol=dim(Index.other)[1]*(dim(Index.ref)[1]-1))
      if(is.na(state.names[1])){
        row.names <- c("Total Life", paste("Status=",1:(states-1),sep=''))
      }
      else{
        row.names <- c("Total Life", paste(c(state.names),sep=''))
      }
    }
    col.names <- NULL
  }
  else{
    table <- matrix(nrow =length(state.include),ncol=dim(Index.other)[1]*(dim(Index.ref)[1]-1))
    if(is.na(state.names[1])){
      row.names <- paste("Status=",state.include ,sep='')
    }
    else{
      temp <- c(state.names,"death")
      row.names <- paste(temp[state.include],sep='')
    }
    col.names <- NULL
  }
  
  for(i in 1:dim(Index.ref)[1]){
    if(all(Index.ref[i,] == ref.level)){ref.level.num <- i}
    else{next}
  }
  
  Index <- as.matrix(Index.ref[-ref.level.num, ])
  
  #Calculate
  for(i in 1:dim(index.matrix)[1]){
    
    if(!all(index.matrix[i,ref.var] == ref.level)){next}
    else{
      #Find current reference index
      index.ref <- paste(index.matrix[i,], collapse = '')
      index.other <- index.matrix[i,other.var]
      
      #Find index.other.num
      for(j in 1:dim(Index.other)[1]){
        if(all(Index.other[j,] == index.other)){index.other.num <- j}
        else{next}
      }
      
      
      for(j in 1:dim(Index)[1]){
        
        #Find index.com
        for(k in 1:dim(index.matrix)[1]){
          if(all(index.matrix[k,ref.var] == Index[j,]) &
             all(index.matrix[k,other.var] == index.other)){
            
            index.com <- paste(index.matrix[k,],collapse = '')
            break
          }
          else{next}
        }
        
        table[,j+dim(Index)[1]*(index.other.num-1)] <- life_compare_one(file_path, file,
                                                                        state.include , states,
                                                                        index.ref,
                                                                        index.com,
                                                                        criterion = cri,
                                                                        prop = prop)
        col.names[j+dim(Index)[1]*(index.other.num-1)] <- paste("group",index.com,sep = '')
      }
      
    }
  }
  
  rownames(table) <- row.names
  colnames(table) <- col.names
  
  utils::write.csv(table, file = paste(file, "/Compare.csv",sep=''))
  
}

#This function will compare the life expectancy to one specific group.
life_compare_one <- function(file_path,file,
                             state.include , states,
                             index.ref,index.com,
                             criterion = ">",prop){
  
  value <- NULL
  
  file_name.ref <- paste(paste(file_path,"/lifetable",sep=''),
                         index.ref,
                         ".txt",
                         sep="")
  file_name.com <- paste(paste(file_path,"/lifetable",sep=''),
                         index.com,
                         ".txt",
                         sep="")
  
  data.ref <- utils::read.table(file_name.ref,header = TRUE)
  data.com <- utils::read.table(file_name.com,header = TRUE)
  
  if(state.include [1] ==0 | is.na(state.include [1])){
    
    
    
    ttle.ref <- rowSums(data.ref[,1:(states-1)])
    ttle.com <- rowSums(data.com[,1:(states-1)])
    
    #Find Proportion of expectancy years
    for(i in 1:(states)){
      if(criterion == ">"){
        if(i == 1){value[i] <- mean(as.vector(ttle.ref)>as.vector(ttle.com))}
        else{
          value[i] <- mean(as.vector(data.ref[,i-1])>as.vector(data.com[,i-1]))
        }
      }
      else if(criterion == "<"){
        if(i == 1){value[i] <- mean(as.vector(ttle.ref)<as.vector(ttle.com))}
        else{
          value[i] <- mean(as.vector(data.ref[,i-1])<as.vector(data.com[,i-1]))
        }
      }
      
    }
    #Find Proportion of expectancy years proportions
    if(prop == TRUE){
      for(i in 1:(states-1)){
        if(criterion == ">"){
          value[states + i] <- mean(as.vector(data.ref[,i]/ttle.ref)>as.vector(data.com[,i]/ttle.com))
        }
        else if(criterion == "<"){
          value[states + i] <- mean(as.vector(data.ref[,i]/ttle.ref)<as.vector(data.com[,i]/ttle.com))
        }
      }
    }
  }
  else{
    for(i in 1:length(state.include )){
      if(criterion == ">"){value[i] <- mean(as.vector(data.ref[,state.include [i]])>as.vector(data.com[,state.include [i]]))}
      else if(criterion == "<"){value[i] <- mean(as.vector(data.ref[,state.include [i]])<as.vector(data.com[,state.include[i]]))}
    }
  }

  return(value)
}