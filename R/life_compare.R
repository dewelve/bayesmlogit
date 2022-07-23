#' @title Compare life expectancies
#' @description  A function for comparing life expectancies of each subgroup. By default, this function will calculate the proportion of samples that have a larger life expectancy or life expectancy proportion in your reference group than other groups.
#' @param status.include The status we aim to compare. It can be a number or a vector. Default is 0, which means we'll consider all status.
#' @param ref.var A vector that contains all covariates used as reference variables when comparing each subgroup.
#' @param file_path The file path for reading data.
#' @param file The file path for outputs.
#' @param states The total number of transition states in our data.
#' @param ref.level A vector that declares the reference value of each reference variable.
#' @param index.matrix A matrix that generated in \code{mlifeTable_plot()}. You don't need to specify it when using \code{mlifeTable_plot()}.
#' @param status.names A vector used to specify names of each status except death. 
#' @param prop The indicator for proportion plots and tables. If TRUE, this function will output life expectancy proportion plots and tables in addition to orginal life expectancy plots. Default is TRUE. 
#' @param criterion The criterion for comparison, which can be either ">" or "<". Default is ">".
#' @import utils
#' @export
#' @return A \code{.csv} file.
#' @examples
#' \dontrun{
#' 
#' #By setting the parameter 'compare' in mlifeTable_plot() to TRUE. We can directly use this function.
#' mlifeTable_plot(X=lifedata[,-1],status.include = 3,
#'       groupby = c("male","black","hispanic","other"), 
#'       cred = 0.92, 
#'       states = 3,
#'       file_path = ".",
#'       compare = TRUE,
#'       ref.var = c("black","hispanic","other"),
#'       ref.level = c(0,0,0))
#' 
#'}

life_compare <- function(file_path,file,
                         status.include = 0 , states,
                         ref.var, ref.level, index.matrix,
                         prop = TRUE,
                         criterion = ">",
                         status.names = NA){
  UseMethod("life_compare")
}

#' @export
##Construct a function for comparsion
life_compare <- function(file_path,file,
                         status.include = 0 , states,
                         ref.var, ref.level, index.matrix,
                         prop = TRUE,
                         criterion = ">",
                         status.names = NA){
  #Initialize
  cri <- criterion
  other.var <- colnames(index.matrix)[which(!(colnames(index.matrix) %in% ref.var))]
  if(length(other.var) == 0){Index.other <- matrix("",nrow=1, ncol=1)}
  else{Index.other <- unique(as.matrix(index.matrix[,other.var]))}

  Index.ref <- unique(as.matrix(index.matrix[,ref.var]))
  
  if(status.include [1] ==0 | is.na(status.include [1])){
    
    if(prop == TRUE){
      table <- matrix(nrow =2*states-1,ncol=dim(Index.other)[1]*(dim(Index.ref)[1]-1))
      if(is.na(status.names[1])){
        row.names <- c("Total Life", paste("Status=",1:(states-1),sep=''),paste("Status=",1:(states-1),"_prop",sep=''))
      }
      else{
        row.names <- c("Total Life", c(status.names),paste(c(status.names),"_prop",sep=''))
      }
    }
    else{
      table <- matrix(nrow =states,ncol=dim(Index.other)[1]*(dim(Index.ref)[1]-1))
      if(is.na(status.names[1])){
        row.names <- c("Total Life", paste("Status=",1:(states-1),sep=''))
      }
      else{
        row.names <- c("Total Life", paste(c(status.names),sep=''))
      }
    }
    col.names <- NULL
  }
  else{
    table <- matrix(nrow =length(status.include),ncol=dim(Index.other)[1]*(dim(Index.ref)[1]-1))
    if(is.na(status.names[1])){
      row.names <- paste("Status=",status.include ,sep='')
    }
    else{
      temp <- c(status.names,"death")
      row.names <- paste(temp[status.include],sep='')
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
                                                                        status.include , states,
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
                             status.include , states,
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
  
  if(status.include [1] ==0 | is.na(status.include [1])){
    
    
    
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
    for(i in 1:length(status.include )){
      if(criterion == ">"){value[i] <- mean(as.vector(data.ref[,status.include [i]])>as.vector(data.com[,status.include [i]]))}
      else if(criterion == "<"){value[i] <- mean(as.vector(data.ref[,status.include [i]])<as.vector(data.com[,status.include[i]]))}
    }
  }

  return(value)
}