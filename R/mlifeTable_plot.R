#' @title Plot life expectancies
#' @description  A function for plotting posterior means and their credible intervals. Can also be used as a subfunction in \code{mlifetable()}.
#' @param state.include A vector or a number used to specify the states whose expectancy years are of interest. Default is 0, which means we'll generate plots for all states. For multiple states specified, we will get the expectancy years for each state and their sum.
#' @param groupby A vector that contains covariates for subgroup comparisons. It can be inherited from \code{mlifetable()}.
#' @param file_path The file path for outputs. It can be inherited from \code{mlifetable()}.
#' @param X A matrix of covariates. Note that X must include age as a convariate. It can be inherited from \code{mlifetable()}.
#' @param cred Credible level. For example, if \code{cred = 0.84}, we will get the 84\% credible interval.
#' @param states The total number of states in data. It can be inherited from \code{mlifetable()}.
#' @param prop If TRUE, this function will output life expectancy proportion plots and tables in addition to original life expectancy plots. Default is TRUE. 
#' @param subgroup.names A vector that contains names of each subgroup. You can also specify them in the output files. 
#' @param compare If TRUE, this function will quote \code{life_compare()} and generate a table with all comparsion results based on the reference variables and reference levels specified. Default is FALSE.
#' @param state.names A vector used to specify names of each state except death. It can be inherited from \code{mlifetable()}.
#' @param midpoint.type A character used to specify the midpoint type for credible interval plots. Can be either "mean" or "median". Default is "mean", which means the plots will use mean values as the middle point.
#' @param ... Extra parameters for \code{life_compare()}. See details using \code{?life_compare()}.
#' @import ggplot2 utils stats
#' @export
#' @return Plots and tables for posterior means and credible intervals of each subgroups.
#' @examples
#' \dontrun{
#' 
#' #Generate plots and corresponding tables only.
#' mlifeTable_plot(X=lifedata[,-1],state.include = 0,
#'       groupby = c("male","black","hispanic"), 
#'       cred = 0.84, 
#'       states = 3,
#'       file_path = ".")
#'       
#' #Additionally generate the comparsion results to the reference level.
#' mlifeTable_plot(X=lifedata[,-1],state.include = 0,
#'       groupby = c("male","black","hispanic"), 
#'       cred = 0.84, 
#'       states = 3,
#'       file_path = ".",
#'       compare = TRUE,
#'       ref.var = c("black","hispanic"),
#'       ref.level = c(0,0))
#' }

mlifeTable_plot <- function(state.include = 0,
                            groupby,
                            file_path,
                            X,
                            cred=0.84,
                            states,
                            prop = TRUE,
                            subgroup.names =NULL,
                            state.names=NA,
                            compare=FALSE,
                            midpoint.type = "mean",
                            ...){
  UseMethod("mlifeTable_plot")
}

#' @export
mlifeTable_plot.default <- function(state.include = 0,
                                    groupby,
                                    file_path,
                                    X,
                                    cred=0.84,
                                    states,
                                    prop = TRUE,
                                    subgroup.names =NULL,
                                    state.names=NA,
                                    compare=FALSE,
                                    midpoint.type = "mean",
                                    ...){
  
  file <- paste(file_path,"/mplotResults",sep='')
  dir.create(file)
  #Output Names
  
  if(is.na(state.names[1])){
    if(state.include[1] == 0 | is.na(state.include[1])){output.names <- c(1:(states-1))}
    else{output.names <- state.include}
  }
  else{
    if(state.include[1] == 0 | is.na(state.include[1])){output.names <- state.names}
    else{output.names <- state.names[state.include]}
  }
  ##Subgroup
  age.index <- which(colnames(as.data.frame(X)) %in% "age" ==TRUE)
  X <- X[,-age.index]
  data <- as.data.frame(X)
  cols <- colnames(data)
  vars.group <- groupby
  vars.other <- setdiff(cols,groupby)
  
  ##Construct index matrix
  if(!is.na(vars.group[1])){
    if(length(vars.group)>1){
      data.group.list <- lapply(data[,vars.group],c)
      index.matrix <- sapply(unique(data[,vars.group]),as.character)
    }
    else{
      data.group.list <- list(data[,vars.group])
      names(data.group.list) <- vars.group
      index.matrix <- as.matrix(sapply(unique(data[,vars.group]),as.character))
      colnames(index.matrix) <- vars.group
    }
    
  }
  
  index.dim <- ifelse(is.null(dim(index.matrix)[1]),1,dim(index.matrix)[1])
  
  total <- matrix(nrow=index.dim,ncol=3)
  
  ##For based-population
  
  if(state.include[1] ==0 | is.na(state.include[1])){
    
    ##Construct lists for different status
    life.list <- list()
    life.list.prop <- list()
    for(i in 1:(states)){
      
      temp <- list(matrix(nrow=index.dim,ncol=3))
      names(temp) <- paste("state","=",i,collapse = '')
      
      life.list <- append(life.list,temp)
      life.list.prop <- append(life.list.prop,temp)
      
    }
    
    ##Tables and plots for each subgroup
    for(i in 1:index.dim){
      
      file_name <- paste(paste(file_path,"/lifetable",sep=''),
                         ifelse(is.null(dim(index.matrix)[1]),
                                1,
                                paste(index.matrix[i,],collapse = '')),
                         ".txt",
                         sep="")
      
      data <- utils::read.table(file_name, header = TRUE)
      colnum <- dim(data)[2]
      index <- index.matrix[i,]
      
      ##Total life expectancy
      ttle <- rowSums(data[,1:(colnum)])
      if(midpoint.type == "mean"){
        row1 <- c(mean(ttle),stats::quantile(ttle, c((1-cred)/2, (1+cred)/2)))
      }
      else if(midpoint.type == "median"){
        row1 <- c(median(ttle),stats::quantile(ttle, c((1-cred)/2, (1+cred)/2)))
      }
      
      total[i,] <- row1
      ##Calculate posterior means/medians and credible intervals for each status
      for(j in 1:(states)){
        if(midpoint.type == "mean"){
          life.list[[j]][i,] <- c(mean(data[,j]),stats::quantile(data[,j], c((1-cred)/2, (1+cred)/2)))
          life.list.prop[[j]][i,] <- c(mean(data[,j]/ttle),stats::quantile(data[,j]/ttle, c((1-cred)/2, (1+cred)/2)))
        }
        else if(midpoint.type == "median"){
          life.list[[j]][i,] <- c(median(data[,j]),stats::quantile(data[,j], c((1-cred)/2, (1+cred)/2)))
          life.list.prop[[j]][i,] <- c(median(data[,j]/ttle),stats::quantile(data[,j]/ttle, c((1-cred)/2, (1+cred)/2)))
        }
      }
    }
    
    life.list <- append(list("Total" = total),life.list)
    
    ##Save plots and tables
    tables_plots_1(index.dim,
                   index.matrix,
                   states,
                   life.list,
                   life.list.prop,
                   file,
                   subgroup.names,
                   prop=prop,
                   output.names,
                   midpoint.type = midpoint.type)
    
  }
  
  ##For one or several given status
  else{
    life.list <- list()
    life.list.prop <- list()
    for(j in state.include){
      
      temp <- list(matrix(nrow=index.dim,ncol=3))
      names(temp) <- paste("state","=",j,collapse = '')
      
      life.list <- append(life.list,temp)
      life.list.prop <- append(life.list.prop,temp)
    }
    
    ##Tables and plots for each subgroup
    for(i in 1:index.dim){
      
      
      file_name <- paste(paste(file_path,"/lifetable",sep=''),
                         ifelse(is.null(dim(index.matrix)[1]),
                                1,
                                paste(index.matrix[i,],collapse = '')),
                         ".txt",
                         sep="")
      
      data <- utils::read.table(file_name,header = TRUE)
      colnum <- dim(data)[2]
      index <- index.matrix[i,]
      
      if(length(state.include)>1){
        ##Total life expectancy
        ttle <- rowSums(data[,1:(colnum)])
        if(midpoint.type == "mean"){
          row1 <- c(mean(ttle),stats::quantile(ttle, c((1-cred)/2, (1+cred)/2)))
        }
        else if(midpoint.type == "median"){
          row1 <- c(median(ttle),stats::quantile(ttle, c((1-cred)/2, (1+cred)/2)))
        }
        
        total[i,] <- row1
        ##Calculate posterior means/medians and credible intervals for each status
        for(j in 1:(states)){
          if(midpoint.type == "mean"){
            life.list[[j]][i,] <- c(mean(data[,j]),stats::quantile(data[,j], c((1-cred)/2, (1+cred)/2)))
            life.list.prop[[j]][i,] <- c(mean(data[,j]/ttle),stats::quantile(data[,j]/ttle, c((1-cred)/2, (1+cred)/2)))
          }
          else if(midpoint.type == "median"){
            life.list[[j]][i,] <- c(median(data[,j]),stats::quantile(data[,j], c((1-cred)/2, (1+cred)/2)))
            life.list.prop[[j]][i,] <- c(median(data[,j]/ttle),stats::quantile(data[,j]/ttle, c((1-cred)/2, (1+cred)/2)))
          }
        }
      }
      else{
        ##Calculate posterior means/medians and credible intervals for each status
        for(j in 1:length(state.include)){
          if(midpoint.type == "mean"){
            life.list[[j]][i,] <- c(mean(data[,j]),stats::quantile(data[,j], c((1-cred)/2, (1+cred)/2)))
          }
          else if(midpoint.type == "median"){
            life.list[[j]][i,] <- c(median(data[,j]),stats::quantile(data[,j], c((1-cred)/2, (1+cred)/2)))
          }
        }
      }
    
    }
    
    
    ##Save plots and tables
    if(length(state.include)>1){
      
      life.list <- append(list("Total" = total),life.list)
      
      tables_plots_1(index.dim,
                     index.matrix,
                     states,
                     state.include,
                     life.list,
                     life.list.prop,
                     file,
                     subgroup.names,
                     prop=prop,
                     output.names,
                     midpoint.type = midpoint.type)
    }
    else{
      tables_plots_2(index.dim,
                     index.matrix,
                     state.include,
                     life.list,
                     file,
                     subgroup.names,
                     output.names,
                     midpoint.type = midpoint.type)
    }
    
  }
  if(compare == TRUE){life_compare(file_path,
                                   file,
                                   state.include = state.include, 
                                   states = states,
                                   state.names = state.names,
                                   ...,
                                   index.matrix = index.matrix, 
                                   prop = prop)
  }
  
}

##Function for tables and plots when state.include = 0 or length(state.include)>1.
tables_plots_1 <- function(index.dim,
                           index.matrix,
                           states,
                           state.include,
                           life.list,life.list.prop,
                           file,
                           subgroup.names,
                           prop,
                           output.names,
                           midpoint.type = "mean"){
  table.length <- ifelse(state.include[1] == 0 | is.na(state.include[1]),
                         states-1,
                         length(state.include))
  
  ##Creat subgroup names using index.matrix
  if(is.null(subgroup.names[1])){
    subgroup.names <- NULL
    for(i in 1:index.dim){
      subgroup.names[i] <- paste("group",paste(index.matrix[i,],collapse=""),sep="")
    }
  }
  
  
  ##Write total life expectancy
  temp <- data.frame(life.list[[1]],"subgroup" = subgroup.names)
  
  if(midpoint.type == "mean"){
    colnames(temp) <- c("mean", "left.bound","right.bound","subgroup")
  }
  else if(midpoint.type == "median"){
    colnames(temp) <- c("median", "left.bound","right.bound","subgroup")
  }
  
  
  ##Tables
  utils::write.table(x=temp,
                     file = paste(paste(file,"/total_life_expectancy",sep=''),
                                  ".txt",
                                  sep=""),
                     row.names = FALSE)
  ##Plots
  if(midpoint.type == "mean"){
    p <- ggplot2::ggplot(data = temp, 
                         ggplot2::aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbarh(height = 0) +
      ggplot2::theme_light() + 
      ggplot2::labs(x = "Total extectancy of remaining life", y = "Subgroup", title = "Total life expectancy for each subgroup")
    ggplot2::ggsave(paste(paste(file,"/total_life_expectancy",sep=''),
                          ".png",
                          sep=""),
                    width=10,height = 6.18,units="in")
    
    for(i in 1:table.length){
      ##Life extectancy
      temp1 <- data.frame(life.list[[i+1]],"subgroup" = subgroup.names)
      colnames(temp1) <- c("mean", "left.bound","right.bound","subgroup")
      
      
      utils::write.table(x=temp1,
                         file = paste(paste(file,"/life_expectancy_","state_",output.names[i],sep=''),
                                      ".txt",
                                      sep=""),
                         row.names = FALSE)
      p <- ggplot2::ggplot(data = temp1, 
                           ggplot2::aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
        ggplot2::geom_point() +
        ggplot2::geom_errorbarh(height = 0) +
        ggplot2::theme_light() + 
        ggplot2::labs(x = "Extectancy years of remaining life", y = "Subgroup", title = "Total life expectancy for each subgroup")  
      ggplot2::ggsave(paste(paste(file,"/life_expectancy_","status_",output.names[i],sep=''),
                            ".png",
                            sep=""),
                      width=10,height = 6.18,units="in")
      if(prop == TRUE){
        ##Life extectancy proportion
        temp2 <- data.frame(life.list.prop[[i]],"subgroup" = subgroup.names)
        colnames(temp2) <- c("mean", "left.bound","right.bound","subgroup")
        
        
        utils::write.table(x=temp2,
                           file = paste(paste(file,"/life_expectancy_prop_","state_",output.names[i],sep=''),
                                        ".txt",
                                        sep=""),
                           row.names = FALSE)
        p <- ggplot2::ggplot(data = temp2, 
                             ggplot2::aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
          ggplot2::geom_point() +
          ggplot2::geom_errorbarh(height = 0) +
          ggplot2::theme_light() + 
          ggplot2::labs(x = "Life Extectancy proportion of remaining life", y = "Subgroup", title = "Life Extectancy proportion for each subgroup")  
        ggplot2::ggsave(paste(paste(file,"/life_expectancy_prop_","state_",output.names[i],sep=''),
                              ".png",
                              sep=""),
                        width=10,height = 6.18,units="in")
      }
    }
  }
  else if(midpoint.type == "median"){
    
    p <- ggplot2::ggplot(data = temp, 
                         ggplot2::aes_string(x = 'median', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbarh(height = 0) +
      ggplot2::theme_light() + 
      ggplot2::labs(x = "Total extectancy of remaining life", y = "Subgroup", title = "Total life expectancy for each subgroup")
    ggplot2::ggsave(paste(paste(file,"/total_life_expectancy",sep=''),
                          ".png",
                          sep=""),
                    width=10,height = 6.18,units="in")
    
    for(i in 1:table.length){
      ##Life extectancy
      temp1 <- data.frame(life.list[[i+1]],"subgroup" = subgroup.names)
      colnames(temp1) <- c("median", "left.bound","right.bound","subgroup")
      
      
      utils::write.table(x=temp1,
                         file = paste(paste(file,"/life_expectancy_","state_",output.names[i],sep=''),
                                      ".txt",
                                      sep=""),
                         row.names = FALSE)
      p <- ggplot2::ggplot(data = temp1, 
                           ggplot2::aes_string(x = 'median', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
        ggplot2::geom_point() +
        ggplot2::geom_errorbarh(height = 0) +
        ggplot2::theme_light() + 
        ggplot2::labs(x = "Extectancy years of remaining life", y = "Subgroup", title = "Total life expectancy for each subgroup")  
      ggplot2::ggsave(paste(paste(file,"/life_expectancy_","state_",output.names[i],sep=''),
                            ".png",
                            sep=""),
                      width=10,height = 6.18,units="in")
      if(prop == TRUE){
        ##Life extectancy proportion
        temp2 <- data.frame(life.list.prop[[i]],"subgroup" = subgroup.names)
        colnames(temp2) <- c("median", "left.bound","right.bound","subgroup")
        
        
        utils::write.table(x=temp2,
                           file = paste(paste(file,"/life_expectancy_prop_","state_",output.names[i],sep=''),
                                        ".txt",
                                        sep=""),
                           row.names = FALSE)
        p <- ggplot2::ggplot(data = temp2, 
                             ggplot2::aes_string(x = 'median', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
          ggplot2::geom_point() +
          ggplot2::geom_errorbarh(height = 0) +
          ggplot2::theme_light() + 
          ggplot2::labs(x = "Life Extectancy proportion of remaining life", y = "Subgroup", title = "Life Extectancy proportion for each subgroup")  
        ggplot2::ggsave(paste(paste(file,"/life_expectancy_prop_","state_",output.names[i],sep=''),
                              ".png",
                              sep=""),
                        width=10,height = 6.18,units="in")
      }
    }
    
  }
  
  
}
##Function for tables and plots when state.include != 0.
tables_plots_2 <- function(index.dim,index.matrix,state.include,
                           life.list,
                           file,
                           subgroup.names,
                           output.names,
                           midpoint.type = "mean"){
  
  ##Creat subgroup names using index.matrix
  if(is.null(subgroup.names[1])){
    subgroup.names <- NULL
    for(i in 1:index.dim){
      subgroup.names[i] <- paste("group",paste(index.matrix[i,],collapse=""),sep="")
    }
  }
  
  ##Write total life expectancy
  temp <- data.frame(life.list[[1]],"subgroup" = subgroup.names)
  
  if(midpoint.type == "mean"){
    colnames(temp) <- c("mean", "left.bound","right.bound","subgroup")
    for(i in 1:length(state.include)){
      ##Life extectancy
      temp1 <- data.frame(life.list[[i]],"subgroup" = subgroup.names)
      colnames(temp1) <- c("mean", "left.bound","right.bound","subgroup")
      
      
      utils::write.table(x=temp1,
                         file = paste(paste(file,"/life_expectancy_","state_",output.names[i],sep=''),
                                      ".txt",
                                      sep=""),
                         row.names = FALSE)
      p <- ggplot2::ggplot(data = temp1, 
                           ggplot2::aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
        ggplot2::geom_point() +
        ggplot2::geom_errorbarh(height = 0) +
        ggplot2::theme_light() +
        ggplot2::labs(x = "Extectancy years of remaining life", y = "Subgroup", title = "Total life expectancy for each subgroup")  
      ggplot2::ggsave(paste(paste(file,"/life_expectancy_","state_",output.names[i],sep=''),
                            ".png",
                            sep=""),
                      width=10,height = 6.18,units="in")
    }
  }
  else if(midpoint.type == "median"){
    
    colnames(temp) <- c("median", "left.bound","right.bound","subgroup")
    for(i in 1:length(state.include)){
      ##Life extectancy
      temp1 <- data.frame(life.list[[i]],"subgroup" = subgroup.names)
      colnames(temp1) <- c("median", "left.bound","right.bound","subgroup")
      
      
      utils::write.table(x=temp1,
                         file = paste(paste(file,"/life_expectancy_","state_",output.names[i],sep=''),
                                      ".txt",
                                      sep=""),
                         row.names = FALSE)
      p <- ggplot2::ggplot(data = temp1, 
                           ggplot2::aes_string(x = 'median', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
        ggplot2::geom_point() +
        ggplot2::geom_errorbarh(height = 0) +
        ggplot2::theme_light() +
        ggplot2::labs(x = "Extectancy years of remaining life", y = "Subgroup", title = "Total life expectancy for each subgroup")  
      ggplot2::ggsave(paste(paste(file,"/life_expectancy_","state_",output.names[i],sep=''),
                            ".png",
                            sep=""),
                      width=10,height = 6.18,units="in")
    }
  }
  
  
  
}

