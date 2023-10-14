#' @title Multistate Life Table Method
#' @description  A Bayesian Multistate Life Table Method for survey data, developed by Lynch and Zang (2022), allowing for large state spaces with quasi-absorbing states (i.e., structural zeros in a transition matrix). 
#' @details This function generates life tables based on the estimates from the Bayesian multinomial logit regressions, which can be obtained using the \code{bayesmlogit()} function. The values in the generated life table represent the expected remaining years to be spent in each state conditional on a give age. Current version was designed to only generate life tables based on data with a death state.
#' @param y A vector of transitions.
#' @param X A matrix of covariates. Note that \code{X} must include age as a convariate.
#' @param trans The posterior samples generated using \code{?bayesmlogit()}.
#' @param states The total number of states in data.
#' @param file_path The file path for outputs.
#' @param groupby A vector that contains the covariates for subgroup comparisons. Default is NA, which means that we won't make subgroups.
#' @param no_control The covariates that we don't want to control in subgroup analysis. Default is NA, which means we will control all covariates in X. As an example, in Lynch and Zang's study (2022), they incorporated education into the multinomial logit model. However, in the life table calculation, if one does not want to control for education, one could opt to use its region-specific mean rather than the sample mean using no_control.
#' @param values A list that specifies values for covariates. Default is NA. If both no_control and values are specified, the option values takes precedence.
#' @param status A numeric value. The option allows producing status-based life tables. Default is 0, produces population-based life tables.
#' @param startages Start age of the life table. Default is 0.
#' @param endages End age of the life table. Default is 110.
#' @param age.gap This option allows users to specify the age interval of the life table. Default is 1. For example, if the survey data were sampled every 2 years, users can specify the age interval to be 2 in the life table.
#' @param nums Number of life tables generated for each subgroup. Default is the size of posterior samples we used.
#' @param mlifeTable_plot If TRUE, this option will create a new directory \code{mplotResults} under given \code{file_path} and output corresponding plots and tables for posterior means and credible intervals. Default is FALSE. 
#' @param state.names A vector used to specify names of each state except death. You can also specify them in the output files.
#' @param ... Extra parameters for \code{mlifeTable_plot()}. See more details using \code{?mlifeTable_plot()}.
#' @seealso \code{\link{bayesmlogit}}, \code{\link{mlifeTable_plot}}
#' @import utils
#' @export
#' @return Life tables for each subgroup.
#' @examples
#' \dontrun{
#' #The life tables generated in the example have 3 columns, which correspond to 3 states: 
#' #1: health; 2: unhealthiness; 3: death;
#' 
#' data <- lifedata
#' y <- data[,1]
#' X <- data[,-1]
#' 
#' # This example will take about 30 mins.
#' 
#' out <- bayesmlogit(y, X ,samp=1000, burn=500,verbose=10) 
#'
#' trans <- out$outwstepwidth
#' mlifeTable(y,X,trans =trans,
#'            groupby = c("male","black","hispanic"),
#'            no_control = "mar",
#'            startages=50,
#'            age.gap=1,
#'            states=3,
#'            file_path=".")
#'
#' # To name each subgroup, try the subgroup.names option.
#' mlifeTable(y,X,trans =trans,
#'            groupby = c("male","black","hispanic"),
#'            no_control = "mar",
#'            states=3,
#'            startages=50,
#'            age.gap=1,
#'            file_path=".",
#'            subgroup.names= c("F-W","M-W","M-B","F-B","F-H","M-H"))
#'            
#' # To generate plots, try the mlifeTable_plot option
#' mlifeTable(y,X,trans =trans,
#'            groupby = c("male","black","hispanic"),
#'            no_control = "mar",
#'            states=3,
#'            startages=50,
#'            age.gap=1,
#'            nums = 400,
#'            file_path=".",
#'            subgroup.names= c("F-W","M-W","M-B","F-B","F-H","M-H"),
#'            mlifeTable_plot = T,
#'            cred = 0.84)
#'            
#' # To specify a variable at a fixed value other than the mean value. Try option "values".
#' mlifeTable(y,X,trans =trans,
#'            groupby = c("male","black","hispanic"),
#'            no_control = "mar",
#'            values = list("cohort" = 36),
#'            states=3,
#'            startages=50,
#'            age.gap=1,
#'            nums = 400,
#'            file_path=".",
#'            subgroup.names= c("F-W","M-W","M-B","F-B","F-H","M-H"),
#'            mlifeTable_plot = T,
#'            cred = 0.84)       
#'                            
#' }

mlifeTable <- function(y,X,trans,states,
                       file_path,
                       groupby=NA,
                       no_control = NA,
                       values = NA,
                       status = 0,
                       startages=0,
                       endages=110,
                       age.gap=1,
                       nums = dim(trans)[1],
                       mlifeTable_plot = FALSE,
                       state.names = NA,
                       ...
){
  if (all(colnames(as.data.frame(X)) != "age")){
    stop("Please include a variable named as 'age' in X.",
         call. = FALSE)
  }
  if (any(names(values) %in% no_control)) {
    warning("Please do not fix a variable which is not controlled.")
  }
  ##Subgroup
  age.index <- which(colnames(as.data.frame(X)) %in% "age" ==TRUE)
  value.list <- values
  data <- as.data.frame(X[,-age.index])
  colnames(data) <- colnames(X)[-age.index]
  cols <- colnames(data)
  # no_control <- c(groupby,no_control)
  no_control.group <- groupby
  no_control.other <- setdiff(cols,groupby)
  if(is.null(dim(trans)[1])){
    trans <- as.data.frame(matrix(trans,nrow=1,byrow = TRUE))
  }
  else{trans <- as.data.frame(trans)}
  
  ##Construct index matrix
  if(!is.na(no_control.group[1])){
    if(length(no_control.group)>1){
      data.group.list <- lapply(data[,no_control.group],c)
      index.matrix <- sapply(unique(data[,no_control.group]),as.character)
    }
    else{
      data.group.list <- list(data[,no_control.group])
      names(data.group.list) <- no_control.group
      index.matrix <- as.matrix(sapply(unique(data[,no_control.group]),as.character))
      colnames(index.matrix) <- no_control.group
    }
    
  }
  else{
    data.group.list <- list('a' = rep(1,dim(data)[1]))
    index.matrix <- sapply(expand.grid(list('a' = 1)),as.character)
  }
  
  
  
  
  ##Subgroup means
  data.sub <- list()
  data.sub.sample <- list(tapply(data[,1],data.group.list,length))
  for(i in no_control.other){
    if(i %in% no_control){
      data.sub <- append(data.sub,list(tapply(data[,i],data.group.list,mean)))
    }
    else{
      data.sub <- append(data.sub,list(mean(data[,i])))
    }
    
  }
  names(data.sub) <- no_control.other
  
  ##Construct life tables
  g <- trans
  e <- matrix(NA,nums,states)
  ages <- seq(startages,endages,by=age.gap)
  
  #Create the values for matrix covariates.
  for(index in 1:ifelse(is.null(dim(index.matrix)[1]),1,dim(index.matrix)[1])){
    
    if(is.null(colnames(index.matrix)[1]) | is.na(no_control[1])){
      
      values <- colMeans(data)
      names(values) <- cols
      
      for(i in 1:length(cols)){
        if(cols[i] %in% no_control.group){
          values[i] <- as.numeric(index.matrix[index,cols[i]])
        }
      }
      
    }
    
    #Check the existence of this subgroup
    else if(!is.na(data.sub[[no_control[1]]][t(index.matrix[index,no_control.group])]) |
            !is.null(data.sub[[no_control[1]]][t(index.matrix[index,no_control.group])])){
      
      values <- NULL
      for(i in 1:length(cols)){
        
        #Find the place of this covariate.
        if(cols[i] %in% no_control.other & cols[i] %in% no_control){
          
          values[i] <- data.sub[[cols[i]]][t(index.matrix[index,no_control.group])]
          
        }
        else if(cols[i] %in% no_control.group){
          
          values[i] <- as.numeric(index.matrix[index,cols[i]])

        }
        else{values[i] <- as.numeric(data.sub[[cols[i]]][1])}
        
      }
    }
    else{next}
    names(values) <- cols
    
    if(all(!is.na(names(value.list))) & all(!is.null(names(value.list)))){
      if(!all(names(value.list) %in% cols)){
        stop("Please make sure names of specified variables are consistent with colnames in your dataset.",
             call. = FALSE)
      }
      
      values[names(value.list)] <- as.numeric(value.list)
      
    }
    else if(!(all(is.na(names(value.list))))){ 
      stop("Please specify names for each included variable",
           call. = FALSE)
    }

    #Construct life tables.
    for(reps in 1:nums){
      b <- g[reps,]
      
      for(i in 1:(length(ages))){
        
        covariates <- matrix(c(1,append(values, ages[i], after=(age.index-1)))
                             ,1,length(cols)+2)
        
        # covariates <- matrix(c(append(values, ages[i], after=(age.index-1)),1)
        #                      ,1,length(cols)+2)

        c_length <- ncol(covariates)
        
        xb <- covariates%*%matrix(as.numeric(b),c_length,length(g)/c_length)

        tp <- matrix(0,length(xb)+1)
        denom <- 1+sum(exp(xb),na.rm = T)
        
        for(j in 1:length(xb)){
          tp[j] <- exp(xb[j])/denom
        }
        
        tp[length(xb)+1]=1-sum(tp,na.rm = T)
        
        p=matrix(c(rep(0,states-1),1),states,states,byrow=T)
        
        pos=sort(unique(y))

        #populate transition matrix
        count <- 0
        m <- 1
        for(j in 1:(states-1)){
          for(k in 1:states){
            count=count+1

            if(pos[m]==count | m>length(pos)){
              p[j,k]=ifelse(is.na(tp[m]),
                            ifelse(k==states,10e-10,0)
                            ,tp[m])
              # p[j,k]=tp[m]
              m=m+1
            }
          }
        }
        

        #establish radix
        if(i==1){
          radix=rowSums(p)
          radix[states]=0
          if(status!=0 & is.numeric(status)){
            radix=rep(0,states)
            radix[status]=1
          }
          lx=matrix(radix,length(ages),states,byrow=T)
          
          Lx=Tx=ex=matrix(0,length(ages),states)
          
        }
        
        for(j in 1:(states-1)){
          p[j,]=p[j,]/sum(p[j,],na.rm = T)
        }
        
        
        if(i<length(ages)){
          #generate lx+1 from lx & p
          lx[i+1,]=lx[i,]%*%p
          
          #generate Lx from lx and lx+1
          Lx[i,]=0.5*age.gap*(lx[i,]+lx[(i+1),])
          
        }
       
        ######to close out life table Lx
        if(i==length(ages)){
          
          Lxx=age.gap*lx[(length(ages)),1:(nrow(p)-1)]%*%solve(diag(nrow(p)-1)-p[1:(states-1),1:(states-1)])
          Lx[(length(ages)),]=cbind(Lxx,c(0))
         
          #print(lx)
          # Lx[(length(ages)),]=age.gap*lx[(length(ages)),]%*%solve(p)
          
        }
        
        #lx shows the mortality.
        #close ages loop
        
      }
      
      
      #generate life expectancies
      for(i in 1:(length(ages)-1)){
        Tx[i,]=colSums(Lx[i:(length(ages)),])
        ex[i,]=Tx[i,]/sum(lx[i,1:(ncol(lx)-1)],na.rm = T)
      }
      Tx[(length(ages)),]=Lx[(length(ages)),]
      ex[(length(ages)),]=Tx[(length(ages)),]/sum(lx[(length(ages)),1:(ncol(lx)-1)],na.rm = T)

      #save only e0
      e[reps,]=round(ex[1,],5)
      #print(c(index.matrix[index,],reps))
      
    }
    
    
    tables <- e
    if(!is.na(state.names[1])){
      colnames(tables) <- c(state.names,"death")
    }
    
    utils::write.table(tables,
                       file = paste(paste(file_path,"/lifetable",sep=''),
                                    ifelse(is.null(dim(index.matrix)[1]),
                                           1,
                                           paste(index.matrix[index,],collapse = '')),
                                    ".txt",
                                    sep=""),
                       sep = " ",
                       row.names = FALSE)
    #write(tables,
    #      file = paste(paste(file_path,"/lifetable",sep=''),
    #                   ifelse(is.null(dim(index.matrix)[1]),
    #                          1,
    #                          paste(index.matrix[index,],collapse = '')),
    #                   ".txt",
    #                   sep=""),
    #      sep = " ",
    #      ncolumns=ncol(e))
    
    print(paste("complete subgroup:",
                paste(ifelse(is.null(dim(index.matrix)[1]),
                             1,
                             paste(index.matrix[index,],collapse = '')),
                      collapse = '')))
    if(!is.null(dim(index.matrix)[1])){
      print(paste("subgroup sample size:",
                  data.sub.sample[[1]][t(index.matrix[index,])]))
    }
    else{print(paste("subgroup sample size:",
                     data.sub.sample[[1]][1]))}
    
  }
  
  if(mlifeTable_plot == TRUE & !is.null(dim(index.matrix)[1])){
    mlifeTable_plot(X=X,
          groupby = groupby, 
          states = states,
          file_path = file_path,
          state.names = state.names,
          ...)
  }
  
}




