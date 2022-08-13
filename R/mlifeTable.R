#' @title Multistate Life Table Method
#' @description  A Bayesian Multistate Life Table Method for survey data, developed by Lynch and Zang (2022), allowing for large state spaces with quasi-absorbing states (i.e., structural zeros in a transition matrix). This package generates life tables based on the estimates from the Bayesian multinomial logit regressions, which can be obtained using the \code{bayesmlogit()} function. The values in the generated life table represent the expected remaining years to be spent in each state conditional on a give age. Current version was designed to only generate life tables based on data with a death state.
#' @param y A vector of transitions.
#' @param X A matrix of covariates. Note that \code{X} must include age as a convariate.
#' @param trans The posterior samples generated using \code{bayesmlogit()}.
#' @param states The total number of states in the data.
#' @param file_path The file path for outputs.
#' @param groupby A vector that contains the covariates for subgroup comparisons. Default is NA, which means we won't make subgroups.
#' @param vars The covariates considered in subgroup analysis. For covariates that are not specified in vars, we will consider them have the same effect in each subgroup. Please make sure you have specified at least one variable otherwise the life tables would be the same. Default is NA.
#' @param status A numeric value. The option allows producing status-based life tables. Default is 0, produces population-based life tables.
#' @param startages Start age of the life table. Default is 0.
#' @param endages End age of the life table. Default is 110.
#' @param age.gap This option allows users to specify the age interval of the life table. Default is 1. For example, if the survey data were sampled every 2 years, users can specify the age interval to be 2 in the life table.
#' @param nums Number of life tables generated for each subgroup. Default is the size of posterior samples we used.
#' @param mlifeTable_plot If TRUE, this option will create a new directory \code{mplotResults} under given \code{file_path} and output corresponding plots and tables for posterior means and credible intervals. Default is FALSE. 
#' @param state.names A vector used to specify names of each state except death. You can also specify them in the output files.
#' @param ... Extra parameters for \code{mlifeTable_plot()}. See more details using \code{?lifedata}.
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
#'            vars = "mar",
#'            startages=50,
#'            age.gap=1,
#'            states=3,
#'            file_path=".")
#'
#' # To name each subgroup, try the subgroup.names option.
#' mlifeTable(y,X,trans =trans,
#'            groupby = c("male","black","hispanic","other"),
#'            vars = "mar",
#'            states=3,
#'            startages=50,
#'            age.gap=1,
#'            file_path=".",
#'            subgroup.names= c("F-W","M-W","M-B","F-B","F-H","M-H"))
#'            
#' # To generate plots, try the mlifeTable_plot option
#' mlifeTable(y,X,trans =trans,
#'            groupby = c("male","black","hispanic","other"),
#'            vars = "mar",
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
                       vars = NA,
                       status = 0,
                       startages=0,
                       endages=110,
                       age.gap=1,
                       nums = dim(trans)[1],
                       mlifeTable_plot = FALSE,
                       state.names = NA,
                       ...
){
  
  ##Subgroup
  age.index <- which(colnames(as.data.frame(X)) %in% "age" ==TRUE)
  data <- as.data.frame(X[,-age.index])
  cols <- colnames(data)
  vars.group <- groupby
  vars.other <- setdiff(cols,groupby)
  trans <- data.frame(trans)
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
  else{
    data.group.list <- list('a' = rep(1,dim(data)[1]))
    index.matrix <- sapply(expand.grid(list('a' = 1)),as.character)
  }
  
  
  
  
  ##Subgroup means
  data.sub <- list()
  data.sub.sample <- list(tapply(data[,1],data.group.list,length))
  for(i in vars.other){
    if(i %in% vars){
      data.sub <- append(data.sub,list(tapply(data[,i],data.group.list,mean)))
    }
    else{
      data.sub <- append(data.sub,list(mean(data[,i])))
    }
    
  }
  names(data.sub) <- vars.other
  
  ##Construct life tables
  g <- trans
  e <- matrix(NA,nums,states)
  ages <- seq(startages,endages,by=age.gap)
  
  #Create the values for matrix covariates.
  for(index in 1:ifelse(is.null(dim(index.matrix)[1]),1,dim(index.matrix)[1])){
    
    if(is.null(colnames(index.matrix)[1]) | is.na(vars[1])){
      
      values <- colMeans(data)
      
    }
    
    #Check the existence of this subgroup
    else if(!is.na(data.sub[[vars[1]]][t(index.matrix[index,vars.group])]) |
            !is.null(data.sub[[vars[1]]][t(index.matrix[index,vars.group])])){
      
      values <- NULL
      for(i in 1:length(cols)){
        
        #Find the place of this covariate.
        if(cols[i] %in% vars.other & cols[i] %in% vars){
          
          values[i] <- data.sub[[cols[i]]][t(index.matrix[index,vars.group])]
          
        }
        else if(cols[i] %in% vars.group){
          
          values[i] <- as.numeric(index.matrix[index,cols[i]])
          
        }
        else{values[i] <- as.numeric(data.sub[[cols[i]]][1])}
        
      }
    }
    else{next}
    
    #Construct life tables.
    for(reps in 1:nums){
      b <- g[reps,]
      
      
      for(i in 1:(length(ages))){
        
        covariates <- matrix(c(1,append(values, ages[i], after=(age.index-1)))
                             ,1,length(cols)+2)

        c_length <- ncol(covariates)
        
        xb <- covariates%*%matrix(as.numeric(b),c_length,length(g)/c_length)
        
        tp <- matrix(0,length(xb)+1)
        denom <- 1+sum(exp(xb))
        
        for(j in 1:length(xb)){
          tp[j] <- exp(xb[j])/denom
        }
        
        tp[length(xb)+1]=1-sum(tp)
        
        p=matrix(c(rep(0,states-1),1),states,states,byrow=T)
        
        pos=sort(unique(y))

        #populate transition matrix
        count <- 0
        m <- 1
        for(j in 1:(states-1)){
          for(k in 1:states){
            count=count+1

            if(pos[m]==count | m>length(pos)){
              p[j,k]=tp[m]
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
          p[j,]=p[j,]/sum(p[j,])
        }
        
        
        if(i<length(ages)){
          #generate lx+1 from lx & p
          lx[i+1,]=lx[i,]%*%p
          
          #generate Lx from lx and lx+1
          Lx[i,]=lx[i,]+lx[(i+1),]
        }
        
        ######to close out life table Lx
        if(i==length(ages)){
          Lxx=age.gap*lx[(length(ages)),1:(nrow(p)-1)]%*%solve(diag(nrow(p)-1)-p[1:(states-1),1:(states-1)])
          Lx[(length(ages)),]=cbind(Lxx,c(0))
          
          #Lx[(length(ages)),]=age.gap*lx[(length(ages)),]%*%solve(p)
        }
        
        #lx shows the mortality.
        #close ages loop
        
      }
      
      
      #generate life expectancies
      for(i in 1:(length(ages)-1)){
        Tx[i,]=colSums(Lx[i:(length(ages)),])
        ex[i,]=Tx[i,]/sum(lx[i,1:(ncol(lx)-1)])
      }
      Tx[(length(ages)),]=Lx[(length(ages)),]
      ex[(length(ages)),]=Tx[(length(ages)),]/sum(lx[(length(ages)),1:(ncol(lx)-1)])

      #save only e0
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
    print(paste("subgroup sample size:",
                data.sub.sample[[1]][t(index.matrix[index,])]))
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




