
meanPlusMinus <- function(df, fixedEffects = NA, twoDecIndex, zeroDecIndex, depVarNames){
    
  df <- df[, which(!is.na(match(colnames(df), c(fixedEffects, depVarNames))))]
  
  factors <- fixedEffects
    y <- c("dplyr", "tidyr", "plotrix") # Load packages
    for(i in 1:length(y)){
          is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
          if(!is_installed(y[i])){
                install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN", dependencies = TRUE)
                }
          library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)
    } 
    
    # Convert character vector to list of symbols
    dots <- lapply(factors, as.symbol)
    dfFactor <- colnames(df)[which(unlist(lapply(df, class)) =="factor")]

    if (!is.na(factors)){
    # Perform frequency counts
    meanIsoHyperData <- df %>%
      group_by_(.dots=dots) %>%
      summarise_all(funs(mean(., na.rm = TRUE)))
    semIsoHyperData <- df %>%
      group_by_(.dots=dots) %>%
      summarise_all(funs(std.error(., na.rm = TRUE)))
    } else {
      meanIsoHyperData <- df %>%
        summarise_all(funs(mean(., na.rm = TRUE)))
      semIsoHyperData <- df %>%
        summarise_all(funs(std.error(., na.rm = TRUE)))
    }
    # opposit of %in%
    '%!in%' <- function(x,y)!('%in%'(x,y))
    # round values
    meanIsoHyperData[,colnames(meanIsoHyperData)%in%zeroDecIndex] <- 
      round(meanIsoHyperData[,colnames(meanIsoHyperData)%in%zeroDecIndex], 0)
    semIsoHyperData[,colnames(semIsoHyperData)%in%zeroDecIndex] <- 
      round(semIsoHyperData[,colnames(semIsoHyperData)%in%zeroDecIndex], 0)
  
    meanIsoHyperData[,colnames(meanIsoHyperData)%in%twoDecIndex] <- 
      round(meanIsoHyperData[,colnames(meanIsoHyperData)%in%twoDecIndex], 2)
    semIsoHyperData[,colnames(semIsoHyperData)%in%twoDecIndex] <- 
      round(semIsoHyperData[,colnames(semIsoHyperData)%in%twoDecIndex], 2)
    
    meanIsoHyperData[,colnames(meanIsoHyperData) %!in%  c(twoDecIndex, zeroDecIndex, factors) ] <- 
      round(meanIsoHyperData[,colnames(meanIsoHyperData) %!in% c(twoDecIndex, zeroDecIndex, factors) ], 1)
    semIsoHyperData[,colnames(semIsoHyperData) %!in%  c(twoDecIndex, zeroDecIndex, factors) ] <- 
      round(semIsoHyperData[,colnames(semIsoHyperData) %!in% c(twoDecIndex, zeroDecIndex, factors)], 1)
  
    # merge mean and sem
    meanNames <- colnames(meanIsoHyperData)[which(unlist(lapply(meanIsoHyperData, class)) =="numeric")]
    
    mergeMeanSem <- merge(meanIsoHyperData, semIsoHyperData, by = factors,
                        suffixes = c(".mean", ".sem"))
    # get mean sem col names
    meanNames <- colnames(mergeMeanSem)[which(unlist(lapply(mergeMeanSem, class)) =="numeric")]
     semNames <- meanNames[((length(meanNames)/2)+1):length(meanNames)]
    meanNames <- meanNames[1:(length(meanNames)/2)]
   
    #loop to concatenate mean and sem into a df
    for (i in seq_len(length(meanNames))){
        x <-  strtrim(meanNames[i], nchar(meanNames[i])-5)
        mergeMeanSem <- unite_(mergeMeanSem, x , c(meanNames[i], semNames[i]), sep = " ± ")
    }
    
    # sort df by factors
   mergeMeanSem <- mergeMeanSem[with(mergeMeanSem, do.call(order,dots)), ]
    
  return(mergeMeanSem)
}

