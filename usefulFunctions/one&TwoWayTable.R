# factor order is important in origindal df for fixedEffects
# order of dep vars in output order

oneAndTwoWayTable <-  function(df, meanDf, fixedEffects, randomEffects, depVarNames){

  # this should go first
  df <- df[, which(!is.na(match(colnames(df), c(fixedEffects, randomEffects, depVarNames))))]
  
  # Load packages
  y <- c("tcltk", "plyr", "dplyr", "lmerTest", "multcomp")
  
  
  
  for(i in seq_len(length(y))){
    is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])
      }
    if(!is_installed(y[i])){ 
      install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN", dependencies = TRUE)
      }
    library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)
    }
  
  substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
  }
  
  fixNum <- length(fixedEffects)
  toRun <- colnames(df)[which(unlist(lapply(df, class)) =="numeric")]
  if (fixNum == 1){
    pVals <- matrix(nrow = length(toRun), ncol = 1)
    colnames(pVals) <- fixedEffects
    rownames(pVals) <- toRun
    sigCodeList <- list()
    for (i in seq_len(length(toRun))){
      fullModel <- paste(toRun[i], " ~ ", fixedEffects,
        " + (1| ", randomEffects, ")", sep = "")
      lmeModFull <- lmer(as.formula(fullModel), data = na.omit(df))
      full <- anova(lmeModFull)
      full$`Pr(>F)` <- round(full$`Pr(>F)`,2)
      sig <- which(full$`Pr(>F)`<0.05)
      full$`Pr(>F)`[sig] <- "< 0.05"
      pVals[i] <- full$`Pr(>F)`
      if (full$`Pr(>F)` < 0.05){
        sumModCoefs <- summary(lmeModFull)$coefficients[2:length(unique(df[, which(!is.na(match(colnames(df), fixedEffects[1])))])),]
        sumModCoefs[,4] <- substrRight(rownames(sumModCoefs), 
                                 (nchar(rownames(sumModCoefs)) - nchar(fixedEffects)))
        colnames(sumModCoefs)[4] <- fixedEffects
        sigCodeList[[i]] <- sumModCoefs[which(as.numeric(sumModCoefs[,5])<0.05),4]
        names(sigCodeList)[i] <- toRun[i] 
        newColNum <-length(unique(df[, which(!is.na(match(colnames(df), fixedEffects)))]))
        factorsOneWay <- unique(df[, which(!is.na(match(colnames(df), fixedEffects)))])
        factorsOneWay <- factorsOneWay[order(factorsOneWay)]
        
        
        }else{sigCodeList[[i]] <- NA}
      
    }
    pVals <- data.frame(c(NA, pVals), c(fixedEffects, depVarNames))
    rownames(pVals) <- c(fixedEffects, depVarNames)
    colnames(pVals) <- c("pval", "factor")
  }
  if (fixNum == 2){
    pVals <- matrix(nrow = length(toRun), ncol = 3)
    colnames(pVals) <- c(fixedEffects, "interaction")
    rownames(pVals) <- toRun
    for (i in seq_len(length(toRun))){
      fullModel <- paste(toRun[i], " ~ ", fixedEffects[1], "*", 
        fixedEffects[2], " + (1| ", randomEffects, ")", sep = "")
      lmeModFull <- lmer(as.formula(fullModel), data = na.omit(df))
      full <- anova(lmeModFull)
      full$`Pr(>F)` <- round(full$`Pr(>F)`,2)
      sig <- which(full$`Pr(>F)`<0.05)
      full$`Pr(>F)`[sig] <- "< 0.05"
      pVals[i,] <- full$`Pr(>F)`
      sigCodeList <- list()
      newColNum <-length(unique(df[, which(!is.na(match(colnames(df), fixedEffects[1])))]))*
        length(unique(df[, which(!is.na(match(colnames(df), fixedEffects[2])))]))
      
      
    }
    pVals <- as.data.frame(cbind(rbind(NA,NA, pVals), c(fixedEffects, depVarNames)))
    rownames(pVals) <- c(fixedEffects, depVarNames)
    colnames(pVals) <- c(fixedEffects,"interaction", "factor")
    }
  
  
  
  meanDf <- as.data.frame(t(meanDf))
  meanDf <- cbind(meanDf, factor = rownames(meanDf))
  meanDf[, ncol(meanDf)] <- factor(meanDf[, ncol(meanDf)], 
                            levels =  c(fixedEffects, depVarNames))
  # meanDf <- meanDf[order(meanDf$factor),]
  pVals$factor <- factor(meanDf[, ncol(meanDf)], 
                         levels =  c(fixedEffects, depVarNames))
  meanDf <- merge(meanDf, pVals, by="factor", all=TRUE)
  meanDf <- meanDf[order(meanDf$factor),]
  meanDf <- data.frame(lapply(meanDf, as.character), stringsAsFactors=FALSE)
  rownames(meanDf) <- meanDf[,1]
  meanDf <- meanDf[,-1]
  if(fixNum == 2){
    meanDf[1,ncol(meanDf):(ncol(meanDf)-2)] <- colnames(meanDf)[ncol(meanDf):(ncol(meanDf)-2)]
  }
  if (fixNum == 1){
    sigCode <- unlist(sigCodeList)
    namesNew <- names(sigCode)
    namesNew[namesNew == ""] <- NA
    bind <- data.frame(cbind( match(sigCode,
                factorsOneWay), namesNew))
    bind[,3] <- bind[,2]
    bind2 <- data.frame(cbind(c(fixedEffects, depVarNames), 1:nrow(meanDf)))
    bind <- bind[which(complete.cases(bind)),]
    bind[,2] <- as.character(bind[,2])
    bind2[, 1] <- as.character(bind2[,1])
    for (p in seq_len(length(depVarNames))){
      match <- agrep(bind2[p+1,1], bind[,2], max.distance = 0)
      if(length(match)>0){ bind[match,2] <- bind2[p+1,1]}
    }
    
    bind[,2] <-  as.character(c(1:nrow(bind2))[match(as.character(bind[,2]), 
                                                    as.character(bind2[,1]))])
    bind[,1] <-  as.numeric(as.character(bind[,1]))
    bind[,2] <-  as.numeric(bind[,2])
   
    for (k in 1:nrow(bind)){
      meanDf[bind[k,2], bind[k,1]] <-  paste(meanDf[bind[k,2], bind[k,1]], "*", sep = "")
    }
   if (fixNum == "2"){pVals[1,1:(ncol(pVals)-1)] <- c(fixedEffects,"interaction")}
    
  }
  return(meanDf)
    }

