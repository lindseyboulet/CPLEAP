


maxminValExtract <- function(list){
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

  
df <- data.frame(matrix(nrow = length(list), ncol = 7))
colnames(df) <- c("id","condition","type", "file", "beat", "maxGlobal", "minGlobal")
df[,5] <- rep(1:3, length(list)/3)



for (i in 1:length(list)){
  df1 <- list[[i]]
  timeRange <- range(df1$time)
  if (df1$type[1] == "sr"){
  
max <- max(df1[df1$measure == "global" & df1$time>timeRange[2]*0.1 & df1$time<timeRange[2]*0.70,3])

indexMax <- which(df1$value == max & df1$measure == "global")
indexGlobal <- which(df1$time == timeRange[2] & df1$measure == "global")

globOne <- df1$value[df1$measure == "global"]
maxInd <- which(globOne == max)
globTwo <- globOne[-1]
globTwo <- c(globTwo, 0)
globThree <- globOne - globTwo
globThree <- globThree[maxInd:length(globThree)]
totalInd <- which(globThree < 0) 
actualInd <- totalInd[1] + maxInd
minVal <- globOne[actualInd-1]



df[i,7] <- minVal
df[i,6] <- max
df[i,4] <- names(list)[i]
str <- strtrim(names(list[i]), 19)
df[i,1] <- substrRight(str,4)
df[i,2] <- df1[1,4]
df[i,3] <- df1[1,5]
  } else{
    max <- max(df1[df1$measure == "global" & df1$time>timeRange[2]*0.1 & df1$time<timeRange*0.70,3])
    df[i,6] <- max
    df[i,4] <- names(list)[i]
    str <- strtrim(names(list[i]), 19)
    df[i,1] <- substrRight(str,4)
    df[i,2] <- df1[1,4]
    df[i,3] <- df1[1,5]
    df <- df[,-7]
  }
  
}
return(df)
}
