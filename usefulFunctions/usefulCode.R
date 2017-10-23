#---------------------------------------------------------#
#                      Shortcuts                          #
#---------------------------------------------------------#
#                 Collapse — Alt+L                        #
#               Expand — Shift+Alt+L                      #
#               Collapse All — Alt+O                      #
#             Expand All — Shift+Alt+O                    #
#---------------------------------------------------------#

# Load Packages -----------------------------------------------------------
y <- c("tcltk", "plyr", "dplyr", "reshape", "reshape2", "ggplot2", "psych", "Rmisc", "gridExtra", "plotrix", "taRifx", "stargazer", "knitr", "matrixStats", "xtable", "tools", "nlme", "multcomp", "grid", "chron", "RColorBrewer", "pBrackets", "tidyr", "melt")
for(i in 1:length(y)){is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
        if(!is_installed(y[i])){install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN")
        }
        library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)
}


# abs and rel change between variables separated by rows ------------------

df <- ddply(df,.(indepVar1, indepVar2),transform,newColumnName=((depVarColumnName-depVarColumnName[2])/depVarColumnName[2])*100)


# Color Brewer ------------------------------------------------------------

display.brewer.all()

# random color combo generator from a select pallette
colorSelect <- function(n, pal){
        seed <- sample(1:1e6,1)
        set.seed(seed)
        id <- sample(1:n, 1)
        colors <- brewer.pal(n, pal)
        color <- list(id = colors, rand = id, seed = seed)
        color
}

colorSelect(3, "Set1")

# find and replace --------------------------------------------------------
df[df==""]<-NA # individual replacement
# find and replace by groups
tableData$drug <- as.character(c("PBO", "MZ", "AZ"))[ match(tableData$drug, c("A", "B", "C"))] 

# change class of all columns ---------------------------------------------

poik <- japply(poik, which(sapply(poik, class)=="factor"), as.character )
tab_poik <- japply(tab_poik, which(sapply(tab_poik, class)=="factor"), as.character )

poik <- japply(poik, which(sapply(poik, class)=="character"), as.numeric )
tab_poik <- japply(tab_poik, which(sapply(tab_poik, class)=="character"), as.numeric )


# return columns of a certain class ---------------------------------------

dfFactor <- colnames(df)[which(unlist(lapply(df, class)) =="factor")]

# load all files in a dir to a list ----------------------------------------------
filenames <- list.files( pattern="*.txt", full.names=TRUE)
ldf <- lapply(filenames, read.delim)


# remove all rows with any NA value in them -------------------------------

df[complete.cases(df),]


# reshaping data ----------------------------------------------------------

# wide to long

rt.dat.long <- gather(rt.dat, condition, measurement, po2.pbo:pco2.az, factor_key=TRUE)

# long to wide  -- work in progress

rt.dat2 <- melt()


# string trimming ---------------------------------------------------------


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
trtrim(names(list[i]), 19)


# average 3 beats - echo data-----------------------------------------------------------------------

echo_data <- summarise_each(group_by(echo_data, subject, drug, time),
                            funs(mean(., na.rm = TRUE)))


# measuring processing time -----------------------------------------------

# Start the clock!
ptm <- proc.time()

## function here

# Stop the clock
proc.time() - ptm

# using dynamic variables in ddply ----------------------------------------
dots <- lapply(factors, as.symbol)
meanIsoHyperData <- df %>% ### use nonstandard notation with underscore
  group_by_(.dots=dots) %>%
  summarise_all(funs(mean(., na.rm = TRUE)))

# match a vector of names to another --------------------------------------
meanDf <- meanDf[, which(!is.na(match(colnames(meanDf), c(fixedEffects, depVarNames))))]


# match a string by segments of string ------------------------------------
match <- agrep(bind2[p+1,1], bind[,2], max.distance = 0)


# copy files to new location ----------------------------------------------
file.copy(from = paste(getwd(), substrRight(fileNames[a:b],nchar(fileNames[a:b])-1), sep = ""), to = saveName,overwrite = TRUE)



# write to .xlsx file -----------------------------------------------------
write.xlsx("tabledat", filename, row.names = FALSE, col.names = FALSE, showNA = FALSE)



# use string variables in a function that requires no quotes --------------

absChange <- function(df, indepVar1, indepVar2, newName, depVar, varIndex){ # create the function 
  
  allData2 <-  eval(parse(text = paste("ddply(df,.(", indepVar1,",",indepVar2, "),transform, ", newName,
                                       " = ", depVar, "-", depVar, "[",varIndex, "])")))
  return(allData2)
}  

# Load files in a folder to a list and name them --------------------------
      # must set working directory first
fileNames <- list.files( pattern="*.txt", full.names=TRUE)
dataList <- lapply(fileNames, read.delim)
names(dataList) <- list.files( pattern="*.txt")

# opposite of %in% --------------------------------------------------------
'%!in%' <- function(x,y){!('%in%'(x,y))}

# concatenate mean and sem ------------------------------------------------
  
  # get mean sem col names
  meanNames <- colnames(mergeMeanSem)[which(unlist(lapply(mergeMeanSem, class)) =="numeric")]
  semNames <- meanNames[((length(meanNames)/2)+1):length(meanNames)]
  meanNames <- meanNames[1:(length(meanNames)/2)]
  
  #loop to concatenate mean and sem into a df
  for (i in seq_len(length(meanNames))){
    x <-  strtrim(meanNames[i], nchar(meanNames[i])-5)
    mergeMeanSem <- unite_(mergeMeanSem, x , c(meanNames[i], semNames[i]), sep = " ± ")
  }

# load all excel files in a directory into a list -------------------------

  source("https://gist.github.com/schaunwheeler/5825002/raw/3526a15b032c06392740e20b6c9a179add2cee49/xlsxToR.r")
  
  excelFilnames <- list.files( pattern="*.xlsx", full.names=TRUE)
  sheet.names <- getSheets(loadWorkbook(excelFilnames[1]))
  sheet.names <- names(sheet.names)
  
  
  
  excelFiles <- lapply(excelFilnames, xlsxToR)
  
  
  bigListNames <-  list.files( pattern="*.xlsx")
  gg <- substrRight(bigListNames, nchar(bigListNames)-10)
  gg <- strtrim(gg, nchar(gg)-5)
  names(excelFiles) <-gg
  
  for (i in 1:length(excelFiles)){
    names(excelFiles[[i]]) <- sheet.names
  }
  
  allLcFiles <- unlist(excelFiles, recursive=FALSE)
  lcFileLabels <- names(allLcFiles)
  
  cnames <- allLcFiles[[1]][2,]
  allLcFiles <- lapply(allLcFiles, `colnames<-`,cnames)
  
  
# subset from all dfs in a list -------------------------------------------
  allLcFiles <- lapply(allLcFiles, "[",-(1:2),) 
  allLcFiles <- lapply(allLcFiles, "[",,-4)
