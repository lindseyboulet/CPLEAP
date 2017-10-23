

source("https://gist.github.com/schaunwheeler/5825002/raw/3526a15b032c06392740e20b6c9a179add2cee49/xlsxToR.r")

# Load packages
y <- c( "plyr", "dplyr", "Cairo", "rJava", "XML")

for(i in 1:length(y)){
  is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
  if(!is_installed(y[i])){
    install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN", dependencies = TRUE)
  }
  library(y[i], character.only=TRUE,quietly=TRUE,verbose=FALSE)
}

substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}

# get document info
excelFilnames <- list.files( pattern="*.xlsx", full.names=TRUE)
sheet.names <- getSheets(loadWorkbook(excelFilnames[1]))
sheet.names <- names(sheet.names)

# load excel files
excelFiles <- lapply(excelFilnames, xlsxToR)

# return names of files
bigListNames <-  list.files( pattern="*.xlsx")
gg <- substrRight(bigListNames, nchar(bigListNames)-10)
gg <- strtrim(gg, nchar(gg)-5)
names(excelFiles) <-gg

# apply names to loaded files
for (i in 1:length(excelFiles)){
  names(excelFiles[[i]]) <- sheet.names
}

# flatten the list into a df
allLcFiles <- unlist(excelFiles, recursive=FALSE)
lcFileLabels <- names(allLcFiles)

cnames <- allLcFiles[[1]][2,]
allLcFiles <- lapply(allLcFiles, `colnames<-`,cnames)

# pull particular columns
allLcFiles <- lapply(allLcFiles, "[",-(1:2),) 
allLcFiles <- lapply(allLcFiles, "[",,-4)
