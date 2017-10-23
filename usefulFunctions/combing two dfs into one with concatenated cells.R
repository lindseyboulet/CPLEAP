# This code allows you to concatenate the character strings of two identical 
# data frames.  This example is taking a data frame of mean values and a df of
# sem values and combining them with a plusorminus symbol into a new df

# Load packages
y <- c("taRifx")

for(i in 1:length(y)){
        is_installed <- function(mypkg){is.element(mypkg,
                                                   installed.packages()[,1])}
        if(!is_installed(y[i])){ 
                install.packages(y[i],
                                 repos="http://lib.stat.cmu.edu/R/CRAN")
        }
        library(y[i], character.only=TRUE,
                quietly=TRUE,verbose=FALSE)
}

# change columns into class = character
df <- japply(df, which(sapply(df, class)=="numeric"), as.character )

# determine column names of interest 
# (in this eg. it's 4-last column ie. excluding 1-3)
col_labs <- colnames(df)[4:ncol(df)]

# create a blank df the size of the other two dfs to be concatenated
concate <- data.frame(matrix(nrow = nrow(df), ncol = (ncol(df)-3)))

# set an index variable since you are looping over a vector of characters
p <- 1

# build a for loop to loop over the column names
for (i in col_labs){
        
        # paste mean column, plusorminus and sem together into a single column
        # in the new df
        concate[,p] <-  paste(poikmean[[i]], "\u00B1", poikse[[i]])  
        
        # increase the index count
        p <- p+1
}
