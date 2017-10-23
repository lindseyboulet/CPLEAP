# 1. source this code
# 2. put all figures into a list (eg. plotlist <- list(p1, p2, p3, ...)) - 8 plots max
# 3. run plotPDF(plotList, ncol, filename, width, height)
#       b. ncol: number of columns in your plot grid, grid will be filled in by row left to right
#       c. filename: in quotes with .pdf at the end (eg. "plotGrid.pdf")
#       d. width and height: set width and height of grid in inches, normal page is 8 x 11


plotPDF <- function(plotList, ncol, filename, width, height){
        # Load packages
        y <- c("Cairo", "grid", "gtable")
        for(i in 1:length(y)){
                is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
                if(!is_installed(y[i])){ 
                        install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN", 
                                         dependencies = TRUE)
                }
                library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)
        }
        
        cairo_pdf(file = filename, width = width, height = height)
        
        gpVec <- c("gp1", "gp2", "gp3", "gp4", "gp5", "gp6", "gp7", "gp8")
        p1vec <- c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8")
        plotNum <- length(plotList)
        names(plotList) <- p1vec[1:plotNum]
        args <- ""
        colArgs <- ""
        
        for (i in 1:plotNum){
              eval(parse(text = paste(gpVec[i], "<-ggplot_gtable(ggplot_build(plotList[[i]]))",
                                                      sep = "")))
                args <- paste(args, ", ", gpVec[i], "$widths[2:3]", sep = "")
                colArgs <- paste(colArgs, ", ", gpVec[i], sep = "")
        }
        substrRight <- function(x, n){
                substr(x, nchar(x)-n+1, nchar(x))
        }
        args <- substrRight(args, nchar(args)-2)
        colArgs <- substrRight(colArgs, nchar(colArgs)-2)

        maxWidth = eval(parse(text = paste("unit.pmax(", args, ")", sep = "")))
        gpList <- list()
        
        for (i in 1:plotNum){
                eval(parse(text = paste(gpVec[i], "$widths[2:3]", "<- maxWidth", sep = "")))
                eval(parse(text = paste(gpVec[i], "$heights", "<- gp", plotNum, "$heights", sep = "")))
                gpList[[i]] <- eval(parse(text = gpVec[i]))
                }

        eval(parse(text = paste("grid.arrange(", colArgs, ", ncol = ", ncol, ")", sep = "")))
        
        
        dev.off()
}