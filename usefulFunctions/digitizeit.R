# Load packages
y <- c("digitize", "RColorBrewer")

for(i in 1:length(y)){
  is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
  if(!is_installed(y[i])){ 
    install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN")
    }
  library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)
    }
# choose image file
plot.file <- file.choose()
# calibrate axes in this order - xmin, xmax, ymin, ymax
cal = ReadAndCal(plot.file)
# select data points of interest
data.points = DigitData(col = 'red')
# calibrate axes - same order as above
df = Calibrate(data.points, cal, 20, 50, 0, 160)
# look at results
head(df)

df$drug <- c(rep("pbo", 45), rep("az",46))
df <- df[-48,]
write.csv(df, "metabolicHyperbole.csv")

hyperbolaData <- data.frame(matrix(c(37, 31, 31, 35,27,27, 100.75, 0, 122.25, 117.25, 0, 154.75), ncol = 2))
hyperbolaData$drug <- c("PLA", "PLA", "PLA", "AZ", "AZ", "AZ")
df1 <- df
df1$y[df1$x > 33] <- df1$y[df$x > 33]-2
df1$y[df1$x > 43] <- df1$y[df$x > 43]-3
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



p1 <- ggplot() + 
  geom_smooth(data = df1, aes(x = x, y = y, color = drug))+
  geom_point(data = hyperbolaData, aes(x=X1, y=X2, color = drug), size = 3) +
  scale_x_continuous(limits = c(20,50)) +
  scale_y_continuous(limits = c(00,170)) + 
  # geom_segment(aes(x = 31, xend = 37, y = 0, yend = 100.75), color = "#377EB8") + 
  geom_segment(aes(x = 27, xend = 35, y = 0, yend = 117.25), color = "#E41A1C") +
  scale_color_manual(values = c("#E41A1C", "#E41A1C","#377EB8",  "#377EB8"),
                     breaks = c("AZ", "PLA")) +
  theme_classic() +
  labs(x = expression(P[ET]*CO[2]), y = expression(V[E]~"(% of resting PBO eupnia)")) +
  theme(legend.title =element_blank())
  

p1
# white = #FFFFFF
c("#E41A1C", "#E41A1C","#377EB8",  "#377EB8")