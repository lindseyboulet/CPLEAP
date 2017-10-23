# Load packages -----------------------------------------------------------

y <- c("ggplot2", "grid", "gridExtra")
for(i in 1:length(y)){
        is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
        if(!is_installed(y[i])){install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN")
        }
        library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)
}



# Labels ------------------------------------------------------------------


labs(y = expression(Delta~"PASP (mmHg)"), x = "Time (h)") 
labs(title = "D")
ggtitle("A.")


# Plot Axes ---------------------------------------------------------------

scale_x_discrete(labels = c("PLA", "MZ", "AZ"))
scale_x_reverse(limits = c(-5,-40))
        
scale_y_continuous(limits = c(10,40))
scale_y_continuous(breaks = seq(-5,15,5)) 
scale_y_continuous(expand = c(0, 0))

expand_limits(x = 0, y = 0)



scale_shape_discrete(labels = c("PLA", "MZ", "AZ"))
scale_color_discrete(labels = c("PLA", "MZ", "AZ"))



# Theme Options -----------------------------------------------------------

theme(legend.position="none")
theme(legend.title=element_blank(), legend.position="none")
theme(plot.title = element_text(hjust = -.05))
theme(axis.line.y = element_line(),
      text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.02, margin = margin(t = 10, b = -20)),
      axis.text.x = element_text(angle = 45, hjust = 1))



theme(legend.position=c(0.4,0.7),legend.key.width = unit(3, "cm"),  legend.key.height = unit(1.5, "cm")) ## change legend size

guides(lty=guide_legend(override.aes=list(size=1.2))) ## change line size in legend



# Other Geoms -------------------------------------------------------------


geom_errorbar(data = react, aes(ymin = pasp.react - se, ymax = pasp.react + se), width = .25, color = "red") 
geom_errorbarh(aes(xmin = spo2.abs-se.spo2, xmax = spo2.abs+se.spo2, group = drug, height = 1)) 

geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 2, binwidth = .1, fill = NA)

geom_segment(aes(x = x1, y = y1, xend = x2, 
                 yend = y2), lty = 3, data = df)

geom_rect(fill="red", alpha=0.5, xmin=0, xmax=0.72, ymin=8, ymax=45)

stat_summary(fun.y=median, na.rm = TRUE, aes(ymin=..y.., ymax=..y..), 
             geom = 'errorbar', width = 0.5, size = .75)

geom_abline(slope = 1, intercept = 0, color = "blue", lty = 2)

# Saving as files ---------------------------------------------------------
pdf(file = "poster_figs.pdf", res = 108, width = 1000,
    height = 600)

# png(file = "poster_figs.png", res = 108, width = 1000,
    # height = 600)

gp1 <- ggplot_gtable(ggplot_build(p3))
gp2 <- ggplot_gtable(ggplot_build(p4))
gp3 <- ggplot_gtable(ggplot_build(p5))
gp5 <- ggplot_gtable(ggplot_build(p7))

maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3], gp3$widths[2:3], gp5$widths[2:3])


gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
gp3$widths[2:3] <- maxWidth
gp5$widths[2:3] <- maxWidth

gp1$heights <- gp2$heights
gp2$heights <- gp2$heights
gp3$heights <- gp2$heights
gp5$heights <- gp2$heights


gs <- list(gp1, gp2, gp3, gp5)

grid.arrange(gp1, gp2, gp3, gp5, ncol = 2)


dev.off()



geom_text(size = 5, position = position_stack(vjust = 0.5))
annotate("text", x = 15.0, y = 17.25, angle = 90, label = lab)
annotate("text", x = 3, y = 39, label = "†", size = 5)

# Shifting points from ticks on discrete variables ------------------------

geom_point(data = spvrcor.sum, aes(x = as.numeric(drug)+0.15, y = spvrcor.abs, shape = drug),size = 5) +
        geom_errorbar(data = spvrcor.sum, aes(ymax = spvrcor.abs + se, ymin = spvrcor.abs - se, x = as.numeric(drug)+0.15))
        

# Add brackets with annotation custom -------------------------------------
b1 <- bracketsGrob(0.79, 0.463, 0.79, .205, lwd=2, col="black", ticks = NA,  curvature = 0, h = .015)
b2 <- bracketsGrob(0.845, 0.463, 0.845, 0.115, lwd=2, col="black", ticks = NA,  curvature = 0, h = .015)

annotation_custom(b1) +
        annotation_custom(b2)



# Turn discrete scale into continuous for customization -------------------

p2 <- ggplot() + 
        geom_jitter(data = poik, aes(x = as.numeric(drug)-0.15, y = co.abs, shape = drug, color = subject),
                    size = 3, width = 0.13) +
        # geom_label(data = poik, 
        # aes(label=subject, x = as.numeric(drug)-0.22, y = co.abs, fill = subject)) +
        geom_point(data = co.sum, aes(x = as.numeric(drug)+0.15, y = co.abs, shape = drug),
                   size = 3) +
        geom_errorbar(data = co.sum, 
                      aes(ymax = co.abs + se, ymin = co.abs - se, x = as.numeric(drug)+0.15),
                      width = 0.15) + 
        theme_classic(base_size  = 14) +
        labs(y = expression(Delta~"Cardiac Output (l/min)"), x = "") +
        theme(text = element_text(face = "bold"), legend.position="none") +
        ggtitle("B") +
        scale_x_continuous(labels = c("PBO", "MZ", "AZ"), breaks = c(1,2,3), limits=c(0.5,3.5)) +
        scale_y_continuous(limits = c(-1, 3.5), expand = c(0, 0))+
        annotate("text", x = 2, y = 3.3, label = "NSD", size = 4) 
p2   


# annotate with two lines -------------------------------------------------

annotate("text", x = -7.5, y = 2.2, 
         label =paste("atop(' r = '*",round(r,2), "*', P < 0.01'", ", 'y = 0.96x + 0.29')"), size = 5, parse = TRUE)
