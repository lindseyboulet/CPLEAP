lmerMod <- lmer(pasp ~  ve +  pAco2 +chplus + transPAO2 + co +  (1|subject), data = statData)
lmerTest::step(lmerMod)


m1 <- lmer(formula = pasp ~ chplus + transPAO2 + (1 | subject), 
           data = statData)

summary(m1)
predData <- data.frame(matrix(nrow = 4, ncol = 2, data = c(log10(94.1), 45.8, log10(50), 45.8, log10(85.6), 37.2, log10(50), 37.2), byrow = TRUE))
colnames(predData) <- c( "transPAO2","chplus")
predData <- predData[,c(2,1)]


fixedformula <- lm(pasp ~ chplus + transPAO2, data = statData) 
prediction <- predict.lm(fixedformula, predData, se.fit = TRUE)
prediction$se.fit <- prediction$se.fit*3

predAgreementData <- data.frame(cbind("meanPaspEst"=prediction$fit, "sdPaspEst" = prediction$se.fit, 
                                      "meanPaspMeas"=c(20.9,27.2,23.3,34.8), "sdPaspMeas" = c(-2.408376963, -3.8534, 
                                                                                              1.926701571, 4.335078534)))

p1 <- ggplot(predAgreementData, aes(x=meanPaspEst, y = meanPaspMeas)) + geom_point()+
  geom_abline(slope = 1) + scale_x_continuous(limits = c(15,35))  + scale_y_continuous(limits = c(15,35)) + geom_smooth(method = "lm")

p1


k$fitMsgs
m1

r.squaredGLMM(m1)

-28.9*log10(94.1) + (((10^-7.339)*10^9)*-0.26) + 91.8

lm.beta.lmer <- function(mod) {
  b <- fixef(mod)[-1]
  sd.x <- apply(getME(mod,"X")[,-1],2,sd)
  sd.y <- sd(getME(mod,"y"))
  b*sd.x/sd.y
}

lm.beta.lmer(m1)
