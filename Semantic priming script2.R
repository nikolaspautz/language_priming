### Import data

library(haven)
primingt <- read_sav("E:/Dropbox/Language differences in semantic priming/primingt.sav")
View(primingt)


##install ML package


library(lme4)
library(nlme)
library(effects)
library(ggplot2)
library(psych)


#null model

m1 <- glm(Outcome ~ 1,
          family = binomial(link="logit"),
          data = primingt)

summary(m1)


#add ppt random intercepts

m2 <- glmer(Outcome ~ 1 + (1| PPT),
            data = priming,
            family = binomial(link="logit"),
            control = glmerControl(optimizer = "bobyqa"),
            nAGQ = 1)

summary(m2)

#Fit differences 

deviance <- 2623.6 - 2520 #(m1 - m2 deviance)
1 - pchisq(deviance,2) 


#ICC

library(ICCbin)
iccbin(PPT, Outcome, data = primingt)

#Including both Positive and Negative Affect Scores

m3 <- glmer(Outcome ~ Language  + RS_scr +  NA_CR + PA_CR + Affect_group + Language:RS_scr:NA_CR + Language:RS_scr:PA_CR + (1| PPT),
            data = primingt, family = binomial(link="logit"),
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(m3)


#Including Negative Affect Scores only 

m4 <- glmer(Outcome ~ Language  + RS_scr +Affect_group+NA_CR + Language:RS_scr:NA_CR + (1| PPT),
            data = primingt, family = binomial(link="logit"),
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(m4)

#With Affect_Group as random effect

m5 <- glmer(Outcome ~ Language  + RS_scr +  NA_CR + Language:RS_scr:NA_CR + (1| PPT) + (1|Affect_group),
            data = primingt, family = binomial(link="logit"),
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(m5)


#Plotting the interaction

library(sjPlot)
library(labelVector)



primingt$WMCSQR <- set_label(primingt$WMCSQR, "Cube Root Working Memory Capacity Scores")

figure1 <- plot_model(m4, type = "pred", terms = c("RS_scr [all]", "NA_CR [2.15, 2.58, 3.01]", "Language" ), 
                      colors = "bw", axis.linecolor = "white", legend.title = "Cube Root NA",
                      title = " ")

tiff("Figure 1.tiff", units="in", width=6, height=5, res=500) #save to WD
figure1 +     labs(x = "Working Memory Capacity",
                   y = "Probability of Primed Response",
                   colour = "Cube Root Negative Affect Score")
dev.off()
