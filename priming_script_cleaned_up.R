### Import data


library(readxl)
primingt <- read_excel("priming_data.xlsx")
View(primingt)

library(readxl)
PANAS <- read_excel("PANAS.xlsx")


### summary

summary(primingt)


##packages


library(lme4)
library(nlme)
library(effects)
library(ggplot2)
library(psych)
library(tidyverse)
library(dplyr)
library(effectsize)
options(scipen=999)

###transform affect scores

primingt$NA_Log <- log(primingt$N_affect)
primingt$PA_Log <- log(primingt$P_affect)
primingt$WMC_Log <- log(primingt$RS_scr)

###factor labels 

primingt$Gender <- factor(primingt$Gender, levels = c(1,2),
                          labels = c("female", "male"))
primingt$Language <- factor(primingt$Language, levels = c(1,2),
                            labels = c("L1", "L2"))
primingt$Affect_group <- factor(primingt$Affect_group, levels = c(1,2),
                                labels = c("Negative Affect Group", "Positive Affect Group"))


#######get aggregate priming scores

primingt2 <- aggregate(primingt$Outcome, by=list(PPT=primingt$PPT), FUN=mean)
primingt2 <- as.tibble(primingt2)
primingt3 <- primingt[ -c(2) ] 
primingt3 <- primingt3[!duplicated(primingt3$PPT), ] 
primingt4 <- list(primingt2, primingt3) 
primingt5 <- primingt4 %>% reduce(inner_join, by = "PPT")

names(primingt5)[names(primingt5)=="x"] <- "priming_prop"



####aggregate scores based on priming blocks


primingt2 <- aggregate(primingt[, 2], list(primingt$PPT, primingt$Block), mean)
primingt2 <- spread(primingt2, Group.2, Outcome)

names(primingt2)[names(primingt2)=="1"] <- "Block1"
names(primingt2)[names(primingt2)=="2"] <- "Block2"


##correlation between priming blocks

cor1 <- cor.test(primingt2$Block1, primingt2$Block2, method = c("pearson"))

cor1

######descriptive stats

mean(primingt5$Age)
sd(primingt5$Age)

dsum1 <- primingt5 %>% group_by(Language) %>%
  summarise(
            total =n())


dsum1

dsum1 <- primingt5 %>% group_by(Affect_group, Language) %>%
  summarise(
    total =n())


dsum1

####PANAS reliability

library(psych)

###positive affect

myvars <- c("V1", "V3", "V5", "V9", "V10", "V12", "V14", "V16", "V17", "V19")
PANAS_PA <- PANAS[myvars]

alpha(PANAS_PA)

###negative affect

myvars <- c("V2", "V4", "V6", "V7", "V8", "V11", "V13", "V15", "V18", "V20")
PANAS_NA <- PANAS[myvars]

alpha(PANAS_NA)

###descriptives of PANAS scores

dsum1 <- primingt5 %>% group_by(Affect_group) %>%
  summarise(M = mean(N_affect),
            SD = sd(N_affect),
            total =n(),
            CI = 1.96*(SD/(sqrt(total))),
            lowerCI = M-CI,
            upperCI =M+CI)

dsum1

dsum1 <- primingt5 %>% group_by(Affect_group) %>%
  summarise(M = mean(P_affect),
            SD = sd(P_affect),
            total =n(),
            CI = 1.96*(SD/(sqrt(total))),
            lowerCI = M-CI,
            upperCI =M+CI)

dsum1

###MANOVA


man1 <- manova(cbind(N_affect, P_affect) ~ Affect_group, data = primingt5)
summary(man1)
summary.aov(man1)

###mean priming scores

dsum1 <- primingt %>% group_by(Language) %>%
  summarise(M = mean(Outcome),
            SD = sd(Outcome),
            total =n(),
            CI = 1.96*(SD/(sqrt(total))),
            lowerCI = M-CI,
            upperCI =M+CI)


###tests of normality of IVs

library(nortest)

cvm.test(primingt$P_affect)

#null model

m1 <- glm(Outcome ~ 1,
          family = binomial(link="logit"),
          data = primingt)

summary(m1)



#add ppt random intercepts

m2 <- glmer(Outcome ~ 1 + (1| PPT),
            data = primingt,
            family = binomial(link="logit"),
            control = glmerControl(optimizer = "bobyqa"),
            nAGQ = 1)

summary(m2)



#Fit differences differences 

deviance <- 3293.7 - 3151.1 #(m1 - m2 deviance)
1 - pchisq(deviance,2) 

#ICC

library(ICCbin)
iccbin(PPT, Outcome, data = primingt)

#Including both Positive and Negative Affect Scores

m3 <- glmer(Outcome ~ Language  + RS_scr +  NA_Log + PA_Log + Affect_group + Language:RS_scr:NA_Log + Language:RS_scr:PA_Log + (1| PPT),
            data = primingt, family = binomial(link="logit"),
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(m3)


#Including Negative Affect Scores only 


m4 <- glmer(Outcome ~ NA_Log + RS_scr  + Language + NA_Log:RS_scr :Language + Affect_group +  (1| PPT),
            data = primingt, family = binomial(link="logit"),
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(m4)

####plotting 

library(sjPlot)
library(sjmisc)
library(jtools)
library(labelVector)

log1 <- mean(primingt$NA_Log)
sd(primingt$NA_Log)


primingt$WMCSQR <- set_label(primingt$WMCSQR, "Square Root Working Memory Capacity Scores")

figure1 <- plot_model(m4, type = "pred", terms = c("RS_scr", "NA_Log", "Language" ), 
                      colors = "bw", axis.linecolor = "white", legend.title = "Negative Affect 
Score (Log)",
                      title = " ", show.p = TRUE)

figure1


tiff("Figure 2.tiff", units="in", width=6, height=5, res=600) #save to WD
figure1 +     labs(x = "Working Memory Capacity",
                   y = "Probability of Primed Response",
                   colour = "Negative Affect Score (Log)")
dev.off()



#####

library(ggeffects)
library(splines)


mydf <- ggpredict(m4, terms = c("RS_scr", "NA_Log", "Language" ))

plot(mydf)


####

NA_LogL <- primingt$NA_Log - sd(primingt$NA_Log)

primingt %>% 
  ggplot(aes(x = RS_scr, y = Outcome, colour = NA_LogL)) +
  stat_summary(fun = "mean", geom = "pointrange", stat = "identity", position = position_dodge(.9)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", position = position_dodge(.9), width = .4) +
  theme_bw()  +
  facet_wrap(~ Language)


####BOOTSTRAP####

library(boot)

confint(m4,method="boot",nsim=1000,boot.type="perc")

confint(m3,method="boot",nsim=1000,boot.type="perc")


#######



#####bayesian analysis######


library(lme4)
library(nlme)
library(effects)
library(ggplot2)
library(psych)
library(brms)
library(bayesplot)
library(rstanarm)
library(rstan)
library(loo)


get_prior(Outcome ~ NA_Log + RS_scr  + Language + NA_Log:RS_scr :Language + Affect_group +  (1| PPT), data = primingt, family = binomial)

prior <- prior("student_t(3, 0, 2.5)", class = "Intercept") + # intercept
  
  prior("student_t(3, 0, 2.5)", class = "sd") # subject variance / random 


mod1 = brms::brm(
  Outcome ~ NA_Log + RS_scr  + Language + NA_Log*RS_scr*Language + Affect_group +  (1| PPT), 
  data = primingt,
  family=binomial(link= logit),
  prior = prior,
  iter = 3000,
  chains = 4,
  cores = 6,
  control = list(adapt_delta = 0.99),
)

plot(mod1)
figure2 <- plot_model(mod1, type = "pred", terms = c("RS_scr", "NA_Log", "Language" ), 
                      colors = "bw", axis.linecolor = "white", legend.title = "Log10 (NA)",
                      title = " ", ci.lvl = 95, bpe = "mean")


figure3 <- plot_model(mod1, type = "int",terms = c("RS_scr", "NA_Log", "Language"), posterior_epred())
figure3

m1 <- conditional_effects(mod1, nsamples = 200, spaghetti = TRUE)
plot(mod1, ask = FALSE, points = TRUE)
mcmc_plot(mod1)
pp_check(mod1)
launch_shinystan(mod1)



######

"L1" <- subset(primingt5, primingt5$Language=="1")
"L2" <- subset(primingt5, primingt5$Language=="2")



ks.test(L1$RS_scr, L2$RS_scr)
ks.test(L1$NA_Log, L2$NA_Log)
ks.test(L1$PA_Log, L2$PA_Log)

hist(L1$RS_scr)
hist(L2$RS_scr)


library(moments)

kurtosis(L1$RS_scr)
skewness(L1$RS_scr)
kurtosis(L2$RS_scr)
skewness(L2$RS_scr)

kurtosis(L1$NA_Log)
skewness(L1$NA_Log)
kurtosis(L2$NA_Log)
skewness(L2$NA_Log)

kurtosis(L1$Outcome)
skewness(L1$Outcome)
kurtosis(L2$Outcome)
skewness(L2$Outcome)