
rm(list=ls())

setwd("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 3")

library(tidyquant)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(PerformanceAnalytics)
library(formattable)
library(tidyr)
library(janitor)
library(readxl)
library(writexl)
library(haven)
library(pacman)
library(modelsummary)
library(rbacon)
library(patchwork)
library(EventStudy)
library(kableExtra)
library(dygraphs)
library(xts)
library(latticeExtra)
library(moments)
library(stargazer)
library(eventstudies)
library(car)
library(qpcR)
library(hpiR)
library(fastDummies)
library(stats)

nerlov <- read_excel("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 3/Raw Data/nerlov.xlsx")
update <- read_excel("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 3/Raw Data/update.xlsx")

#2.a

nerlov$LNCP3 <- log(nerlov$COSTS/nerlov$PF)
nerlov$LNP13 <- log(nerlov$PL/nerlov$PF)
nerlov$LNP23 <- log(nerlov$PK/nerlov$PF)
nerlov$LNKWH <- log(nerlov$KWH)

summary(nerlov)

#2.b

reg1 <- lm(LNCP3 ~ LNKWH + LNP13 + LNP23, data = nerlov)
summary(reg1)

#2.c

CI1 <- confint(reg1, "LNKWH", level=.95)

r <- 1/coef(reg1)[2]

#2.d

a1 <- (coef(reg1)[3] * r)
a2 <- (coef(reg1)[4] * r)

#2.e

residualPlot(reg1)

cor(nerlov$LNKWH, reg1$residuals)


#3.a

nerlov <- nerlov %>%
  mutate(
    Sub1 = ifelse(101 <= ORDER & 129 >= ORDER, 1, 0),
    Sub2 = ifelse(201 <= ORDER & 229 >= ORDER, 1, 0),
    Sub3 = ifelse(301 <= ORDER & 329 >= ORDER, 1, 0),
    Sub4 = ifelse(401 <= ORDER & 429 >= ORDER, 1, 0),
    Sub5 = ifelse(501 <= ORDER & 529 >= ORDER, 1, 0)
  )

reg2.1 <- lm(LNCP3 ~ LNKWH + LNP13 + LNP23, data = nerlov, subset = Sub1 > 0.9)
summary(reg2.1)

reg2.2 <- lm(LNCP3 ~ LNKWH + LNP13 + LNP23, data = nerlov, subset = Sub2 > 0.9)
summary(reg2.2)

reg2.3 <- lm(LNCP3 ~ LNKWH + LNP13 + LNP23, data = nerlov, subset = Sub3 > 0.9)
summary(reg2.3)

reg2.4 <- lm(LNCP3 ~ LNKWH + LNP13 + LNP23, data = nerlov, subset = Sub4 > 0.9)
summary(reg2.4)

reg2.5 <- lm(LNCP3 ~ LNKWH + LNP13 + LNP23, data = nerlov, subset = Sub5 > 0.9)
summary(reg2.5)

stargazer(reg2.1, reg2.2, reg2.3, reg2.4, reg2.5, type = "html", out = "Table1.html")

#3.b

r1 <- 1/coef(reg2.1)[2]
r2 <- 1/coef(reg2.2)[2]
r3 <- 1/coef(reg2.3)[2]
r4 <- 1/coef(reg2.4)[2]
r5 <- 1/coef(reg2.5)[2]

stargazer(r1, r2, r3, r4, r5, type = "html", out = "Table2.html")

#3.c

reg3 <- lm(LNCP3 ~ LNKWH + LNP13 + LNP23 +  (Sub2*LNKWH) +  (Sub3*LNKWH) +  (Sub4*LNKWH) +  (Sub5*LNKWH), data = nerlov)
summary(reg3)

# add LNKWH + LNKWN:Subx 

BY1 <- coef(reg3)[2] 
BY2 <- coef(reg3)[2] + coef(reg3)[9]
BY3 <- coef(reg3)[2] + coef(reg3)[10]
BY4 <- coef(reg3)[2] + coef(reg3)[11]
BY5 <- coef(reg3)[2] + coef(reg3)[12]

#4.d


R1 <- 1/BY1
R2 <- 1/BY2
R3 <- 1/BY3
R4 <- 1/BY4
R5 <- 1/BY5

print(c(R1,R2,R3,R4,R5))

stargazer("Returns to Scale", c(R1, R2, R3, R4, R5), rownames = TRUE, type = "html", out = "Table3.html")

#3.e

ARES <- anova(reg2.1, reg2.2, reg2.3, reg2.4, reg2.5)

RSUM <- sum(ARES$RSS)

CRES <- anova(reg3)

RSS3 <- CRES[12, 2]

DF = sum(CRES[,1])

DF1 <- DF+1

K1 <- length(coef(reg3))

FTEST = ((RSS3 - RSUM)/(K1))/((RSUM/(145-(5*K1))))

qf(p=.05,df1=K1,df2=85,lower.tail = FALSE)

#3.f

reg3f <- lm(LNCP3 ~ LNKWH + I(LNKWH^2) + LNP13 + LNP23, data = nerlov)
summary(reg3f)

CI3f1 <- confint(reg3f, "LNKWH", level=.95)
CI3f2 <- confint(reg3f, "I(LNKWH^2)", level=.95)

stargazer(reg3f, type = "html", out = "Table4.html")

#4.a

update$LNC70 <- log(update$COST70/update$PF70)
update$LNY70 <-  log(update$KWH70)
update$LNP170 <- log(update$PL70/update$PF70)
update$LNP270 <- log(update$PK70/update$PF70)

KWH70MEAN <- mean(update$KWH70)
KWNMEAN <- mean(nerlov$KWH)

#4.b

reg4 <- lm(LNC70 ~ LNY70 + LNP170 + LNP270, data = update)
summary(reg4)

stargazer(reg4, type = "html", out = "Table5.html")

CI4 <- confint(reg4, "LNY70", level=.95)

RS <- r <- 1/coef(reg4)[2]

#4.c
reg4b <- lm(LNC70 ~ LNY70 + I(LNY70^2) + LNP170 + LNP270, data = update)
summary(reg4b)

stargazer(reg4b, type = "html", out = "Table6.html")

rgen = 1/(coef(reg4b)[2] + 2*coef(reg4b)[3] * mean(update$LNY70))

CI4b1 <- confint(reg4b, "LNY70", level=.95)
CI4b2 <- confint(reg4b, "I(LNY70^2)", level=.95)



#4.d 


update <- update %>%
  mutate(
    Sub1 = ifelse(1 <= Obs & 20 >= Obs, 1, 0),
    Sub2 = ifelse(21 <= Obs & 40 >= Obs, 1, 0),
    Sub3 = ifelse(41 <= Obs & 60 >= Obs, 1, 0),
    Sub4 = ifelse(61 <= Obs & 80 >= Obs, 1, 0),
    Sub5 = ifelse(81 <= Obs & 99 >= Obs, 1, 0)
  )

reg4.1 <- lm(LNC70 ~ LNY70 + LNP170 + LNP270, data = update, subset = Sub1 > 0.9)
summary(reg4.1)

reg4.2 <- lm(LNC70 ~ LNY70 + LNP170 + LNP270, data = update, subset = Sub2 > 0.9)
summary(reg4.2)

reg4.3 <- lm(LNC70 ~ LNY70 + LNP170 + LNP270, data = update, subset = Sub3 > 0.9)
summary(reg4.3)

reg4.4 <- lm(LNC70 ~ LNY70 + LNP170 + LNP270, data = update, subset = Sub4 > 0.9)
summary(reg4.4)

reg4.5 <- lm(LNC70 ~ LNY70 + LNP170 + LNP270, data = update, subset = Sub5 > 0.9)
summary(reg4.5)

stargazer(reg4.1, reg4.2, reg4.3, reg4.4, reg4.5, type = "html", out = "Table7.html")

r41 <- 1/coef(reg4.1)[2]
r42 <- 1/coef(reg4.2)[2]
r43 <- 1/coef(reg4.3)[2]
r44 <- 1/coef(reg4.4)[2]
r45 <- 1/coef(reg4.5)[2]

stargazer("Returns to Scale", c(r41, r42, r43, r44, r45), rownames = TRUE, type = "html", out = "Table8.html")

#4.e
residualPlot(reg4)


