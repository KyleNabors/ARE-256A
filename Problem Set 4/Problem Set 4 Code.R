
rm(list=ls())

setwd("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 4")

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
library(plm)
library(tables)
library(AER)
library(strucchange)
library(oaxaca)

CPS85 <- read_excel("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 4/Raw Data/cps85.xlsx")

#2.a 

reg1 <- lm(LNWAGE ~ FE + UNION + NONWH + HISP + ED + EX + EXSQ, data=CPS85)
summary(reg1)

stargazer(reg1, type = "html", out = "Table1.html")

reg1fe <- lm(LNWAGE ~ FE + UNION + ED + EX + EXSQ, data=CPS85)

anova(reg1, reg1fe)

linearHypothesis(reg1, c("NONWH=0", "HISP=0"))
linearHypothesis(reg1, "NONWH=HISP")
linearHypothesis(reg1, "NONWH=0")
linearHypothesis(reg1, "HISP=0")

#2.b

CPS85 <- CPS85 %>%
  mutate(
    NONWHSet = ifelse(1 == NONWH, 1, 0),
    HISPSet = ifelse(1 == HISP, 1, 0),
    OTHERSet = ifelse(0 == NONWH & HISP == 0, 1, 0)
  )

CPSN <- CPS85[CPS85$NONWHSet == 1, ]

CPSH <- CPS85[CPS85$HISPSet == 1, ]

CPSO <- CPS85[CPS85$OTHERSet == 1, ]

stargazer(as.data.frame(CPSN), as.data.frame(CPSH), as.data.frame(CPSO), type = "html", out = "Table2.html")


NONWH <- CPSN %>%
  group_by(NONWHSet) %>%
  summarise_at(vars(c(LNWAGE, FE, UNION, ED, EX, EXSQ)), list(mean))

HISP <- CPSH %>%
  group_by(HISPSet) %>%
  summarise_at(vars(c(LNWAGE, FE, UNION, ED, EX, EXSQ)), list(mean))

OTHER <- CPSO %>%
  group_by(OTHERSet) %>%
  summarise_at(vars(c(LNWAGE, FE, UNION, ED, EX, EXSQ)), list(mean))

NONWH <- NONWH[1,-1]
HISP <- HISP[1,-1]
OTHER <-  OTHER[1,-1]

DIF1 <- OTHER - NONWH
DIF2 <- OTHER - HISP

stargazer(DIF1, DIF2, type = "html", summary = FALSE, out = "Table3.html")

#2.c 

regN <- lm(LNWAGE ~ FE + UNION + ED + EX + EXSQ, data = CPSN)
regH <- lm(LNWAGE ~ FE + UNION + ED + EX + EXSQ, data = CPSH)
regO <- lm(LNWAGE ~ FE + UNION + ED + EX + EXSQ, data = CPSO)

stargazer(regN, regH, regO, type = "html", summary = FALSE, out = "Table4.html")

SE_ON <- sqrt(diag(vcov(regO)) + diag(vcov(regN)))

SE_OH <- sqrt(diag(vcov(regO)) + diag(vcov(regH)))

stargazer(SE_ON, SE_OH, type = "html", summary = FALSE, out = "TableSE.html")


#2.d

reg1d <- lm(LNWAGE ~ FE + UNION + ED + EX + EXSQ, data=CPS85)
summary(reg1d)

RSS1 <- anova(reg1d)["Residuals", "Sum Sq"]
RSSN <- anova(regN)["Residuals", "Sum Sq"]
RSSH <- anova(regH)["Residuals", "Sum Sq"]
RSSO <- anova(regO)["Residuals", "Sum Sq"]

n <- nobs(reg1d)
kplus1 <- length(coef(reg1d))
F_CHOW_num <- (RSS1 - (RSSN + RSSH + RSSO)) / kplus1

F_CHOW_DENOM <- (RSSN + RSSH + RSSO) / (n - 3*kplus1)

F_CHOW <- F_CHOW_num/F_CHOW_DENOM
print(F_CHOW)

qf(p=.05, df1=kplus1, df2 = (n-3*kplus1),lower.tail = FALSE)

sctest(coef(regO) ~ coef(regH), type = "Chow")

#2.e


CPS85 <- CPS85 %>%
  mutate(
    Dis1 = ifelse(0 == OTHERSet & NONWH == 1, 1, 0),
    Dis2 = ifelse(0 == OTHERSet & HISP == 1, 1, 0),
    Dis3 = ifelse(1 == OTHERSet & HISP == 0, 1, 0)
  )

oaxaca.results.1 <- oaxaca(LNWAGE ~ FE + UNION + ED + EX + EXSQ | Dis1, data = CPS85)
#summary(oaxaca.results.1)

print(oaxaca.results.1$y$y.diff)

plot(oaxaca.results.1)

#2.f 

oaxaca.results.2 <- oaxaca(LNWAGE ~ FE + UNION + ED + EX + EXSQ | Dis2, data = CPS85)
#summary(oaxaca.results.2)
print(oaxaca.results.2$y$y.diff)

plot(oaxaca.results.2)

#2.g

oaxaca.results.3 <- oaxaca(LNWAGE ~ FE + UNION + ED + EX + EXSQ | Dis3, data = CPS85)
#summary(oaxaca.results.2)
print(oaxaca.results.3$y$y.diff)

plot(oaxaca.results.3)


#3.a

tradational <- sqrt(diag(vcov(reg1)))
trad <- sqrt(diag(vcovHC(reg1, type = "const")))
HCSE <- sqrt(diag(vcovHC(reg1, type = "HC3")))
compare <- HCSE - trad

stargazer(compare, type = "html", summary = FALSE, out = "Table5.html")

#3.b

reg2 <- lm(LNWAGE ~ FE + UNION + NONWH + HISP + ED + EX + EXSQ, data=CPS85, weights = 1/reg1$fitted.values^2)
summary(reg2)

stargazer(reg1, reg2, type = "html", out = "Table6.html")

#3.c

regc <- lm(resid(reg1)^2 ~ ED +  EX + EXSQ + FE + UNION + NONWH + HISP, data = CPS85)
summary(regc)

regaux <- lm(resid(reg1)^2 ~ fitted(reg1) + I(fitted(reg1)^2))
summary(regaux)

coeftest(regaux)
stargazer(print(coeftest(regaux)), type = "html", summary = FALSE, out = "Table7.html")

alpha <- 0.05

ressq <- resid(reg1)^2

modres <- lm(ressq~ FE + UNION + NONWH + HISP + ED + EX + EXSQ, data=CPS85)
N <- nobs(modres)
gmodres <- glance(modres)
S <- gmodres$df 

chisqcr <- qchisq(1-alpha, S-1)
Rsqres <- gmodres$r.squared
chisq <- N*Rsqres
pval <- 1-pchisq(chisq,S-1)

reg3 <- lm(LNWAGE ~ FE + UNION + NONWH + HISP + ED + EX + EXSQ, data=CPS85, weights = 1/reg1$fitted.values^2)
summary(reg2)

ressq2 <- resid(reg3)^2

modres2 <- lm(ressq~ FE + UNION + NONWH + HISP + ED + EX + EXSQ, data=CPS85, weights = 1/reg1$fitted.values^2)
N2 <- nobs(modres2)
gmodres2 <- glance(modres2)
S2 <- gmodres2$df 

chisqcr2 <- qchisq(1-alpha, S2-1)
Rsqres2 <- gmodres2$r.squared
chisq2 <- N*Rsqres2
pval2 <- 1-pchisq(chisq2,S2-1)

print(chisq2)
print(pval2)
