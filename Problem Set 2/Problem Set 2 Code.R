
rm(list=ls())

setwd("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 2")

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

#2
data1 <- read.table("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 2/Raw Data/Practicum 2 Waugh Data.txt", header = TRUE, sep = "", dec = ".")

data1 <- data1[, c(4, 1, 2, 3)]

#2.a

reg1 <- lm(PRICE ~ GREEN + NOSTALKS + DISPERSE, data=data1)
summary(reg1)

stargazer(reg1, type = "html", allign = TRUE, no.space = TRUE, out = "Table1.html")

#2.b

pricem <- mean(data1$PRICE)
pricem
greenm <- mean(data1$GREEN)
greenm
nostalksm <- mean(data1$NOSTALKS)
nostalksm
dispersem <- mean(data1$DISPERSE)
dispersem

stargazer(data1, type = "html", align = TRUE, out = "Table2.html")

#2.c

cov(data1)
stargazer(cov(data1), type = "html", align = TRUE, out = "Table3.html")

#2.d

data1$PI <- data1$PRICE * 2.782
regPI <- lm(PI~ GREEN + NOSTALKS + DISPERSE, data = data1)

stargazer(regPI, type = "html",
          summary = FALSE,
          align = TRUE,
          out = "Table9.html")

#3
#3.a

stargazer(cor(data1), type = "html", align = TRUE, out = "Table4.html")

#3.b

regg <- lm(PRICE ~ GREEN, data=data1)
summary(regg)
r2g <- summary(regg)$r.squared
rg <- sqrt(r2g)

regn <- lm(PRICE ~ NOSTALKS, data=data1)
summary(regn)
r2n <- summary(regn)$r.squared
rn <- sqrt(r2n)

regd <- lm(PRICE ~ DISPERSE, data=data1)
summary(regd)
r2d <- summary(regd)$r.squared
rd <- sqrt(r2d)


regg2 <- lm(GREEN ~ PRICE , data=data1)
summary(regg2)
r2g2 <- summary(regg2)$r.squared
rg2 <- sqrt(r2g2)

regn2 <- lm(NOSTALKS ~ PRICE, data=data1)
summary(regn2)
r2n2 <- summary(regn2)$r.squared
rn2 <- sqrt(r2n2)

regd2 <- lm(DISPERSE ~ PRICE, data=data1)
summary(regd2)
r2d2 <- summary(regd2)$r.squared
rd2 <- sqrt(r2d2)


table1 <- data.frame(regression=c("GREEN", "NOSTALKS", "DISPERSE"),
                     "R^2"=c(r2g,r2n, r2d),
                     R=c(rg, rn, rd),
                     "Reverse R^2"=c(r2g2, r2n2, r2d2),
                     "Reverse R"=c(rg2, rn2, rd2))
                     
stargazer(table1, type = "html",summary = FALSE, align = TRUE, out = "Table5.html")

#3.c

regGN <- lm(PRICE ~ GREEN + NOSTALKS, data=data1)
summary(regGN)

regGD <- lm(PRICE ~ GREEN + DISPERSE, data=data1)
  
regND <- lm(PRICE ~ NOSTALKS + DISPERSE, data=data1)

stargazer(regGN, type = "html", allign = TRUE, no.space = TRUE, out = "Table6.html")

stargazer(reg1, regGN, regGD, regND, regg, regn, regd, type = "html", allign = TRUE, no.space = TRUE, out = "Table7.html")

#3.e

pvar <- sum((data1$PRICE - pricem)^2)

Gsum <- sum((data1$GREEN - greenm)*(data1$PRICE - pricem))
Nsum <- sum((data1$NOSTALKS - nostalksm)*(data1$PRICE - pricem))
Dsum <- sum((data1$DISPERSE - dispersem)*(data1$PRICE - pricem))

GB <- summary(reg1)$coefficients["GREEN", "Estimate"]
greend2 <- (GB*(Gsum/pvar))

NB <- summary(reg1)$coefficients["NOSTALKS", "Estimate"]
nostalksd2 <- (NB*(Nsum/pvar))

DB <- summary(reg1)$coefficients["DISPERSE", "Estimate"]
dispersed2 <- (DB*(Dsum/pvar))

D2SUM <- (greend2 + nostalksd2 + dispersed2)
D2WSUM <- (0.40837 + 0.14554 + 0.02133)

table2 <- data.frame("J Variable"=c("GREEN", "NOSTALKS", "DISPERSE"),
                     "D^2"=c(greend2, nostalksd2, dispersed2),
                     "D^2 Sum" = D2SUM,
                     "D^2 Waugh"=c(0.40837, 0.14554, 0.02133),
                     "D^2 Sum W"= D2WSUM)

stargazer(table2, type = "html",
          summary = FALSE,
          align = TRUE,
          out = "Table8.html")

#3.f

reg3 <- lm(PRICE ~ GREEN + NOSTALKS + DISPERSE, data = data1)
summary(reg3)

reg_GP <- lm(PRICE ~ GREEN, data=data1)
reg_NP <- lm(PRICE ~ NOSTALKS, data=data1)
reg_DP <- lm(PRICE ~ DISPERSE, data=data1)

data1$GREENP <- predict(reg_GP, data1)
data1$NOSTALKSP <- predict(reg_NP, data1)
data1$DISPERSEP <- predict(reg_DP, data1)

reg2_2 <- lm(PRICE ~ GREENP + NOSTALKSP + DISPERSEP, data=data1)
summary(reg2_2)

stargazer(reg2_2, reg1, type = "html", allign = TRUE, no.space = TRUE, out = "Table10.html")


#4

chow <- read_excel("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 2/Raw Data/Practicum 2 Chow data.xlsx")

#4.a

chow <- chow %>%
  mutate(logrent = log(RENT)) %>%
  mutate(logmult = log(MULT)) %>%
  mutate(logaccess = log(ACCESS)) %>%
  mutate(logadd = log(ADD)) %>%
  mutate(mem = WORDS*BINARY*DIGITS) %>%
  mutate(logmem = log(mem))

restricted <- lm(logrent ~ logmult + logaccess + logmem + factor(YEAR),
                 data=subset(chow,YEAR>=60 & YEAR<=65))
summary(restricted)

ssr_r<-RSS(restricted)
ssr_r

df_r<-restricted$rank - 1
df_r

unrestricted<-lm(logrent ~ factor(YEAR)+factor(YEAR)*logmem +factor(YEAR)*logmult
                 +factor(YEAR)*logaccess+logmem+logmult+logaccess,
                 data=subset(chow,YEAR>=60 & YEAR<=65))
summary(unrestricted)

ssr_u<-RSS(unrestricted)
ssr_u

df_u<-unrestricted$rank - 1
df_u

obs <- nobs(restricted)
obs

f <- ((ssr_r - ssr_u) / (df_u - df_r)) / (ssr_u / (obs - df_u - 1))
print(paste0("F = ", f))

print(paste0("p-v = ", 1-pf(f, df_u - df_r, obs - df_u - 1)))

stargazer(restricted, unrestricted, f, type = "html", allign = TRUE, no.space = TRUE, out = "Table11.html")

#4.b

rm(df_u, df_r, ssr_r, ssr_u, obs, rss1)

restricted_b <- lm(logrent ~ logmult + logaccess + logmem + factor(YEAR),
                   data=subset(chow,YEAR>=54 & YEAR<=59))
summary(restricted_b)

ssr_r<-RSS(restricted_b)
ssr_r

df_r<-restricted_b$rank - 1
df_r

# Unrestricted
unrestricted_b<-lm(logrent ~ factor(YEAR)+factor(YEAR)*logmem +factor(YEAR)*logmult
                   +factor(YEAR)*logaccess+logmem+logmult+logaccess,
                   data=subset(chow,YEAR>=54 & YEAR<=59))
summary(unrestricted_b)

ssr_u<-RSS(unrestricted_b)
ssr_u

df_u<-unrestricted_b$rank - 1
df_u

obs <- nobs(unrestricted_b)

# F - statistic

f_b <- ((ssr_r - ssr_u) / (df_u - df_r)) / (ssr_u / (obs - df_u - 1))
print(paste0("F = ", f_b))

print(paste0("p-v = ", 1-pf(f_b, df_u - df_r, obs - df_u - 1)))

#4.c

rm(df_u, df_r, ssr_r, ssr_u, obs, rss1)

# The restricted model 

restricted_b <- lm(logrent ~ logmult + logaccess + logmem + factor(YEAR),
                   data=chow)
summary(restricted_b)

ssr_r<-RSS(restricted_b)
ssr_r

df_r<-restricted_b$rank - 1
df_r

# The unrestricted model assuming coefficients vary by period

chow <- chow %>% mutate(period = ifelse(YEAR>=60 & YEAR<=65,1,0))

# ifelse(test, yes, no)

unrestricted_c<-lm(logrent ~ factor(YEAR) + factor(period)*logmem + 
                     factor(period)*logmult + factor(period)*logaccess, 
                   data=chow)
summary(unrestricted_c)

ssr_u<-RSS(unrestricted_c)
ssr_u

df_u<-unrestricted_b$rank - 1
df_u

obs <- nobs(unrestricted_c)

# F - statistic

f_c <- ((ssr_r - ssr_u) / (df_u - df_r)) / (ssr_u / (obs - df_u - 1))
print(paste0("F = ", f_c))

print(paste0("p-v = ", 1-pf(f_c, df_u - df_r, obs - df_u - 1)))

# The unrestricted model. by YEAR

unrestricted_c<-lm(logrent ~ factor(YEAR) + factor(YEAR)*logmem + 
                     factor(YEAR)*logmult + factor(YEAR)*logaccess, 
                   data=chow)
summary(unrestricted_c)

ssr_u<-RSS(unrestricted_c)
ssr_u

df_u<-unrestricted_b$rank - 1
df_u

obs <- nobs(unrestricted_c)

# F - statistic

f_c <- ((ssr_r - ssr_u) / (df_u - df_r)) / (ssr_u / (obs - df_u - 1))
print(paste0("F = ", f_c))

print(paste0("p-v = ", 1-pf(f_c, df_u - df_r, obs - df_u - 1)))


#5 

#5.1

TEST5 <- lm(logrent ~ factor(YEAR) + logmult + logaccess + logmem, data = chow)
 
chow$dum54 <- ifelse(chow$YEAR == '54', 1, 0)
chow$dum55 <- ifelse(chow$YEAR == '55', 1, 0)
chow$dum56 <- ifelse(chow$YEAR == '56', 1, 0)
chow$dum57 <- ifelse(chow$YEAR == '57', 1, 0)
chow$dum58 <- ifelse(chow$YEAR == '58', 1, 0)
chow$dum59 <- ifelse(chow$YEAR == '59', 1, 0)
chow$dum60 <- ifelse(chow$YEAR == '60', 1, 0)
chow$dum61 <- ifelse(chow$YEAR == '61', 1, 0)
chow$dum62 <- ifelse(chow$YEAR == '62', 1, 0)
chow$dum63 <- ifelse(chow$YEAR == '63', 1, 0)
chow$dum64 <- ifelse(chow$YEAR == '64', 1, 0)
chow$dum65 <- ifelse(chow$YEAR == '65', 1, 0)

x1 <- lm(logrent ~ dum54 + logmult + logaccess + logmem, data = chow)
x2 <- lm(logrent ~ dum55 + logmult + logaccess + logmem, data = chow)
x3 <- lm(logrent ~ dum56 + logmult + logaccess + logmem, data = chow)
x4 <- lm(logrent ~ dum57 + logmult + logaccess + logmem, data = chow)
x5 <- lm(logrent ~ dum58 + logmult + logaccess + logmem, data = chow)
x6 <- lm(logrent ~ dum59 + logmult + logaccess + logmem, data = chow)
x7 <- lm(logrent ~ dum60 + logmult + logaccess + logmem, data = chow)
x8 <- lm(logrent ~ dum61 + logmult + logaccess + logmem, data = chow)
x9 <- lm(logrent ~ dum62 + logmult + logaccess + logmem, data = chow)
x10 <- lm(logrent ~ dum63 + logmult + logaccess + logmem, data = chow)
x11 <- lm(logrent ~ dum64 + logmult + logaccess + logmem, data = chow)
x12 <- lm(logrent ~ dum65 + logmult + logaccess + logmem, data = chow)

#stargazer(regdum54, regdum55, regdum56, regdum57, regdum58, type = "html", allign = TRUE, no.space = TRUE, out = "Table12.html")

stargazer(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, type = "html", allign = TRUE, no.space = TRUE, out = "Table13.html")

regALL <- lm(logrent ~ dum55 + dum56 + dum57 + dum58 + dum59 + dum60 + dum61 + dum62 + dum63 + dum64 + dum65 + logmult + logaccess + logmem, data = chow)
summary(regALL)

stargazer(regALL, type = "html", allign = TRUE, no.space = TRUE, out = "Table14.html")

#5.b 

Beta1954<-coef(x1)[2]
Beta1955<-coef(x2)[2]
Beta1956<-coef(x3)[2]
Beta1957<-coef(x4)[2]
Beta1958<-coef(x5)[2]
Beta1959<-coef(x6)[2]
Beta1960<-coef(x7)[2]
Beta1961<-coef(x8)[2]
Beta1962<-coef(x9)[2]
Beta1963<-coef(x10)[2]
Beta1964<-coef(x11)[2]
Beta1965<-coef(x12)[2]



hedCoef54<- coef(regALL)[2]
hedCoef55<- coef(regALL)[3]
hedCoef56<- coef(regALL)[4]
hedCoef57<- coef(regALL)[5]
hedCoef58<- coef(regALL)[6]
hedCoef59<- coef(regALL)[7]
hedCoef60<- coef(regALL)[8]
hedCoef61<- coef(regALL)[9]
hedCoef62<- coef(regALL)[10]
hedCoef63<- coef(regALL)[11]
hedCoef64<- coef(regALL)[12]
hedCoef65<- coef(regALL)[23]

d54<-Beta1954-hedCoef54
d55<-Beta1955-hedCoef55
d56<-Beta1956-hedCoef56
d57<-Beta1957-hedCoef57
d58<-Beta1958-hedCoef58
d59<-Beta1959-hedCoef59
d60<-Beta1960-hedCoef60
d61<-Beta1961-hedCoef61
d62<-Beta1962-hedCoef62
d63<-Beta1963-hedCoef63
d64<-Beta1964-hedCoef64
d65<-Beta1965-hedCoef65

B54i <- exp(Beta1954)
B55i <- exp(Beta1955)
B56i <- exp(Beta1956)
B57i <- exp(Beta1957)
B58i <- exp(Beta1958)
B59i <- exp(Beta1959)
B60i <- exp(Beta1960)
B61i <- exp(Beta1961)
B62i <- exp(Beta1962)
B63i <- exp(Beta1963)
B64i <- exp(Beta1964)
B65i <- exp(Beta1965)


index1<- B54i
index2<- B54i+B55i
index3<- B54i+B55i+B56i
index4<- B54i+B55i+B56i+B57i
index5<- B54i+B55i+B56i+B57i + B58i
index6<- B54i+B55i+B56i+B57i + B58i + B59i
index7<- B54i+B55i+B56i+B57i + B58i + B59i + B60i
index8<- B54i+B55i+B56i+B57i + B58i + B59i + B60i + B61i
index9<- B54i+B55i+B56i+B57i + B58i + B59i + B60i + B61i+ B62i
index10<-B54i+B55i+B56i+B57i + B58i + B59i + B60i + B61i+ B62i + B63i
index11<-B54i+B55i+B56i+B57i + B58i + B59i + B60i + B61i+ B62i + B63i+ B64i
index12<-B54i+B55i+B56i+B57i + B58i + B59i + B60i + B61i+ B62i + B63i +B64i+ B65i

print(B54i)
print(B55i)
print(B56i)
print(B57i)
print(B58i)
print(B59i)
print(B60i)
print(B61i)
print(B62i)
print(B63i)
print(B64i)
print(B65i)


print(index1)
print(index2)
print(index3)
print(index4)
print(index5)
print(index6)
print(index7)
print(index8)
print(index9)
print(index10)
print(index11)
print(index12)



