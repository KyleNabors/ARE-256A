###############################################################################
#   ARE 256A - Assignment 2
#   Adelaida Ortega
#   Discussion Session # 6
#   October 14th, 2022
###############################################################################

rm(list=ls())

setwd("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 2")

pacman::p_load(tidyquant, tidyverse, eventstudies, dplyr, haven, data.table,
               kableExtra, car, tidyr, qpcR, readxl)

# qpcR is for the RSS function


# Question 4

chow <- read_excel("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 2/Raw Data/Practicum 2 Chow data.xlsx")
View(chow)

library(readxl)
Practicum_2_Chow_data <- read_excel("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 2/Raw Data/Practicum 2 Chow data.xlsx")
View(Practicum_2_Chow_data)


# Create the variables we need for the model

chow <- chow %>%
        mutate(logrent = log(RENT))       %>%
        mutate(logmult = log(MULT))       %>%
        mutate(logaccess = log(ACCESS))   %>%
        mutate(logadd = log(ADD))         %>%
        mutate(mem = WORDS*BINARY*DIGITS) %>%
        mutate(logmem = log(mem))

# Note in the %>% sequencing you can both create and then call an object (mem)

# a.i) The restricted model 

restricted <- lm(logrent ~ logmult + logaccess + logmem + factor(YEAR),
                 data=subset(chow,YEAR>=60 & YEAR<=65))
summary(restricted)


# Lets look at a multivariate linear regression:

#1. How do you interpret the coefficients for the logmult, logaccess and 
#   logmem variables? It is a log-log model

# RENT: Monthly rent of copmuts

# MULT: Time to obtain and complete multiplication instructions
  
# R/  1% increase in time doing multiplications decreases rent by 0.06%

# ACCESS: Average time to access information 

# R/ 1% increase in time zccessing decreases rent by 0.14%

# MEM: The number of equivalent binary digits

# R/ 1% increase in number of equivalent binary digits increases rent by 0.57%

# YEAR FE:

# R/ Intercept is the mean rent for 1960. Notice Year.60 is not in the FE 
# coefficients; this means its the reference year. The mean rent for
# computers without any feature (mult, access, mem) is -0.10 (hard to interpret).

# For other years its more intuitive: the rent in Year 1961 is 0.13% lower than
# in 1960 for a PC with the same characteristics.

# The F-Statistic :

# What is it testing? 

# H0 : beta_1 = beta_2 = ... = beta_k = 0 
# Ha : At least one beta_j is different than zero.

# Formula :   F = (SSE / DF_sse) / (SSR / DF_ssr)
#             F = ((y^_i - ~y) / number of parameters) / ((y_i - ~y)^2 / N - k - 1)

# What is k here? How many observations do we have? 

# How do we find the critical value? F-Table : F(k , n-k-1)

# http://www.socr.ucla.edu/Applets.dir/F_Table.html
#nobs(restricted)

# Make objects with the SSR and the degrees of freedom 

ssr_r<-RSS(restricted)
ssr_r

df_r<-restricted$rank - 1
df_r


# Run the unrestricted models: each pair coefficient-year can vary 

unrestricted<-lm(logrent ~ factor(YEAR)+factor(YEAR)*logmem +factor(YEAR)*logmult
                         +factor(YEAR)*logaccess+logmem+logmult+logaccess,
                 data=subset(chow,YEAR>=60 & YEAR<=65))
summary(unrestricted)

# How do we interpret this model?

# Make objects with the SSR and the degrees of freedom 

ssr_u<-RSS(unrestricted)
ssr_u

df_u<-unrestricted$rank - 1
df_u

obs <- nobs(restricted)
obs

# Compute the F-test 

# What are we testing in this case?

# H0 : The unrestricted model does not explain the monthly rental of computers

# F test
f <- ((ssr_r - ssr_u) / (df_u - df_r)) / (ssr_u / (obs - df_u - 1))
print(paste0("F = ", f))

print(paste0("p-v = ", 1-pf(f, df_u - df_r, obs - df_u - 1)))

# The F-statistic shows how much of the variance in the dependent variable, 
# the restricted model is not able to explain as compared to the unrestricted
# model relative to the unexplained variance of the unrestricted model.


#b) Repeat part (a) and test the slope coefficients are equal over the 1954 era

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

# c) 

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


###############################################################################

# A note of log-log, linear-log and log-linear models:

# log(y)-log(x) : a 1% increase in X implies a BETA PERCENTAGE y.

# log(y)-lin(y) : a 1 UNIT increase in X implies a (exp(BETA)-1)*100 PERCENTAGE in y

# lin(y)-log(x) : a 1% increase in X implies a (BETA/100) UNIT change in y
