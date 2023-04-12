###############################################################################
#   ARE 256A - Assignment 2
#   Adelaida Ortega
#   Discussion Session # 6
#   October 14th, 2022
###############################################################################

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



# qpcR is for the RSS function



# Question 4

chow <- read_excel("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 2/Raw Data/Practicum 2 Chow data.xlsx")

# Construct the appropriate variables as in part (a) of Exercise 3, and then 
# estimate parameters in two models, one in which the slope coefficients are 
# constrained to be the same in all years 1960-1965 (a pooled regression) and 
# the other in which these coefficients are allowed to differ (separate, 
# year-by-year regressions).

# Based on the sums of squared residuals from these individual regressions, 
# test the null hypothesis that the slope coefficients are equal over the 
# 1960-1965 time period. Be particularly careful in calculating the appropriate 
# degrees of freedom for the F-test.



# a.i) What is the restricted model ?

chow <- chow %>%
  mutate(logrent = log(RENT)) %>%
  mutate(logmult = log(MULT)) %>%
  mutate(logaccess = log(ACCESS)) %>%
  mutate(logadd = log(ADD)) %>%
  mutate(mem = WORDS*BINARY*DIGITS) %>%
  mutate(logmem = log(mem))
  


# a.1

regname <- lm(logrent ~ logmem + logmult + logaccess + factor(YEAR),
                data = subset(chow, YEAR >= 60 & YEAR <= 65))
summary(regname)



# Lets look at a multivariate linear regression:

#1. How do you interpret the coefficients for the logmult, logaccess and 
#   logmem variables? It is a log-log model

# RENT: Monthly rent of copmuts

# MULT: Time to obtain and complete multiplication instructions

# R/  

# ACCESS: Average time to access information 

# R/

# MEM: The number of equivalent binary digits

# R/

# YEAR FE:

# R/

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

# What is the unrestricted model? How do we interpret this model?






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


#c) In essence, parts (a) and (b) tested for slope parameter stability within 
#   the first and the second generations of computers, respectively. To test 
#   whether the hedonic relationship changed between the first and second 
#   generations, it will be useful to run one additional regression covering the 
#   entire 1954-1965 time period, namely, a specification in which LNRENT is 
#   regressed on a constant, year-specific dummy variables for 1955 through 
#   1965, LNMEM, LNMULT, and LNACCESS. Having run this regression, and initially 
#   assuming equality of the slope parameters within the first (1954-1959) and 
#   the second (1960- 1965) generations, test the null hypothesis that the slope
#   coefficients of the first generation equal those of the second generation. 
#   Does this result surprise you? Why or why not? Next. relax the assumption of
#   slope parameter equality within each generation, and test the null hypothesis 
#   that slope parameters are equal over the entire 1954-1965 time span against 
#   the alternative hypothesis that these slope coefficients varied from year to 
#   year. Note that calculation of the appropriate F-statistic requires comparing 
#   the sums of squared residuals from the 12 separate year-by-year regressions 
#   with that from the pooled 1954-1965 regression and then adjusting by the 
#   appropriate degrees of freedom. Interpret your results. Are the two test 
#   results of part (c) mutually consistent? Why or why not?


# The restricted model 






# The unrestricted model assuming coefficients vary by PERIOD






# The unrestricted model by YEAR





# The F - statistic

