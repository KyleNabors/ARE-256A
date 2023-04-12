###############################################################################
#   ARE 256A - Assingment 1
#   Adelaida Ortega
#   Discussion Session # 3
#   October 7th, 2022
###############################################################################

# Some new packages you should install

#install.packages("eventstudies")
#install.packages("pacman")
#install.packages("devtools")
#install.packages("kableExtra")
#install.packages("car")

devtools::install_github("nipfpmf/eventstudies", ref="master")

# Pacman es a package for shortcuts. It makes a lot of things easier and has 
# very intuitive options and names. For example:

pacman::p_load(tidyquant, tidyverse, eventstudies, dplyr, haven, data.table, car)

# Here pacman just lets you call into the library many packages in just one line
# We would have wrote:

library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(haven)

# Download the Delta Airlines (DAL) and United Airlines (UAL) stock price 
# using tidyquant:

stocks <- tq_get(c("DAL", "UAL"),
                 from = "2010-01-01",
                 to = "2020-12-31",
                 get = "stock.prices")


# Calculate the daily return of stocks:

stocks <- stocks %>%
  group_by(symbol) %>%
  mutate(lag.close = lag(close)) %>%
  mutate(daily_return = (close - lag.close) / lag.close) %>% 
  rename(company = symbol)


# Download market variables: 

### S&P 500
sp500 <- tq_get("^GSPC",
                 from = "2010-01-01",
                 to = "2020-12-31",
                 get = "stock.prices")

# Daily return of the market
sp500 <- sp500 %>%
         mutate(lag.close = lag(close)) %>%
         mutate(return_market = (close - lag.close) / lag.close)

# Keep only the variables we need
sp500 <- select(sp500, date, return_market)

### Treasury bonds 13 years

tr13 <- tq_get("^IRX",
                from = "2010-01-01",
                to = "2020-12-31",
                get = "stock.prices")

tr13 <- rename(tr13, risk_free = close)

tr13 <- select(tr13, date, risk_free)


# Now me "merge" the three data sets in a way that for each company we have 
# information of the market return and the risk free asset: 

?merge

yah_fin <- merge(x=stocks, y=sp500, 
                 by ="date", 
                 all.x = TRUE, all.y = TRUE )

dplyr::anti_join(stocks, sp500, by = "date")

yah_fin <- merge(x=yah_fin, y=tr13, 
                 by ="date", 
                 all = TRUE)

dplyr::anti_join(yah_fin, tr13, by = "date")

yah_fin <- arrange(yah_fin, company, date)

# Important notes about this function:

# all.x : tells R what to do if it can not match an observation in data "x". 
#         The default if "FALSE" which means that the final output will NOT 
#         have any unmatched observations from x. We don't want to lose data 
#         and we want to know why some variables are not matched so 
#         WE WANT TO KEEP IT: all.x = TRUE

# all.y : same as all.x and we still want to keep all our data : all.y = TRUE

# -> We can also just type: all = TRUE (implies all.x=TRUE, all.y=TRUE)

# Check for missings

is.na(yah_fin)

# Get premiums

yah_fin <- yah_fin %>% 
           mutate(yah_fin, stock_premium = daily_return - risk_free) %>%
           mutate(yah_fin, market_premium = return_market - risk_free)


# Run the CAPM

capm_delta <- lm(stock_premium ~ market_premium, 
           data = subset(yah_fin, company=="DAL"))

summary(capm_delta)

yah_fin$pred_delta <- predict(capm_delta, yah_fin)


capm_united <- lm(stock_premium ~ market_premium, 
                 data = subset(yah_fin, company=="UAL"))
summary(capm_united)

yah_fin$pred_united <- predict(capm_united, yah_fin)



###### How do we get the residuals? What is the definition?


# Line graph 

library(viridis)
library(ggplot2)
library(dplyr)
library(hrbrthemes)

ggplot(yah_fin, aes(x=date, y=stock_premium, group=company, color=company)) + 
       geom_line() +
       ggtitle("Risk premium by company") +
       theme_ipsum() +
       ylab("Risk premium : Daily return - Market return")


