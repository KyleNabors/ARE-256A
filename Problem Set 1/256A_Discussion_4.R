###############################################################################
#   ARE 256A - Assignment 1
#   Adelaida Ortega
#   Discussion Session # 4
#   October 14th, 2022
###############################################################################

rm(list=ls())
setwd("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 1")


# Make sure you have installed all the packages before calling them in the library

pacman::p_load(tidyquant, tidyverse, eventstudies, dplyr, haven, data.table,
               kableExtra, car, tidyr)

### EVENT STUDIES

# What is an event study? 

# An event study is a regression where we try to see the effect of treatment
# or a shock over time. We will not only see Beta for before and after but 
# between [t-n , t+m], t is the moment of the shock. 

# Where are we going? 




###    1.  Download the stock prices and calculate returns


name <- c("DAL", "UAL", "^GSPC")
yah_fin <- tq_get(name,
                 from = "2019-09-19",
                 to = "2021-10-01",
                 get = "stock.prices")

yah_fin <- yah_fin %>%
  group_by(symbol) %>%
  mutate(lag.close = lag(close)) %>%
  mutate(daily_return = 100.0 * (close - lag.close) / lag.close)
head(yah_fin) %>% kable()




###    2. Define event date and create dataframes that store event date 

event.date <- "2020-03-19"
event.date

# Dataframe with event date
SplitDates <- cbind(c("DAL", "UAL",  "GSPC"), rep(event.date, 1)) %>%
  as.data.frame() %>%
  rename(when = V2)%>%
  rename(name = V1)

### DO NOT CHANGES THIS NAMES!!!! 

view(SplitDates)

# We need this weird dataframe because the zoo package (we´ll talk about it in
# a second) needs this two variable names: when and name (literally those names)




###   3. Set date variable to date format (this orders the variable)

SplitDates$when <- as.Date(SplitDates$when)



###   4. Make dataframe for each stock

delta_SplitDates<-SplitDates %>% filter(name=="DAL")
delta_SplitDates


united_SplitDates<-SplitDates %>% filter(name=="UAL")
united_SplitDates


GSPC_SplitDates<-SplitDates %>% filter(name=="GSPC")
GSPC_SplitDates



###   5. Reshape date to wide format


###############################################################################

# What does this mean?

# This is a Long dataset
long <- read_dta("long_data.dta")
view(long)

# This is a wide dataset 
wide <- read_dta("wide_data.dta")
view(wide)

# How do we go from one format to the other

# Long to wide : pivot_wider     [Stata: reshape wide]

long_to_wide <- pivot_wider(long, names_from = "quarter", names_prefix = "q", 
                            values_from = c(micro,macro,metrics))

view(long_to_wide)


# Wide to long : pivot_longer    [Stata: reshape long] 

# Like ours
wide_to_long <- pivot_longer(wide, !id,  
                              names_to = c(".value", "quarter"), 
                              names_pattern = "^([a-z]+)(\\d+)")
# Fully long
wide_to_long2 <- pivot_longer(wide, !id,  
                             names_to = c("course", "quarter"),
                             names_pattern ="([A-Za-z]+)(\\d+)",
                             values_to = "grade")

wide_to_long2 <- arrange(wide_to_long2, id, quarter, course)

rm(wide, long, wide_to_long, long_to_wide, wide_to_long2)
###############################################################################


# Now reshape our data

PriceReturns <- yah_fin %>%
                pivot_wider(!c(open, high, low, close, volume, adjusted, lag.close), 
              names_from = "symbol", values_from = "daily_return")

head(PriceReturns) %>% kable()



###   6. Again new dataframes with data for each stock: return and date

delta<- PriceReturns %>%
  select(c("date","DAL"))

united<- PriceReturns %>%
  select(c("date","UAL"))

GSPC<- PriceReturns %>%
  select(c("date","^GSPC")) %>%
  rename(GSPC=`^GSPC`)




###   7. Convert dataframes to "zoo" format (eventstudies package)


###############################################################################

# What is the "zoo" format? Z’s Ordered Observations (Z from the author Zeileis)
# It is a format to work with irregular time series i.e. is similar to ts class

###############################################################################


delta_zoo <- read.zoo(delta, drop=FALSE)
head(delta_zoo)

united_zoo <- read.zoo(united, drop=FALSE)
head(united_zoo)

GSPC_zoo <- read.zoo(GSPC, drop=FALSE)


view(delta_zoo)

### BACK TO THE HOMEWORK




### 3.a.i. Plot stocks' price over the three months before and one month after 
#          the COVID


# Subset dates
graph_3_a_i<- yah_fin[yah_fin$date >= "2019-12-19" & yah_fin$date <= "2020-04-19", ]


# Define positions of vline (vertical line)
dates_vline <- as.Date("2020-03-19")  # eventdate
dates_vline <- which(graph_3_a_i$date %in% dates_vline)


graph_3_a_i %>%
  ggplot(aes(x = date, y = close, color = symbol)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~symbol,scales = 'free_y', 
              labeller = labeller(symbol = c("DAL" = "Delta", 
                                             "UAL" = "United",
                                             "^GSPC" = "S&P500")))  +
  theme(strip.background = element_blank())  +
  labs(x = 'Date',
       y = "Close Price",
       title = "Price Chart") +
  scale_x_date(date_breaks = "month",
               date_labels = "%b\n%y")+
  geom_vline(xintercept = as.numeric(graph_3_a_i$date[dates_vline]), color = "black")


### 3.a.ii. Compute the mean return model using a reasonable estimation window, 
#           Compute the abnormal returns during the event window.  


# Here we use 60 days as the event window  

# Convert the delta_zoo to format with eventtime  

delta_SplitDates$name<-as.character(delta_SplitDates$name)

delta_results <- phys2eventtime(
                 z = delta_zoo,
                 events = delta_SplitDates,
                 width = 60)                            # width = event window #

head(delta_results)

# Calculate mean return in estimation window

# constantMeanReturn if a function
?constantMeanReturn

delta_Mean <- constantMeanReturn(delta_results$z.e[which(attributes(delta_results$z.e)$index %in% -120:-30), ], 
                               residual = FALSE)

delta_Mean

# Calculate sd in estimation window : specify sd() at the end

delta_sd <-constantMeanReturn(delta_results$z.e[which(attributes(delta_results$z.e)$index %in% -120:-30), ], 
                           residual = TRUE) %>% sd()
delta_sd


# Calculate the abnormal return: daily return minus the mean for the time window
delta_abn <- delta_results$z.e - delta_Mean

# Set event window
delta_w <- window(delta_abn,
                start = -29,
                end = +30)

head(delta_w)
delta_abn<-delta_w %>% as.data.frame() %>% setDT()

# Create confidence interval for AR: matrix and we add a column each time
delta_abn[,abn_CI_low:=`1`-delta_sd*1.96]
delta_abn[,abn_CI_high:=`1`+delta_sd*1.96]
delta_abn$date <-c(-29:30)

head(delta_abn)

# Plot the abnormal return int he time window

graph_3_a_ii<-ggplot(delta_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = abn_CI_low, ymax = abn_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Abnormal return Delta")

graph_3_a_ii


### a.3.iii Plot cumulative abnormal returns and show the 95% confidence interval.


# Calculate cumulative abnormal returns

# remap: calculates the cumulative sum of a time series

delta_cum <- remap.cumsum(delta_w, base = 0) 
delta_cum<-delta_cum %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

delta_cum[,CAR_CI_low:=CAR-sqrt(60)*delta_sd*1.96]
delta_cum[,CAR_CI_high:=CAR+sqrt(60)*delta_sd*1.96]
delta_cum$date <-c(-29:30)

graph_3_a_iii<-ggplot(delta_cum, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Cumulative Abnormal Returns Delta")
graph_3_a_iii



### b.1. Plot the abnormal returns using the market model


# Convert the GSPC.zoo to format with eventtime  
GSPC_SplitDates$name<-as.character(GSPC_SplitDates$name)


GSPC_results <- phys2eventtime(
                z = GSPC_zoo,
                events = GSPC_SplitDates,
                width = 60) 

?marketModel
# Run OLS for estimation window (-120, -30)

# marketModel: runs a linear model on a zoo environment. Don´t panic...
# ...this is the structure: 

#          marketModel(firm.returns, market.returns, residuals = TRUE)

# With the long lines we are just calling a subset of each of the returns. 

market_model<-marketModel(delta_results$z.e[which(attributes(delta_results$z.e)$index %in% -120:-30), ],
                         GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                         residual = FALSE)
market_model

# Save the coefficients from OLS
alpha_hat<-market_model$coefficients[1]
beta_hat<-market_model$coefficients[2]
alpha_hat
beta_hat

# Save the standard deviation from residuals
AR_resid<-marketModel(delta_results$z.e[which(attributes(delta_results$z.e)$index %in% -120:-30), ],
                      GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                      residual = TRUE) 

# This is the estimator for sd: mean centered second moment / n-2
AR_sd<- sqrt(sum((AR_resid-mean(AR_resid))^2/(length(AR_resid)-2)))
AR_sd


# Calculate the abnormal returns (observed minus predicted)
delta_abn <- delta_results$z.e - alpha_hat - beta_hat*GSPC_results$z.e

# Set event window
delta_w <- window(delta_abn,
                  start = -29,
                  end = +30)

# Convert to data.table format
delta_abn<-delta_w %>% as.data.frame() %>% setDT() 

# Create confidence interval for AR
delta_abn[,AR_CI_low:=`1`-AR_sd*1.96]
delta_abn[,AR_CI_high:=`1`+AR_sd*1.96]

# Add event date column
delta_abn$date <-c(-29:30)

# Plot
graph_3_b_i<-ggplot(delta_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = AR_CI_low, ymax = AR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Delta: Market model of abnorma returns")

graph_3_b_i


### 3.b.ii. Plot the cumulative abnormal returns using a market model over the 
#       week following the event - show the 95% confidence interval.

delta_car <- remap.cumsum(delta_w, base = 0) 

# Convert to data.table format & rename variable
delta_car<-delta_car %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

# Create confidence interval for CAR
delta_car[,CAR_CI_low:=CAR-sqrt(60)*AR_sd*1.96]
delta_car[,CAR_CI_high:=CAR+sqrt(60)*AR_sd*1.96]
delta_car$date <-c(-29:30)

# Plot
graph_3_b_ii<-ggplot(delta_car, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Delta: Market Model of Cumulative Abnormal Returns") 

graph_3_b_ii


