

setwd("/Users/kylenabors/Dropbox/UC Davis/Fall 2022/ARE 256A/R Programmes/Problem Set 1")


library(tidyquant)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(PerformanceAnalytics)
library(data.table)
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
#1.a


AAPL <- tq_get("AAPL", from = "2010-01-01", to = "2019-12-31")
GOOGL <- tq_get("GOOGL", from = "2010-01-01", to = "2019-12-31")


XOM <- tq_get("XOM", from = "2010-01-01", to = "2019-12-31")
CVX <- tq_get("CVX", from = "2010-01-01", to = "2019-12-31")


SP <- tq_get("^GSPC", from = "2010-01-01", to = "2019-12-31")
TR <- tq_get("^IRX", from = "2010-01-01", to = "2019-12-31")



AAPL_Ra <- AAPL %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "daily",
    col_rename = "Ra"
  )

AAPL <-
  left_join(
    AAPL,
    AAPL_Ra,
    by = c("date" = "date"),
    copy = FALSE,
    keep = FALSE
  )

AAPL_mean <- AAPL %>%
  group_by(symbol.x) %>%
  tq_performance(Ra = Ra,
                 Rb = NULL,
                 performance_fun = mean)
AAPL <- select(AAPL, symbol.x, date, Ra)

GOOGL_Ra <- GOOGL %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "daily",
    col_rename = "Ra"
  )

GOOGL <-
  left_join(
    GOOGL,
    GOOGL_Ra,
    by = c("date" = "date"),
    copy = FALSE,
    keep = FALSE
  )

GOOGL_mean <- GOOGL %>%
  group_by(symbol.x) %>%
  tq_performance(Ra = Ra,
                 Rb = NULL,
                 performance_fun = mean)
GOOGL <- select(GOOGL, symbol.x, date, Ra)


XOM_Ra <- XOM %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "daily",
    col_rename = "Ra"
  )

XOM <-
  left_join(
    XOM,
    XOM_Ra,
    by = c("date" = "date"),
    copy = FALSE,
    keep = FALSE
  )

XOM_mean <- XOM %>%
  group_by(symbol.x) %>%
  tq_performance(Ra = Ra,
                 Rb = NULL,
                 performance_fun = mean)
XOM <- select(XOM, symbol.x, date, Ra)


CVX_Ra <- CVX %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "daily",
    col_rename = "Ra"
  )

CVX <-
  left_join(
    CVX,
    CVX_Ra,
    by = c("date" = "date"),
    copy = FALSE,
    keep = FALSE
  )

CVX_mean <- CVX %>%
  group_by(symbol.x) %>%
  tq_performance(Ra = Ra,
                 Rb = NULL,
                 performance_fun = mean)
CVX <- select(CVX, symbol.x, date, Ra)


SP_Ra <- SP %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "daily",
    col_rename = "Ra"
  )

SP <-
  left_join(
    SP,
    SP_Ra,
    by = c("date" = "date"),
    copy = FALSE,
    keep = FALSE
  )

SP_mean <- SP %>%
  group_by(symbol.x) %>%
  tq_performance(Ra = Ra,
                 Rb = NULL,
                 performance_fun = mean)
SP <- select(SP, symbol.x, date, Ra)

TR <- TR %>%
  mutate(close = close * 100)

TR <- TR %>%
  mutate(d = ((100 - close) / 100) * (360 / 88))

TR <- TR %>%
  mutate(Ra =  (((1 / (1 - ((d * 88) / 360
  ))) ^ (1 / 88) - 1)) / 100)

TR = na.omit(TR)
TR = TR %>% filter(complete.cases(TR))

colnames(TR)[1] = "symbol.x"

TR_mean <- TR %>%
  group_by(symbol.x) %>%
  tq_performance(Ra = Ra,
                 Rb = NULL,
                 performance_fun = mean)

TR <- select(TR, symbol.x, date, Ra)

TECH <- rbind(AAPL, GOOGL) #Safe Stocks
OIL <- rbind(XOM, CVX)
MARKET <- rbind(TECH, OIL, SP, TR)

Stock_Mean <-
  rbind(AAPL_mean, GOOGL_mean, XOM_mean, CVX_mean, SP_mean, TR_mean)
Stock_Mean %>% select(symbol.x, mean.1)

MARKET <- MARKET %>% filter(date <= "2014-12-31")
AAPL <- AAPL %>% filter(date <= "2014-12-31")
GOOGL <- GOOGL %>% filter(date <= "2014-12-31")
TECH <- TECH %>% filter(date <= "2014-12-31")
XOM <- XOM %>% filter(date <= "2014-12-31")
CVX <- CVX %>% filter(date <= "2014-12-31")
OIL <- OIL %>% filter(date <= "2014-12-31")
SP <- SP %>% filter(date <= "2014-12-31")
TR <- TR %>% filter(date <= "2014-12-31")

#3.b

AAPL_return <- left_join(AAPL, TR, by = c("date" = "date"))
AAPL_return <- AAPL_return %>%
  mutate(returnOLS = Ra.x - Ra.y)

Market_Return <- left_join(SP, TR, by = c("date" = "date"))
Market_Return <- Market_Return %>%
  mutate(returnOLS = Ra.x - Ra.y)

AAPL_CAPM <-
  left_join(AAPL_return, Market_Return, by = c("date" = "date"))

AAPL_CAPM

AAPL_CAPM = na.omit(AAPL_CAPM)
AAPL_CAPM = AAPL_CAPM %>% filter(complete.cases(AAPL_CAPM))

CAPM_AAPL <- lm(AAPL_CAPM$returnOLS.x ~ AAPL_CAPM$returnOLS.y, 
                 data = subset(AAPL_CAPM, symbol.x.x.x=="AAPL"))
summary(CAPM_AAPL)


AAPL_Beta <- coef(CAPM_AAPL)[2]

AAPL_CAPM_Table <- AAPL_CAPM %>%
  group_by(symbol.x.x.x) %>%
  tq_performance(Ra = Ra.x.x,
                 Rb = returnOLS.y,
                 performance_fun = table.CAPM)
AAPL_CAPM_Table

AAPL_CAPM_Table %>% select(symbol.x.x.x, Alpha, Beta)

AAPL_CAPM$premium <- predict.lm(CAPM_AAPL, AAPL_CAPM, type="response")

XOM_return <- left_join(XOM, TR, by = c("date" = "date"))
XOM_return <- XOM_return %>%
  mutate(returnOLS = Ra.x - Ra.y)

XOM_CAPM <-
  left_join(XOM_return, Market_Return, by = c("date" = "date"))

XOM_CAPM = na.omit(XOM_CAPM)
XOM_CAPM = XOM_CAPM %>% filter(complete.cases(XOM_CAPM))

CAPM_XOM <- lm(XOM_return$returnOLS ~ Market_Return$returnOLS)
summary(CAPM_XOM)

XOM_Beta <- coef(CAPM_XOM)[2]

XOM_CAPM_Table <- XOM_CAPM %>%
  group_by(symbol.x.x.x) %>%
  tq_performance(Ra = returnOLS.x,
                 Rb = returnOLS.y,
                 performance_fun = table.CAPM)
XOM_CAPM_Table

XOM_CAPM_Table %>% select(symbol.x.x.x, Alpha, Beta)

#3.c

AAPL_CAPM$AdjP <- (AAPL_CAPM$returnOLS.x - AAPL_CAPM$premium)

x <- AAPL_CAPM$date
var1 <- AAPL_CAPM$returnOLS.x
var2 <- AAPL_CAPM$premium
var3 <- AAPL_CAPM$AdjP
data <- data.frame(x,var1,var2, var3)

xyplot(var1 + var2 + var3 ~ x, data, 
       type = "l", 
       col=c("green", "blue", "red"), 
       lwd=3,
       text = c("Risk Preimum", "Predicted Risk Premium", "Associated Residuals"),
       xlab = "Year 2010 - 2014",
       ylab = "Risk Premium")


#3.d

summary(CAPM_AAPL)
summary(CAPM_XOM)

#3.e

confint(CAPM_AAPL)
confint(CAPM_XOM)

#3.f
AAPL_Risk <- AAPL_CAPM %>%
  group_by(symbol.x.x.x) %>%
  tq_performance(Ra = Ra.x.x,
                 Rb = returnOLS.y,
                 performance_fun = table.SpecificRisk)

XOM_Risk <- XOM_CAPM %>%
  group_by(symbol.x.x.x) %>%
  tq_performance(Ra = Ra.x.x,
                 Rb = returnOLS.y,
                 performance_fun = table.SpecificRisk)

#3.g

Cor_Table <- rbind(AAPL_CAPM_Table, XOM_CAPM_Table)


print(cor(Cor_Table$Beta, Cor_Table[,11]))

#4.c
MSFT <- tq_get("MSFT", from = "2010-01-01", to = "2014-12-31")
PEP <- tq_get("PEP", from = "2010-01-01", to = "2014-12-31")
DAL <- tq_get("DAL", from = "2010-01-01", to = "2014-12-31")

MSFT_Ra <- MSFT %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "monthly",
    col_rename = "Ra"
  )

MSFT <-
  left_join(
    MSFT,
    MSFT_Ra,
    by = c("date" = "date"),
    copy = FALSE,
    keep = FALSE
  )

MSFT = na.omit(MSFT)
MSFT = MSFT %>% filter(complete.cases(MSFT))

PEP_Ra <- PEP %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "monthly",
    col_rename = "Ra"
  )

PEP <-
  left_join(
    PEP,
    PEP_Ra,
    by = c("date" = "date"),
    copy = FALSE,
    keep = FALSE
  )

PEP = na.omit(PEP)
PEP = PEP %>% filter(complete.cases(PEP))

DAL_Ra <- DAL %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "monthly",
    col_rename = "Ra"
  )

DAL <-
  left_join(
    DAL,
    DAL_Ra,
    by = c("date" = "date"),
    copy = FALSE,
    keep = FALSE
  )

DAL = na.omit(DAL)
DAL = DAL %>% filter(complete.cases(DAL))

MSFT$Month <- months(as.Date(MSFT$date))

MSFT$DUMJ <- ifelse(MSFT$Month=='January', 1, 0)
MSFT$DUMJ2 <- ifelse(MSFT$Month=='January', 0, 1)

PEP$Month <- months(as.Date(PEP$date))

PEP$DUMJ <- ifelse(PEP$Month=='January', 1, 0)
PEP$DUMJ2 <- ifelse(PEP$Month=='January', 0, 1)

DAL$Month <- months(as.Date(DAL$date))

DAL$DUMJ <- ifelse(DAL$Month=='January', 1, 0)
DAL$DUMJ2 <- ifelse(DAL$Month=='January', 0, 1)

MSFT_LM <- lm(MSFT$Ra ~ MSFT$DUMJ)
summary(MSFT_LM)

PEP_LM <- lm(PEP$Ra ~ PEP$DUMJ)
summary(PEP_LM)

DAL_LM <- lm(DAL$Ra ~ DAL$DUMJ)
summary(DAL_LM)

#4.d

MSFT <-
  left_join(MSFT, Market_Return, by = c("date" = "date"))

PEP <-
  left_join(PEP, Market_Return, by = c("date" = "date"))

DAL <-
  left_join(DAL, Market_Return, by = c("date" = "date"))

MSFT_LM2 <- lm(MSFT$Ra ~ MSFT$DUMJ + MSFT$returnOLS)
summary(MSFT_LM2)

PEP_LM2 <- lm(PEP$Ra ~ PEP$DUMJ + PEP$returnOLS)
summary(PEP_LM2)

DAL_LM2 <- lm(DAL$Ra ~ DAL$DUMJ + DAL$returnOLS)
summary(DAL_LM2)

#5

MCD <- tq_get("MCD", from = "2019-09-09", to = "2021-10-01")
QSR <- tq_get("QSR", from = "2019-09-19", to = "2021-10-01")
YUM <- tq_get("YUM", from = "2019-09-19", to = "2022-10-14")
COST <- tq_get("COST", from = "2019-09-19", to = "2021-10-01")
WMT <- tq_get("WMT", from = "2019-09-19", to = "2021-10-01")
ZM <- tq_get("ZM", from = "2019-09-19", to = "2021-10-01")
PTON <- tq_get("PTON", from = "2019-09-19", to = "2021-10-01")
GSPC <- tq_get("^GSPC", from = "2019-09-19", to = "2021-10-01")


COVID <- rbind(MCD, QSR, YUM, COST, WMT, ZM, PTON, GSPC)

COVID <- COVID %>%
  group_by(symbol) %>%
  tq_mutate(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "daily",
    col_rename = "Ra"
  )

COVID_Mean <- COVID %>%
  group_by(symbol) %>%
  tq_performance(Ra = Ra,
                 Rb = NULL,
                 performance_fun = mean)

summary(COVID_Mean)

COVID <- COVID %>%
  group_by(symbol) %>%
  mutate(lag.close = lag(close))
#  mutate(daily_return = 100.0 * (close - lag.close) / lag.close)

head(COVID) %>% kable()

event.date <- "2020-03-19"


SplitDates <- cbind(c("MCD", "QSR",  "YUM", "COST", "WMT", "ZM", "PTON", "GSPC"), rep(event.date, 8)) %>%
  as.data.frame() %>%
  rename(when = V2)%>%
  rename(name = V1)

SplitDates$when <- as.Date(SplitDates$when)

MCD_SplitDates<-SplitDates %>% filter(name=="MCD")
MCD_SplitDates

QSR_SplitDates<-SplitDates %>% filter(name=="QSR")
QSR_SplitDates

YUM_SplitDates<-SplitDates %>% filter(name=="YUM")
YUM_SplitDates

COST_SplitDates<-SplitDates %>% filter(name=="COST")
COST_SplitDates

WMT_SplitDates<-SplitDates %>% filter(name=="WMT")
WMT_SplitDates

ZM_SplitDates<-SplitDates %>% filter(name=="ZM")
ZM_SplitDates

PTON_SplitDates<-SplitDates %>% filter(name=="PTON")
PTON_SplitDates

GSPC_SplitDates<-SplitDates %>% filter(name=="GSPC")
GSPC_SplitDates


PriceReturns <- COVID %>%
  pivot_wider(!c(open, high, low, close, volume, adjusted, lag.close), 
              names_from = "symbol", values_from = "Ra")

head(PriceReturns) %>% kable()


MCD2<- PriceReturns %>%
  select(c("date","MCD"))

QSR2<- PriceReturns %>%
  select(c("date","QSR"))

YUM2<- PriceReturns %>%
  select(c("date","YUM"))

COST2<- PriceReturns %>%
  select(c("date","COST"))

WMT2<- PriceReturns %>%
  select(c("date","WMT"))

ZM2<- PriceReturns %>%
  select(c("date","ZM"))

PTON2<- PriceReturns %>%
  select(c("date","PTON"))

GSPC2<- PriceReturns %>%
  select(c("date","^GSPC")) %>%
  rename(GSPC=`^GSPC`)

MCD_zoo <- read.zoo(MCD2,drop=FALSE)
head(MCD_zoo)

QSR_zoo <- read.zoo(QSR2,drop=FALSE)
head(QSR_zoo)

YUM_zoo <- read.zoo(YUM2,drop=FALSE)
head(YUM_zoo)

COST_zoo <- read.zoo(COST2,drop=FALSE)
head(COST_zoo)

WMT_zoo <- read.zoo(WMT2,drop=FALSE)
head(WMT_zoo)

ZM_zoo <- read.zoo(ZM2,drop=FALSE)
head(ZM_zoo)

PTON_zoo <- read.zoo(PTON2,drop=FALSE)
head(PTON_zoo)

GSPC_zoo <- read.zoo(GSPC2,drop=FALSE)

# Subset dates
graph_3_a_i<- COVID[COVID$date >= "2019-12-19" & COVID$date <= "2020-04-19", ]


# Define positions of vline (vertical line)
dates_vline <- as.Date("2020-03-19")  # eventdate
dates_vline <- which(graph_3_a_i$date %in% dates_vline)


graph_3_a_i %>%
  ggplot(aes(x = date, y = close, color = symbol)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~symbol,scales = 'free_y',)  +
  theme(strip.background = element_blank())  +
  labs(x = 'Date',
       y = "Close Price",
       title = "Price Chart") +
  scale_x_date(date_breaks = "month",
               date_labels = "%b\n%y")+
  geom_vline(xintercept = as.numeric(graph_3_a_i$date[dates_vline]), color = "black")




MCD_SplitDates<-SplitDates %>% filter(name=="MCD")
MCD_SplitDates
GSPC_SplitDates<-SplitDates %>% filter(name=="GSPC")
GSPC_SplitDates




# Convert the MCD_zoo to format with eventtime  

MCD_SplitDates$name<-as.character(MCD_SplitDates$name)

MCD_results <- phys2eventtime(
  z = MCD_zoo,
  events = MCD_SplitDates,
  width = 60)                            # width = event window #

head(MCD_results)

# Calculate mean return in estimation window

# constantMeanReturn if a function
?constantMeanReturn

MCD_Mean <- constantMeanReturn(MCD_results$z.e[which(attributes(MCD_results$z.e)$index %in% -120:-30), ], 
                                 residual = FALSE)

MCD_Mean

# Calculate sd in estimation window : specify sd() at the end

MCD_sd <-constantMeanReturn(MCD_results$z.e[which(attributes(MCD_results$z.e)$index %in% -120:-30), ], 
                              residual = TRUE) %>% sd()
MCD_sd


# Calculate the abnormal return: daily return minus the mean for the time window
MCD_abn <- MCD_results$z.e - MCD_Mean

# Set event window
MCD_w <- window(MCD_abn,
                  start = -29,
                  end = +30)

head(MCD_w)
MCD_abn<-MCD_w %>% as.data.frame() %>% setDT()

# Create confidence interval for AR: matrix and we add a column each time
MCD_abn[,abn_CI_low:=`1`-MCD_sd*1.96]
MCD_abn[,abn_CI_high:=`1`+MCD_sd*1.96]
MCD_abn$date <-c(-29:30)

head(MCD_abn)

# Plot the abnormal return int he time window

graph_3_a_ii<-ggplot(MCD_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = abn_CI_low, ymax = abn_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Abnormal return MCD")

graph_3_a_ii


### a.3.iii Plot cumulative abnormal returns and show the 95% confidence interval.


# Calculate cumulative abnormal returns

# remap: calculates the cumulative sum of a time series

MCD_cum <- remap.cumsum(MCD_w, base = 0) 
MCD_cum<-MCD_cum %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

MCD_cum[,CAR_CI_low:=CAR-sqrt(60)*MCD_sd*1.96]
MCD_cum[,CAR_CI_high:=CAR+sqrt(60)*MCD_sd*1.96]
MCD_cum$date <-c(-29:30)

graph_3_a_iii<-ggplot(MCD_cum, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Cumulative Abnormal Returns MCD")
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

market_model<-marketModel(MCD_results$z.e[which(attributes(MCD_results$z.e)$index %in% -120:-30), ],
                          GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                          residual = FALSE)
market_model

# Save the coefficients from OLS
alpha_hat<-market_model$coefficients[1]
beta_hat<-market_model$coefficients[2]
alpha_hat
beta_hat

# Save the standard deviation from residuals
AR_resid<-marketModel(MCD_results$z.e[which(attributes(MCD_results$z.e)$index %in% -120:-30), ],
                      GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                      residual = TRUE) 

# This is the estimator for sd: mean centered second moment / n-2
AR_sd<- sqrt(sum((AR_resid-mean(AR_resid))^2/(length(AR_resid)-2)))
AR_sd


# Calculate the abnormal returns (observed minus predicted)
MCD_abn <- MCD_results$z.e - alpha_hat - beta_hat*GSPC_results$z.e

# Set event window
MCD_w <- window(MCD_abn,
                  start = -29,
                  end = +30)

# Convert to data.table format
MCD_abn<-MCD_w %>% as.data.frame() %>% setDT() 

# Create confidence interval for AR
MCD_abn[,AR_CI_low:=`1`-AR_sd*1.96]
MCD_abn[,AR_CI_high:=`1`+AR_sd*1.96]

# Add event date column
MCD_abn$date <-c(-29:30)

# Plot
graph_3_b_i<-ggplot(MCD_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = AR_CI_low, ymax = AR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("MCD: Market model of abnorma returns")

graph_3_b_i


### 3.b.ii. Plot the cumulative abnormal returns using a market model over the 
#       week following the event - show the 95% confidence interval.

MCD_car <- remap.cumsum(MCD_w, base = 0) 

# Convert to data.table format & rename variable
MCD_car<-MCD_car %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

# Create confidence interval for CAR
MCD_car[,CAR_CI_low:=CAR-sqrt(60)*AR_sd*1.96]
MCD_car[,CAR_CI_high:=CAR+sqrt(60)*AR_sd*1.96]
MCD_car$date <-c(-29:30)

# Plot
graph_3_b_ii<-ggplot(MCD_car, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("MCD: Market Model of Cumulative Abnormal Returns") 

graph_3_b_ii


# Convert the QSR_zoo to format with eventtime  

QSR_SplitDates$name<-as.character(QSR_SplitDates$name)

QSR_results <- phys2eventtime(
  z = QSR_zoo,
  events = QSR_SplitDates,
  width = 60)                            # width = event window #

head(QSR_results)

# Calculate mean return in estimation window

# constantMeanReturn if a function
?constantMeanReturn

QSR_Mean <- constantMeanReturn(QSR_results$z.e[which(attributes(QSR_results$z.e)$index %in% -120:-30), ], 
                               residual = FALSE)

QSR_Mean

# Calculate sd in estimation window : specify sd() at the end

QSR_sd <-constantMeanReturn(QSR_results$z.e[which(attributes(QSR_results$z.e)$index %in% -120:-30), ], 
                            residual = TRUE) %>% sd()
QSR_sd


# Calculate the abnormal return: daily return minus the mean for the time window
QSR_abn <- QSR_results$z.e - QSR_Mean

# Set event window
QSR_w <- window(QSR_abn,
                start = -29,
                end = +30)

head(QSR_w)
QSR_abn<-QSR_w %>% as.data.frame() %>% setDT()

# Create confidence interval for AR: matrix and we add a column each time
QSR_abn[,abn_CI_low:=`1`-QSR_sd*1.96]
QSR_abn[,abn_CI_high:=`1`+QSR_sd*1.96]
QSR_abn$date <-c(-29:30)

head(QSR_abn)

# Plot the abnormal return int he time window

graph_3_a_ii<-ggplot(QSR_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = abn_CI_low, ymax = abn_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Abnormal return QSR")

graph_3_a_ii


### a.3.iii Plot cumulative abnormal returns and show the 95% confidence interval.


# Calculate cumulative abnormal returns

# remap: calculates the cumulative sum of a time series

QSR_cum <- remap.cumsum(QSR_w, base = 0) 
QSR_cum<-QSR_cum %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

QSR_cum[,CAR_CI_low:=CAR-sqrt(60)*QSR_sd*1.96]
QSR_cum[,CAR_CI_high:=CAR+sqrt(60)*QSR_sd*1.96]
QSR_cum$date <-c(-29:30)

graph_3_a_iii<-ggplot(QSR_cum, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Cumulative Abnormal Returns QSR")
graph_3_a_iii



### b.1. Plot the abnormal returns using the market model


# Convert the GSPC.zoo to format with eventtime  
GSPC_SplitDates$name<-as.character(GSPC_SplitDates$name)


GSPC_results <- phys2eventtime(
  z = GSPC_zoo,
  events = GSPC_SplitDates,
  width = 60) 

# Run OLS for estimation window (-120, -30)

# marketModel: runs a linear model on a zoo environment. Don´t panic...
# ...this is the structure: 

#          marketModel(firm.returns, market.returns, residuals = TRUE)

# With the long lines we are just calling a subset of each of the returns. 

market_model<-marketModel(QSR_results$z.e[which(attributes(QSR_results$z.e)$index %in% -120:-30), ],
                          GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                          residual = FALSE)
market_model

# Save the coefficients from OLS
alpha_hat<-market_model$coefficients[1]
beta_hat<-market_model$coefficients[2]
alpha_hat
beta_hat

# Save the standard deviation from residuals
AR_resid<-marketModel(QSR_results$z.e[which(attributes(QSR_results$z.e)$index %in% -120:-30), ],
                      GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                      residual = TRUE) 

# This is the estimator for sd: mean centered second moment / n-2
AR_sd<- sqrt(sum((AR_resid-mean(AR_resid))^2/(length(AR_resid)-2)))
AR_sd


# Calculate the abnormal returns (observed minus predicted)
QSR_abn <- QSR_results$z.e - alpha_hat - beta_hat*GSPC_results$z.e

# Set event window
QSR_w <- window(QSR_abn,
                start = -29,
                end = +30)

# Convert to data.table format
QSR_abn<-QSR_w %>% as.data.frame() %>% setDT() 

# Create confidence interval for AR
QSR_abn[,AR_CI_low:=`1`-AR_sd*1.96]
QSR_abn[,AR_CI_high:=`1`+AR_sd*1.96]

# Add event date column
QSR_abn$date <-c(-29:30)

# Plot
graph_3_b_i<-ggplot(QSR_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = AR_CI_low, ymax = AR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("QSR: Market model of abnorma returns")

graph_3_b_i


### 3.b.ii. Plot the cumulative abnormal returns using a market model over the 
#       week following the event - show the 95% confidence interval.

QSR_car <- remap.cumsum(QSR_w, base = 0) 

# Convert to data.table format & rename variable
QSR_car<-QSR_car %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

# Create confidence interval for CAR
QSR_car[,CAR_CI_low:=CAR-sqrt(60)*AR_sd*1.96]
QSR_car[,CAR_CI_high:=CAR+sqrt(60)*AR_sd*1.96]
QSR_car$date <-c(-29:30)

# Plot
graph_3_b_ii<-ggplot(QSR_car, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("QSR: Market Model of Cumulative Abnormal Returns") 

graph_3_b_ii

# Convert the ZM_zoo to format with eventtime  

ZM_SplitDates$name<-as.character(ZM_SplitDates$name)

ZM_results <- phys2eventtime(
  z = ZM_zoo,
  events = ZM_SplitDates,
  width = 60)                            # width = event window #

head(ZM_results)

# Calculate mean return in estimation window

# constantMeanReturn if a function
?constantMeanReturn

ZM_Mean <- constantMeanReturn(ZM_results$z.e[which(attributes(ZM_results$z.e)$index %in% -120:-30), ], 
                              residual = FALSE)

ZM_Mean

# Calculate sd in estimation window : specify sd() at the end

ZM_sd <-constantMeanReturn(ZM_results$z.e[which(attributes(ZM_results$z.e)$index %in% -120:-30), ], 
                           residual = TRUE) %>% sd()
ZM_sd


# Calculate the abnormal return: daily return minus the mean for the time window
ZM_abn <- ZM_results$z.e - ZM_Mean

# Set event window
ZM_w <- window(ZM_abn,
               start = -29,
               end = +30)

head(ZM_w)
ZM_abn<-ZM_w %>% as.data.frame() %>% setDT()

# Create confidence interval for AR: matrix and we add a column each time
ZM_abn[,abn_CI_low:=`1`-ZM_sd*1.96]
ZM_abn[,abn_CI_high:=`1`+ZM_sd*1.96]
ZM_abn$date <-c(-29:30)

head(ZM_abn)

# Plot the abnormal return int he time window

graph_3_a_ii<-ggplot(ZM_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = abn_CI_low, ymax = abn_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Abnormal return ZM")

graph_3_a_ii


### a.3.iii Plot cumulative abnormal returns and show the 95% confidence interval.


# Calculate cumulative abnormal returns

# remap: calculates the cumulative sum of a time series

ZM_cum <- remap.cumsum(ZM_w, base = 0) 
ZM_cum<-ZM_cum %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

ZM_cum[,CAR_CI_low:=CAR-sqrt(60)*ZM_sd*1.96]
ZM_cum[,CAR_CI_high:=CAR+sqrt(60)*ZM_sd*1.96]
ZM_cum$date <-c(-29:30)

graph_3_a_iii<-ggplot(ZM_cum, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Cumulative Abnormal Returns ZM")
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

market_model<-marketModel(ZM_results$z.e[which(attributes(ZM_results$z.e)$index %in% -120:-30), ],
                          GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                          residual = FALSE)
market_model

# Save the coefficients from OLS
alpha_hat<-market_model$coefficients[1]
beta_hat<-market_model$coefficients[2]
alpha_hat
beta_hat

# Save the standard deviation from residuals
AR_resid<-marketModel(ZM_results$z.e[which(attributes(ZM_results$z.e)$index %in% -120:-30), ],
                      GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                      residual = TRUE) 

# This is the estimator for sd: mean centered second moment / n-2
AR_sd<- sqrt(sum((AR_resid-mean(AR_resid))^2/(length(AR_resid)-2)))
AR_sd


# Calculate the abnormal returns (observed minus predicted)
ZM_abn <- ZM_results$z.e - alpha_hat - beta_hat*GSPC_results$z.e

# Set event window
ZM_w <- window(ZM_abn,
               start = -29,
               end = +30)

# Convert to data.table format
ZM_abn<-ZM_w %>% as.data.frame() %>% setDT() 

# Create confidence interval for AR
ZM_abn[,AR_CI_low:=`1`-AR_sd*1.96]
ZM_abn[,AR_CI_high:=`1`+AR_sd*1.96]

# Add event date column
ZM_abn$date <-c(-29:30)

# Plot
graph_3_b_i<-ggplot(ZM_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = AR_CI_low, ymax = AR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("ZM: Market model of abnorma returns")

graph_3_b_i


### 3.b.ii. Plot the cumulative abnormal returns using a market model over the 
#       week following the event - show the 95% confidence interval.

ZM_car <- remap.cumsum(ZM_w, base = 0) 

# Convert to data.table format & rename variable
ZM_car<-ZM_car %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

# Create confidence interval for CAR
ZM_car[,CAR_CI_low:=CAR-sqrt(60)*AR_sd*1.96]
ZM_car[,CAR_CI_high:=CAR+sqrt(60)*AR_sd*1.96]
ZM_car$date <-c(-29:30)

# Plot
graph_3_b_ii<-ggplot(ZM_car, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("ZM: Market Model of Cumulative Abnormal Returns") 

graph_3_b_ii


# Convert the WMT_zoo to format with eventtime  

WMT_SplitDates$name<-as.character(WMT_SplitDates$name)

WMT_results <- phys2eventtime(
  z = WMT_zoo,
  events = WMT_SplitDates,
  width = 60)                            # width = event window #

head(WMT_results)

# Calculate mean return in estimation window

# constantMeanReturn if a function
?constantMeanReturn

WMT_Mean <- constantMeanReturn(WMT_results$z.e[which(attributes(WMT_results$z.e)$index %in% -120:-30), ], 
                               residual = FALSE)

WMT_Mean

# Calculate sd in estimation window : specify sd() at the end

WMT_sd <-constantMeanReturn(WMT_results$z.e[which(attributes(WMT_results$z.e)$index %in% -120:-30), ], 
                            residual = TRUE) %>% sd()
WMT_sd


# Calculate the abnormal return: daily return minus the mean for the time window
WMT_abn <- WMT_results$z.e - WMT_Mean

# Set event window
WMT_w <- window(WMT_abn,
                start = -29,
                end = +30)

head(WMT_w)
WMT_abn<-WMT_w %>% as.data.frame() %>% setDT()

# Create confidence interval for AR: matrix and we add a column each time
WMT_abn[,abn_CI_low:=`1`-WMT_sd*1.96]
WMT_abn[,abn_CI_high:=`1`+WMT_sd*1.96]
WMT_abn$date <-c(-29:30)

head(WMT_abn)

# Plot the abnormal return int he time window

graph_3_a_ii<-ggplot(WMT_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = abn_CI_low, ymax = abn_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Abnormal return WMT")

graph_3_a_ii


### a.3.iii Plot cumulative abnormal returns and show the 95% confidence interval.


# Calculate cumulative abnormal returns

# remap: calculates the cumulative sum of a time series

WMT_cum <- remap.cumsum(WMT_w, base = 0) 
WMT_cum<-WMT_cum %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

WMT_cum[,CAR_CI_low:=CAR-sqrt(60)*WMT_sd*1.96]
WMT_cum[,CAR_CI_high:=CAR+sqrt(60)*WMT_sd*1.96]
WMT_cum$date <-c(-29:30)

graph_3_a_iii<-ggplot(WMT_cum, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Cumulative Abnormal Returns WMT")
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

market_model<-marketModel(WMT_results$z.e[which(attributes(WMT_results$z.e)$index %in% -120:-30), ],
                          GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                          residual = FALSE)
market_model

# Save the coefficients from OLS
alpha_hat<-market_model$coefficients[1]
beta_hat<-market_model$coefficients[2]
alpha_hat
beta_hat

# Save the standard deviation from residuals
AR_resid<-marketModel(WMT_results$z.e[which(attributes(WMT_results$z.e)$index %in% -120:-30), ],
                      GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                      residual = TRUE) 

# This is the estimator for sd: mean centered second moment / n-2
AR_sd<- sqrt(sum((AR_resid-mean(AR_resid))^2/(length(AR_resid)-2)))
AR_sd


# Calculate the abnormal returns (observed minus predicted)
WMT_abn <- WMT_results$z.e - alpha_hat - beta_hat*GSPC_results$z.e

# Set event window
WMT_w <- window(WMT_abn,
                start = -29,
                end = +30)

# Convert to data.table format
WMT_abn<-WMT_w %>% as.data.frame() %>% setDT() 

# Create confidence interval for AR
WMT_abn[,AR_CI_low:=`1`-AR_sd*1.96]
WMT_abn[,AR_CI_high:=`1`+AR_sd*1.96]

# Add event date column
WMT_abn$date <-c(-29:30)

# Plot
graph_3_b_i<-ggplot(WMT_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = AR_CI_low, ymax = AR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("WMT: Market model of abnorma returns")

graph_3_b_i


### 3.b.ii. Plot the cumulative abnormal returns using a market model over the 
#       week following the event - show the 95% confidence interval.

WMT_car <- remap.cumsum(WMT_w, base = 0) 

# Convert to data.table format & rename variable
WMT_car<-WMT_car %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

# Create confidence interval for CAR
WMT_car[,CAR_CI_low:=CAR-sqrt(60)*AR_sd*1.96]
WMT_car[,CAR_CI_high:=CAR+sqrt(60)*AR_sd*1.96]
WMT_car$date <-c(-29:30)

# Plot
graph_3_b_ii<-ggplot(WMT_car, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("WMT: Market Model of Cumulative Abnormal Returns") 

graph_3_b_ii

# Convert the COST_zoo to format with eventtime  

COST_SplitDates$name<-as.character(COST_SplitDates$name)

COST_results <- phys2eventtime(
  z = COST_zoo,
  events = COST_SplitDates,
  width = 60)                            # width = event window #

head(COST_results)

# Calculate mean return in estimation window

# constantMeanReturn if a function
?constantMeanReturn

COST_Mean <- constantMeanReturn(COST_results$z.e[which(attributes(COST_results$z.e)$index %in% -120:-30), ], 
                                residual = FALSE)

COST_Mean

# Calculate sd in estimation window : specify sd() at the end

COST_sd <-constantMeanReturn(COST_results$z.e[which(attributes(COST_results$z.e)$index %in% -120:-30), ], 
                             residual = TRUE) %>% sd()
COST_sd


# Calculate the abnormal return: daily return minus the mean for the time window
COST_abn <- COST_results$z.e - COST_Mean

# Set event window
COST_w <- window(COST_abn,
                 start = -29,
                 end = +30)

head(COST_w)
COST_abn<-COST_w %>% as.data.frame() %>% setDT()

# Create confidence interval for AR: matrix and we add a column each time
COST_abn[,abn_CI_low:=`1`-COST_sd*1.96]
COST_abn[,abn_CI_high:=`1`+COST_sd*1.96]
COST_abn$date <-c(-29:30)

head(COST_abn)

# Plot the abnormal return int he time window

graph_3_a_ii<-ggplot(COST_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = abn_CI_low, ymax = abn_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Abnormal return COST")

graph_3_a_ii


### a.3.iii Plot cumulative abnormal returns and show the 95% confidence interval.


# Calculate cumulative abnormal returns

# remap: calculates the cumulative sum of a time series

COST_cum <- remap.cumsum(COST_w, base = 0) 
COST_cum<-COST_cum %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

COST_cum[,CAR_CI_low:=CAR-sqrt(60)*COST_sd*1.96]
COST_cum[,CAR_CI_high:=CAR+sqrt(60)*COST_sd*1.96]
COST_cum$date <-c(-29:30)

graph_3_a_iii<-ggplot(COST_cum, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Cumulative Abnormal Returns COST")
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

market_model<-marketModel(COST_results$z.e[which(attributes(COST_results$z.e)$index %in% -120:-30), ],
                          GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                          residual = FALSE)
market_model

# Save the coefficients from OLS
alpha_hat<-market_model$coefficients[1]
beta_hat<-market_model$coefficients[2]
alpha_hat
beta_hat

# Save the standard deviation from residuals
AR_resid<-marketModel(COST_results$z.e[which(attributes(COST_results$z.e)$index %in% -120:-30), ],
                      GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                      residual = TRUE) 

# This is the estimator for sd: mean centered second moment / n-2
AR_sd<- sqrt(sum((AR_resid-mean(AR_resid))^2/(length(AR_resid)-2)))
AR_sd


# Calculate the abnormal returns (observed minus predicted)
COST_abn <- COST_results$z.e - alpha_hat - beta_hat*GSPC_results$z.e

# Set event window
COST_w <- window(COST_abn,
                 start = -29,
                 end = +30)

# Convert to data.table format
COST_abn<-COST_w %>% as.data.frame() %>% setDT() 

# Create confidence interval for AR
COST_abn[,AR_CI_low:=`1`-AR_sd*1.96]
COST_abn[,AR_CI_high:=`1`+AR_sd*1.96]

# Add event date column
COST_abn$date <-c(-29:30)

# Plot
graph_3_b_i<-ggplot(COST_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = AR_CI_low, ymax = AR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("COST: Market model of abnorma returns")

graph_3_b_i


### 3.b.ii. Plot the cumulative abnormal returns using a market model over the 
#       week following the event - show the 95% confidence interval.

COST_car <- remap.cumsum(COST_w, base = 0) 

# Convert to data.table format & rename variable
COST_car<-COST_car %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

# Create confidence interval for CAR
COST_car[,CAR_CI_low:=CAR-sqrt(60)*AR_sd*1.96]
COST_car[,CAR_CI_high:=CAR+sqrt(60)*AR_sd*1.96]
COST_car$date <-c(-29:30)

# Plot
graph_3_b_ii<-ggplot(COST_car, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("COST: Market Model of Cumulative Abnormal Returns") 

graph_3_b_ii


# Convert the PTON_zoo to format with eventtime  

PTON_SplitDates$name<-as.character(PTON_SplitDates$name)

PTON_results <- phys2eventtime(
  z = PTON_zoo,
  events = PTON_SplitDates,
  width = 60)                            # width = event window #

head(PTON_results)

# Calculate mean return in estimation window

# constantMeanReturn if a function
?constantMeanReturn

PTON_Mean <- constantMeanReturn(PTON_results$z.e[which(attributes(PTON_results$z.e)$index %in% -120:-30), ], 
                                residual = FALSE)

PTON_Mean

# Calculate sd in estimation window : specify sd() at the end

PTON_sd <-constantMeanReturn(PTON_results$z.e[which(attributes(PTON_results$z.e)$index %in% -120:-30), ], 
                             residual = TRUE) %>% sd()
PTON_sd


# Calculate the abnormal return: daily return minus the mean for the time window
PTON_abn <- PTON_results$z.e - PTON_Mean

# Set event window
PTON_w <- window(PTON_abn,
                 start = -29,
                 end = +30)

head(PTON_w)
PTON_abn<-PTON_w %>% as.data.frame() %>% setDT()

# Create confidence interval for AR: matrix and we add a column each time
PTON_abn[,abn_CI_low:=`1`-PTON_sd*1.96]
PTON_abn[,abn_CI_high:=`1`+PTON_sd*1.96]
PTON_abn$date <-c(-29:30)

head(PTON_abn)

# Plot the abnormal return int he time window

graph_3_a_ii<-ggplot(PTON_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = abn_CI_low, ymax = abn_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Abnormal return PTON")

graph_3_a_ii


### a.3.iii Plot cumulative abnormal returns and show the 95% confidence interval.


# Calculate cumulative abnormal returns

# remap: calculates the cumulative sum of a time series

PTON_cum <- remap.cumsum(PTON_w, base = 0) 
PTON_cum<-PTON_cum %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

PTON_cum[,CAR_CI_low:=CAR-sqrt(60)*PTON_sd*1.96]
PTON_cum[,CAR_CI_high:=CAR+sqrt(60)*PTON_sd*1.96]
PTON_cum$date <-c(-29:30)

graph_3_a_iii<-ggplot(PTON_cum, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("Cumulative Abnormal Returns PTON")
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

market_model<-marketModel(PTON_results$z.e[which(attributes(PTON_results$z.e)$index %in% -120:-30), ],
                          GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                          residual = FALSE)
market_model

# Save the coefficients from OLS
alpha_hat<-market_model$coefficients[1]
beta_hat<-market_model$coefficients[2]
alpha_hat
beta_hat

# Save the standard deviation from residuals
AR_resid<-marketModel(PTON_results$z.e[which(attributes(PTON_results$z.e)$index %in% -120:-30), ],
                      GSPC_results$z.e[which(attributes(GSPC_results$z.e)$index %in% -120:-30), ], 
                      residual = TRUE) 

# This is the estimator for sd: mean centered second moment / n-2
AR_sd<- sqrt(sum((AR_resid-mean(AR_resid))^2/(length(AR_resid)-2)))
AR_sd


# Calculate the abnormal returns (observed minus predicted)
PTON_abn <- PTON_results$z.e - alpha_hat - beta_hat*GSPC_results$z.e

# Set event window
PTON_w <- window(PTON_abn,
                 start = -29,
                 end = +30)

# Convert to data.table format
PTON_abn<-PTON_w %>% as.data.frame() %>% setDT() 

# Create confidence interval for AR
PTON_abn[,AR_CI_low:=`1`-AR_sd*1.96]
PTON_abn[,AR_CI_high:=`1`+AR_sd*1.96]

# Add event date column
PTON_abn$date <-c(-29:30)

# Plot
graph_3_b_i<-ggplot(PTON_abn, aes(x = date, y = `1`, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = AR_CI_low, ymax = AR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("PTON: Market model of abnorma returns")

graph_3_b_i


### 3.b.ii. Plot the cumulative abnormal returns using a market model over the 
#       week following the event - show the 95% confidence interval.

PTON_car <- remap.cumsum(PTON_w, base = 0) 

# Convert to data.table format & rename variable
PTON_car<-PTON_car %>% as.data.frame() %>% rename(CAR=".") %>% setDT()

# Create confidence interval for CAR
PTON_car[,CAR_CI_low:=CAR-sqrt(60)*AR_sd*1.96]
PTON_car[,CAR_CI_high:=CAR+sqrt(60)*AR_sd*1.96]
PTON_car$date <-c(-29:30)

# Plot
graph_3_b_ii<-ggplot(PTON_car, aes(x = date, y = CAR, group = 1)) +
  geom_line(col='red') +
  geom_ribbon(aes(ymin = CAR_CI_low, ymax = CAR_CI_high), alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black")+
  geom_hline(yintercept =0, linetype="dashed")+
  ggtitle("PTON: Market Model of Cumulative Abnormal Returns") 

graph_3_b_ii

