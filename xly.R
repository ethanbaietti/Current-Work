#install.packages("devtools") # if not installed
#install.packages("FinancialInstrument") #if not installed
#install.packages("PerformanceAnalytics") #if not installed
#install.packages("timetk")
# next install blotter from GitHub
#devtools::install_github("braverock/blotter")
# next install quantstrat from GitHub
#devtools::install_github("braverock/quantstrat")
#####USE LINE IF PACKAGES AREN'T COMPILING
#options(buildtools.check = function(action) TRUE)

library(blotter)
library(quantstrat)
library(FinancialInstrument)
library(PerformanceAnalytics)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyquant)


#initialization date for your backtest
initdate <- "2003-06-01"
from <- "2006-10-08"
to <- "2020-04-01"

Sys.setenv(TZ = "EST")
currency("USD")

getSymbols("XLY", from = from, to = to, src = "yahoo", adjust = TRUE)

stock("XLY", currency = "USD")

tradesize <- 100000
initeq <- 100000

strategy.st <- "firststrat"
portfolio.st <- "firststrat"
account.st <- "firststrat"

#Initialize Portfolio
initPortf(portfolio.st, symbols = "XLY", initDate = initdate, currency = "USD")

#Intialize Account
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)

#Intialize the orders 
initOrders(portfolio.st, initDate = initdate)

#Store the Strategy
strategy(strategy.st, store = T)

### CHAPTER 3 - INDICATORS

#200 Day Moving Average
xly_sma <- SMA(Cl(XLY), n = 200)

#200 Day Moving Average
xly_rsi <- RSI(Cl(XLY), n = 3)

#Add 200-day SMA indcator to strategy
add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 200), label = "SMA200")
#Add 50-day SMA indcator to strategy
add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 50), label = "SMA50")
#Add 3 Period  RSI indcator to strategy
add.indicator(strategy = strategy.st, name = "RSI", arguments = list(price = quote(Cl(mktdata)), n = 3), label = "RSI_3")

# Write the calc_RSI_avg function
RSI_avg <- function(price, n1, n2) {
  rsi_1 <- RSI(price = price, n = 1)
  rsi_2 <- RSI(price = price, n = 2)
  RSI_avg <- (rsi_1/rsi_2)/2
  colnames(RSI_avg) <- "RSI_avg"
  return (RSI_avg)
}

# Add this function as RSI_3_4 to your strategy with n1 = 3 and n2 = 4
add.indicator(strategy.st, name = "RSI_avg", arguments = list(price = quote(Cl(mktdata)), n1 = 3, n2 = 4), label = "RSI_3_4")

# Declare the David Varadi Oscillator (DVO) function
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  
  # Compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC)/(Hi(HLC) + Lo(HLC))/2
  
  # Smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  
  # Convert ratio into a 0-100 value using runPercentRank()
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}

# Add the DVO indicator to your strategy
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126),
              label = "DVO_2_126")

# Use applyIndicators to test out your indicators
test <- applyIndicators(strategy = strategy.st, portfolio.st, mktdata = OHLC(XLY))

# Subset your data between Sep. 1 and Sep. 5 of 2013
test_subset <- test["2013-09-01/2013-09-05"]


########## CHAPTER 4 SIGNALS

# Add a sigComparison which specifies that SMA50 must be greater than SMA200, call it longfilter
add.signal(strategy.st, name = "sigComparison", arguments = list(columns = c("SMA50", "SMA200"), 
          relationship = "gt"), label = "longfilter")

# Add a sigCrossover which specifies that the SMA50 is less than the SMA200 and label it filterexit
add.signal(strategy.st, name = "sigCrossover", arguments = list(columns = c("SMA50", "SMA200"),
            relationship = "lt"), label = "filterexit")

# Implement a sigThreshold which specifies that DVO_2_126 must be less than 20, label it longthreshold
add.signal(strategy.st, name = "sigThreshold", arguments = list(column = "DVO_2_126", 
            threshold = 20, relationship = "lt",cross = F), label = "longthreshold")

# Add a sigThreshold signal to your strategy that specifies that DVO_2_126 must cross above 80 and label it thresholdexit
add.signal(strategy.st, name = "sigThreshold",arguments = list(column = "DVO_2_126", threshold = 80, 
            relationship = "gt", cross = T), label = "thresholdexit")

##sigFormula excersize

test_init <- applyIndicators(strategy.st, mktdata = OHLC(XLY))
test <- applySignals(strategy = strategy.st, mktdata = test_init)

add.signal(strategy.st, name = "sigFormula",arguments = list(formula = "longfilter & longthreshold",cross = T),
           label = "longentry")

###### CHAPTER 5 RULES

# Fill in the rule's type as exit
add.rule(strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "filterexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), type = "exit")

#Enter Rule
add.rule(strategy.st, name = "ruleSignal", arguments=list(sigcol = "longentry",sigval = T, 
           orderqty = 1,ordertype = "market", orderside = "long",replace = F, prefer = "Open"), type = "enter")

#order Sizing Function
add.rule(strategy = strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longentry", sigval = TRUE, ordertype = "market",
                          orderside = "long", replace = FALSE, prefer = "Open",
                          
                          # Use the osFUN called osMaxDollar
                          osFUN = osMaxDollar,
                          
                          # The tradeSize argument should be equal to tradesize (defined earlier)
                          tradeSize = tradesize,
                          
                          # The maxSize argument should be equal to tradesize as well
                          maxSize = tradesize),
         type = "enter")

###### CHAPTER 6 ANALYZE YOUR STRAT

#COPY AND PASTE STATS FOR THE FUTURE

# Use applyStrategy() to apply your strategy. Save this to out
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

# Update your portfolio (portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

# Update your account (account.st)
updateAcct(account.st, daterange)
updateEndEq(account.st)

tstats <- tradeStats(Portfolios = portfolio.st)
print(tstats$Profit.Factor)

###Charting your position
chart.Posn(portfolio.st)

# Compute the SMA50
sma50 <- SMA(x = Cl(XLY), n = 50)

# Compute the SMA200
sma200 <- SMA(x = Cl(XLY), n = 200)

# Compute the DVO_2_126 with an navg of 2 and a percentlookback of 126
dvo <- DVO(HLC = HLC(XLY), navg = 2, percentlookback = 126)

# Recreate the chart.Posn of the strategy from the previous exercise
chart.Posn(Portfolio = portfolio.st, Symbol = "XLY")

# Overlay the SMA50 on your plot as a blue line
add_TA(sma50, on = 1, col = "blue")

# Overlay the SMA200 on your plot as a red line
add_TA(sma200, on = 1, col = "red")

# Add the DVO_2_126 to the plot in a new window
add_TA(dvo)

##Genreate P&L time series

#Sharpe Ratio
portpl <- .blotter$portfolio.firststrat$summary$Net.Trading.PL
SharpeRatio.annualized(portpl, geometric=FALSE)

# Get instrument returns
instrets <- PortfReturns(portfolio.st)

# Compute Sharpe ratio from returns
SharpeRatio.annualized(instrets, geometric = FALSE)



###Plotting
xly_gr <- tq_get("XLY", from = from, to = to)
plot <- ggplot(xly_gr, aes(x = date)) + geom_line(aes(y = close)) + geom_line(aes(xly_sma), col = "red")
plot

plot(Cl(XLY))

# Overlay a 200-day SMA
lines(SMA(Cl(XLY), n = 200), col = "red")

plot(RSI(Cl(XLY), n = 2))
