# Check if required packages installed. If not, install them
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("forecast")) install.packages("forecast")
if (!require("plotrix")) install.packages("plotrix")

if(!file.exists("trade_history.csv")){
  trade_history <- data.frame(type=factor(levels = c("deposit", "wtihdraw", "exchange")),
                              deposit_amount=double(),
                              deposit_currency=factor(),
                              withdraw_amount=double(),
                              withdraw_currency=factor(),
                              exchange_name=character(),
                              remark=character(),
                              date=character(), # year-month-day format eg 2017-07-18
                              stringsAsFactors=FALSE)
  write.csv(x = trade_history, file = "trade_history.csv", sep = ",", row.names = FALSE)
}

trade_history <- read.csv(file = "trade_history.csv", header = TRUE, sep = ",")
head(trade_history)

add_trade <- function(type, deposit_amount = 0, deposit_currency = NA,
                      withdraw_amount = 0, withdraw_currency = NA, exchange_name = NA,
                      remark = NA, date = NA) {
  new_trade <- data.frame(type, deposit_amount, deposit_currency,
                          withdraw_amount, withdraw_currency, exchange_name, remark, date)
  read.csv(file = "trade_history.csv", header = TRUE, sep = ",") %>%
    rbind(new_trade) -> "trade_history"
  write.csv(x = trade_history, file = "trade_history.csv", sep = ",", row.names = FALSE)
  assign(x = "trade_history", trade_history, envir = globalenv())
}

# `add_trade` examples

add_trade(type = "deposit", deposit_amount = 0.2, deposit_currency = "BTC", remark = "gift from brother", date = "2017-07-01")
add_trade(type = "deposit", deposit_amount = 5, deposit_currency = "XMR", remark = "purchased", exchange_name = "poloniex", date = "2017-07-02")
add_trade(type = "deposit", deposit_amount = 1, deposit_currency = "ETH", remark = "mining reward", date = "2017-07-05")
add_trade(type = "deposit", deposit_amount = 200, deposit_currency = "STEEM", remark = "Steemit rewarrd", date = "2017-07-06")
add_trade(type = "trade", deposit_amount = 1.1, deposit_currency = "ZEC", withdraw_amount = 0.1, withdraw_currency = "BTC", remark = "Exchanged BTC for ZEC", date = "2017-07-09")
add_trade(type = "trade", deposit_amount = 4.6, deposit_currency = "ETC",  withdraw_amount = 2, withdraw_currency = "XMR", remark = "Exchanged XMR ETC", date = "2017-07-10")
add_trade(type = "trade", deposit_amount = 65, deposit_currency = "EOS",  withdraw_amount = 0.5, withdraw_currency = "ETH", date = "2017-07-14")
add_trade(type = "withdraw", withdraw_amount = 0.5, withdraw_currency = "XMR", remark = "lost wallet key")

# portfolio summary


portfolio <- function() {
  deposit <- aggregate(trade_history[c("deposit_amount")], 
                       by = trade_history[c("deposit_currency")], FUN=sum)
  names(deposit) <- c("currency", "deposit_amount")
  withdraw <- aggregate(trade_history[c("withdraw_amount")], 
                                  by = trade_history[c("withdraw_currency")], FUN=sum)
  names(withdraw) <- c("currency", "withdraw_amount")
  portfolio <- full_join(x = deposit, y = withdraw, by = "currency")
  portfolio[is.na(portfolio)] <- 0
  portfolio$available <- portfolio$deposit_amount - portfolio$withdraw_amount
  assign(x = "portfolio", portfolio, envir = globalenv())
  print(portfolio)
}

# portfolio summary example
portfolio()


portfolio_value <- function(priced_in) {
  for(i in 1:nrow(portfolio)) {
    url <- paste0("https://min-api.cryptocompare.com/data/price?fsym=", portfolio[i, 1], "&tsyms=", priced_in, collapse = "")
    unit_price <- fromJSON(url)[[1]]
    portfolio$value[i] <- unit_price * portfolio$available[i]
  }
  assign(x = "portfolio_value", portfolio, envir = globalenv())
  print(portfolio_value)
  print(paste("Total portfolio value in", priced_in, sum(portfolio_value$value)))
  lbls <- paste0(portfolio$currency, " : ", # Create labels for plot
                 sprintf("%.2f", (portfolio$value / sum (portfolio$value))*100), "%")
  pie3D(portfolio$value, labels = lbls,
        explode=0.1, main="Portfolio value")
}

# Example
portfolio_value("CNY")

# predict currency price
predict_currency <- function(currency, priced_in, period) {
  url <- paste0("https://min-api.cryptocompare.com/data/histoday?fsym=", currency, "&tsym=", priced_in, "&allData=true", collapse = "")
  histoday <- fromJSON(url)
  ts <- ts(histoday$Data$close, start = histoday$Data$time[1])
  fit_arima <- auto.arima(ts)
  autoplot(forecast(fit_arima, period))
}
# predict currency example

predict_currency("BTC", "USD", 30)
