# Collection of financial functions


# Get Historical Prices ---------------------------------------------------

# http://www.business-science.io/investments/2016/11/30/Russell2000_Analysis.html#purrr


# get_stock_prices("AAPL")
# "AAPL" %>%
#   get_stock_prices() %>%
#     get_log_returns()


get_stock_prices <- function(ticker, return_format = "tibble", ...) {
  # Get stock prices
  stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...)
  # Rename
  names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  # Return in xts format if tibble is not specified
  if (return_format == "tibble") {
    stock_prices <- stock_prices_xts %>%
      as_tibble() %>%
      rownames_to_column(var = "Date") %>%
      mutate(Date = ymd(Date))
  } else {
    stock_prices <- stock_prices_xts
  }
  
  stock_prices
}


get_log_returns <- function(x, return_format = "tibble", period = 'daily', ...) {
  # Convert tibble to xts
  if (!is.xts(x)) {
    x <- xts(x[,-1], order.by = x$Date)
  }
  # Get stock prices
  log_returns_xts <- periodReturn(x = x$Adjusted, type = 'log', period = period, ...)
  # Rename
  names(log_returns_xts) <- "Log.Returns"
  # Return in xts format if tibble is not specified
  if (return_format == "tibble") {
    log_returns <- log_returns_xts %>%
      as_tibble() %>%
      rownames_to_column(var = "Date") %>%
      mutate(Date = ymd(Date))
  } else {
    log_returns <- log_returns_xts
  }
  log_returns
}  




QuandlTickers <- data.table(read.delim("http://www.sharadar.com/meta/sf0-tickers.txt", sep = "\t"))
sector <- unique(QuandlTickers$Sector)[order(unique(QuandlTickers$Sector))]
IndustryTable <- data.frame(table(QuandlTickers$Sector, QuandlTickers$Industry))

colnames(IndustryTable) <- c("Sector", "Industry", "Count")
IndustryTable <- IndustryTable[order(IndustryTable$Sector,-IndustryTable$Count),]

MarketCaps <- data.frame(`Range` = c('0','50000000','300000000','2000000000','10000000000','200000000000')  , `Market Capitalization` = c('Nano Cap','Micro Cap','Small Cap','Mid Cap','Large Cap','Mega Cap'))

