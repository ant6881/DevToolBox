usePackage("kableExtra")
usePackage("knitr")


TickersInIndustry <- QuandlTickers$Ticker[QuandlTickers$Industry == unique(QuandlTickers$Industry)[1]]


MyData <- data.table(FinData[Ticker %in% TickersInIndustry,])

NumCols <- colnames(MyData)[!colnames(MyData) %in% c("Ticker", "FilingDate")]


MyData <- QuandlTickers[MyData, on="Ticker"]


YoY <- paste(NumCols, "YoY" ,sep=".")
FYG <- paste(NumCols, "FiveYearGrowth" ,sep=".")


MyData[order(Ticker, FilingDate), (YoY):= ((.SD - shift(.SD, 1, type = "lag")) 
                                                    / shift(.SD, 1, type = "lag")), by=Ticker  , .SDcols=NumCols]

MyData[order(Ticker, FilingDate), (FYG):= ((.SD - shift(.SD, 5, type = "lag")) 
                                                 / shift(.SD, 5, type = "lag")), by=Ticker  , .SDcols=NumCols]


MyData[,("CoefOfVar"):=sapply(.SD, function(x) round(sd(x) / mean(x) , 3)), by=Ticker, .SDcols = NumCols]


first_last <- MyData[order(Ticker, FilingDate), .(LastTwoYears = .I[c(.N-1,.N)]), keyby=Ticker]



idx = first_last$LastTwoYears

DispCol <- c("Ticker", "FilingDate", "REVENUEUSD")
             # 
DispCol1 <- c(DispCol[-3],grep("REVENUEUSD", colnames(MyData), ignore.case = T, value = T))
Revs <- MyData[,..DispCol] 
# 2) extract rows by number
MyData[idx,..DispCol1] %>%
  kable("html") %>%
  kable_styling("striped", full_width = F, position = "left")


summ.stats <- function(vec) {
  list(
    Metric = names(vec),
    Min = sapply(vec, min, na.rm=T),
    Mean = sapply(vec, mean, na.rm=T),
    Median = sapply(vec, median, na.rm=T),
    Max = sapply(vec, max, na.rm=T)
}

plot_ly(data = Revs, x = ~FilingDate, y = ~REVENUEUSD, linetype = ~Ticker)



unique(MyData[]$Fama.Industry)
unique(MyData[]$Sector)
unique(MyData[]$Industry)

IndustryMetrics[[IndustryMetricsNames[a]]] <- Financials[[a]][, summ.stats(.SD), by=c("Industry","MarketCap","Year"), .SDcols=cols]