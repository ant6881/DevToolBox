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

MyData[,("Year"):=format(FilingDate, "%Y")]

first_last <- MyData[order(Ticker, FilingDate), .(LastTwoYears = .I[c(.N-1,.N)]), keyby=Ticker]



idx = first_last$LastTwoYears

DispCol <- c("Ticker", "FilingDate", "REVENUEUSD")
             # 
DispCol1 <- c(DispCol[-3],grep("REVENUEUSD", colnames(MyData), ignore.case = T, value = T))

# 2) extract rows by number
MyData[idx,..DispCol1] %>%
  kable("html") %>%
  kable_styling("striped", full_width = F, position = "left")


Revs <- MyData[,..DispCol] 
plot_ly(data = Revs, x = ~FilingDate, y = ~REVENUEUSD, linetype = ~Ticker)



Fama <- MyData[, lapply(.SD, mean, na.rm=T), by=c("Fama.Industry", "Year"), .SDcols = "REVENUEUSD.YoY"]

plot_ly(data = Fama, x = ~Year, y = ~REVENUEUSD.YoY, linetype = ~Fama.Industry, type = 'scatter')

unique(MyData[]$Fama.Industry)
unique(MyData[]$Sector)
unique(MyData[]$Industry)

