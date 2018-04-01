usePackage <- function(p){
  if(!is.element(p, installed.packages()[,1]))
    install.packages(p, dependencies = TRUE)
  require(p, character.only = TRUE)
}


usePackage("odbc")
usePackage("DBI")
usePackage("shinydashboard")
usePackage("DT")
usePackage("shiny")
usePackage("rpivotTable")
usePackage("shinyDND")
usePackage("data.table")
usePackage("rvest")
usePackage("quantmod")
usePackage("tibble")
usePackage("dplyr")
usePackage("lubridate")
usePackage("flexdashboard")
usePackage("plotly")
usePackage("glue")