rm(list=ls())

# Load required library
library(tidyquant)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

get_date_from_yahoo <- function(tickers, start_date, end_date) {
  
  getSymbols( tickers
              ,from = start_date
              ,to = end_date
              ,warnings = FALSE
              ,auto.assign = TRUE
              , src = "yahoo")
  
  
  
  apple_df <-data.frame(AAPL)
  apple_df$AAPL.Date<-rownames(apple_df)
  apple_df$Company.Name <- "APPLE"
  rownames(apple_df)<-NULL 
  write.csv(apple_df,'C:/Users/sachi/Desktop/DPA-571/Project/Stock_Data/raw_apple_Stock.csv')
  
  google_df <-data.frame(GOOGL)
  google_df$GOOGL.Date<-rownames(google_df)
  google_df$Company.Name <- "GOOGLE"
  rownames(google_df)<-NULL
  write.csv(google_df,'C:/Users/sachi/Desktop/DPA-571/Project/Stock_Data/raw_google_Stock.csv')
  
  tesla_df <-data.frame(TSLA)
  tesla_df$TSLA.Date<-rownames(tesla_df)
  tesla_df$Company.Name <- "TESLA"
  rownames(tesla_df)<-NULL
  write.csv(tesla_df,'C:/Users/sachi/Desktop/DPA-571/Project/Stock_Data/raw_tesla_Stock.csv')
  
  colnames(tesla_df)
  
  colnames(apple_df) <- c("Open","High","Low","Close","Volume","Adjusted","Date", "Company.Name")
  colnames(google_df) <- c("Open","High","Low","Close","Volume","Adjusted","Date", "Company.Name")
  colnames(tesla_df) <- c("Open","High","Low","Close","Volume","Adjusted","Date", "Company.Name")
  
  #all data
  all.stock.df <- rbind(apple_df, google_df, tesla_df)
  write.csv(all.stock.df,'C:/Users/sachi/Desktop/DPA-571/Project/Stock_Data/all_company_stock.csv')
  
  #only date, close and company
  close.stock.df <- all.stock.df[, c('Date', 'Close', 'Company.Name')]
  write.csv(close.stock.df,'C:/Users/sachi/Desktop/DPA-571/Project/Stock_Data/all_company_close_stock.csv')
}


tickers = c("AAPL", "GOOGL", "TSLA")
start.date <- "2000-01-01"
today <- format(Sys.Date(), "%Y-%m-%d")
get_date_from_yahoo(tickers, start.date, today)
