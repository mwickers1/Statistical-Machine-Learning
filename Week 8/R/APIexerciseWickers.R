library(httr)
library(XML)
library(lubridate)
library(ggplot2)


# https://corpslocks.usace.army.mil/lpwb/xml.tonnage?in_river=GI&in_lock=01&in_mon_yr=102019
#query must look like the url above 
#use paste0 in order to ensure no spaces are between the text we are pasting together.

GetQuery <- function(river = 'MI', lock = '27', date){
  ##Function builds query, configures security settings,querys the url, converts it to xml
  ##converts xml to dataframe, assigns the date to the data, and prints the data.
  
  query <- paste0('https://corpslocks.usace.army.mil/lpwb/xml.tonnage','?', 'in_river=',river,'&','in_lock=',lock,'&','in_mon_yr=',date)
      
  #security settings must be confgured to allow the qeury to reach the api
  httr::set_config(config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))    
  urlquery <- GET(query)
  
  xml <- content(urlquery, 'text')
  
  data <- xmlToDataFrame(xml)
  
  data$date <- date
  data
}

dates <- c('012018', '022018','032018', '042018', '052018','062018', '072018','082018', '092018', '102018','112018','122018',
           '012019', '022019','032019', '042019', '052019','062019', '072019','082019', '092019', '102019')


datalist <- list()

for(i in 1:length(dates)){
  ##loop iterates through the dates and querys the url specific to those dates and builds a dataframe
  ##the dataframes are then put into a list in order to be able to combine them later.
  
  data <- GetQuery(date = dates[i])
  datalist[[i]] <- data

}

data <- do.call(rbind, datalist)#bind all dataframes in the list into one dataframe.

#convert character numbers into numeric
data$UPBOUND_TONS <- as.numeric(as.character(data$UPBOUND_TONS))
data$DOWNBOUND_TONS <- as.numeric(as.character(data$DOWNBOUND_TONS))
data$TOTAL_TONS <- as.numeric(as.character(data$TOTAL_TONS))

#convert date string into date format
data$date2 <- format(mdy(data$date),"%m/%Y")

#ensure date is in correct order
data$date <- factor(data$date, levels = dates, labels = dates)

theme_set(theme_minimal())
ggplot(data = data, aes(x= date2, y = TOTAL_TONS))+
  geom_line(aes(group = COMM_DESC,color = COMM_DESC)) +
  xlab("")+
  ylab('Total Tons')+
  labs(color = 'Product')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = data, aes(x= date, y = TOTAL_TONS))+ 
  geom_bar(stat = 'identity',aes(fill = COMM_DESC))+
  xlab("")+
  ylab('Total Tons')+
  labs(fill = 'Product')+
  ggtitle('Total Monthly Tons Grouped by Product')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = data, aes(x= date, y = UPBOUND_TONS))+ 
  geom_bar(stat = 'identity',aes(fill = COMM_DESC))+
  xlab("")+
  ylab('Total Upbound Tons')+
  labs(fill = 'Product')+
  ggtitle('Monthly Upbound Tons Grouped by Product')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = data, aes(x= date, y = DOWNBOUND_TONS))+ 
  geom_bar(stat = 'identity',aes(fill = COMM_DESC))+
  xlab("")+
  ylab('Total Downbound Tons')+
  labs(fill = 'Product')+
  ggtitle('Monthly Downbound Tons Grouped by Product')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
