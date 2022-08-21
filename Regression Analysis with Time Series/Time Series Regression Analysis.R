library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(tidyverse)
library(forecast)
library(tidyr)
library(lubridate)
library(zoo)
library(readxl)
library(xlsx)
library(scales)
library(ggthemes)


#NASA
nasa <- read.csv('C:/Users/Matth/Desktop/Matthew/Smith MMA/MMA 867 - Predictive Modelling/Assignments/Team Assignments/Team Assignment 1/nasa.csv',
                 skip = 1)




nasa_num <- nasa%>%
  select(2:13,)%>%
  mutate_all(as.numeric)%>%
  mutate_if(is.numeric, ~. + 14.0)

nasa$Year <- as.numeric(nasa$Year)

nasa <- cbind(nasa$Year, nasa_num)
nasa <- nasa[1:141,] #Removing 2021 row as has NAs


str(nasa)



months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec')


tidy_nasa <- gather(nasa, Month, Temperature, months)

tidy_nasa$month_num <- ifelse(tidy_nasa$Month == 'Jan', '01',
                              ifelse(tidy_nasa$Month == 'Feb', '02',
                                     ifelse(tidy_nasa$Month == 'Mar', '03',
                                            ifelse(tidy_nasa$Month == 'Apr', '04',
                                                   ifelse(tidy_nasa$Month =='May', '05',
                                                          ifelse(tidy_nasa$Month == 'Jun', '06',
                                                                 ifelse(tidy_nasa$Month =='Jul', '07',
                                                                        ifelse(tidy_nasa$Month =='Aug', '08',
                                                                               ifelse(tidy_nasa$Month =='Sep','09',
                                                                                      ifelse(tidy_nasa$Month == 'Oct', '10',
                                                                                             ifelse(tidy_nasa$Month == 'Nov', 11,
                                                                                                    ifelse(tidy_nasa$Month =='Dec',12,0))))))))))))

tidy_nasa$Day <- 1


tidy_nasa$Date <- as.Date(with(tidy_nasa, paste(`nasa$Year`, month_num, Day,sep="-")), "%Y-%m-%d")


nasa_cleaned <- tidy_nasa%>%
  select(Date, Temperature)%>%
  arrange(Date)


str(nasa_cleaned)



#Converting to TS
nasa_ts <- ts(nasa_cleaned$Temperature, start = c(1880,1), frequency = 12) #starting from January 1880

#Decomposing the Data
fit <- decompose(nasa_ts, type = 'additive')
plot(fit)

fit <- decompose(nasa_ts, type = 'multiplicative')
plot(fit)

stl <- stl(nasa_ts, t.window = 13, s.window = 'periodic')
plot(stl, main = 'NASA STL Decomposition')

mstl <- mstl(nasa_ts, s.window = 13) #auto finds best values for stl (need to set s.window to 13)
plot(mstl, main = 'NASA MSTL Decomposition')

#Creating some Models
nasa_MMZ <- ets(nasa_ts, model = 'MMZ')
nasa_MMZ_pred <- forecast(nasa_MMZ, h=960, level = c(0.8,0.90))

nasa_MMZ_pred_df <- as.data.frame(nasa_MMZ_pred) #to view all predictions

plot(nasa_MMZ_pred, xlab = 'Year', ylab = 'Predicted Global Temperature (C°)')

nasa_MMZ_perf <- accuracy(nasa_MMZ_pred)

nasa_AAZ <- ets(nasa_ts, model = 'AAZ')
nasa_AAZ_pred <- forecast(nasa_AAZ, h=960, level = c(0.8,0.90))

plot(nasa_AAZ_pred, xlab = 'Year', ylab = 'Predicted Global Temperature (C°)')

nasa_AAZ_perf <- accuracy(nasa_AAZ_pred)

#Viewing MMZ and AAZ together
par(mfrow=c(1,2))
plot(nasa_MMZ_pred,xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))
plot(nasa_AAZ_pred, xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))


#ARIMA
#Acf
Acf(diff(log(nasa_ts),12), main = '') #many significant predictors

#Pacf
par(mfrow=c(1,1))
Pacf(nasa_ts, main ='')
Pacf(diff(log(nasa_ts),12),main ='') #differencing to remove seasonality

nasa_arima_n <- auto.arima(nasa_ts, seasonal = FALSE)
nasa_arima_n_pred <- forecast(nasa_arima_n, h=960, level = c(0.8,0.9))
nasa_arima_n_pred_df <- as.data.frame(nasa_arima_n_pred)

nasa_arima_n_perf <- accuracy(nasa_arima_n_pred)

plot(nasa_arima_n_pred,main = 'NASA Forecasts from ARIMA(3,1,3)(0,0,2)[12] with Drift', 
     xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))

nasa_arima_s <- auto.arima(nasa_ts, seasonal = TRUE)
nasa_arima_s_pred <- forecast(nasa_arima_s, h=960, level = c(0.8,0.9))
nasa_arima_s_pred_df <- as.data.frame(nasa_arima_s_pred)

nasa_arima_s_pred

nasa_arima_s_perf <- accuracy(nasa_arima_s_pred) #Stronger MAPE than MMZ and AAZ

plot(nasa_arima_s_pred,xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))

#Viewing MMZ and ARIMA together 
par(mfrow=c(1,2))
plot(nasa_arima_pred,xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))
plot(nasa_MMZ_pred, xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))

#TBATS
nasa_tbats <- tbats(nasa_ts)
nasa_tbats_pred <- forecast(nasa_tbats, h=960)
nasa_tbats_pred_df <- as.data.frame(nasa_tbats_pred)

nasa_tbats_perf <- accuracy(nasa_tbats_pred) #Slightly worse than Arima

plot(nasa_tbats_pred,ylim = c(11.0,19))

#MMZ and TBATS
par(mfrow=c(1,2))
plot(nasa_MMZ_pred,xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))
plot(nasa_tbats_pred, xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))


#Viewing combined performance
nasa_perf_row_names <- c('MMZ', 'AAZ', 'TBATS', 'ARIMA N','ARIMA S')

nasa_model_perf_df <- rbind(nasa_MMZ_perf, nasa_AAZ_perf, nasa_tbats_perf, nasa_arima_n_perf, nasa_arima_s_perf)
rownames(nasa_model_perf_df) <- nasa_perf_row_names




#Testing with cv
f_MMZ <- function(y, h) forecast(ets(y, model = 'MMZ'), h=h)
errors_MMZ <- tsCV(nasa_ts, f_MMZ, h=1, window = 846) #used 846 as it is half of 1692 (total number of obs)


f_MMZ  <- function(y, h) forecast(ets(y, model="MMZ"), damped = TRUE, h = h)
errors_MMZ <- tsCV(yahoo_ts, f_MMZ, h=1, window=846) 

#MAPE score comparison
mape_mmz <- mean(abs(errors_MMZ/nasa_ts), na.rm = TRUE)*100


#--------------------------------------------------------------------------------------------------------------------------------------
#UK MET
met <- read_excel('C:/Users/Matth/Desktop/Matthew/Smith MMA/MMA 867 - Predictive Modelling/Assignments/Team Assignments/Team Assignment 1/UKMET.xlsx')

#Removing the integer values
row_delete <- function(df, n) df[-(seq(n,to=nrow(df), by=n)),]

met <- row_delete(met, 2)


met_num <- met%>%
  select(2:13,)%>%
  mutate_all(as.numeric)%>%
  mutate_if(is.numeric, ~. + 14.0)
  


sum(met_num$Jan)

met <- cbind(met$Year, met_num)
met <- met[1:171,] #Removing 2021 row as has NAs

met$`met$Year` <- as.character(met$`met$Year`)

str(met)


months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec')


tidy_met <- gather(met, Month, Temperature, months)

tidy_met$month_num <- ifelse(tidy_met$Month == 'Jan', '01',
                              ifelse(tidy_met$Month == 'Feb', '02',
                                     ifelse(tidy_met$Month == 'Mar', '03',
                                            ifelse(tidy_met$Month == 'Apr', '04',
                                                   ifelse(tidy_met$Month =='May', '05',
                                                          ifelse(tidy_met$Month == 'Jun', '06',
                                                                 ifelse(tidy_met$Month =='Jul', '07',
                                                                        ifelse(tidy_met$Month =='Aug', '08',
                                                                               ifelse(tidy_met$Month =='Sep','09',
                                                                                      ifelse(tidy_met$Month == 'Oct', '10',
                                                                                             ifelse(tidy_met$Month == 'Nov', 11,
                                                                                                    ifelse(tidy_met$Month =='Dec',12,0))))))))))))

tidy_met$Day <- 1


tidy_met$Date <- as.Date(with(tidy_met, paste(`met$Year`, month_num, Day,sep="-")), "%Y-%m-%d")


met_cleaned <- tidy_met%>%
  select(Date, Temperature)%>%
  arrange(Date)


str(met_cleaned)


#Converting to TS
met_ts <- ts(met_cleaned$Temperature, start = c(1850,1), frequency = 12) #starting from January 1850

#Decomposing the Data
fit <- decompose(met_ts, type = 'additive')
plot(fit)

fit <- decompose(met_ts, type = 'multiplicative')
plot(fit)

met_stl <- stl(met_ts, t.window = 13, s.window = 'periodic')
plot(met_stl, main = 'UK Met STL Decomposition')

met_mstl <- mstl(met_ts, s.window = 13) #auto finds best values for stl (need to set s.window to 13)
plot(met_mstl)


#Creating some models
#ETS
met_MMZ <- ets(met_ts, model = 'MMZ')
met_MMZ_pred <- forecast(met_MMZ, h=960, level = c(0.8,0.90))
met_MMZ_pred

met_MMZ_pred_df <- as.data.frame(met_MMZ_pred)

met_MMZ_perf <- accuracy(met_MMZ_pred)

par(mfrow=c(1,1))
plot(met_MMZ_pred, xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))

met_AAZ <- ets(met_ts, model = 'AAZ')
met_AAZ_pred <- forecast(met_AAZ, h=960, level = c(0.8,0.90))
met_AAZ_pred

met_AAZ_pred_df <- as.data.frame(met_AAZ_pred)

met_AAZ_perf <- accuracy(met_AAZ_pred) #More accurate MAPE at first glance than MMZ

plot(met_AAZ_pred, xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))

#Viewing MMZ and AAZ Together
par(mfrow=c(1,2))
plot(met_MMZ_pred, xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))
plot(met_AAZ_pred, xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))

#TBATS
met_tbats <- tbats(met_ts)
met_tbats_pred <- forecast(met_tbats, h=960, level = c(0.8,0.9))

met_tbats_pred
met_tbats_pred_df <- as.data.frame(met_tbats_pred)

met_tbats_perf <- accuracy(met_tbats_pred) #63% MAPE

par(mfrow=c(1,1))
plot(met_tbats_pred, main = 'UK Met Forecasts TBATS(1,{0,0},0.814,{<12,5>})',
     xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))

#ARIMA
#ACF
Acf(diff(log(met_ts),12), main='') #0-6 all significant

Pacf(diff(log(met_ts),12), main='') #0 and 12 both significant 

met_arima <- auto.arima(met_ts, seasonal = FALSE)
met_arima_pred <- forecast(met_arima, h=960, level = c(0.8,0.9))

met_arima_pred
met_arima_pred_df <- as.data.frame(met_arima_pred)

met_arima_n_perf <- accuracy(met_arima_pred) #64.9% Mape

plot(met_arima_pred, xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))

#With Seasonality
met_arima_s <- auto.arima(met_ts, seasonal = TRUE)
met_arima_s_pred <- forecast(met_arima_s, h=960, level = c(0.8,0.9))

met_arima_s_pred
met_arima_s_pred_df <- as.data.frame(met_arima_s_pred)

met_arima_s_perf <- accuracy(met_arima_s_pred) #64.4% MAPE

plot(met_arima_s_pred, xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))

#Viewing both ARIMA models
par(mfrow=c(1,2))
plot(met_arima_pred, xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))
plot(met_arima_s_pred, xlab = 'Year', ylab = 'Predicted Global Temperature (C°)',ylim = c(11.0,20))

#Viewing combined performance
met_perf_row_names <- c('MMZ', 'AAZ', 'TBATS', 'ARIMA N','ARIMA S')

met_model_perf_df <- rbind(met_MMZ_perf, met_AAZ_perf, met_tbats_perf, met_arima_n_perf, met_arima_s_perf)
rownames(met_model_perf_df) <- met_perf_row_names



#Cross Validation 
f_arima <- function(y, h) forecast(auto.arima(y), h = h)
errors_arima_met <- tsCV(met_ts, f_arima, h=1, window = 200)

mape_arima_met <- mean(abs(errors_arima_met/met_ts), na.rm = TRUE)*100
mape_arima_met #63.8%



#--------------------------------------------------------------------------------------------------------------------------------------
#Kingston 

kingston <- read.csv('C:/Users/Matth/Desktop/Matthew/Smith MMA/MMA 867 - Predictive Modelling/Assignments/Team Assignments/Team Assignment 1/kingston ON historical temp data.csv')

kingston_df <- kingston%>%
  select(4:16,)%>%
  rename_with(str_to_title) #Changing col names from all caps to normal

kingston_df$Year <- as.character(kingston_df$Year)

months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec')


tidy_kingston <- gather(kingston_df, Month, Temperature, months)

tidy_kingston$month_num <- ifelse(tidy_kingston$Month == 'Jan', '01',
                             ifelse(tidy_kingston$Month == 'Feb', '02',
                                    ifelse(tidy_kingston$Month == 'Mar', '03',
                                           ifelse(tidy_kingston$Month == 'Apr', '04',
                                                  ifelse(tidy_kingston$Month =='May', '05',
                                                         ifelse(tidy_kingston$Month == 'Jun', '06',
                                                                ifelse(tidy_kingston$Month =='Jul', '07',
                                                                       ifelse(tidy_kingston$Month =='Aug', '08',
                                                                              ifelse(tidy_kingston$Month =='Sep','09',
                                                                                     ifelse(tidy_kingston$Month == 'Oct', '10',
                                                                                            ifelse(tidy_kingston$Month == 'Nov', 11,
                                                                                                   ifelse(tidy_kingston$Month =='Dec',12,0))))))))))))

tidy_kingston$Day <- 1


tidy_kingston$Date <- as.Date(with(tidy_kingston, paste(Year, month_num, Day,sep="-")), "%Y-%m-%d")


kingston_cleaned <- tidy_kingston%>%
  select(Date, Temperature)%>%
  arrange(Date)


str(kingston_cleaned)


#Converting to TS
kingston_ts <- ts(kingston_cleaned$Temperature, start = c(1981,1), frequency = 12) #starting from January 1981

#Decomposing the Data
fit <- decompose(kingston_ts, type = 'additive')
plot(fit)

fit <- decompose(kingston_ts, type = 'multiplicative')
plot(fit)

kingston_stl <- stl(kingston_ts, t.window = 13, s.window = 'periodic')
plot(met_stl)

kingston_mstl <- mstl(kingston_ts, s.window = 13) #auto finds best values for stl (need to set s.window to 13)
plot(met_mstl)


#Creating some models
#ETS
kingston_MMZ <- ets(kingston_ts, model = 'MMZ') #Doesn't work as has negative values
kingston_MMZ_pred <- forecast(kingston_MMZ, h=972, level = c(0.8,0.90))
kingston_MMZ_pred

kingston_MMZ_pred_df <- as.data.frame(kingston_MMZ_pred)

accuracy(kingston_MMZ_pred)

par(mfrow=c(1,1))
plot(kingston_MMZ_pred, xlab = 'Year', ylab = 'Predicted Kingston Temperature (C°)')

kingston_AAZ <- ets(kingston_ts, model = 'AAZ')
kingston_AAZ_pred <- forecast(kingston_AAZ, h=972, level = c(0.8,0.90))
kingston_AAZ_pred

kingston_AAZ_pred_df <- as.data.frame(kingston_AAZ_pred)

kingston_AAZ_perf <- accuracy(kingston_AAZ_pred) #More accurate MAPE at first glance than MMZ

plot(kingston_AAZ_pred, xlab = 'Year', ylab = 'Predicted Kingston Temperature (C°)')

#Viewing MMZ and AAZ Together
par(mfrow=c(1,2))
plot(kingston_MMZ_pred, xlab = 'Year', ylab = 'Predicted Kingston Temperature (C°)')
plot(kingston_AAZ_pred, xlab = 'Year', ylab = 'Predicted Kinsgton Temperature (C°)')

#TBATS
kingston_tbats <- tbats(kingston_ts)
kingston_tbats_pred <- forecast(kingston_tbats, h=972, level = c(0.8,0.9))

kingston_tbats_pred
kingston_tbats_pred_df <- as.data.frame(kingston_tbats_pred)

kingston_tbats_perf <- accuracy(kingston_tbats_pred) #63.7% MAPE & 1.76 RMSE; best performing model

par(mfrow=c(1,1))
plot(kingston_tbats_pred, main = 'Kingston Forecasts from TBATS(1,{0,0},0.8,{12,4})',
     xlab = 'Year', ylab = 'Predicted Kingston Temperature (C°)')


#ARIMA
Acf(diff(log(kingston_ts),12), main='') #0-6 all significant

Pacf(diff(log(kingston_ts),12), main='') #0 and 12 both significant 

kingston_arima <- auto.arima(kingston_ts, seasonal = FALSE)
kingston_arima_pred <- forecast(kingston_arima, h=972, level = c(0.8,0.9))

kingston_arima_pred
kingston_arima_pred_df <- as.data.frame(kingston_arima_pred)

kingston_arima_n_perf <- accuracy(kingston_arima_pred) #86.1% Mape; horrible model

plot(kingston_arima_pred, xlab = 'Year', ylab = 'Predicted Kingston Temperature (C°)')

#With Seasonality
kingston_arima_s <- auto.arima(kingston_ts, seasonal = TRUE)
kingston_arima_s_pred <- forecast(kingston_arima_s, h=972, level = c(0.8,0.9))

kingston_arima_s_pred
kingston_arima_s_pred_df <- as.data.frame(kingston_arima_s_pred)

kingston_arima_s_perf <- accuracy(kingston_arima_s_pred) #67.9% MAPE

plot(kingston_arima_s_pred, xlab = 'Year', ylab = 'Predicted Kingston Temperature (C°)')

#Viewing both ARIMA models
par(mfrow=c(1,1))
plot(kingston_arima_pred, xlab = 'Year', ylab = 'Predicted Kingston Temperature (C°)')
plot(kingston_arima_s_pred, xlab = 'Year', ylab = 'Predicted Kingston Temperature (C°)')

#Viewing combined performance
perf_row_names <- c('AAZ', 'TBATS', 'ARIMA N', 'ARIMA S')

kingston_model_perf_df <- rbind(kingston_AAZ_perf, kingston_tbats_perf, kingston_arima_n_perf, kingston_arima_s_perf)
rownames(kingston_model_perf_df) <- perf_row_names

#Running one-point cross validation on TBATS (best performing model)
f_TBATS <- function(y,h) forecast(tbats(y), h=h)
errors_TBATS_kingston <- tsCV(kingston_ts, f_TBATS, h=1, window = 328) #70% of 468 total observations

mean(abs(errors_TBATS_kingston/kingston_ts), na.rm = TRUE)*100
mean(abs(errors_TBATS_kingston - kingston_ts), na.rm = TRUE)

#EDA----------------------------------------------------------------------------------------------------------------------------------
nasa_top10 <- nasa_cleaned%>%
  slice_max(Temperature, n=10)

mean(nasa_top10$Temperature)

met_top10 <- met_cleaned%>%
  slice_max(Temperature, n=10)

mean(met_top10$Temperature)


#Q5

#Met
met_split <- split(met_cleaned$Temperature, ceiling(seq_along(met_cleaned$Temperature)/120))
met_split <- as.data.frame(met_split)

split_dates <- c('1850-1859', '1860-1869', '1870-1879', '1880-1889', '1890-1899',
                 '1900-1909', '1910-1919', '1920-1929', '1930-1939', '1940-1949',
                 '1950-1959', '1960-1969', '1970-1979', '1980-1989', '1990-1999',
                 '2000-2009', '2010-2019', '2020')


colnames(met_split) <- split_dates

met_2019 <- met_split[1:17]
met_2020 <- as.data.frame(met_split[1:12, 18])
  

std_met_2019 <- as.data.frame(sapply(met_2019, sd))
colnames(std_met_2019) <- 'Met Standard Deviation'

std_met_2020 <- as.data.frame(sapply(met_2020, sd))
colnames(std_met_2020) <- 'Met Standard Deviation'
rownames(std_met_2020) <- '2020'

std_met <- rbind(std_met_2019, std_met_2020)

mean(std_met$`Met Standard Deviation`)

#Plotting Standard Deviation
#Need to add in a column for labels
met_plot_dates <- c(1850,1860,1870, 1880, 1890,
                     1900, 1910, 1920, 1930, 1940,
                     1950, 1960, 1970, 1980, 1990,
                     2000, 2010, 2020)

std_met_plot <- std_met%>%
  mutate(Decade = met_plot_dates)

str(std_met_plot)

ggplot(data=std_met_plot, aes(x=Decade, y=`Met Standard Deviation`)) +
  geom_line()+
  scale_y_continuous(breaks = seq(0.12, 0.20, by=0.01))+
  scale_x_continuous(breaks = seq(1850,2020, by=10))+
  labs(title = 'Met Standard Deviation by Decade')+
  theme_clean()+
  theme(plot.title = element_text(hjust = 0.5))


#Doing the same to calculate average temps in each decade
met_2019_means <- as.data.frame(colMeans(met_2019))
colnames(met_2019_means) <- 'Met Temperature Mean'

met_2020_means <- as.data.frame(colMeans(met_2020))
colnames(met_2020_means) <- 'Met Temperature Mean'
rownames(met_2020_means) <- '2020'

mean_met <- rbind(met_2019_means, met_2020_means) #2020 warmest average temp on record

mean_met_plot <- mean_met%>%
  mutate(Decade = met_plot_dates)

ggplot(data=mean_met_plot, aes(x=Decade, y=`Met Temperature Mean`)) +
  geom_line()+
  scale_y_continuous(breaks = seq(13.5, 15, by=0.1))+
  scale_x_continuous(breaks = seq(1850,2020, by=10))+
  labs(title = 'Met Global Average Temperature by Decade')+
  theme_clean()+
  theme(plot.title = element_text(hjust = 0.5))



#NASA
nasa_split <- split(nasa_cleaned$Temperature, ceiling(seq_along(nasa_cleaned$Temperature)/120))
nasa_split <- as.data.frame(nasa_split)

nasa_split_dates <- c('1880-1889', '1890-1899',
                 '1900-1909', '1910-1919', '1920-1929', '1930-1939', '1940-1949',
                 '1950-1959', '1960-1969', '1970-1979', '1980-1989', '1990-1999',
                 '2000-2009', '2010-2019', '2020')

colnames(nasa_split) <- nasa_split_dates

nasa_2019 <- nasa_split[1:14]
nasa_2020 <- as.data.frame(nasa_split[1:12, 15])

std_nasa_2019 <- as.data.frame(sapply(nasa_2019, sd))
colnames(std_nasa_2019) <- 'Nasa Standard Deviation'

std_nasa_2020 <- as.data.frame(sapply(nasa_2020, sd))
colnames(std_nasa_2020) <- 'Nasa Standard Deviation'
rownames(std_nasa_2020) <- '2020'

std_nasa <- rbind(std_nasa_2019, std_nasa_2020)

mean(std_nasa$`Nasa Standard Deviation`)

#Plotting Standard Deviation
#Need to add in a column for labels
nasa_plot_dates <- c(1880, 1890,
                     1900, 1910, 1920, 1930, 1940,
                     1950, 1960, 1970, 1980, 1990,
                     2000, 2010, 2020)

std_nasa_plot <- std_nasa%>%
  mutate(Decade = nasa_plot_dates)

str(std_nasa_plot)

ggplot(data=std_nasa_plot, aes(x=Decade, y=`Nasa Standard Deviation`)) +
  geom_line()+
  scale_y_continuous(breaks = seq(0.13, 0.18, by=0.01))+
  scale_x_continuous(breaks = seq(1880,2020, by=10))+
  labs(title = 'NASA Standard Deviation by Decade')+
  theme_clean()+
  theme(plot.title = element_text(hjust = 0.5))

#Doing the same to calculate average temps in each decade
nasa_2019_means <- as.data.frame(colMeans(nasa_2019))
colnames(nasa_2019_means) <- 'Nasa Temperature Mean'

nasa_2020_means <- as.data.frame(colMeans(nasa_2020))
colnames(nasa_2020_means) <- 'Nasa Temperature Mean'
rownames(nasa_2020_means) <- '2020'

mean_nasa <- rbind(nasa_2019_means, nasa_2020_means) #2020 warmest average temp on record

mean_nasa_plot <- mean_nasa%>%
  mutate(Decade = nasa_plot_dates)

ggplot(data=mean_nasa_plot, aes(x=Decade, y=`Nasa Temperature Mean`)) +
  geom_line()+
  scale_y_continuous(breaks = seq(13.5, 15, by=0.1))+
  scale_x_continuous(breaks = seq(1850,2020, by=10))+
  labs(title = 'Nasa Global Average Temperature by Decade')+
  theme_clean()+
  theme(plot.title = element_text(hjust = 0.5))




#CSV----------------------------------------------------------------------------------------------------------------------------------
write.csv(nasa_arima_s_pred, 'Nasa Predictions.csv')

write.csv(nasa_cleaned, 'Nasa_Cleaned.csv')
write.csv(met_cleaned, 'Met_Cleaned.csv')


#NASA Predictions (ARIMA Seasonal)
write.xlsx(nasa_arima_s_pred_df, 'NASA ARIMA w Seasonality Predictions.xlsx')

#MET Predictions (TBATS)
write.xlsx(met_tbats_pred_df, 'UK Met TBATS Predictions.xlsx')

#Kingston Predictions (TBATS)
write.xlsx(kingston_tbats_pred_df, 'Kingston TBATS Predictions.xlsx')

write.xlsx(std_nasa, 'STD Nasa.xlsx')
write.xlsx(std_met, 'STD Met.xlsx')

#Model Performance
write.xlsx(nasa_model_perf_df, 'Nasa Model Performance.xlsx')
write.xlsx(met_model_perf_df, 'Met Model Performance.xlsx')
write.xlsx(kingston_model_perf_df, 'Kingston Model Performance.xlsx')



