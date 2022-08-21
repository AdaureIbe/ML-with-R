library(tidyverse)
library(readxl)
library(mice)
library(car)
library(caret)
library(corrplot)
library(tidyr)
library(estimatr)


#Pass in Faye final data file below in quotes
master_df <- read_excel('C:/Users/Matth/Desktop/Matthew/Smith MMA/MMA 860 - Acquisition and Management of Data/Team Assignments/Major Project/Dataset/master_file.xlsx')

#***DATA CLEANING***

#Removing $ sign from fuel price
master_df$Fuel_Price <- as.numeric(gsub( "[$,]", "", master_df$Fuel_Price))

#Adding dummy variable to indicate a holiday
master_df$IsHoliday <- ifelse(master_df$IsHoliday == 'TRUE', 1, 0)

#Adding in dummy variables for Store class
master_df$Class_A <- ifelse(master_df$Type == 'A', 1 ,0)
master_df$Class_B <- ifelse(master_df$Type == 'B', 1 ,0)
master_df$Class_C <- ifelse(master_df$Type == 'C', 1 ,0)

str(master_df)
summary(master_df$Sales)

#Changing NA's to 0 in Markdown column

master_df$MarkDown1 <- ifelse(master_df$MarkDown1 == 'NA', as.numeric(0), as.numeric(master_df$MarkDown1))
master_df$MarkDown2 <- ifelse(master_df$MarkDown2 == 'NA', as.numeric(0), as.numeric(master_df$MarkDown2))
master_df$MarkDown3 <- ifelse(master_df$MarkDown3 == 'NA', as.numeric(0), as.numeric(master_df$MarkDown3))
master_df$MarkDown4 <- ifelse(master_df$MarkDown4 == 'NA', as.numeric(0), as.numeric(master_df$MarkDown4))
master_df$MarkDown5 <- ifelse(master_df$MarkDown5 == 'NA', as.numeric(0), as.numeric(master_df$MarkDown5))

master_df$Store <- as.character(master_df$Store) #Changing to character so does not enter correlation graph



#***DATA EXPLORATION***

#Correlation
#Filtering for all continuous variables
cor_df <- master_df%>%
  select(Temperature, Fuel_Price, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5,
         CPI, Unemployment, Sales, size)

corr <- cor(cor_df[sapply(cor_df, is.numeric)]) 

corrplot(corr)
#Looks like Size of store is highly correlated with sales
#CPI, UI and holiday do not appear to be highly correlated with sales


#Looking at Store A characteristics 
store_A <- master_df%>%
  filter(Class_A == 1)

summary(store_A)
hist(store_A$Sales)

sqft_A <- c(max(store_A$size), min(store_A$size))

#Store B
(store_B <- master_df%>%
  filter(Class_B == 1))

hist(store_B$Sales)
summary(store_B)
#interesting that max sales in B store are close to A
#Wonder which store it was and what the sqft is

sqft_B <- c(max(store_B$size), min(store_B$size))


#Store C
(store_C <- master_df%>%
    filter(Class_C == 1))

hist(store_C$Sales)
summary(store_C)

mean(store_C$Sales)

sqft_C <- c(max(store_C$size), min(store_C$size))


#Putting all SQFT together
sqft_df <- as.data.frame(cbind(sqft_A, sqft_B, sqft_C)) %>%
  gather(Store, Size)


#Examining other relationships
plot(master_df$Sales, master_df$Temperature)
cor(master_df$Sales, master_df$Temperature)

ggplot(master_df, aes(x = Sales, y = Temperature))+
  geom_hex(bins =20)
#not much of a relationship between sales and temp

plot(master_df$Sales, master_df$Fuel_Price)
cor(master_df$Sales, master_df$Fuel_Price)

#Fuel price is interesting; as fuel price goes up, sales go down

plot(master_df$Sales, master_df$CPI)
cor(master_df$Sales, master_df$CPI)
#When CPI goes up, sales go down; makes sense as rising CPI indicates inflation & higher pricing 

plot(master_df$Sales, master_df$Unemployment)
cor(master_df$Sales, master_df$Unemployment)
#Close to zero correlation between UI and sales; almost no linear relationship between them

plot(master_df$Sales, master_df$size)
cor(master_df$Sales, master_df$size)
#easier to view via hexbin
ggplot(master_df, aes(x = Sales, y = size))+
  geom_hex()


cor(master_df$CPI, master_df$Fuel_Price)



#***PREDICTIVE MODELLING*** Use FINAL MODEL from line 161 instead

#Below is just here for the sake of thought process
#Takeaway from below is that we found collinearity between Class and Size

variable.names(master_df)

#Using TTT Approach to begin
reg <- lm(Sales ~ Temperature + Fuel_Price + MarkDown1 + MarkDown2 + MarkDown3 +
            MarkDown4 + MarkDown5 + CPI + Unemployment + IsHoliday + size +
            Class_B + Class_C, master_df)
summary(reg)

vif(reg)

#Testing to see if MD1, MD2, MD3 beling
linearHypothesis(reg, c('MarkDown1 = 0', 'MarkDown2 = 0', 'MarkDown4 = 0'))
#insignificant - leave out of model

reg <- lm(Sales ~ Temperature + Fuel_Price +  MarkDown3 +
             MarkDown5 + CPI + Unemployment + IsHoliday + size +
            Class_A, master_df)
summary(reg)

plot(density(resid(reg)))

plot(reg)
ncvTest(reg)


summary(reg)



#FINAL MODEL
#Decided to predict by segmenting by holiday
holiday_df <- master_df%>%
  filter(IsHoliday == 1)

#Splitting into train/test data
sample <- sample.int(n = nrow(holiday_df), size=floor(.7*nrow(holiday_df)), replace = F)
train <- holiday_df[sample,]
test <- holiday_df[-sample,]

#Using TTT approach to begin
reg <- lm(Sales ~ Temperature + Fuel_Price + MarkDown1 + MarkDown2 + MarkDown3 +
            MarkDown4 + MarkDown5 + CPI + Unemployment + size, train)
summary(reg)

vif(reg)
#MarkDown 1, 4 , 5 all have high values in VIF; evidence of collinearity

linearHypothesis(reg, c('MarkDown1 = 0', 'MarkDown2 = 0', 'MarkDown4 = 0', 'MarkDown5 = 0'))
#insignificant - leave out of model

reg <- lm(Sales ~ Temperature + Fuel_Price + MarkDown3 + CPI  + size, train)
summary(reg)
plot(density(resid(reg))) #looks much more normal

vif(reg) #Data now appears to be clean of collinearity 

ncvTest(reg)
#significant p-value; therefore data is heteroskedastic

reg <- lm_robust(Sales ~ Temperature + Fuel_Price + MarkDown3 +
            MarkDown5 + CPI  + size, train, se_type = 'HC2')
summary(reg)

pred <- predict(reg, test)

R2 = R2(pred, test$Sales) #Final R2 of 0.6347 on test data
RMSE = RMSE(pred, test$Sales)
MAE = MAE(pred, test$Sales)













