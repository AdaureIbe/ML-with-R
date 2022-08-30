#Install and Load Packages 

library(sqldf)
library(tidyr)
library(readxl)
library(ggplot2)
library(stringr)
library(caret)
library(visdat)
library(mice)
library(rstatix)
library(car)
library(estimatr)
library(glmnet)
library(ISLR)
library(dplyr)
library(tidyverse) 
library(dplyr)
library(reshape2) 
library(plyr)
library(scales) 
library(ggrepel)
library(corrplot) 
library(GGally) 
library(ggthemes) 
library(ggalt) 
library(ggdendro) 
library(crosstalk)
library(plotly)

#Import Data

htrain <-read.csv(file.choose(), header = TRUE, sep = ",",stringsAsFactors = TRUE) #Import train data set

#Exploratory Data Analysis
str(htrain)
dim(htrain) #1460 obs   81 variables

#Exploring Distribution

#Histogram- Data is noticeable skewed to the right
ggplot(data=htrain[!is.na(htrain$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="purple", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

#Exploring Correlation for numerical variables

numvar <-select_if(htrain, is.numeric) #38 numerical variables 
numvarname <- names(numvar)
numvarcor <- cor(numvar, use="pairwise.complete.obs") 
cor_sorted <- as.matrix(sort(numvarcor[,'SalePrice'], decreasing = FALSE))#sort on increasing correlations with SalePrice
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5))) #select only high correlations
numvarcor <- numvarcor[CorHigh, CorHigh]
corrplot.mixed(numvarcor, tl.col="brown", tl.pos = "lt")

#BoxPlot With The Highest Correlation

ggplot(data=htrain[!is.na(htrain$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='steel blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

#ScatterPlot with the second Highest Correlation (Explore Other Scatterplot Variables)

ggplot(data=htrain[!is.na(htrain$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='steel blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(htrain$GrLivArea[!is.na(htrain$SalePrice)]>4500, rownames(htrain), '')))

#BivariateAnalysis

htrain$totalsqfootage<-htrain$X1stFlrSF + htrain$X2ndFlrSF 
ggplot(htrain, aes(x=totalsqfootage, y=SalePrice))+
  geom_smooth()+
  labs(title = " Square Footage VS Price") #Explore relationship between SqFt and Price

ggplot(htrain, aes(x=GrLivArea, y=SalePrice))+
  geom_smooth()+
  labs(title = " Living Area VS Price") #Explore relationship between  Living Area and Price

htrain$AvgScore<-(htrain$OverallCond + htrain$OverallQual)/2
ggplot(htrain, aes(x=AvgScore, y=SalePrice))+
  geom_smooth()+
  labs(title = "Quality VS Price") #Explore relationship between Average Score vs Price

#Review How Variables Relate To Price

htrain$Bsmt<-ifelse(is.na(htrain$BsmtQual), "WO_Basement","W_Basement")
htrain$Garage<-ifelse(is.na(htrain$BsmtQual), "WO_Garage","W_Garage")
htrain$Remod<-ifelse(htrain$YearRemodAdd-htrain$YearBuilt>0, "Remodel","No_Remodel")
ggplot(htrain, aes(x=AvgScore, y=SalePrice, color=Bsmt, shape=BldgType))+
  geom_jitter()+
  labs(title = "Overall Condition & Price") #Explore relationship between overall condition vs Price

htrain$HomeAge<-htrain$YrSold-htrain$YearBuilt
ggplot(htrain, aes(x=HomeAge, y=SalePrice, color=BldgType))+
  geom_jitter()+
  labs(title = "Home Age Vs Price") #Explore the relationship between Home Age vs Price

#Reviewing Missing Values
vis_dat(htrain)
vis_miss(htrain)
vis_miss(htrain, cluster = TRUE)

htrain_missing <- function(x){sum(is.na(x))/length(x)*100} #function to measure missingness 
htrain_missing2 <-as.matrix(apply(htrain,2,htrain_missing))

htrain_missing2 #output of missing data by percenatge 

MissingV <- names(which(apply(htrain_missing2, 1, function(x) abs(x)>0)))#names of numeric variables with missing data
MissingV

sort(colSums(sapply(htrain[MissingV], is.na)), decreasing = TRUE)


#Data Combination for modeling and cross validation

fullhouse <-read.csv(file.choose(), header = TRUE, sep = ",")

#Treat Missing Value Variables

fullhouse$MSZoning<-ifelse(is.na(fullhouse$MSZoning), "RL", 
                      ifelse(fullhouse$MSZoning=="C (all)","C",fullhouse$MSZoning)) #clean Mszoning
fullhouse$Alley<-ifelse(is.na(fullhouse$Alley), "AB",fullhouse$Alley)#replace missing values with mode
fullhouse$Utilities<-ifelse(is.na(fullhouse$Utilities), "AllPub",fullhouse$Utilities)#replace missing values with mode
fullhouse$Exterior1st<-ifelse(is.na(fullhouse$Exterior1st), "VinylSd",fullhouse$Exterior1st)#replace missing values with mode
fullhouse$Exterior2nd<-ifelse(is.na(fullhouse$Exterior2nd), "VinylSd",fullhouse$Exterior2nd)#replace missing values with mode
fullhouse$MasVnrType<-ifelse(is.na(fullhouse$MasVnrType) & fullhouse$MasVnrArea>0, "BrkFace",fullhouse$MasVnrType)#replace missing values with mode
fullhouse$MasVnrType<-ifelse(fullhouse$MasVnrArea==0, "None",fullhouse$MasVnrType)#Replace "0" with Non Type
fullhouse$MasVnrArea<-ifelse(fullhouse$MasVnrType=="None",0,fullhouse$MasVnrArea)#Replace non types with O
fullhouse$BsmtQual<-ifelse(is.na(fullhouse$BsmtQual), "Ab",fullhouse$BsmtQual)#Replace NA with Absent
fullhouse$BsmtCond<-ifelse(is.na(fullhouse$BsmtCond), "Ab",fullhouse$BsmtCond)#Replace NA with Absent
fullhouse$BsmtExposure<-ifelse(is.na(fullhouse$BsmtExposure), "Ab",fullhouse$BsmtExposure)#Replace NA with Absent
fullhouse$BsmtFinType1<-ifelse(is.na(fullhouse$BsmtFinType1), "Abs",fullhouse$BsmtFinType1)#Replace NA with Absent
fullhouse$BsmtFinSF1<-ifelse(is.na(fullhouse$BsmtFinSF1),0,fullhouse$BsmtFinSF1)#Replace blanks with 0
fullhouse$BsmtFinType2<-ifelse(is.na(fullhouse$BsmtFinType2), "Abs",fullhouse$BsmtFinType2)##Replace NA with Absent
fullhouse$BsmtFinSF2<-ifelse(is.na(fullhouse$BsmtFinSF2),0,fullhouse$BsmtFinSF2)#Replace blanks with 0
fullhouse$BsmtUnfSF<-ifelse(is.na(fullhouse$BsmtUnfSF),0,fullhouse$BsmtUnfSF)#Replace blanks with 0
fullhouse$TotalBsmtSF<-ifelse(is.na(fullhouse$TotalBsmtSF),0,fullhouse$TotalBsmtSF)#Replace blanks with 0
fullhouse$Electrical<-ifelse(is.na(fullhouse$Electrical), "SBrkr",fullhouse$Electrical) #Replace NA with mode
fullhouse$BsmtFullBath<-ifelse(is.na(fullhouse$BsmtFullBath),0,fullhouse$BsmtFullBath) #Replace NA with 0
fullhouse$BsmtHalfBath<-ifelse(is.na(fullhouse$BsmtHalfBath),0,fullhouse$BsmtHalfBath) #Replace NA with 0
fullhouse$KitchenQual<-ifelse(is.na(fullhouse$KitchenQual),"TA",fullhouse$KitchenQual) ##Replace NA with TA
fullhouse$Functional<-ifelse(is.na(fullhouse$Functional),"Typ",fullhouse$Functional) #Replace NA as Typ
fullhouse$FireplaceQu<-ifelse(is.na(fullhouse$FireplaceQu),"Ab",fullhouse$FireplaceQu) #Replace NA with Absent 
fullhouse$GarageType<-ifelse(is.na(fullhouse$GarageType),"Ab",fullhouse$GarageType) #Replace NA with Absent 
fullhouse$GarageFinish<-ifelse(fullhouse$GarageType=="Detchd" & is.na(fullhouse$GarageFinish),"Unf",home$GarageFinish)#Replace missing values with Mode
fullhouse$GarageCars<-ifelse(is.na(fullhouse$GarageCars),0,fullhouse$GarageCars) #Replace NA with 0
fullhouse$GarageArea<-ifelse(is.na(fullhouse$GarageArea),0,fullhouse$GarageArea) #Replace NA with 0
fullhouse$GarageQual<-ifelse(is.na(fullhouse$GarageQual),"Ab",fullhouse$GarageQual) #Replace NA as Ab
fullhouse$GarageCond<-ifelse(is.na(fullhouse$GarageCond),"Ab",fullhouse$GarageCond) #Replace NA as Ab
fullhouse$PoolQC<-ifelse(is.na(fullhouse$PoolQC),"Ab",fullhouse$PoolQC) #Replace NA as Ab
fullhouse$Fence<-ifelse(is.na(fullhouse$Fence),"Ab",fullhouse$Fence) #Replace NA as Ab
fullhouse$MiscFeature<-ifelse(is.na(fullhouse$MiscFeature),"Ab",fullhouse$MiscFeature) #Replace NA as Ab
fullhouse$SaleType<-ifelse(is.na(fullhouse$SaleType),"WD",fullhouse$SaleType) #Replace NA with Mode
fullhouse$GarageFinish<-ifelse(is.na(fullhouse$GarageFinish),"Ab",fullhouse$GarageFinish) #Replace NA as Ab
fullhouse$MasVnrType<-ifelse(is.na(fullhouse$MasVnrType), "None",fullhouse$MasVnrType) #Replace NA with None
fullhouse$GarageYrBlt<-ifelse(fullhouse$GarageYrBlt==2207, "1984", fullhouse$GarageYrBlt) #Replace error value with Avg Attached garage year
fullhouse$YearRemodAdd<-ifelse(fullhouse$YearRemodAdd-fullhouse$YearBuilt<0,fullhouse$YearBuilt,fullhouse$YearRemodAdd) #ensure remodel year is newer than or equal to build year
fullhouse$RoofMatl<-ifelse(fullhouse$RoofMatl %in% c("ClyTile","Membran","Metal","Roll"),"Oth",fullhouse$RoofMat)
fullhouse$Condition1<-ifelse(fullhouse$Condition1 %in% c("RRAe","RRNn","RRAn","RRNe"),"RR",fullhouse$Condition1)
fullhouse$Condition2<-ifelse(fullhouse$Condition2 %in% c("RRAe","RRNn","RRAn","RRNe"),"RR",fullhouse$Condition2)
fullhouse$LotFrontage<-ifelse(is.na(fullhouse$LotFrontage),0,fullhouse$LotFrontage)
fullhouse$GarageYrBlt<-ifelse(is.na(fullhouse$GarageYrBlt),fullhouse$YearBuilt,fullhouse$GarageYrBlt)
fullhouse$MasVnrArea<-ifelse(is.na(fullhouse$MasVnrArea),0,fullhouse$MasVnrArea)
fullhouse$GarageYrBlt<-ifelse(fullhouse$GarageType=="Ab",0,fullhouse$GarageYrBlt)

#Check missing data after treating NA's

#Review data after treating missing data
vis_dat(fullhouse)
vis_miss(fullhouse, cluster = TRUE) #Huge Decrease in NA

t(lapply(fullhouse, function(x) sum(is.na(x)))) #Missing Values Listed in Output

#Convert strings to factors

fullhouse<-strings2factors(fullhouse) #convert strings variables to factors to prep for modelling

#Reviewing Converted Data 

str(fullhouse)

#Feature Engineering

#Interactions, Features & Dummies

fullhouse$Accssbl<-ifelse(fullhouse$Alley=="Ab",0,1)#Create Accecesibilty dummy  
fullhouse$OvQC<-(fullhouse$OverallCond+fullhouse$OverallQual)/2 # Creating the average of quality and condo rating
fullhouse$NewRmdl<-ifelse(fullhouse$YrSold-fullhouse$YearRemodAdd<3, 1,0) # highlight homes that were modeled less than three years before sale
fullhouse$Remd<-ifelse(fullhouse$YearBuilt==fullhouse$YearRemodAdd, 0,1)#highlight homes that have been remodeled
fullhouse$Firplc<-ifelse(fullhouse$FireplaceQu=="Ab", 0,1)#highlight homes with fireplaces
fullhouse$Garage<-ifelse(fullhouse$GarageType=="Ab",0,1)#higlight homes that have garages
fullhouse$Sq_Foot<- fullhouse$TotalBsmtSF + fullhouse$X1stFlrSF + fullhouse$X2ndFlrSF+fullhouse$WoodDeckSF+fullhouse$OpenPorchSF+fullhouse$X3SsnPorch+fullhouse$ScreenPorch+fullhouse$PoolArea+fullhouse$GarageArea #Total Square footage.
fullhouse$Bsmt<-ifelse(fullhouse$BsmtQual=="Ab",0,1) #highlight homes that have Basements
fullhouse$Pool<-ifelse(fullhouse$PoolQC=="Ab",0,1) #highlight homes that have pools
fullhouse$Porch<-ifelse((fullhouse$OpenPorchSF+fullhouse$EnclosedPorch+fullhouse$X3SsnPorch+fullhouse$ScreenPorch)>0,1,0) #Highlight homes that include porches
fullhouse$IndPlay<-ifelse(fullhouse$BsmtFinType1=="Rec" || fullhouse$BsmtFinType2=="Rec", 1,0)# Highlight rec room as indoor play
fullhouse$OutPlay<-ifelse(fullhouse$MiscFeature=="Tenc"|| fullhouse$LotConfig %in% c("FR2","FR3"),1,0) #Highlight tennis courts and extra frontages as outdoor play
fullhouse$HighTraf<-ifelse(fullhouse$Condition1=="Artery" || fullhouse$Condition2=="Artery",1,0)#Highlight arterial streets as high traffic areas
fullhouse$Noise<-ifelse(fullhouse$Condition1 == "RR"|| fullhouse$Condition2 =="RR",1,0) #Highlight homes close to railway as noisy areas
fullhouse$BigFamily<-ifelse(fullhouse$BedroomAbvGr>4,1,0) #Highlight homes with more than 4 bedrooms 
fullhouse$KitchenProj<-ifelse(fullhouse$KitchenQual %in% c("Fa","Po"),1,0) #Highlight kitchen quality 
fullhouse$GarageProj<-ifelse(fullhouse$GarageQual %in% c("Fa","Po"),1,0) #Highlight garage quality 
fullhouse$HeatingProj<-ifelse(fullhouse$HeatingQC %in% c("Fa","Po"),1,0) #Highlight heating quality 
fullhouse$BasementProj<-ifelse(fullhouse$BsmtCond %in% c("Fa","Po"),1,0) #Highlight basement condition
fullhouse$ExternProj<-ifelse(fullhouse$ExterCond %in% c("Fa","Po"),1,0) #Highlight home external condition 
fullhouse$ProjHome<-ifelse((fullhouse$KitchenProj+fullhouse$HeatingProj+fullhouse$ExternProj+fullhouse$BasementProj)>2,1,0) #Highlight if heating kitchen and external condition are poor and need to be fixed
fullhouse$SeasonSold<-as.factor(ifelse(fullhouse$MoSold>2,"Spring", ifelse(fullhouse$MoSold>5,"Summer",ifelse(fullhouse$MoSold>7,"Fall","Winter")))) #Highlight season for Month sold
fullhouse$Privacy<-ifelse(fullhouse$Fence %in% c("GdPrv", "MnPrv"),1,0) #Highlight homes where fence provides privacy
fullhouse$BetterAir<-ifelse(fullhouse$CentralAir=="Y",1,0) #Highlight homes with central air
fullhouse$HomeAge<-fullhouse$YrSold-fullhouse$YearBuilt #Highlight home age
fullhouse$AvgScore<-(fullhouse$OverallCond + fullhouse$OverallQual)/2 #calculate home score
fullhouse$Rebuild<-ifelse(fullhouse$AvgScore<5 || fullhouse$Remd==0, 1,0) #Highlight homes with overall score of less than 5 as renovated homes
fullhouse$Insulation<-ifelse(fullhouse$YearBuilt<1950 || fullhouse$Remd==0,1,0) #Highlight that homes built before 1950 that haven't been remodeled will need new insulation
fullhouse$Bsmtbath<-fullhouse$BsmtFullBath+fullhouse$BsmtHalfBath #total baths in basement
fullhouse$AbvBath<-fullhouse$FullBath+fullhouse$HalfBath #total baths above ground
fullhouse$TotBath<-fullhouse$Bsmtbath + fullhouse$AbvBath #Total Baths
fullhouse$TopMsnVnr<-ifelse(fullhouse$MasVnrType %in% c("BrkFace","Stone"),1,0) #Premium Masonry Veneer Types
fullhouse$LwCst<-ifelse(fullhouse$Neighborhood %in% c("MeadowV","IDOTRR","BrDale"),1,0) #neighborhood with lowest prices
fullhouse$Affluent<-ifelse(fullhouse$Neighborhood %in% c("StoneBr","NridgHt","NoRidge"),1,0) #neighborhood with highest prices
fullhouse$BestMonth<-ifelse(fullhouse$MoSold %in% c(9,11),1,0) #Months with highest prices
fullhouse$BetterYrs<-ifelse(fullhouse$YrSold %in% c(2006,2007),1,0) #years with highest prices
fullhouse$PremGrg<-ifelse(fullhouse$GarageType %in% c("Attchd","BuiltIn"),1,0) #garage types with higher prices
fullhouse$BetFence<-ifelse(fullhouse$Fence %in% c("GdPrv","Ab"),1,0)#Fence options with higher prices
fullhouse$BetElect<- ifelse(fullhouse$Electrical=="SBrkr",1,0) #Electrical options with highest prices
fullhouse$BetHeat<- ifelse(fullhouse$Heating %in% c("GasA","GasW","OthW"),1,0) #Gas options with highest prices
fullhouse$BetRoofMatl<- ifelse(fullhouse$RoofMatl %in% c("Membran","WdShake","WdShngl"),1,0) #Roof Material options with highest prices
fullhouse$BetRoofType<- ifelse(fullhouse$RoofStyle %in% c("Hip","Shed"),1,0) #Roof Style options with highest prices
fullhouse$BestBldType<- ifelse(fullhouse$BldgType %in% c("1Fam","TwnhsE"),1,0) #Building Type options with highest prices
fullhouse$BestLotConfig<- ifelse(fullhouse$LotConfig %in% c("FR3","CulDSac"),1,0) #Lotconfig options with highest prices
fullhouse$BetUt<- ifelse(fullhouse$Utilities=="AllPub",1,0) #Utility options with highest prices
fullhouse$BestSlCnd<-ifelse(fullhouse$SaleCondition=="Partial",1,0) #Sale condition options with highest prices
fullhouse$LwSlCnd<-ifelse(fullhouse$SaleCondition=="AdjLand",1,0) #Sale condition options with lowest prices

#Split Into Test and Train. 

fullhouse_train<-subset(fullhouse, Id<=1460) 
fullhouse_predict<-subset(fullhouse, Id>1460)


#Create cross fold validation dataset
fullhouse_test<- subset(fullhouse_train,Id>1000)
fullhouse_train<- subset(fullhouse_train,Id<=1000)


#Model 1 - TTT, Linear Regression

reg1<-lm(SalePrice~.-Id, fullhouse_train)
summary(reg1)
plot(reg1)
plot(density(resid(reg1)))
ncvTest(reg1) #RegIsHeteroscedastic


#Model 2 - Logistic Regression

reg2<-lm(log(SalePrice)~MSSubClass + MSZoning + LotFrontage + LotArea + Street + Alley + LotShape +
          LandContour + Utilities + LotConfig + LandSlope + Neighborhood+Condition1+ Condition2+
          BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd +
          RoofMatl + MasVnrType + MasVnrArea + ExterQual + ExterCond +
          Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 +
          BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + HeatingQC + CentralAir + Electrical + 
          log(X1stFlrSF) + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + 
          FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + log(TotRmsAbvGrd) + Functional + 
          Fireplaces + FireplaceQu + GarageType + GarageFinish + GarageCars +
          GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + 
          EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + Fence  +
          MiscVal + MoSold + YrSold + SaleType+SaleCondition + Accssbl + OvQC + NewRmdl + 
          Remd + Firplc + Garage + log(Sq_Foot) +Bsmt + Pool + Porch + BetterAir + HomeAge +
          IndPlay +  OutPlay + HighTraf + Noise + BigFamily + KitchenProj + GarageProj + 
          HeatingProj + BasementProj + ExternProj + ProjHome + SeasonSold + Privacy + 
          AvgScore + Rebuild + Insulation + Bsmtbath + AbvBath + TotBath+TopMsnVnr + 
          LwCst + Affluent + BestMonth + BetterYrs + PremGrg + BetFence + BetElect + 
          BetHeat + BetRoofMatl + BetRoofType + BestBldType + BestLotConfig + BetUt+ 
          BestSlCnd + LwSlCnd, fullhouse_train)


summary(reg2)
plot(reg2)
plot(density(resid(reg2)))

#Model Quality 
predict_reg1<-predict(reg1,fullhouse_test)

percent_error<-abs((fullhouse_test$SalePrice-predict_reg)/fullhouse_test$SalePrice)*100 #Calculate percent error
mape<-mean(percent_error)
mape

#Model 3 - Lasso Regression 


# #create the y variable and matrix (capital X) of x variables (will make the code below easier to read + will ensure that all interactoins exist)

y <-log(fullhouse_train$SalePrice)

X<-model.matrix(Id ~ MSSubClass + MSZoning + LotFrontage + LotArea + Street + Alley + LotShape +
                  LandContour + Utilities + LotConfig + LandSlope + Neighborhood+Condition1+ Condition2+
                  BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle+
                  RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond +
                  Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 +
                  BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical + 
                  X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + 
                  FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Functional + 
                  Fireplaces + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars +
                  GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + 
                  EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature +
                  MiscVal + MoSold + YrSold + SaleType+SaleCondition + Accssbl + OvQC + NewRmdl + 
                  Remd + Firplc + Garage + Sq_Foot +Bsmt + Pool + Porch + BetterAir + HomeAge +
                  IndPlay +  OutPlay + HighTraf + Noise + BigFamily + KitchenProj + GarageProj + 
                  HeatingProj + BasementProj + ExternProj + ProjHome + SeasonSold + Privacy + 
                  AvgScore + Rebuild + Insulation + Bsmtbath + AbvBath + TotBath+TopMsnVnr + 
                  LwCst + Affluent + BestMonth + BetterYrs + PremGrg + BetFence + BetElect + 
                  BetHeat + BetRoofMatl + BetRoofType + BestBldType + BestLotConfig + BetUt+ 
                  BestSlCnd + LwSlCnd+log(Sq_Foot)/TotRmsAbvGrd + AvgScore*YearBuilt+Sq_Foot*TotRmsAbvGrd + 
                  OverallQual*YearBuilt + OverallQual*YearRemodAdd + OverallQual*MasVnrArea + OverallQual*BsmtFinSF1+
                  KitchenQual*KitchenAbvGr*YearBuilt+WoodDeckSF*ScreenPorch*X3SsnPorch+
                  SaleCondition*YearBuilt+log(Sq_Foot)*OverallQual+Affluent*OverallQual+GrLivArea*AvgScore,fullhouse)[,-1]
X<-cbind(fullhouse$Id,X)

# split X into testing, training/holdout and prediction as before

X.train<-subset(X,X[,1]<=1000)
X.test<-subset(X, (X[,1]>1000 & X[,1]<=1460))
X.predict<-subset(X,X[,1]>=1461)

#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.train, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossvalidate <-  cv.glmnet(x = X.train, y = y, alpha = 1) #create cross-validation data
plot(crossvalidate)
penalty.lasso <- crossvalidate$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
plot(crossvalidate,xlim=c(-6.0,-4.0),ylim=c(0.00,0.04)) # lets zoom-in
lasso.opt.fit <-glmnet(x = X.train, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
lasso.test <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.test))
mean(abs(lasso.test-fullhouse_test$SalePrice)/fullhouse_test$SalePrice*100) #calculate and display MAPE
RMSE(lasso.test,fullhouse_test$SalePrice)

#create dataframe of Coefficients and Features

myCoefs <- coef(lasso.opt.fit)
myCoefs[which(myCoefs != 0 ) ]         
myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ]

 output <- data.frame(
  features = myCoefs@Dimnames[[1]][ which(myCoefs != 0 ) ], #intercept included
  coefs    = myCoefs              [ which(myCoefs != 0 ) ]  #intercept included
)
write.csv(output, file = "submissionfileagainagain.csv") 

getwd()

predicted.prices.log.i.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.predict))
write.csv(predicted.prices.log.i.lasso, file = "FinalSubmission22.csv") # export the predicted prices into a CSV file

getwd()
