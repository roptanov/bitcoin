library(dplyr)
library(rusquant)
library(quantmod)
library(rpart)
library(rpart.plot)
library(caret)
library(curl)
library(nnet)
library(lubridate)
library(dplyr)
library("e1071")
library(ggplot2)
library(lubridate)
library(xts)

coindesk = read.csv('C:/Users/Ilya/Desktop/МОР/coindesk.csv', sep = ',', header=TRUE)
bitc = coindesk[2000:2742,]
bitc$Date = ymd_hms(bitc$Date) # пребразовываем в time-series 
rownames(bitc) = bitc$Date
stocks <- xts(bitc[,-1], order.by=as.Date(bitc[,1], "%Y-%m-%d %H:%M:%S"))
bitc = stocks

PriceChange<- Cl(bitc)-Op(bitc)
Responce<-data.frame(ifelse(PriceChange>0,"UP", "DOWN"))
colnames(Responce) = 'Result'
compare <- data.frame(PriceChange, Responce)

Responce <- as.character(Responce$Result)
Responce <- c(Responce, NA)
Responce <- Responce[2:length(Responce)]
Responce




bitc = na.omit(bitc)

ADX <- ADX(bitc, n=4)
ADX <- ADX[,4]


BB <- BBands(Cl(bitc), n=8, sd = 2)
BBdiff <- BB$up-BB$dn
BBpos <- (BB$up-Cl(bitc))/BBdiff

BB <- BBands(Cl(bitc), n=9, sd = 2)
BB = na.omit(BB)
BBdiff <- BB$up-BB$dn

CCI <- CCI(Cl(bitc),n=6)
CCImove <- diff(CCI(Cl(bitc),n=32))

RSI <- RSI(Cl(bitc),n=3)
RSImove <- diff(RSI(Cl(bitc),n=18))

EMA<-EMA(Cl(bitc),n=5) 
EMAcross<- Cl(bitc)-EMA(Cl(bitc),n=4) 
EMAmove <- diff(Cl(bitc)-EMA(Cl(bitc),n=4))

MACD<-MACD(Cl(bitc), nFast = 4, nSlow = 8, nSig = 7) 
MACDdiff<-MACD[,1]-MACD[,2] 
MACD<-MACD(Cl(bitc), nFast = 9, nSlow = 16, nSig = 7) 
MACDmove<-diff(MACD[,1]-MACD[,2])


data<-data.frame(Responce,ADX,BBpos,CCI,CCImove,RSI,RSImove,EMAcross,EMAmove,MACDdiff,MACDmove) 
colnames(data)<-c('Responce','ADX', 'BBpos','CCI','CCImove','RSI','RSImove','EMAcross','EMAmove','MACDdiff','MACDmove') 
data<-na.omit(data)


train<-data[1:c(round(nrow(data)*0.8, digits = 0)), ] 
test<-data[c(round(nrow(data)*0.8, digits = 0) + 1): nrow(data), ]
set.seed(3)


library(randomForest)
# пробуем случайный лес
model = randomForest(Responce~.,data=train,ntree=25)
model.predict = predict(model,newdata=test,ntree=25)
confusionMatrix(model.predict,test$Responce)

# пробуем дерево решений
Resptree<-rpart(Responce~.,data=train, cp=.001, method="class")
prp(Resptree,type=2,extra=8)
printcp(Resptree)
Respprunedtree<-prune(Resptree,cp= 0.001)
prp(Respprunedtree, type=2, extra=8)
confusionMatrix(predict(Respprunedtree,train,type="class"), train[,1])
confusionMatrix(predict(Respprunedtree,test,type="class"), test[,1])
