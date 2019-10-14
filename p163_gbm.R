# 實作--嬰兒出生體重預測
# (C1) bwt : 出生時的體重 (ounces)
# (C2) gestation : 懷孕總日數
# (C3) parity : 胎序,0= 第一個小孩, 9=unknown
# (C4) age : 母親年齡
# (C5) height : 母親身高 (inches)
# (C6) weight : 母親在懷孕前的體重 (pounds)
# (C7) smoke : 母親是否抽煙. 0:沒有, 1:有
#讀入CSV檔：babies.csv
babyData=read.table(file.choose(),header=T,sep = ",",row.names=NULL)
#排除有遺漏值的資料列
babyData=na.exclude(babyData)
test.MAPE.ls <- data.frame(cart=numeric(),GBM=numeric())
#訓練樣本70%與測試樣本30%
n=0.3*nrow(babyData)
test.index=sample(1:nrow(babyData),n)
Train=babyData [-test.index,]
Test=babyData[test.index,]
# 使用分類回歸樹 CART建模----
library(rpart)
baby.tree=rpart(bwt~. ,data=Train) 
#MAPE
y=babyData$bwt[test.index]
y_hat=predict(baby.tree,newdata=Test, type="vector")
test.MAPE=mean(abs(y-y_hat)/y)
test.MAPE.ls[nrow(test.MAPE.ls)+1,'cart'] <- test.MAPE
cat("MAPE(test)=",test.MAPE*100,"%\n")


#使用GBM建模,解決預測問題(參數：損失函數distribution選擇 "gaussian")----
# install.packages("gbm")
library(gbm)

# set.seed(123)
bwt_GBM =gbm(bwt~.,data=Train, distribution= "gaussian",n.trees =5000,interaction.depth =4, shrinkage = 0.001, bag.fraction = 0.5)
# distribution：損失函數 ; n.trees：迭代次數 ; interaction.depth：決策樹深度
# shrinkage: learning rate避免過度訓練; bag.fraction建模一開始隨機選取訓練數據進行後續模型訓練的抽樣比率
summary(bwt_GBM) #檢視變數重要性
plot(bwt_GBM, i="gestation", ylab="f(gestation)") #繪圖檢視X變數與Y變數的關係
y_hat=predict(bwt_GBM ,newdata =Test,n.trees =5000)
test.MAPE=mean(abs(y-y_hat)/y)
test.MAPE.ls[nrow(test.MAPE.ls),'GBM'] <- test.MAPE
cat("MAPE(test)=",test.MAPE*100,"%\n")

sapply(test.MAPE.ls, mean)
