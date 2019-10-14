# RJDBC 連結MySQL
# install.packages("RJDBC")
library(RJDBC)

# 設定 jdbc連結路徑
drv <- JDBC("com.mysql.jdbc.Driver",
            "./mysql-connector-java-5.1.48.jar"
)


# 設定連線
# DB name: test123, login id: root, login password: tony, 預設port 3306
conn <- dbConnect(drv,
                  "jdbc:mysql://172.104.90.53:3306/iii",
                  "iii",
                  "iii@WSX1qaz"
                  )

# 讀取資料
dbListTables(conn) # 等同於 mysql> show tables

a<-dbGetQuery(conn,"select * from airquality ")#執行SQL語法,結果儲存於a
class(a) # a會是一個data.frame的格式

# 回寫資料
data(iris)
iris_test <- iris
names(iris_test) <- c('S_L','S_W','P_L','P_W','Species') #更改欄位名稱
dbWriteTable(conn,"iris_test",iris_test) #寫入MySql
dbListTables(conn)  # 等同於 mysql> show tables
# 刪除Table
dbRemoveTable(conn," iris_test ")
# 結束連線
dbDisconnect(conn)

# 建置臭氧濃度預測模型 ----
# 資料集介紹
?airquality

# Step 1 建立MySQL連結
library(RJDBC)
# 啟動Driver
drv <- JDBC("com.mysql.jdbc.Driver","C:\\r_work\\ mysql-connector-java-5.1.46.jar ")
# 設定連線
# DB name: iot, login id: ltu, login password: ltu@WSX1qaz, 預設port 3306
conn <- dbConnect(drv,
                    "jdbc:mysql://172.104.90.53:3306/iii",
                    "iii",
                    "iii@WSX1qaz"
)
# Step 2 整理機器學習演算法所需的資料 allitems
sensor <- dbGetQuery(conn,"select * from sensor")
airquality <- dbGetQuery(conn,"select * from airquality ")
# 安裝sqldf,利用SQL語法整理資料
# install.packages("sqldf")
library(sqldf)

# 將sensor收集的資料整理成 月, 日, 當日平均溫度, 當日平均濕度,然後透過月、日與
# airquality 作資料勾稽(Left join)
df_sensor <- sqldf("SELECT cast(substr(trim(dt),7,1) as int) month
                          ,cast(substr(trim(dt),9,2) as int) day
                          ,avg(temperature) avg_temperature
                          ,avg(humidity) avg_humidity
                    FROM sensor
                    GROUP BY cast(substr(trim(dt),7,1) as int)
                            ,cast(substr(trim(dt),9,2) as int)
                    HAVING cast(substr(trim(dt),7,1) as int) <>0
                   ")

df_allitems <- sqldf("SELECT a.*,b.avg_temperature,b.avg_humidity
                      FROM airquality a
                      LEFT JOIN df_sensor b
                      ON a.Month=b.month and a.Day = b.day
                     ")

# Step 3 建置多元回歸模型
lmTrain <- lm(formula = Ozone ~ Solar_R+Wind+avg_temperature+avg_humidity, 
                data = subset(df_allitems, complete.cases(df_allitems))) #排除null
# 模型摘要
summary(lmTrain ) 

# Step 4 預測明日臭氧濃度
New_data <- data.frame(Solar_R =200, Wind=12, avg_temperature=32.1, avg_humidity =62.7)
predicted <- predict(lmTrain , newdata = New_data)
predicted/1000 
# 結束連線
dbDisconnect(conn)

# 以客戶流失模型為例 ----
#CART
library(rpart)
library(C50)
data(churn)   
data_train = churnTrain[,-3]  # 排除 "area_code"欄位
churn.tree=rpart(churn~.,data=data_train)
y = churnTest$churn
y_hat=predict(churn.tree, newdata=churnTest, type="class")
table.test=table(y,y_hat)
#預測正確率 = 矩陣對角對角總和 / 矩陣總和
cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100,"%\n")

#Adaboost
# install.packages("fastAdaboost")
library(fastAdaboost)
churn_adaboost <- adaboost(churn~., data_train, 10) # 10表示有10個弱分類器
pred <- predict( churn_adaboost,newdata=churnTest)
cat("Correct Classification Ratio(test)=", (1- pred$error)*100,"%\n")

# 實作--嬰兒出生體重預測 ----
#讀入CSV檔：babies.csv
babyData=read.table(file.choose(),header=T,sep = ",",row.names=NULL)
#排除有遺漏值的資料列
babyData=na.exclude(babyData)
#訓練樣本70%與測試樣本30%
n=0.3*nrow(babyData)
test.index=sample(1:nrow(babyData),n)
Train=babyData [-test.index,]
Test=babyData[test.index,]
# 使用分類回歸樹 CART建模
library(rpart)
baby.tree=rpart(bwt~. ,data=Train) 
#MAPE
y=babyData$bwt[test.index]
y_hat=predict(baby.tree,newdata=Test, type="vector")
test.MAPE=mean(abs(y-y_hat)/y)
cat("MAPE(test)=",test.MAPE*100,"%\n")

#使用GBM建模,解決預測問題(參數：損失函數distribution選擇 "gaussian")
# install.packages("gbm")
library(gbm)

set.seed(123)
bwt_GBM =gbm(bwt~.,data=Train, distribution= "gaussian",n.trees =5000,interaction.depth =4, shrinkage = 0.001, bag.fraction = 0.5)
# distribution：損失函數 ; n.trees：迭代次數 ; interaction.depth：決策樹深度(經驗尚不大於6)(<6)
# shrinkage: learning rate避免過度訓練; bag.fraction建模一開始隨機選取訓練數據進行後續模型訓練的抽樣比率
par(mfrow=c(1,2))
summary(bwt_GBM) #檢視變數重要性
plot(bwt_GBM, i="gestation") #繪圖檢視X變數與Y變數的關係
y_hat=predict(bwt_GBM ,newdata =Test,n.trees =5000)
test.MAPE=mean(abs(y-y_hat)/y)
cat("MAPE(test)=",test.MAPE*100,"%\n")

# 實作--電信業客戶流失模型 ----
#載入C50 churn資料集
data(churn)   #載入C50 churn資料集
#CART
library(rpart)
library(C50)
library(caret)
data(churn)   
data_train = churnTrain[,-3]  # 排除 "area_code"欄位
churn.tree=rpart(churn~.,data=data_train)
y = churnTest$churn
y_hat=predict(churn.tree, newdata=churnTest, type="class")
table.test=table(y,y_hat)
#預測正確率 = 矩陣對角對角總和 / 矩陣總和
cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100, "%\n")
#Adaboost
# install.packages("fastAdaboost")
library(fastAdaboost)
churn_adaboost <- adaboost(churn~., data_train, 10)
pred <- predict( churn_adaboost,newdata=churnTest)
cat("Correct Classification Ratio(test)=", (1- pred$error)*100,"%\n")

#使用GBM建模,解決分類問題(參數：損失函數distribution選擇"bernoulli")
set.seed(123)
data_train$churn = ifelse(data_train$churn=='yes',1,0) #GBM的Y變數僅識別 0與1
churn_GBM =gbm(churn~.,
               data=data_train , 
               distribution= "bernoulli", #損失函數
               n.trees =10000, #迭代次數
               interaction.depth =4, #決策樹深度
               shrinkage = 0.01, # learning rate避免過度訓練
               bag.fraction = 0.5, #隨機選取訓練數據進行後續模型訓練的抽樣比率
               cv.folds=5 # 交叉驗證組數
)# GBM作者建議shrinkage參數設在0.01 ~ 0.001之間
# n.trees參數設在3000-10000之間
# 用交叉驗證確定最佳迭代次數
best.iter <- gbm.perf(churn_GBM,method='cv')
#利用最佳迭代次數再次建模
churn_GBM =gbm(churn~.,
               data=data_train , 
               distribution= "bernoulli", #損失函數
               n.trees = best.iter, #迭代次數
               interaction.depth =4, #決策樹深度
               shrinkage = 0.01, # learning rate避免過度訓練
               bag.fraction = 0.5, #隨機選取訓練數據進行後續模型訓練的抽樣比率
               cv.folds=5 # 交叉驗證組數
              )
summary(churn_GBM ) #檢視變數重要性

#評分
data_test <- churnTest
data_test$churn = ifelse(data_test$churn=='yes',1,0)  #將yes/no轉為 1/0
pred=predict(churn_GBM ,newdata = data_test,n.trees = best.iter)
#繪製 ROC圖
install.packages("stats")
install.packages("pROC")
library(stats)
library(pROC)
churn.roc = roc(data_test$churn,pred)
plot(churn.roc)

#利用coords函數找出 切割1/0的最佳臨界值 threshold
coords(churn.roc,"best")

churn.predict.class = ifelse(pred > coords(churn.roc,"best")["threshold"],"yes","no")
table(data_test$churn,churn.predict.class)
table.test <- table(data_test$churn,churn.predict.class)
#預測正確率 = 矩陣對角對角總和 / 矩陣總和
cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100,"%\n")

# 課堂練習：以客戶流失模型為例(二元分類) ----
# install.packages("xgboost")
library(xgboost)
library(C50)
library(dplyr)
#以客戶流失資料集為例
rm(list = ls())
data(churn)   
data_train = churnTrain[,-3] #data.frame:   3333 obs. of  19 variables
data_test = churnTest[,-3] #data.frame:   1667 obs. of  19 variables
#將訓練與測試資料集轉乘數值型的Matrix格式
dataTrain_matrix <- Matrix::sparse.model.matrix(churn ~ .-1, data = data_train) #-1 是去掉流水號
output_vector_train = churnTrain[,'churn'] == "yes"
train_matrix <- xgb.DMatrix(data = as.matrix(dataTrain_matrix),label=output_vector_train)
dataTest_matrix <- Matrix::sparse.model.matrix(churn ~ .-1, data = data_test)
output_vector_test = churnTest[,'churn'] == "yes"
test_matrix <- xgb.DMatrix(data = as.matrix(dataTest_matrix),label=output_vector_test)

# 模型超參數設定
nc = length(unique(output_vector_train)) #預測變數Y有幾類
params = list( "objective" = "multi:softprob", #結果包含預測機率與預測類別
               "eval_metric" = "mlogloss", #損失函數
               "num_class" = nc # 設定Y的類別
)
watchlist <- list(train=train_matrix , test=test_matrix) #設定建模時需監控的樣本清單
# xgboost模型建置
bst_model <- xgb.train(params = params,
                       data = train_matrix,
                       nrounds = 100,
                       watchlist = watchlist,
                       eta = 0.3,  # Learning Rate, low -> more robust to overfitting
                       max.depth = 5, #預設值:6，每顆樹的最大深度，樹高越深，越容易overfitting
                       seed =123
                      )
# Overfitting檢視
evalue_log <- bst_model$evaluation_log
plot(evalue_log$iter, evalue_log$train_mlogloss, col='blue')
lines(evalue_log$iter, evalue_log$test_mlogloss, col='red')

# 依照最佳迭代次數再次建模
bst_model <- xgb.train(params = params,
                       data = train_matrix,
                       nrounds = 17,
                       watchlist = watchlist,
                       eta = 0.3, # Learning Rate, low - more robust to overfitting
                       max.depth = 5, #預設值:6，每顆樹的最大深度，樹高越深，越容易overfitting
                       seed =123
                      )
#檢視重要變數
var_feature <- xgb.importance(colnames(train_matrix), model = bst_model)
print(var_feature)
xgb.plot.importance(var_feature)

#預測新資料
p <- predict(bst_model, newdata = test_matrix) #模型評分，1667*2筆(因為每人有流失與未流失的機率)
pred <- matrix(p, nrow=nc, ncol=length(p)/nc ) %>%#轉成 2*1667 matrix格式
        t() %>%   #再轉成 1667*2 matrix格式
        data.frame() %>%   #轉成data.frame格式
        mutate(label = output_vector_test, max_prob = max.col(., "last")-1 )
#取得最大機率值的欄位數，然後將欄位編號減1
head(pred, 10)

#預測正確率 = 矩陣對角對角總和 / 矩陣總和
table.test = table(output_vector_test,pred$max_prob)
cat("Correct Classification Ratio(test)=", 
    sum(diag(table.test))/sum(table.test)*100,"%\n")

