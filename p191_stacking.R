# 波士頓房價資料集
# 準備資料----
#從網站讀取數據
bostondata = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data",header = FALSE) 
#對屬性重命名
names(bostondata) = c("CRIM","ZN","INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "PRICE")
#產生訓練樣本(80%)、測試樣本(20%)
library(caret)
test.index <- createDataPartition(y=bostondata$PRICE,p=0.2,list=FALSE)
# n=0.2*nrow(bostondata)
# test.index=sample(1:nrow(bostondata),n)
Train=bostondata[-test.index,] 
Test=bostondata[test.index,]
par(mfrow=c(1,2))
hist(Train$PRICE, main='Train')
hist(Test$PRICE, main='Train')
#將訓練資料及分成3份
# 3-folds
n = 3
n.folds = rep(1:n, each=nrow(Train)/n) #分3組
train.folds = split(Train, n.folds) #3份資料放到清單

# 第一階段(Stacking)----
# 第一個演算法使用多元回歸 ----
# 1st fold for validation
meta.Train_x = vector()
meta.Test_x = list()
stacking.train = rbind(train.folds[[2]], train.folds[[3]])
stacking.valid = train.folds[[1]]
stacking.test = Test
model_1 = lm(PRICE~., stacking.train) #Y1_hat
tmp.meta.Train_x = predict(model_1, stacking.valid) #預測 validation
tmp.meta.Test_x = predict(model_1, stacking.test) #預測 Test
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) #一開始meta.x是空的
meta.Test_x[[1]] = tmp.meta.Test_x

# 2st fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[3]])
stacking.valid = train.folds[[2]]
stacking.test = Test
model_1 = lm(PRICE~., stacking.train) #Y2_hat
tmp.meta.Train_x = predict(model_1, stacking.valid) 
tmp.meta.Test_x = predict(model_1, stacking.test)
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) 
meta.Test_x[[2]] = tmp.meta.Test_x

# 3st fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[2]])
stacking.valid = train.folds[[3]]
stacking.test = Test
model_1 = lm(PRICE~., stacking.train) #Y3_hat
tmp.meta.Train_x = predict(model_1, stacking.valid) 
tmp.meta.Test_x = predict(model_1, stacking.test)
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x)
meta.Test_x[[3]] = tmp.meta.Test_x

# Average Meta.X of Test
mean.meta.Test_x = (meta.Test_x[[1]] + meta.Test_x[[2]] + meta.Test_x[[3]]) / 3
# Meta-Train DataSet & Meta-Test DataSet
meta.train.1 = data.frame(meta_x = meta.Train_x, y=Train$PRICE)
meta.test.1 = data.frame(meta_x = mean.meta.Test_x, y= Test$PRICE)


#第二模型用SVR, support vector regression ----
#install.packages("e1071")
library(e1071)
meta.Train_x = vector()
meta.Test_x = list()
# 1st fold for validation
stacking.train = rbind(train.folds[[2]], train.folds[[3]])
stacking.valid = train.folds[[1]]
stacking.test = Test
model_2 = svm(PRICE~., stacking.train)
tmp.meta.Train_x = predict(model_2, stacking.valid) 
tmp.meta.Test_x = predict(model_2, stacking.test)
#一開始meta.x是空的
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) 
meta.Test_x[[1]] = tmp.meta.Test_x

# 2st fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[3]])
stacking.valid = train.folds[[2]]
stacking.test = Test
model_2 = svm(PRICE~., stacking.train)
tmp.meta.Train_x = predict(model_2, stacking.valid) 
tmp.meta.Test_x = predict(model_2, stacking.test)
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x)
meta.Test_x[[2]] = tmp.meta.Test_x

# 3st fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[2]])
stacking.valid = train.folds[[3]]
stacking.test = Test
model_2 = svm(PRICE~., stacking.train)
tmp.meta.Train_x = predict(model_2, stacking.valid) 
tmp.meta.Test_x = predict(model_2, stacking.test)
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) #一開始meta.x是空的
meta.Test_x[[3]] = tmp.meta.Test_x
# Average Meta.X of Test
mean.meta.Test_x = (meta.Test_x[[1]] + meta.Test_x[[2]] + meta.Test_x[[3]]) / 3
# Mata-Train DataSet
meta.train.2 = data.frame(meta_x = meta.Train_x, y=Train$PRICE)
meta.test.2 = data.frame(meta_x = mean.meta.Test_x, y= Test$PRICE)

#第三模型用CART----
library(rpart)
meta.Train_x = vector()
meta.Test_x = list()
# 1st fold for validation
stacking.train = rbind(train.folds[[2]], train.folds[[3]])
stacking.valid = train.folds[[1]]
stacking.test = Test
model_3 = rpart(PRICE~., stacking.train)
tmp.meta.Train_x = predict(model_3, stacking.valid) 
tmp.meta.Test_x = predict(model_3, stacking.test)
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) #一開始meta.x是空的
meta.Test_x[[1]] = tmp.meta.Test_x

# 2st fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[3]])
stacking.valid = train.folds[[2]]
stacking.test = Test
model_3 = rpart(PRICE~., stacking.train)
tmp.meta.Train_x = predict(model_3, stacking.valid) 
tmp.meta.Test_x = predict(model_3, stacking.test)
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) 
meta.Test_x[[2]] = tmp.meta.Test_x

# 3st fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[2]])
stacking.valid = train.folds[[3]]
stacking.test = Test
model_3 = rpart(PRICE~., stacking.train)
tmp.meta.Train_x = predict(model_3, stacking.valid) 
tmp.meta.Test_x = predict(model_3, stacking.test)
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) 
meta.Test_x[[3]] = tmp.meta.Test_x
# Average Meta.X of Test
mean.meta.Test_x = (meta.Test_x[[1]] + meta.Test_x[[2]] + meta.Test_x[[3]]) / 3
# Mata-Train DataSet
meta.train.3 = data.frame(meta_x = meta.Train_x, y=Train$PRICE)
meta.test.3 = data.frame(meta_x = mean.meta.Test_x, y= Test$PRICE)

# 第二階段(Blending)----
#建構第二階段的 Meta-Model，使用xgboost演算法建模
#install.packages("xgboost")
library(xgboost)
# 先把三個 Meta-Train合併一起
allitems.meta.train = rbind(meta.train.1, meta.train.2, meta.train.3)
allitems.meta.test = rbind(meta.test.1, meta.test.2, meta.test.3)
# 轉換成 xgboost 的格式
train_matrix = xgb.DMatrix(data = as.matrix(allitems.meta.train[,1]), label = allitems.meta.train[, 2])
test_matrix = xgb.DMatrix(data = as.matrix(allitems.meta.test[,1]) , label = allitems.meta.test[, 2])

# 訓練 XGboost 模型----
# step1: 參數設定----
xgb.params = list(
  #col的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
  colsample_bytree = 0.7,   
  # row的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
  subsample = 0.5,                      
  booster = "gbtree",  
  # 樹的最大深度，越高表示模型可以長得越深，模型複雜度越高
  max_depth = 6,           
  # boosting會增加被分錯的資料權重，而此參數是讓權重不會增加的那麼快，因此越大會讓模型愈保守
  eta = 0.03,  
  # 預測問題，用'mae'也可以
  eval_metric = "rmse",                      
  objective = "reg:linear", #預測問題，所以使用regression  
  # 越大，模型會越保守，相對的模型複雜度比較低
  gamma = 0
) 

# step2: 使用xgb.cv()，優化出最佳的決策樹數量
cv.model = xgb.cv(
  params = xgb.params, 
  data = train_matrix,
  nfold = 5,     # 5-fold cv
  nrounds=500,   # 測試1-500，各個樹總數下的模型
  # 如果當nrounds < 50 時，就已經有overfitting情況發生，那表示不用繼續tune下去了，可以提早停止
  early_stopping_rounds = 50, 
  print_every_n = 20 # 每20個單位才顯示一次結果
) 
#Step3. 畫圖檢視Overfitting與取得最適迭代次數
log = cv.model$evaluation_log
plot(x=1:nrow(log), y= log$train_rmse_mean, col='red', xlab="nround", ylab="rmse", main="Check Overfitting") 
points(x=1:nrow(log), y= log$test_rmse_mean, col='blue') 
legend("topright", pch=1, col = c("red", "blue"), 
       legend = c("Train", "Validation") )  
# 獲得 best nround
best.nrounds = cv.model$best_iteration

# 建立模型
xgb.model = xgb.train(paras = xgb.params, 
                      data = train_matrix,
                      nrounds = best.nrounds) 
# 對三組 Meta-Test進行預測：
dtest.1 = xgb.DMatrix(data = as.matrix(meta.test.1[,1]), label = meta.test.1[, 2])
final_1 = predict(xgb.model, dtest.1)
dtest.2 = xgb.DMatrix(data = as.matrix(meta.test.2[,1]), label = meta.test.2[, 2])
final_2 = predict(xgb.model, dtest.2)
dtest.3 = xgb.DMatrix(data = as.matrix(meta.test.3[,1]), label = meta.test.3[, 2])
final_3 = predict(xgb.model, dtest.3)
# 把三組結果平均起來，然後算 MAPE
final_y = (final_1 + final_2 + final_3)/3
test.MAPE=mean(abs(Test$PRICE-final_y)/Test$PRICE)
cat("MAPE(test)=",test.MAPE*100,"%\n") 
