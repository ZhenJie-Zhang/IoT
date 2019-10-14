# 回家功課：以鳶尾花資料集為例----
rm(list = ls())
library(xgboost)
library(C50)
library(dplyr)
library(caret)

# Train and Test
sample_Index <- createDataPartition(y=iris$Species,p=0.7,list=FALSE)
data_train=iris[sample_Index,]
data_test=iris[-sample_Index,]
#將訓練與測試資料集轉乘數值型的Matrix格式
dataTrain_matrix <- as.matrix(select(data_train,-Species))
output_vector_train = as.numeric(data_train$Species)-1
train_matrix <- xgb.DMatrix(data = dataTrain_matrix, label=output_vector_train)
dataTest_matrix <- as.matrix(select(data_test,-Species))
output_vector_test = as.numeric(data_test$Species)-1
test_matrix <- xgb.DMatrix(data = dataTest_matrix,label=output_vector_test)

# 模型參數設定
nc = length(unique(output_vector_train))                  #預測變數Y有幾類
params = list( "objective" = "multi:softprob",             #結果包含預測機率與預測類別
               "eval_metric" = "mlogloss",         #損失函數
               "num_class" = nc            # 設定Y的類別
)
watchlist <- list(train=train_matrix , test=test_matrix) #設定建模時需監控的樣本清單
# xgboost模型建置
bst_model <- xgb.train(params = params,
                       data = train_matrix,
                       nrounds = 100,
                       watchlist = watchlist,
                       eta = 0.3,     # Learning Rate, low -> more robust to overfitting
                       max.depth = 5, #預設值:6，每顆樹的最大深度，樹高越深，越容易overfitting
                       seed =123
)
# Overfitting檢視
evalue_log <- bst_model$evaluation_log
plot(evalue_log$iter, evalue_log$train_mlogloss, col='blue')
lines(evalue_log$iter, evalue_log$test_mlogloss, col='red')
best_nround <- which.min(evalue_log$test_mlogloss)
# 依照最佳迭代次數再次建模
bst_model <- xgb.train(params = params,
                       data = train_matrix,
                       nrounds = best_nround,
                       watchlist = watchlist,
                       eta = 0.3,     # Learning Rate, low -> more robust to overfitting
                       max.depth = 5, #預設值:6，每顆樹的最大深度，樹高越深，越容易overfitting
                       seed =123
)
#檢視重要變數
var_feature <- xgb.importance(colnames(train_matrix), model = bst_model)
print(var_feature)
xgb.plot.importance(var_feature)
#預測新資料
p <- predict(bst_model, newdata = test_matrix)            #模型評分，2筆(因為每人有流失與未流失的機率)
pred <- matrix(p, nrow=nc,  ncol=length(p)/nc ) %>%     #轉成 2*n matrix格式
  t() %>%                                                         #轉成 n*2 matrix格式
  data.frame() %>%                                          #轉成data.frame格式
  mutate(label = output_vector_test, max_prob = max.col(., "last")-1 ) #取得最大機率值的欄位數，然後減1
#預測正確率 = 矩陣對角對角總和 / 矩陣總和
table.test = table(output_vector_test,pred$max_prob)
cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100,"%\n")

