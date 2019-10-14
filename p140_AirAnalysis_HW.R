# 請使用 df_allitems 資料集實作
# XGBoost 模型來預測空氣汙染
library(ggplot2)
library(ggthemes)

# Step 1 建立MySQL連結
library(RJDBC)
# 啟動Driver
# drv <- JDBC("com.mysql.jdbc.Driver","C:\\r_work\\ mysql-connector-java-5.1.46.jar ")
# 設定連線
# DB name: iot, login id: ltu, login password: ltu@WSX1qaz, 預設port 3306
# conn <- dbConnect(drv,
#                   "jdbc:mysql://172.104.90.53:3306/iii",
#                   "iii",
#                   "iii@WSX1qaz"
# )

library(sqldf)

# 將sensor收集的資料整理成 月, 日, 當日平均溫度, 當日平均濕度,然後透過月、日與
# airquality 作資料勾稽(Left join)
# df_allitems <- sqldf("SELECT a.*,b.avg_temperature,b.avg_humidity
                     #  FROM airquality a
                     #  LEFT JOIN df_sensor b
                     #  ON a.Month=b.month and a.Day = b.day
                     # ")
library(VIM)
load('E:/IoT/df_allitems.RData')
aggr_plot <- aggr(df_allitems, col = c('navyblue', 'red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(df_allitems), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data", "Pattern"))
library(mice)
# 檢視遺漏值
md.pattern(df_allitems)
df_allitems[is.na(df_allitems$Ozone),]
df_allitems[is.na(df_allitems$Solar_R),]

# 使用隨機森林，進行遺漏值預測
mice.data <- mice(df_allitems, method = "rf")

# 保存完整輸出
mice_output <-  complete(mice.data)

# 檢視填補後狀況
library(gridExtra)
grid.arrange(layout_matrix=rbind(c(1,3),c(2,4)),
ggplot(df_allitems[!is.na(df_allitems$Solar_R),], aes(x=Solar_R))+
  geom_density(fill = '#99d6ff', alpha=0.4) +
  geom_vline(aes(xintercept=median(Solar_R, na.rm=T)), 
             colour='red', linetype='dashed', lwd=1) +
  ggtitle('df_allitems')+
  theme_few(),
ggplot(mice_output, aes(x=Solar_R))+
  geom_density() +
  geom_density(fill = '#99d6ff', alpha=0.4) +
  geom_vline(aes(xintercept=median(Solar_R, na.rm=T)), 
             colour='red', linetype='dashed', lwd=1) +
  ggtitle('mice_output')+
  theme_few(),
ggplot(df_allitems[!is.na(df_allitems$Ozone),], aes(x=Ozone))+
  geom_density(fill = '#99d6ff', alpha=0.4) +
  geom_vline(aes(xintercept=median(Ozone, na.rm=T)), 
             colour='red', linetype='dashed', lwd=1) +
  ggtitle('df_allitems')+
  theme_few(),
ggplot(mice_output, aes(x=Ozone))+
  geom_density() +
  geom_density(fill = '#99d6ff', alpha=0.4) +
  geom_vline(aes(xintercept=median(Ozone, na.rm=T)), 
             colour='red', linetype='dashed', lwd=1) +
  ggtitle('mice_output')+
  theme_few()
)

library(caret)
sample_Index <- createDataPartition(y=mice_output$Ozone ,p=0.7,list=FALSE)
data_train=iris[sample_Index,]
data_test=iris[-sample_Index,]

train_matrix <- xgb.DMatrix(data = as.matrix(select(data_train,-Species)), 
                            label= as.numeric(data_train$Species)-1)
test_matrix <- xgb.DMatrix(data = as.matrix(select(data_test,-Species)),
                           label= as.numeric(data_test$Species)-1)


library(xgboost)
library(dplyr)

