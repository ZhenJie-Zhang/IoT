# ��@--����X���魫�w��
# (C1) bwt : �X�ͮɪ��魫 (ounces)
# (C2) gestation : �h���`���
# (C3) parity : �L��,0= �Ĥ@�Ӥp��, 9=unknown
# (C4) age : ���˦~��
# (C5) height : ���˨��� (inches)
# (C6) weight : ���˦b�h���e���魫 (pounds)
# (C7) smoke : ���ˬO�_���. 0:�S��, 1:��
#Ū�JCSV�ɡGbabies.csv
babyData=read.table(file.choose(),header=T,sep = ",",row.names=NULL)
#�ư�����|�Ȫ���ƦC
babyData=na.exclude(babyData)
test.MAPE.ls <- data.frame(cart=numeric(),GBM=numeric())
#�V�m�˥�70%�P���ռ˥�30%
n=0.3*nrow(babyData)
test.index=sample(1:nrow(babyData),n)
Train=babyData [-test.index,]
Test=babyData[test.index,]
# �ϥΤ����^�k�� CART�ؼ�----
library(rpart)
baby.tree=rpart(bwt~. ,data=Train) 
#MAPE
y=babyData$bwt[test.index]
y_hat=predict(baby.tree,newdata=Test, type="vector")
test.MAPE=mean(abs(y-y_hat)/y)
test.MAPE.ls[nrow(test.MAPE.ls)+1,'cart'] <- test.MAPE
cat("MAPE(test)=",test.MAPE*100,"%\n")


#�ϥ�GBM�ؼ�,�ѨM�w�����D(�ѼơG�l�����distribution��� "gaussian")----
# install.packages("gbm")
library(gbm)

# set.seed(123)
bwt_GBM =gbm(bwt~.,data=Train, distribution= "gaussian",n.trees =5000,interaction.depth =4, shrinkage = 0.001, bag.fraction = 0.5)
# distribution�G�l����� ; n.trees�G���N���� ; interaction.depth�G�M����`��
# shrinkage: learning rate�קK�L�װV�m; bag.fraction�ؼҤ@�}�l�H������V�m�ƾڶi�����ҫ��V�m����ˤ�v
summary(bwt_GBM) #�˵��ܼƭ��n��
plot(bwt_GBM, i="gestation", ylab="f(gestation)") #ø���˵�X�ܼƻPY�ܼƪ����Y
y_hat=predict(bwt_GBM ,newdata =Test,n.trees =5000)
test.MAPE=mean(abs(y-y_hat)/y)
test.MAPE.ls[nrow(test.MAPE.ls),'GBM'] <- test.MAPE
cat("MAPE(test)=",test.MAPE*100,"%\n")

sapply(test.MAPE.ls, mean)