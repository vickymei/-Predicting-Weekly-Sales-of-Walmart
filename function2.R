setwd("~/Desktop/stat542/project2")
train <- read.table("train.csv")
test <- read.table("test.csv")
test$Date = as.Date(test$Date, '%Y-%m-%d')
start=proc.time
source('function1.R')
traindata_part = train
WMAE1 = 0
WMAE2 = 0
WMAE3 = 0
WMAE4 = 0
WMAE5 = 0
newtest = data.frame()
for(t in 1:20){
  tmp.filename = paste('xxx', t, '.csv', sep='')
  newtest = read.table(file = tmp.filename)
  predict_data = prediction(traindata_part, newtest)
  
  id.x = test$Date == predict_data$Date
  test$Weekly_Pred1[id.x]= predict_data$pred_sale3
  test$Weekly_Pred2[id.x]= predict_data$pred_sale4
  test$Weekly_Pred3[id.x]= predict_data$pred_sale5
  
  w=4*newtest$IsHoliday + 1
  WMAE1[t]=sum(w*abs(predict_data$pred_sale-newtest$Weekly_Sales))/sum(w)
  WMAE2[t]=sum(w*abs(predict_data$pred_sale2-newtest$Weekly_Sales))/sum(w)
  WMAE3[t]=sum(w*abs(predict_data$pred_sale3-newtest$Weekly_Sales))/sum(w)
  WMAE4[t]=sum(w*abs(predict_data$pred_sale4-newtest$Weekly_Sales))/sum(w)
  WMAE5[t]=sum(w*abs(predict_data$pred_sale5-newtest$Weekly_Sales))/sum(w)
  
  traindata_part = rbind(traindata_part, newtest)
}
run_time=proc.time-start
write.table(test,file="test.csv")


