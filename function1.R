library(plyr)  # join
library(reshape)  # cast
library(forecast)  # stlf
library(lubridate)

prediction = function(train,test){
train$Date = as.Date(train$Date, '%Y-%m-%d')
test$Date = as.Date(test$Date, '%Y-%m-%d')
traindata_part = train
testdata_part = test

# model
all.stores = unique(testdata_part$Store)
all.dept = unique(testdata_part$Dept)
num.stores = length(all.stores)
traindata_part.dates = unique(traindata_part$Date)
num.traindata_part.dates = length(traindata_part.dates)
traindata_part.frame = data.frame(Date=rep(traindata_part.dates, num.stores),
                                  Store=rep(all.stores, each=num.traindata_part.dates))


preprocess.svd = function(train, n.comp){
  train.naindex = is.na(train)
  #give the na the average value nearby
  #train[train.naindex] = vapply(which(train.naindex), function(x) mean(c(train[x - 1], l[x + 1])), FUN.VALUE = double(1))
  train[train.naindex] = 0
  z = svd(train[, 2:ncol(train)], nu=n.comp, nv=n.comp)
  s = diag(z$d[1:n.comp])
  train[, 2:ncol(train)] = z$u %*% s %*% t(z$v)
}

n.comp = 12 # keep first 12 components

for(d in all.dept) {
  tr.d = traindata_part.frame
  tr.d = join(tr.d, traindata_part[traindata_part$Dept==d, c('Store','Date','Weekly_Sales')])  # perform a left join.
  tr.d = cast(tr.d, Date ~ Store)  # row is Date, col is Store, entries are the sales
  # apply SVD for tr.d
  tr.d = preprocess.svd(tr.d, n.comp)  
  
  
  testdata_part.dates = unique(testdata_part$Date)
  num.testdata_part.dates = length(testdata_part.dates)
  forecast.frame = data.frame(Date=rep(testdata_part.dates, num.stores),
                              Store=rep(all.stores, each=num.testdata_part.dates))
  fc.d = forecast.frame
  fc.d$Weekly_Sales = 0
  fc.d = cast(fc.d, Date ~ Store)  # similar as tr.d
  fc2.d=fc.d
  
  horizon = nrow(fc.d)  # number of steps ahead to forecast
  
  for(j in 2:(ncol(tr.d) + 1)){ # loop over stores
    s = ts(tr.d[, j - 1], frequency = 13)  # convert sales to time series. 
    fc = stlf(s, h=horizon, s.window=3, method='arima', ic='bic')
    fc2 = stlf(s, h=horizon, s.window=3, method='ets', ic='bic', opt.crit='mae')
    pred = as.numeric(fc$mean)
    pred2 = as.numeric(fc2$mean)
    fc.d[,j] = pred
    fc2.d[,j] = pred2
  }  
  
  fc.d = melt(fc.d)
  fc2.d = melt(fc2.d)
  testdata_part.d.idx = testdata_part$Dept == d
  testdata_part.d = testdata_part[testdata_part.d.idx,c('Store','Date')]
  
  
  
  
  testdata_part1.d = join(testdata_part.d, fc.d)
  testdata_part2.d = join(testdata_part.d, fc2.d)
  testdata_part$pred_sale[testdata_part.d.idx] = testdata_part1.d$value
  testdata_part$pred_sale2[testdata_part.d.idx] = testdata_part2.d$value
  testdata_part$pred_sale3 = testdata_part$pred_sale
  testdata_part$pred_sale4 = testdata_part$pred_sale2
  testdata_part.h.idx = testdata_part$IsHoliday == TRUE
  testdata_part.h.c = month(testdata_part$Date)[testdata_part.h.idx] == 12
  if(dim(testdata_part[testdata_part.h.idx,][testdata_part.h.c,])[1] != 0){
    temp1 = testdata_part[which(month(testdata_part$Date) == 12),][which(testdata_part[which(month(testdata_part$Date) == 12),]$IsHoliday) - 2,c('pred_sale4')]
    temp2 = testdata_part[which(month(testdata_part$Date) == 12),][which(testdata_part[which(month(testdata_part$Date) == 12),]$IsHoliday) - 1,c('pred_sale4')]
    temp3 = testdata_part[which(month(testdata_part$Date) == 12),][which(testdata_part[which(month(testdata_part$Date) == 12),]$IsHoliday),c('pred_sale4')]
    testdata_part[which(month(testdata_part$Date) == 12),][which(testdata_part[which(month(testdata_part$Date) == 12),]$IsHoliday) - 1,c('pred_sale4')] = temp2*(6/7) + temp1*(1/7) + temp3*(1/7)
    testdata_part[which(month(testdata_part$Date) == 12),][which(testdata_part[which(month(testdata_part$Date) == 12),]$IsHoliday) - 1,c('pred_sale4')] = temp2*(6/7) + temp1*(1/7) + temp3*(1/7)
  }
}


traindata_part$Yr = year(traindata_part$Date)
testdata_part$Yr = year(testdata_part$Date)
traindata_part$Mon = month(traindata_part$Date)
testdata_part$Mon = month(testdata_part$Date)

train.wk = traindata_part$Date
train.wk = train.wk - train.wk[1]  # date is now 0, 7, 14, ...
train.wk = train.wk/7 + 5  # make 2010-2-5 as '5', and date becomes continuous integers, i.e., 5, 6, 7, ...
train.wk = as.numeric(train.wk) %% 52  ## 52 weeks in a year
traindata_part$Wk = train.wk

test.wk = testdata_part$Date
test.wk = test.wk - test.wk[1]
test.wk = test.wk/7 + 44 # make 2012-11-02 as '44'.
test.wk = as.numeric(test.wk) %% 52
testdata_part$Wk = test.wk

store = sort(unique(testdata_part$Store))
n.store = length(store)
dept = sort(unique(testdata_part$Dept))
n.dept = length(dept)

for (s in 1:n.store){
  for (d in 1:n.dept){
    
    test.id = which(testdata_part$Store == store[s] & testdata_part$Dept == dept[d])
    test.temp = testdata_part[test.id, ]
    train.id = which(traindata_part$Store == store[s] & traindata_part$Dept == dept[d])
    train.temp = traindata_part[train.id, ]
    
    for (i in 1:length(test.id)){
      id = which(train.temp$Wk == test.temp[i,]$Wk & train.temp$Yr == test.temp[i,]$Yr - 1)
      threeWeeksId = c(id - 1, id, id + 1)  
      
      tempSales = train.temp[threeWeeksId, 'Weekly_Sales']
      if (length(tempSales) == 0){
        testdata_part$pred_sale5[test.id[i]] = 0
      }else{
        testdata_part$pred_sale5[test.id[i]] = median(tempSales)
      }
    }
  }
}

testdata_part$pred_sale5[which(is.na(testdata_part$pred_sale5))] = 0

testdata_part
}

