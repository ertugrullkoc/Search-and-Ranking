#Xgboost pairwise
library(xgboost)
library(mltools)
library(caret)

products_search = merge(term_by_content,products,by="contentId",all.x=TRUE)
products_search[,':='(AgeGroupRatio=sapply(.SD$AGE_GROUP_ID,function(x) sum(x==AGE_GROUP_ID)/.N),
                      GenderRatio=sapply(.SD$GENDER_ID,function(x) sum(x==GENDER_ID)/.N),
                      BrandRatio =sapply(.SD$BRAND_ID,function(x) sum(x==BRAND_ID)/.N)),by=.(Query)]
products_search[PRICE==0]
cross_data = merge(term_by_content,products,by="contentId")
min = as.Date(min(click$ts))
click_buy[, day_order := as.numeric((as.Date(ts)-min+1))]
products[PRICE==0]
trainingday=6 # a is number of day that will be included in traning data
testday=1 # b is number of day that will be used in test data
training_data_click = click_buy[day_order<=trainingday]
test_data_click = click_buy[day_order>trainingday & day_order<= trainingday+testday]
training.rank.data= function (x) {
  x= ClosestMatch(substitute(x))
  print(x)
  train1=training_data_click[!SearchTerm2 == toString(x),]
  train2 = training_data_click[SearchTerm2 ==toString(x),merge(.SD,cross_data[Query %in% names(which.max(table(SearchTerm2)))],by="contentId",all=TRUE),by=.(sid)]
  list = colnames(train1)
  train2 = train2[,list,with=FALSE]
  train2[,SearchTerm2:= ifelse(is.na(SearchTerm2),toString(x),SearchTerm2)]
  train = rbind(train1,train2)
  train[,userid:=ifelse(is.na(userid),head(na.omit(.SD$userid),1),userid),by=.(sid)]
  train[,BUY:=ifelse(is.na(ts),0,BUY)]
  train[,time_spent:=ifelse(is.na(ts),0,time_spent)]
  train[,Search:=ifelse(is.na(ts),0,1)]
  train[,ts:=ifelse(is.na(ts),min(.SD$ts,na.rm = TRUE),ts),by=.(sid)]
  train$ts = ymd_hms(train$ts)
  train = train[,setorder(.SD,ts),by=.(contentId)]
  train[,':='(CumClick = cumsum(Search),CumBuy = cumsum(BUY),CumTimeSpent = cumsum(time_spent)), by=.(contentId)]
  train[,CumTimeSpent:=CumTimeSpent- time_spent,by=.(contentId)]
  train[,CumBuy:=CumBuy- BUY,by=.(contentId)]
  train[,CumClick:=CumClick- Search,by=.(contentId)]
  train[,Search:=NULL]
  output = merge(train[SearchTerm2==toString(x)],products_search[Query==toString(x)],by="contentId",all.x=TRUE)
  output = merge(output,profile,by="userid",all.x=TRUE)
  return(output)
}

test.rank.data = function(x)
{
  x= ClosestMatch(substitute(x))
  test1=test_data_click[!SearchTerm2 == toString(x),]
  test2 = test_data_click[SearchTerm2 ==toString(x),merge(.SD,cross_data[Query %in% names(which.max(table(SearchTerm2)))],by="contentId",all=TRUE),by=.(sid)]
  list = colnames(test1)
  test2 = test2[,list,with=FALSE]
  test2[,SearchTerm2:= ifelse(is.na(SearchTerm2),toString(x),SearchTerm2)]
  test = rbind(test1,test2)
  test[,userid:=ifelse(is.na(userid),head(na.omit(.SD$userid),1),userid),by=.(sid)]
  test[,BUY:=ifelse(is.na(ts),0,BUY)]
  test[,time_spent:=ifelse(is.na(ts),0,time_spent)]
  test[,ts:=ifelse(is.na(ts),min(.SD$ts,na.rm = TRUE),ts),by=.(sid)]
  output = merge(test[SearchTerm2==toString(x)],products_search[Query==toString(x)],by="contentId",all.x=TRUE)
  output = merge(output,profile,by="userid",all.x=TRUE)
  return(output)
}


XgBoostModel = function(x)
{
x = substitute(x)
list = c("PRICE","AgeGroupRatio","GenderRatio","BrandRatio","meanpurchase","totalclick")
train = eval(substitute(training.rank.data(x)))
train = train[,tail(.SD,1), by=.(sid,contentId)]
train = setorder(train,"sid","ts")
groups = train[,.N,by=.(sid)][,N]
train1 = train[,list,with=FALSE]
params <- list(booster = 'gbtree',
               objective = 'rank:pairwise')
dtrain <- xgb.DMatrix(data = as.matrix(train1), label = train$time_spent, group = groups)
rankModel <- xgboost(dtrain,params = params, max.depth = 4, eta = .1, nround = 20, nthread = 2,
                     eval_metric = 'ndcg')
test = eval(substitute(test.rank.data(x)))
xgbTest = test[,list,with=FALSE]
test[,prediction:=predict(rankModel,as.matrix(xgbTest))]
meanaverage= test[,mapk(10,setorder(.SD[time_spent>0],ts),setorder(.SD,-prediction)),by=.(sid)]
MAP_XG = mean(meanaverage$V1)
meanrecip= test[,match(setorder(.SD[time_spent>0],ts)$contentId,setorder(.SD,-prediction)$contentId),by=.(sid)]
MRR_XG = mean(sapply(meanrecip$V1, function(x) 1/x))
output= matrix(c(MAP_XG,MRR_XG),ncol=2,byrow=TRUE)
rownames(output) = "Score"
colnames(output) = c("MAP_XGB","MRR_XGB")
return(output)
}

summaryRankXGB = function(x)
{
  meanaverage= x[,mapk(10,setorder(.SD[time_spent>0],ts),setorder(.SD,-prediction)),by=.(sid)]
  MAP_Naive = mean(meanaverage$V1)
  meanrecip= x[,match(setorder(.SD[time_spent>0],ts)$contentId,setorder(.SD,-prediction)$contentId),by=.(sid)]
  MRR_Naive = mean(sapply(meanrecip$V1, function(x) 1/x))
  output= matrix(c(MAP_Naive,MRR_Naive),ncol=2,byrow=TRUE)
  rownames(output) = "Score"
  colnames(output) = c("MAP_XGB","MRR_XGB")
  return(output)
}

