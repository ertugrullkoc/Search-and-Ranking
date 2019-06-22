RankModelXGB = function(x){
 x= ClosestMatch(substitute(x))
print(x)
train=training_data_click[SearchTerm2 == toString(x),]
train = merge(train,products_search[Query==toString(x)],by="contentId",all.x=TRUE)
train = merge(train,profile,by="userid",all.x=TRUE)

train = train[,tail(.SD,1), by=.(sid,contentId)]
train = setorder(train,"sid","ts")
groups = train[,.N,by=.(sid)][,N]
list = c("PRICE","AgeGroupRatio","GenderRatio","BrandRatio","meanpurchase","totalclick")
train1 = train[,list,with=FALSE]
params <- list(booster = 'gbtree',
               objective = 'rank:pairwise')
str(train1)
dtrain <- xgb.DMatrix(data = as.matrix(train1), label = train$time_spent, group = groups)
rankModel <- xgboost(dtrain,params = params, max.depth = 9, eta = 0.01, nround = 50, nthread = 2,
                     eval_metric = 'ndcg')
test = eval(substitute(test.rank.data(x)))
xgbTest = test[,list,with=FALSE]
test[,prediction:=predict(rankModel,as.matrix(xgbTest))]
meanaverage= test[,mapk(10,setorder(.SD[time_spent>0],ts),setorder(.SD,-prediction)),by=.(sid)]
MAP_XGB = mean(meanaverage$V1)
meanrecip= test[,match(setorder(.SD[time_spent>0],ts)$contentId,setorder(.SD,-prediction)$contentId),by=.(sid)]
MRR_XGB = mean(sapply(meanrecip$V1, function(x) 1/x))
output= matrix(c(MAP_XGB,MRR_XGB),ncol=2,byrow=TRUE)
rownames(output) = "Score"
colnames(output) = c("MAP_XGB","MRR_XGB")
return(output)
}
RankModelXGB("küpe")
RankModelNaive("küpe")
