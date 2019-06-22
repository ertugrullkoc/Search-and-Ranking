
RankModelLogistic = function(x)
{
  x= ClosestMatch(substitute(x))
  print(x)
  list = c("PRICE","AgeGroupRatio","GenderRatio","BrandRatio","meanpurchase","totalclick","BUY")
  train=training_data_click[SearchTerm2 == toString(x),]
  train = merge(train,products_search[Query==toString(x)],by="contentId",all.x=TRUE)
  train = merge(train,profile,by="userid",all.x=TRUE)
  train = train[,tail(.SD,1), by=.(sid,contentId)]
  Logitmodel = glm(formula = BUY~GenderRatio+PRICE,data=train)
  test = eval(substitute(test.rank.data(x)))
  test[,prediction:=predict(Logitmodel,newdata = test,type="response")]
  meanaverage= test[,mapk(10,setorder(.SD[time_spent>0],ts),setorder(.SD,-prediction)),by=.(sid)]
  MAP_Logit = mean(meanaverage$V1)
  meanrecip= test[,match(setorder(.SD[time_spent>0],ts)$contentId,setorder(.SD,-prediction)$contentId),by=.(sid)]
  MRR_Logit = mean(sapply(meanrecip$V1, function(x) 1/x))
  output= matrix(c(MAP_Logit,MRR_Logit),ncol=2,byrow=TRUE)
  rownames(output) = "Score"
  colnames(output) = c("MAP_Logit","MRR_Logit")
  return(output)
}
RankModelLogistic("adidas")
train=training_data_click[SearchTerm2 == toString(x),]
