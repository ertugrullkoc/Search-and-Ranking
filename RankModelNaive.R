RankModelNaive = function(x)
{
  x = substitute(x)
  train = eval(substitute(training.rank.data(x)))
  train = train[,tail(.SD,1), by=.(sid,contentId)]
  meanaverage= train[,mapk(10,setorder(.SD[time_spent>0],ts),setorder(.SD,-CumTimeSpent)),by=.(sid)]
  MAP_Naive = mean(meanaverage$V1)
  meanrecip= train[,match(setorder(.SD[time_spent>0],ts)$contentId,setorder(.SD,-CumTimeSpent)$contentId),by=.(sid)]
  MRR_Naive = mean(sapply(meanrecip$V1, function(x) 1/x))
  output= matrix(c(MAP_Naive,MRR_Naive),ncol=2,byrow=TRUE)
  rownames(output) = "Score"
  colnames(output) = c("MAP_Naive","MRR_Naive")
  return(output)
}
RankModelNaive("")
RankModelXGB("adidas")
XgBoostModel("adidas")
RankModelLogistic("adidas")
