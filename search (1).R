library(data.table)
library(bit64)
library(mlogit)
library(zoo)
library(RecordLinkage)
library(lubridate)
library(recommenderlab)
library(Metrics)
setwd("C:/Users/asus/Desktop/TRENDYOL")
products = fread("boun_project _products.csv",
                 colClasses =list(factor = c("AGE_GROUP_ID","GENDER_ID","BRAND_ID")),
                 encoding = "UTF-8")
products = unique(products,by="contentId")
product_hierarchy =  fread("boun_project_products_hierarchy.csv")
trx = fread("boun_project_trx.csv")
click = fread("trendyol_click.csv",encoding = "UTF-8")
head(click)
#en çok tiklanan kelimeleri veriyor
search_frequency = data.table(sort(table(na.omit(click$SearchTerm2)),decreasing=T))
setnames(search_frequency,c("Query","Frequency"))
head(search_frequency,n=10)

term_by_category = click[,unique(category_id),by=.(SearchTerm2)]
setnames(term_by_category,c("Query","Category_ID"))
# termleri content idleri ile map et
term_by_content = click[,.(contentId=unique(contentId)),by=.(SearchTerm2)]
setnames(term_by_content,c("Query","contentId"))

#One contentId can be seen in one session more than one make it unique
click_unique = click
click_unique = click_unique[,tail(.SD,1), by=.(sid,contentId)]

#Add day column Year/Month/Day to merge trx and click
trx[,day:=as.Date(ORDER_DATE)]
click[,day:=as.Date(ts)]
click_unique[,day:=as.Date(ts)]

#Add column BUY 31b tane ürün satin alinmis
click_buy = merge(click_unique,trx,by = c("userid","contentId","day"),all.x = TRUE)
click_buy[,BUY :=ifelse(is.na(ORDER_DATE),0,1)]
click_buy = click_buy[,c(1,2,4,6,14)]
head(click_buy)
click_buy = click_buy[BUY==1]
click_buy = merge(click,click_buy,by=c("sid","contentId","userid","ts"),all.x = TRUE)
click_buy$BUY = ifelse(is.na(click_buy$BUY),0,click_buy$BUY)
head(click_buy)
sapply(click_buy,function(x) sum(is.na(x)))
#remove click unique
rm(click_unique)


#Impute Time_Spent (na leri doldur)
click_buy$time_spent = as.numeric(click_buy$time_spent)
click_buy[,time_spent := ifelse(is.na(time_spent),mean(time_spent,na.rm = TRUE),time_spent),by=.(sid)]
click_buy[,time_spent:=ifelse(is.na(time_spent),mean(time_spent,na.rm = TRUE),time_spent)]
sapply(click_buy,function(x) sum(is.na(x)))

#Impute Age_Group_ID
click_buy[,AGE_GROUP_ID := ifelse(is.na(AGE_GROUP_ID),Age_Group,AGE_GROUP_ID)]

#Calculate Cumulatives
click_buy$ts = ymd_hms(click_buy$ts)
click_buy = click_buy[,setorder(.SD,ts),by=.(contentId)]
click_buy[,':='(CumClick = cumsum(search),CumBuy = cumsum(BUY),CumTimeSpent = cumsum(time_spent)), by=.(contentId)]
click_buy[,CumTimeSpent:=CumTimeSpent- time_spent,by=.(contentId)]

#Calculate Cumulative in Reverse Order
click_buy = click_buy[,setorder(.SD,-ts),by=.(contentId)]
click_buy[,':='(CumClickReal = cumsum(search),CumBuyReal = cumsum(BUY),CumTimeSpentReal = cumsum(time_spent)), by=.(contentId)]
click_buy[,CumTimeSpentReal:=CumTimeSpentReal- time_spent,by=.(contentId)]

#CREATING PERSONAL PROFILE
trx[,':='(mean_purchase=mean(PRICE),purchase_no = .N),by = .(userid)]
trx[,':='(total_order=uniqueN(order_parent_id),total_pay = sum(PRICE)),by = .(userid)]
trx_product = merge(trx,products,all.x=TRUE,by ="contentId")
trx_product[,':='(Age_Group = names(which.max(table(AGE_GROUP_ID)))),by = .(userid)]
trx_product$Age_Group = as.factor(trx_product$Age_Group)
sapply(trx_product, function(x) sum(is.na(x)))
trx_product[,':='(Gender = names(which.max(table(GENDER_ID)))),by = .(userid)]
trx_product$Gender = as.factor(trx_product$Gender)
person_info = trx_product[,.SD[1],by=.(userid),.SDcols = c("mean_purchase","purchase_no",
                                                           "total_order","total_pay","Age_Group",
                                                          "Gender")]
click_buy1 = click_buy
click_buy1 = merge(click_buy1,products,by = "contentId",all.x = TRUE)
sapply(click_buy1,function(x) sum(is.na(x)))
head(click_buy1)
click_buy1 = merge(click_buy1,person_info,by="userid",all.x = TRUE)
profile = click_buy1[,.(duration = sum(time_spent), total_buy = mean(purchase_no), meanpurchase = mean(mean_purchase),totalclick = .N,
                       User_Gender = head(Gender,1),User_Age = head(Age_Group,1),
                       Favorite_brand = names(which.max(table(BRAND_ID)))) ,by=.(userid)]
profile[,Price_Level := ifelse(meanpurchase<quantile(meanpurchase,0.25),0,
                               ifelse(meanpurchase>quantile(meanpurchase,0.75),2,1)),]

profile[,User_Activity := ifelse(duration<quantile(duration,0.25),0,
                                 ifelse(duration>quantile(duration,0.75),2,1)),]
profile$Price_Level = as.factor(profile$Price_Level)
profile$User_Activity = as.factor(profile$User_Activity)
rm(person_info)
rm(trx_product)

#Impute Price
median_price_category = click_buy1[,.(PRICE = median(PRICE,na.rm=TRUE)),by=.(category_id)]
setnames(median_price_category,c("category_id","MedianPrice"))

click_buy1 = merge(click_buy1,median_price_category,by="category_id",all.x=TRUE)
click_buy1[,PRICE:=ifelse(is.na(PRICE),mean(PRICE,na.rm = TRUE),PRICE),by=.(SearchTerm2)]
click_buy1[,PRICE:=ifelse(is.na(PRICE),MedianPrice,PRICE)]
click_buy1[,MedianPrice:=NULL]


#Search Term analyze in terms of revenue 
total = sum(click_buy1$PRICE*click_buy$BUY)
Basket = term_by_content[,.(TotalItem = .N),by=.(Query)]
search_term = click_buy1[,.(Revenue = sum(PRICE*BUY),TotalClick = .N,Ratio = ((sum(PRICE*BUY)/total)*100)),by=.(SearchTerm2)]
search_term = merge(search_term,Basket,by.x = "SearchTerm2",by.y = "Query",all.x=TRUE)
search_term = setorder(search_term,-"Ratio")
search_term[,Cum_ratio:=cumsum(Ratio)]
rm(Basket)

category = click_buy[,.(gain = sum(PRICE*BUY),frequency = .N,ratio = ((sum(PRICE*BUY)/total)*100)),by=.(category_id)]
category = click_buy[,.(MostUsedTerm =names(which.max(table(SearchTerm2))),Revenue = sum(PRICE*BUY),TotalClick = .N,Ratio = ((sum(PRICE*BUY)/total)*100)),by=.(category_id)]
category = setorder(category,-"Ratio")
category[,Cum_ratio:=cumsum(Ratio)]


#Mean Average Precision
head(click_buy)
a = setorder(click_buy[sid=="89F77776-F5EE-4E50-AC34-A59D33895204"],-CumTimeSpentReal)[,.(contentId,CumTimeSpent)]
b = setorder(click_buy[sid=="89F77776-F5EE-4E50-AC34-A59D33895204"],-CumTimeSpent)[,.(contentId,CumTimeSpent)]
mapk(10,a,b)

mapk(5,c(2,3,4),c(1,2,3,4,5,6,7,8))
#choice data 
choice_data = merge(click_unique,trx,by = c("userid","contentId","day"),all.x = TRUE)
choice_data[,BUY :=ifelse(is.na(ORDER_DATE),0,1)]
choice_data = choice_data[,-c(26,27,28)]
choice_data = choice_data[SearchTerm2=="Kadýn Spor Ayakkabý"]
choice_data[,alt:=rowid(userid),by =.(sid)]
key_data = data.table(sid = unique(choice_data$sid),keyel = seq(1,uniqueN(choice_data$sid),by =1))
choice_data = merge(choice_data,key_data, by ="sid",all.x = TRUE)
choice_data = choice_data[,total:=sum(BUY),by=.(sid)]
choice_data = choice_data[total==1]
choice_data$alt <- as.factor(choice_data$alt)
row.names(choice_data) <- paste(choice_data$keyel, choice_data$alt, sep = ".")

levels = choice_data[,.N,by=.(sid)]
choicedata = mlogit.data(choice_data,choice =  "choice", shape = "long",
                           varying = 12:25,
                           alt.levels = row.names(choice_data))
model = mlogit(BUY~+time_spent,
               data=choicedata)
summary(model)


sum(trx$PRICE)
click_buy[SearchTerm2=="Beko Çamaþýr Makinesi"]
a = click_buy[contentId == 315159]
a = click_buy[is.na(PRICE)]
head(click_buy)
sum(a$BUY)
cor(click_buy[!is.na(time_spent)]$time_spent,click_buy[!is.na(time_spent)]$BUY)
click_buy[,boutiqueId:=NULL]
a = click_buy[,.(mean_buy =mean(BUY),mean_purchase = mean(mean_purchase),mean_price = mean(PRICE,na.rm = TRUE)),by=.(userid)]
a = a[!is.na(mean_price)]
sapply(click_buy,function(x) sum(is.na(x)))
setorder(a,-mean)
cor(a)


#check for sid that may be bot or not
click_buy[,total_duration := sum(time_spent),by=.(sid)]
sum(click_buy[total_duration>4700]$BUY)
max(click_buy$total_duration)

##Creating Training and Test Dataset

#Creating column represents day in data
min = as.Date(min(click$ts))
click_buy[, day_order := as.numeric((as.Date(ts)-min+1))]

min = as.Date(min(trx$ORDER_DATE))
trx[, day_order := as.numeric((as.Date(ORDER_DATE)-min+1))]

#creating training and test set 
a=1 # a is number of day that will be included in traning data
b=1 # b is number of day that will be used in test data

training_data_click = click_buy[day_order<=a]
test_data_click = click_buy[day_order>a & day_order<= a+b]

training_data_trx = trx[day_order<=a]
test_data_trx = trx[day_order>a & day_order<= a+b]


ClosestMatch = function(string){
  
  similarity = levenshteinSim(string, unique(search_frequency$Query))
  a = unique(search_frequency$Query)[which.max(similarity)]
  return(search_term[SearchTerm2 %in% a][which.max(search_term[SearchTerm2 %in% a]$Revenue)]$SearchTerm2[1])
}

ClosestMatch("abidas")
click_buy$BUY = as.factor(click_buy$BUY)
click_buy$time_spent = scale(click_buy$time_spent)
classifier = glm(formula = BUY~GENDER_ID+AGE_GROUP_ID+PRICE,data = click_buy,family=binomial())
summary(classifier)
rank_by_logistic("Fitness Eldiveni")
rank_by_logistic = function(x)
{
  x= ClosestMatch(substitute(x))
  print(x)
  ýtems = term_by_content[Query==x]$contentId
  set = products[contentId %in% ýtems]
  set [,prediction :=predict(classifier,newdata = set, type = "response")]
  setorder(set,-prediction)
  d = set[!is.na(prediction),.(contentId,prediction)]
  return(d)
}

head(click_buy)
plot(click_buy$time_spent)
rank_by_logistic("kadýn")
a = click_buy[contentId == 2340198]
cor(person_info)
person_info = click[,.(mean_spent = mean(mean_purchase),mean_duraion = mean(time_spent,na.rm = TRUE),
                       total_duration=sum(time_spent,na.rm = TRUE)),by=.(userid)]
cor(person_info[!is.na(mean_duraion)])
a = rank_by_logistic("Fitness Eldiveni")
b = naive_rank_click("Fitness Eldiveni")
c = merge(a,b,by ="contentId",all.x = TRUE)
head(term_by_category)
head(term_by_content)
naive_rank_purchase <- function (x) {
  x= ClosestMatch(substitute(x))
  print(x)
  b = term_by_category[Query == x]
  c = b$Category_ID
  d = eval(substitute(products[category_id %in% c]))
  e = trx[,.N,by=.(contentId)]
  f = merge(d,e,by="contentId",all.x =TRUE)
  f$N = ifelse(is.na(f$N),0,f$N)
  eval(substitute(setorder(f,-NN),list(NN="N")))
  f = f[,.(contentId,N)]
  return(f) 
}

naive_rank_click= function (x) {
  x= ClosestMatch(substitute(x))
  print(x)
  b = term_by_category[Query == x]
  c = b$Category_ID
  d = eval(substitute(products[category_id %in% c]))
  e = click[,.N,by=.(contentId)]
  f = merge(d,e,by="contentId",all.x =TRUE)
  f$N = ifelse(is.na(f$N),0,f$N)
  eval(substitute(setorder(f,-NN),list(NN="N")))
  f = f[,.(contentId,N)]
  return(f) 
}
#RANK BY CATEGORYID
naive_rank_category= function (x,type){
  y=substitute(type)
  if(y=="click")
    { 
  x= ClosestMatch(substitute(x))
  print(x)
  b = term_by_category[Query == x]
  c = b$Category_ID
  d = products[category_id %in% c]
  e = click[,.N,by=.(contentId)]
  f = merge(d,e,by="contentId",all.x =TRUE)
  f$N = ifelse(is.na(f$N),0,f$N)
  eval(substitute(setorder(f,-NN),list(NN="N")))
  f = f[,.(contentId,N)]
  return(f) 
  }
  if(y=="buy")
 { 
  x= ClosestMatch(substitute(x))
  print(x)
  b = term_by_category[Query == x]
  c = b$Category_ID
  d = products[category_id %in% c]
  e = trx[,.N,by=.(contentId)]
  f = merge(d,e,by="contentId",all.x =TRUE)
  f$N = ifelse(is.na(f$N),0,f$N)
  eval(substitute(setorder(f,-NN),list(NN="N")))
  f = f[,.(contentId,N)]
  return(f) 
  }
  else {print("CHECK TYPE")}
}

# RANK BY CONTENTID
naive_rank_content= function (x,type){
  y=substitute(type)
  if(y=="click")
  { 
    x= ClosestMatch(substitute(x))
    print(x)
    b = term_by_content[Query == x]
    c = b$contentId
    d = products[contentId %in% c]
    e = click[,.N,by=.(contentId)]
    f = merge(d,e,by="contentId",all.x =TRUE)
    f$N = ifelse(is.na(f$N),0,f$N)
    eval(substitute(setorder(f,-NN),list(NN="N")))
    f = f[,.(contentId,N)]
    return(f) 
  }
  if(y=="buy")
  { 
    x= ClosestMatch(substitute(x))
    print(x)
    b = term_by_content[Query == x]
    c = b$contentId
    d = products[contentId %in% c]
    e = trx[,.N,by=.(contentId)]
    f = merge(d,e,by="contentId",all.x =TRUE)
    f$N = ifelse(is.na(f$N),0,f$N)
    eval(substitute(setorder(f,-NN),list(NN="N")))
    f = f[,.(contentId,N)]
    return(f) 
  }
  else {print("CHECK TYPE")}
}

naive_rank_content("fitness eldiveni",type = buy)
naive_rank_category("adidas",type = buy)


naive_rank("abidas",type = buy)
naive_rank_click("Armani")
rank_by_logistic("Arman")

