library(data.table)
library(bit64)
library(mlogit)
library(mice)
library(Hmisc)
library(caret)
library(zoo)

setwd("C:/Users/asus/Desktop/TRENDYOL")
#content ýd renge bile unique
#category_ýd örnek: sývý sabun
#pid cihaz ýd si
#sid session ýd si
#rm and gc can be used to remove unused data

#Ýmporting data
product_hierarchy =  fread("boun_project_products_hierarchy.csv")
str(product_hierarchy)

products = fread("boun_project _products.csv",
                 colClasses =list(factor = c("AGE_GROUP_ID","GENDER_ID","BRAND_ID")),
                 encoding = "UTF-8")
str(products)
summary(products)
head(products)
sapply(products, function(x) uniqueN(x))

#dublicated content id in products
#1den fazla olan varsa onları bulmak için tablo yaptık
#productı content idye göre uniqueleştirdik
n_occur <- data.frame(table(products$contentId))
n_occur[n_occur$Freq>1,]

products[contentId==1223240]
#dublicated items are same.colour is turkish in one and english in another
#Make contentid unique
products = unique(products,by="contentId")

#transactionları okuttuk baktık
trx = fread("boun_project_trx.csv")
str(trx)
summary(trx)
head(trx)


click = fread("boun_clickstream.csv",
              drop= c("pagetype"),encoding = "UTF-8")
str(click)
summary(click)
head(click)

#we are interested in session that contains search 40mdan 16m e düştü
click = click[,search:=ifelse(previousScreen=="searchResult",1,0)]
click = click[,sum := sum(search),by =.(sid)]
click = click[sum>0]

#search term is splitted to another column
click[screen %like% c('^searchResult,'),  c("silinecek", "SearchTerm"):= (tstrsplit(screen,'searchResult,', fixed=TRUE))]
click[,silinecek:=NULL]

head(click)
uniqueN(click$SearchTerm)
#144b unique search term var

#order every session by ts
click = click[,setorder(.SD,ts),by= .(sid)]

#search term corresponding to each contentid
click[previousScreen %like% 'searchResult' | screen %like% 'searchResult', 
  SearchTerm2 :=na.locf(.SD[,SearchTerm],na.rm = FALSE),by=.(sid)]

#most searched terms
head(click)
terms = na.omit(click$SearchTerm)
#na olanları at
search_frequency = data.table(sort(table(terms),decreasing=T))
head(search_frequency,n=150)
search_frequency[N ==1]

#Creating new columns to get profile of user
trx[,':='(mean_purchase=mean(PRICE),purchase_no = .N),by = .(userid)]
trx[,':='(total_order=uniqueN(order_parent_id),total_pay = sum(PRICE)),by = .(userid)]

#merge trx and products
trx_product = merge(trx,products,all.x=TRUE,by ="contentId")

#Finding user age-category and gender-category by transactions
trx_product[,':='(Age_Group = names(which.max(table(AGE_GROUP_ID)))),by = .(userid)]
trx_product$Age_Group = as.factor(trx_product$Age_Group)
sapply(trx_product, function(x) sum(is.na(x)))
trx_product[,':='(Gender = names(which.max(table(GENDER_ID)))),by = .(userid)]
trx_product$Gender = as.factor(trx_product$Gender)

#Check missing values in trx_product
sapply(trx_product, function(x) sum(is.na(x)))
str(trx_product)

#creating personal profile 62b kişi clicki var 24b in transactionı var
person_info = trx_product[,.SD[1],by=.(userid),.SDcols = c("mean_purchase","purchase_no",
                                                           "total_order","total_pay","Age_Group",
                                                           "Gender")]
#Analyze for personalization
click_person = merge(click,person_info,by="userid",all.x = TRUE)

#transactionı olmayan insanları çıkararak 16mdan 10m a düşürdük
click_person = click_person[!is.na(mean_purchase)]

head(click_person)

#timespent bulduk sessionın son itemi na kalıcak
click_person[, time_spent := difftime(shift(ts,type="lead"),ts, units="secs"),by=.(sid)]
head(click_person)

#sadece search termi olan content idleri tuttuk sadece 10m dan 1.5m a indi
search_click = click_person[!is.na(contentId) & !is.na(SearchTerm2)]
head(search_click)

#gereksiz columnları sil
search_click[,boutiqueId:=NULL]
search_click[,search:=NULL]
search_click[,sum:=NULL]
search_click[,SearchTerm:=NULL]
#gereksiz columları temizle
search1 = search_click[,c(1:8,15)]
head(search1)

write.csv(search1,"trendyol_click.csv",row.names = FALSE,fileEncoding ="UTF-8")

#Creating column represents day in data
min = as.Date(min(click$ts))
click[, day_order := as.numeric((as.Date(ts)-min+1))]
head(click)


#Check click date in click data and trx

max(trx$ORDER_DATE)
min(trx$ORDER_DATE)

max(click$ts)
min(click$ts)

#Encoding factor to dummy (not using now)
products_dmy  = dummyVars(" ~ .", data = products)
encode_products = as.data.table(predict(products_dmy, newdata = products))


barplot(xtabs(formula = ~Gender,trx_product),main = "Sales Distribution Over Gender")
barplot(xtabs(formula = ~Age_Group,trx_product),main = "Sales Distribution Over Age_Group")

#table of trx_product that show all units sold
xtabs(formula = ~Gender+Age_Group,trx_product)
#table of people profile 
xtabs(formula = ~Gender+Age_Group,person_info)

barplot(xtabs(formula = ~Gender,person_info),
        main = "Distribution of Gender of people",col = c("darkblue","red","yellow"))
barplot(xtabs(formula = ~Age_Group,person_info),
        main = "Distribution of Age_Group of people",col = c("darkblue","red","yellow","black"))

#total transaction in trx
totaltransaction = sum(person_info$total_pay)

#Analyzing order number corresponding to date
trx_product[,':='(day = as.Date(trx_product$ORDER_DATE))]
order_by_day = trx_product[,.(number=.N,amount=sum(PRICE.x)),by =.(day)]
order_by_day[,':='(ratio = (amount/number))]
order_by_day = setorder(order_by_day,day)
#Biggest sales number occurred in
biggest_sales = order_by_day[which.max(order_by_day$number)]$day
Summary_list = list(Total_Transaction_in_TL =totaltransaction,
                    Number_of_Transaction = nrow(trx),
                    Date_of_Biggest_Sales_in_Number = biggest_sales,
                    preferred_brand = names(which.max(table(trx_product$BRAND_ID))),
                    Gender_Type_Purchased_Most = names(which.max(table(trx_product$GENDER_ID))),
                    Age_Group_Purchased_Most =names(which.max(table(trx_product$AGE_GROUP_ID))))
                    
#Biggest sales in amount of money occurred in 
order_by_day[which.max(order_by_day$amount)]$day

#Visualize sales number
library(ggplot2)
ggplot()+geom_point(aes(x=order_by_day$day,y=order_by_day$number,
         color="tomato"))+ggtitle("Sales versus Date")+ xlab("Date")+ylab("Sales")

#Visualize sales amount
ggplot()+geom_point(aes(x=order_by_day$day,y=((order_by_day$amount)/1000),
        color="blue"))+ggtitle("Sales Amount versus Date")+ xlab("Date")+ylab("Sales(1000 TL)")

#create day in both trx and click to merge them
trx_product[,day:=as.Date(ORDER_DATE)]
trx[,day:=as.Date(ORDER_DATE)]
click[,day:=as.Date(ts)]

click1 = click[1:100000]
click1 =click1[,unique(.SD,by ="contentId"),by=.(sid)]
#for choice model in every session dublicated contentid should be eliminated
click_choice = merge(click1,trx_product,by = c("userid","contentId","day"),all.x =TRUE)
rm(click1)
click_choice[,':='(choice = ifelse(is.na(ORDER_DATE),0,1))]
sapply(click_choice, function(x) sum(is.na(x)))
choicedata = click_choice[,c(1,2,4,6,27)]
choicedata = merge(choicedata,products,by="contentId",all.x = TRUE)
#choicedata = merge(choicedataV2,person_info,by="userid",all.x = TRUE)
sapply(choicedata, function(x) sum(is.na(x)))
#create alterntive according to sid
selam = choicedata[,sapply(.SD, function(x) rowid(x)),by =.(sid)]

choicedata[,alt:=rowid(userid),by =.(sid)]
head(choicedata)
#drop session that purchase hasn't occurred
choicedatav2 = choicedata[,sum:=sum(choice),by =.(sid)]
choicedatav2 = choicedatav2[sum>0]
choicedatav2 = choicedatav2[,c(1:13)]

#impute missing value with mod
sapply(choicedatav2, function(x) sum(is.na(x)))
choicedatav7$GENDER_ID=ifelse(is.na(choicedatav7$GENDER_ID),
                              names(which.max(table(choicedatav7$GENDER_ID))),
                              choicedatav7$GENDER_ID)
choicedatav2$GENDER_ID = as.factor(choicedatav7$GENDER_ID)


choicedatav2$AGE_GROUP_ID=ifelse(is.na(choicedatav7$AGE_GROUP_ID),
                              names(which.max(table(choicedatav7$AGE_GROUP_ID))),
                              choicedatav7$AGE_GROUP_ID)
choicedatav2$AGE_GROUP_ID = as.factor(choicedatav7$AGE_GROUP_ID)

choicedatav2$PRICE=ifelse(is.na(choicedatav2$PRICE),
                                 mean(choicedatav2$PRICE,na.rm = TRUE),
                          choicedatav2$PRICE)

sapply(choicedatav2, function(x) sum(is.na(x)))
choicedatav2 = setorder(choicedatav2,sid)
choicedatav2$choice = ifelse(choicedatav2$choice==1,TRUE,FALSE)

choicedatav3 = choicedatav2[1:10]
choicedatav3 = unique(choicedatav3,by="contentId")
key_data = data.table(sid = unique(choicedatav2$sid),keyel = seq(1,uniqueN(choicedatav2$sid),by =1))
sum(choicedatav8$choice)
choicedatav2 = merge(choicedatav2,key_data, by ="sid",all.x = TRUE)
choicedatav2$alt <- as.factor(choicedatav2$alt)
row.names(choicedatav2) <- paste(choicedatav2$keyel, choicedatav2$alt, sep = ".")

levels = choicedatav2[,.N,by=.(sid)]
choicedatav3 = mlogit.data(choicedatav3,choice =  "choice", shape = "long",
                           varying = 6:12,
                           alt.levels = row.names(choicedatav3))

model = mlogit(choice~0+PRICE,
               data=choicedatav3)
summary(model)

#To export file
write.csv(order_by_day, file = "order_by_day.csv", row.names = FALSE)
