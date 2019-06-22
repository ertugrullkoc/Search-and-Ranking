predict_naive= function (x,type){
  y=substitute(type)
  if(y=="click")
  { 
    x= ClosestMatch(substitute(x))
    print(x)
    b = eval(substitute(naive_rank_content(x,click),list(click="click",x=toString(x))))
    c = test_data_click[,.(he=.N),by=.(contentId)]
    d = eval(merge(b,c,by.x="contentId",by.y = "contentId",all.x=TRUE))
    d$he = ifelse(is.na(d$he),0,d$he)
    return(d)
  }
  if(y=="buy")
  { 
    x= substitute(ClosestMatch(substitute(x)))
    print(x)
    b = eval(substitute(naive_rank_content(x,click),list(click="click")))
    return(b)
    c = test_data_trx[,.(he=.N),by=.(contentId)]
    d = eval(merge(b,c,by.x="contentId",by.y = "contentId",all.x=TRUE))
  }
  else {print("CHECK TYPE")}
}
predict_naive("fitness eldiveni",type = click)
sum(a$he)
sum(a$N)
names(naive_rank_content("armni",type = click))
names(test_data_click[,.(he=.N),by=.(contentId)])
names(naive)
test_data_click[SearchTerm2=="Adidas"]
