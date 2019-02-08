loo_mean = function(x) (sum(x) - x) / (length(x) - 1)
set_train_lhood = function(df, col, by, noise_sd = 0.02) {
  col = as.symbol(col)
  name = paste0(by, "_likelihood")
  
  df[, (name) := loo_mean(eval(col)), by = by]
  
  # Impute NAs.
  is_na = which(is.na(df[[name]]))
  set(df, is_na, name, df[, loo_mean(eval(col))][is_na] )
  
  # Multiply by noise.
  set(df, NULL, name, pmin(df[[name]] * rnorm(nrow(df),mean=1, sd=noise_sd), 1) )
  
  invisible (NULL)
}


set_test_lhood = function(tr, vd, col, by) {
  col = as.symbol(col)
  name = paste0(by, "_likelihood")
  
  ll = tr[, mean(eval(col)), by = by]
  i = match(vd[[by]], ll[[by]])
  
  vd[, (name) := ll[i, 2]]
  
  # Impute NAs.
  is_na = which(is.na(vd[[name]]))
  set(vd, is_na, name, vd[, mean(eval(col))][is_na] )
  
  invisible (NULL)
}


library(dplyr)
library(xgboost)
library(stringr)
#setwd('/Users/yangjichen/2017DMC/data/raw/')
orig_train = read.csv("train.csv", sep = '|')
orig_items = read.csv("final_unif.csv", sep = ',')
orig_classcsv = read.csv("class.csv", sep = '|')

#将train与item表格用pid关联起来
df = merge(orig_train, orig_items, by = "pid")
classcsv = merge(orig_classcsv, orig_items, by = "pid")
#df_classcsv = merge(orig_classcsv, orig_items, by = "pid")

#多出来的4个"click"           "basket"          "order"               "revenue" 
df = df[order(df$lineID), ]
classcsv = classcsv[order(classcsv$lineID),]
#为了之后encoding，使用这个package
library(data.table)
df = data.table(df)
classcsv = data.table(classcsv)




#新建变量
df$day_7 = df$day %% 7
df$group_12 = str_sub(df$group,1,2)
df$group_34 = str_sub(df$group,3,4)

classcsv$day_7 = classcsv$day %% 7
classcsv$group_12 = str_sub(classcsv$group,1,2)
classcsv$group_34 = str_sub(classcsv$group,3,4)

df$price_discount = as.numeric((df$price / df$rrp)<1)
classcsv$price_discount = as.numeric((classcsv$price / classcsv$rrp)<1)

#这个变量衡量了一个pid出现次数与出现后被购买次数的信息
pid_purchase_info = group_by(df, pid)
pid_purchase_info = summarise(pid_purchase_info, 
                              num_pid_order = sum(order, na.rm=T),
                              pid_pro = num_pid_order/n())
pid_purchase_info = data.frame(pid_purchase_info)
df = merge(df, pid_purchase_info, by = "pid",all.x=TRUE)
classcsv = merge(classcsv, pid_purchase_info, by = "pid",all.x=TRUE)



df[, `:=`(price_discount = (price / rrp<1)
)]
classcsv[, `:=`(price_discount = (price / rrp<1)
)]

#只在训练集里面有的变量
df$quantity = df$revenue / df$price
label = df$revenue


#随机划分数据集
n = nrow(df)
set.seed(1234)
trainindex = sample(1:n,0.8*n)

train = df[trainindex, ]
test = df[-trainindex, ]
lhood_list = c("availability",'pid',
               "day_7")

#这里会报错，因为好多pid在train里面没见过
for(term in lhood_list){
  set_train_lhood(train, "order", term)
  set_test_lhood(train, test, "order", term)
  set_test_lhood(train, classcsv, "order", term)
}

train = data.frame(train)
test= data.frame(test)
classcsv = data.frame(classcsv)

classcsv$pid_likelihood[is.na(classcsv$pid_likelihood)]=0

#这里classcsv应该不用变
label_cols = c("click", "basket", "order", "revenue", "quantity")
train_labels = train[label_cols]
train = train[!(names(train) %in% label_cols)]

test_labels = test[label_cols]
test = test[!(names(test) %in% label_cols)]


#上面这些变量预测时候用不到
#下面的变量是因为编码过所以删除
delete_cols = c(c( "lineID",'day','content','unit','campaignIndex','group','pid1'),
                c("adFlag","availability", "manufacturer",'pid', 'pharmForm',
                  "genericProduct","category",'salesIndex', "day_7","group_12","group_34"))

#delete_cols = c( "lineID",'day','content','unit','campaignIndex','group','pid1')
train = train[!(names(train) %in% delete_cols)]
test = test[!(names(test) %in% delete_cols)]
classcsv = classcsv[!(names(classcsv) %in% delete_cols)]

#这里需要清除一下变量
rm(df)
rm(orig_classcsv)
rm(orig_items)
rm(orig_train)

#character转换成factor
train = lapply(train, function(col) {
  if (!inherits(col, "character"))
    return (col)
  
  unclasscsv(factor(col))
})
train = as.data.frame(train)
train = data.matrix(train)

test = lapply(test, function(col) {
  if (!inherits(col, "character"))
    return (col)
  
  unclasscsv(factor(col))
})
test = data.matrix(as.data.frame(test))

classcsv = lapply(classcsv, function(col) {
  if (!inherits(col, "character"))
    return (col)
  
  unclasscsv(factor(col))
})

#为什么这里一直卡？？？
#想不明白
classcsv = as.data.frame(classcsv)
classcsv = data.matrix(classcsv)

#第一层模型
model = xgboost(data = as.matrix(train), label = train_labels$order,
                max.depth = 4, eta = 0.3, nthread = 64, nround =10, 
                subsample = 1,
                objective = "binary:logistic")
#第一层模型预测
preds = predict(model, test)
preds01 = ifelse(preds <= 0.5, 0, 1)
prop.table(table(test_labels$order, preds01))

#在真实集合预测
print('predict')
preds = predict(model, classcsv)
#print(preds)
preds01 = ifelse(preds <= 0.5, 0, 1)
summary(preds01)

true = read.csv('realclass.csv',sep = '|')
true_order = as.numeric(true$revenue>0)
prop.table(table(true_order, preds01))

classcsv = as.data.frame(classcsv)
final_revenue = preds01*classcsv$price
write.csv(final_revenue,file = 'Pred_Reve.csv')
