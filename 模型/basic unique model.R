library(xgboost)
library(stringr)

setwd("/Users/jiahangxu/Courses/BigDataAnalysis/BigDataAnalysisPJ/DMC_2017_task/")
# 读入数据
orig_train = read.csv("train.csv", sep = '|')
orig_items = read.csv("final_unif.csv", sep = ',')
orig_class = read.csv("class.csv", sep = '|')


filter = orig_train[orig_train$day <= 80, ]
none_filter = orig_train[orig_train$day > 80, ]
none_filter$number = 1



sum <- summarize(group_by(filter, pid, day, adFlag, availability, competitorPrice, price), n(), n_distinct(order), mean(order), mean(revenue))
colnames(sum) <- c("pid", "day", "adFlag", "availability", "competitorPrice", "price", "number", "n_distinct_order", "order","revenue")

tTrySum <- subset(sum,sum["n_distinct_order"]==1)
tTrySum = subset(tTrySum, select = c(-n_distinct_order))
none_filter = subset(none_filter, select = c(-lineID, -click, -basket))

tTrySum = data.frame(tTrySum)
none_filter = data.frame(none_filter)
tbindDF = rbind(tTrySum, none_filter)




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


####---------------下面基本上是杨继琛的代码-------------------####
#将train与item表格用pid关联起来
df = merge(tbindDF, orig_items, by = "pid")
#df_class = merge(orig_class, orig_items, by = "pid")
df = df[order(df$pid), ]

#为了之后encoding，使用这个package
library(data.table)
df = data.table(df)


df$day_7 = df$day %% 7
# df$day_14 = df$day %% 14
# df$day_30 = df$day %% 30

df$group_12 = str_sub(df$group,1,2)
df$group_34 = str_sub(df$group,3,4)

df$quantity = df$revenue / df$price

#df$order_qty = df$quantity*df$order
#plot(density(na.omit(df$order_qty)),xlim=c(0,10))
label = df$revenue

train = df[df$day <= 80, ]
test = df[df$day > 80, ]

#加上了encoding

lhood_list = c("adFlag","availability", "manufacturer",'pid', "manufacturer", 'group',
               'pharmForm',"genericProduct","category",'salesIndex', "day_7","group_12","group_34")

for(term in lhood_list){
  set_train_lhood(train, "order", term)
  set_test_lhood(train, test, "order", term)
}

train = data.frame(train)
test= data.frame(test)

label_cols = c("order", "quantity", "revenue")
train_labels = train[label_cols]
train = train[!(names(train) %in% label_cols)]

test_labels = test[label_cols]
test = test[!(names(test) %in% label_cols)]

#这两个变量预测时候用不到
delete_cols = c(c( 'day','content','unit','campaignIndex','group','pid1'),
                c("adFlag","availability", "manufacturer",'pid', "manufacturer", 'group',
                  'pharmForm',"genericProduct","category",'salesIndex', "day_7","group_12","group_34"))
#delete_cols = c( "lineID",'day','content','unit','campaignIndex','group','pid1')
train = train[!(names(train) %in% delete_cols)]
test = test[!(names(test) %in% delete_cols)]

#character转换成factor
train = lapply(train, function(col) {
  if (!inherits(col, "character"))
    return (col)
  
  unclass(factor(col))
})
train = as.data.frame(train)
train = data.matrix(train)

test = lapply(test, function(col) {
  if (!inherits(col, "character"))
    return (col)
  
  unclass(factor(col))
})
test = data.matrix(as.data.frame(test))





w = train_labels$order
w[w==0]=0.6
w = w/sum(w)
#第一层模型
model = xgboost(data = as.matrix(train), label = train_labels$revenue,
                max.depth = 30, eta = 0.1, nthread = 32, nround = 3, 
                subsample = 1,
                lambda = 3,
                objective = "reg:linear")
#第一层模型预测
preds = predict(model, as.matrix(train))

preds01 = ifelse(preds <= 0.6, 0, 1)
prop.table(table(test_labels$revenue, preds01))


maeCal <- function(pred, obs) mean(abs(pred - obs))

maeCal(train_labels$revenue, preds01)
