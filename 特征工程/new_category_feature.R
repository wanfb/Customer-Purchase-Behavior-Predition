library(stringr)
library(dplyr)
setwd('/Users/yangjichen/2017DMC/data/raw/')

orig_train = read.csv("train.csv", sep = '|')
orig_items = read.csv("final_unif.csv", sep = ',')
orig_class = read.csv("class.csv", sep = '|')

orig_items = subset(orig_items,select = -pid1)
#将train与item表格用pid关联起来
train = merge(orig_train, orig_items, by = "pid")
train = train[order(train$lineID), ]

test = merge(orig_class, orig_items, by = "pid")
test = test[order(test$lineID), ]
#new feature
train[train=='']=NA
test[test=='']=NA

#这些变量是为了看这个pid的产品曾经是否出现过点击记录/购物车记录/购买记录，思路：曾经被购买的商品容易再次被购买

#被点击次数是等于1次的，大于1次的
click_time = group_by(train[train$click==1,], pid)
click_time = summarise(click_time, click_time = n())
click_time = data.frame(click_time)
train = merge(train, click_time, by = "pid",all.x=TRUE)

test = merge(test, click_time, by = "pid",all.x=TRUE)

#被购物车次数是等于1次的，大于1次的
basket_time = group_by(train[train$basket==1,], pid)
basket_time = summarise(basket_time, basket_time = n())
basket_time = data.frame(basket_time)
train = merge(train, basket_time, by = "pid",all.x=TRUE)
test = merge(test, basket_time, by = "pid",all.x=TRUE)

#被购买次数是等于1次的，大于1次的，发现百分之90的商品都会出现购买2次以上的状况
order_time = group_by(train[train$order==1,], pid)
order_time = summarise(order_time, order_time = n())
order_time = data.frame(order_time)
train = merge(train, order_time, by = "pid",all.x=TRUE)
test = merge(test, order_time, by = "pid",all.x=TRUE)

#从pid购买信息中提取先验知识
pid_purchase_info = group_by(train, pid)
pid_purchase_info = summarise(pid_purchase_info, 
                              num_pid_click = log(sum(click, na.rm=T)+1),
                              num_pid_basket = log(sum(basket, na.rm=T)+1),
                              num_pid_order = log(sum(order, na.rm=T)+1))
pid_purchase_info = data.frame(pid_purchase_info)

train = merge(train, pid_purchase_info, by = "pid",all.x=TRUE)
test = merge(test, pid_purchase_info, by = "pid",all.x=TRUE)

#仔细观察了下group这个变量，发现前两位是数字组合或者数字字母组合，34位置是O开头的组合，后面没什么规律，所以我想使用两个新变量，分别是12位、34位
#34位没有值的话怎么去补？   暂时是''这个空字符，好像也可以用？？？ 

#12位置只有21种药物
#34位置有15种药物，而且34位置missing的药物不多，建议不要舍弃这个变量
train$group_12 = str_sub(train$group,1,2)
train$group_34 = str_sub(train$group,3,4)

test$group_12 = str_sub(test$group,1,2)
test$group_34 = str_sub(test$group,3,4)

#从不同group来寻找购买特征
group12_purchase_info = group_by(train[train$order==1,],group_12)
group12_purchase_info = summarise(group12_purchase_info,
                                  group12_order = n())

group34_purchase_info = group_by(train[train$order==1,],group_34)
group34_purchase_info = summarise(group34_purchase_info,
                                  group34_order = n())

group12_purchase_info = data.frame(group12_purchase_info)
group34_purchase_info = data.frame(group34_purchase_info)

train = merge(train, group12_purchase_info, by = "group_12",all.x=TRUE)
train = merge(train, group34_purchase_info, by = "group_34",all.x=TRUE)
test = merge(test, group12_purchase_info, by = "group_12",all.x=TRUE)
test = merge(test, group34_purchase_info, by = "group_34",all.x=TRUE)


#按周划分后
train$day_7 = train$day %% 7
train$day_14 = train$day %% 14
train$day_30 = train$day %% 30

test$day_7 = test$day %% 7
test$day_14 = test$day %% 14
test$day_30 = test$day %% 30
#按周划分后就可以去看每周的平均购买量
week_purchase_info = group_by(train,day_7)
week_purchase_info = summarise(week_purchase_info,
                               week_order = n())
week_purchase_info = data.frame(week_purchase_info)
train = merge(train, week_purchase_info, by = "day_7",all.x=TRUE)
test = merge(test, week_purchase_info, by = "day_7",all.x=TRUE)

train = train[order(train$lineID), ]
test = test[order(test$lineID), ]


write.csv(train,file = 'trainset1221.csv')
write.csv(test,file = 'testset1221.csv')


