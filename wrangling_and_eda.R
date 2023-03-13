library(data.table)
library(dplyr)

train <- fread("data/train.csv", na.strings="",
               col.names=c("id","date","store_nbr","item_nbr","unit_sales","onpromotion"))

# Get a random 10% of observations
train_sample <- train[sample(.N, .1 * .N), ]

write.csv(train_sample, "data/train_sample.csv", row.names=F)


train_2017 <- train[train$date > "2017-01-01",]
write.csv(train_2017, "data/train_2017.csv", row.names=F)




# unit_sales by month
