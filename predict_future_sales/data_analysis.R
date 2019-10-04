# File descriptions
# sales_train.csv - the training set. Daily historical data from January 2013 to October 2015.
# test.csv - the test set. You need to forecast the sales for these shops and products for November 2015.
# sample_submission.csv - a sample submission file in the correct format.
# items.csv - supplemental information about the items/products.
# item_categories.csv  - supplemental information about the items categories.
# shops.csv- supplemental information about the shops.
# 
# 
# Data fields
# ID - an Id that represents a (Shop, Item) tuple within the test set
# shop_id - unique identifier of a shop
# item_id - unique identifier of a product
# item_category_id - unique identifier of item category
# item_cnt_day - number of products sold. You are predicting a monthly amount of this measure
# 
# item_price - current price of an item
# date - date in format dd/mm/yyyy
# date_block_num - a consecutive month number, used for convenience. January 2013 is 0, February 2013 is 1,..., October 2015 is 33
# item_name - name of item
# shop_name - name of shop
# item_category_name - name of item category


library(dplyr)
library(lubridate)
library(keras)



items <- read.csv("predict_future_sales/competitive-data-science-predict-future-sales/items.csv", encoding = "UTF-8")
item_categories <- read.csv("predict_future_sales/competitive-data-science-predict-future-sales/item_categories.csv", encoding = "UTF-8")
shops <- read.csv("predict_future_sales/competitive-data-science-predict-future-sales/shops.csv", encoding = "UTF-8")
sales_train <- read.csv(gzfile("predict_future_sales/competitive-data-science-predict-future-sales/sales_train.csv.gz"), encoding = "UTF-8")

summary(sales_train)
boxplot(sales_train$item_price)
sales_train[sales_train$item_price < 0, ]
sales_train[sales_train$item_price > 50000, ]
hist(sales_train[sales_train$item_id == 11365, "item_price"], breaks = 150) # point with high proce lie outsido of majority of points
items[items$item_id == 11365, ] # Delivery of some items

hist(sales_train[sales_train$item_id == 6066, "item_price"], breaks = 150)
items[items$item_id == 6066, ]                                          # Single item

hist(sales_train[sales_train$item_id == 13199, "item_price"], breaks = 150)
items[items$item_id == 13199, ]                                         # Single item

sales_train <- sales_train %>% 
    mutate(
        total_income = item_cnt_day*item_price,
        weekday = wday(dmy(date)),
        weekend = as.numeric(weekday %in% c(6, 7)),
        month = month(dmy(date))
)

sales_train <- merge(sales_train, items[2:3], all.x = TRUE)

sales_train <- sales_train[sales_train$item_price > 0, ]

x <- sales_train %>% 
    group_by(shop_id) %>% 
    summarise(
        total = sum(item_cnt_day), 
        n = length(unique(item_id)), 
        sum = sum(total_income))

plot(x$n, x$total)
y <- sales_train %>% 
    group_by(shop_id) %>% 
    summarise(
        total = sum(item_cnt_day), 
        avg_price = mean(item_price), 
        sum  =sum(total_income))
plot(y$avg_price, y$total) # Normal distribution



table(sales_train$weekend) #1/3 of all cases sold in weekends

z <- sales_train %>% 
    group_by(new_year_eve) %>% 
    summarise(
        number_of_items = sum(item_cnt_day), 
        sum = sum(item_cnt_day*item_price))
z$number_of_items[2]/z$number_of_items[1]
z$sum[2]/z$sum[1]
1/12

tmp <- sales_train %>% 
    group_by(weekday) %>% 
    summarise(
        n = n(), 
        total = sum(item_cnt_day), 
        sum  = sum(total_income))


tmp1 <- sales_train %>%
    group_by(item_id) %>%
    summarise(
        n = n(),
        sum = sum(total_income),
        tottal = sum(item_cnt_day) 
    )

tmp2 <- sales_train %>%
    group_by(month) %>%
    summarise(n = n(),
              sum = sum(total_income)
              ) %>%
    ungroup() %>%
    mutate(percent = sum/sum(sum)*100)%>%
    arrange(month)

tmp3 <- sales_train %>%
    group_by(item_category_id) %>%
    summarise(n = n(),
              sum = sum(total_income)
    ) %>%
    ungroup() %>%
    mutate(percent = sum/sum(sum)*100)%>%
    arrange(n)


# Creating train set and labels
train <- select(sales_train, shop_id:item_price, weekday, month)
labels <- select(sales_train, item_cnt_day)
train <- as.matrix(unname(train))
labels <- unlist(unname(labels))

model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = dim(train)[2]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)

model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
)

model %>% fit(
    train,
    labels,
    epochs = 3,
    verbose = 0
    
)

