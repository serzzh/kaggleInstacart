
###########################################################################################################
#
# Kaggle Instacart competition
# Fabien Vavrand, June 2017
# Simple xgboost starter, score 0.3791 on LB
# Products selection is based on product by product binary classification, with a global threshold (0.21)
#
###########################################################################################################

library(data.table)
library(dplyr)
library(tidyr)


# Load Data ---------------------------------------------------------------
path <- "../input"

aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp <- fread(file.path(path, "order_products__prior.csv"))
ordert <- fread(file.path(path, "order_products__train.csv"))
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"))

# Counting data-----------------------------------------------------------
# train_users <- data.table(user_id = orders[orders$eval_set=='train']$user_id)
# test_users <- data.table(user_id = orders[orders$eval_set=='test']$user_id)
# train_users_orders <- merge(orders, train_users, by='user_id')
# test_users_orders <- merge(orders, test_users, by='user_id')
# train_users_positions <- merge(train_users_orders, orderp, by='order_id')
# test_users_positions <- merge(test_users_orders, orderp, by='order_id')
# train2_users_positions <- merge(train_users_orders, ordert, by='order_id')
# train_products <- unique(train_users_positions$product_id)
# train2_products <- unique(train2_users_positions$product_id)
# test_products <- unique(test_users_positions$product_id)
# no_products <- data.table(product_id = setdiff(train2_products,intersect(train_products, train2_products)))
# dim(train2_users_positions %>% group_by(user_id, product_id) %>% summarise())
# df <- merge(train_users_positions[,c('user_id', 'product_id')], 
#             train2_users_positions[,c('user_id', 'product_id')],
#             by=c('user_id', 'product_id'))
# dim(df %>% group_by(user_id, product_id) %>% summarise())



#handle None function

add_none <- function(df){
  ids <- df %>% group_by(order_id) %>% summarise(reord_count=sum(reordered))
  add <- data.table(order_id = ids[ids$reord_count == 0,]$order_id)
  add <- cbind(add, product_id = rep("None",nrow(add)))
  add <- cbind(add, add_to_cart_order = rep(1,nrow(add)))
  add <- cbind(add, reordered = rep(1,nrow(add)))
  return(rbind(add,df))
}

orderp <- add_none(orderp)
ordert <- add_none(ordert)

# Reshape data ------------------------------------------------------------
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)

products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)
rm(aisles, departments)

ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

orders_products <- orders %>% inner_join(orderp, by = "order_id")

rm(orderp)
gc()


# Products ----------------------------------------------------------------
prd <- orders_products %>%
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(product_time = row_number()) %>%
  ungroup() %>%
  group_by(product_id) %>%
  summarise(
    prod_orders = n(),
    prod_reorders = sum(reordered),
    prod_first_orders = sum(product_time == 1),
    prod_second_orders = sum(product_time == 2),
    prod_mean_add_to_cart_order = mean(add_to_cart_order),
    prod_days_since_prior = mean(days_since_prior_order, na.rm = T)
  )

prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders

#additional features---------------------------------------------------

prd$prod_penetration <- prd$prod_first_orders / max(orders_products$user_id)
prd$prod_double_pntr <- prd$prod_second_orders / max(orders_products$user_id)

#---------------------------------------------------

prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders)

rm(products)
gc()

# Users -------------------------------------------------------------------
users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T)
  )

#additional features---------------------------------------------------

users$weekly_orders <- users$user_orders*7/users$user_period
        
#---------------------------------------------------


us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id)
  )

users <- users %>% inner_join(us)
users$user_average_basket <- users$user_total_products / users$user_orders
users$user_product_diversity <- users$user_distinct_products / users$user_orders


us <- orders %>%
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set,
         time_since_last_order = days_since_prior_order)

users <- users %>% inner_join(us)

users$user_order_recency <- users$time_since_last_order / users$user_mean_days_since_prior

rm(us)
gc()



# Database ----------------------------------------------------------------
data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),
    up_first_order = min(order_number),
    up_last_order = max(order_number),
    up_average_cart_position = mean(add_to_cart_order))



# Preffered DoW, HoD ----------------------------------------------------------------

# dt1=setDT(orders_products)[, .N, by=.(user_id, product_id, order_dow)][, order_dow[which.max(N)],by=.(user_id, product_id)]
# names(dt1)[names(dt1) == "V1"] = "user_preferred_dow"
# 
# dt2=setDT(orders_products)[, .N, by=.(user_id, product_id, order_hour_of_day)][, order_hour_of_day[which.max(N)],by=.(user_id, product_id)]
# names(dt2)[names(dt2) == "V1"] = "user_preferred_hod"
# 
# data <- data %>% inner_join(dt1, by = c("user_id", "product_id")) %>% inner_join(dt2, by = c("user_id", "product_id"))
#rm(dt1, dt2)
rm(orders_products, orders)

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)

data <- data %>%
  left_join(ordert %>% select(user_id, product_id, reordered),
            by = c("user_id", "product_id"))
rm(ordert)

rm(prd, users)
gc()
#saveRDS(data,file.path(path, "data.RDS"))


# Feature tuning - remove features to study influence
data <- readRDS(file.path(path, "data.RDS"))
#feat <- read.csv(file.path(path, "features.csv"))
# rem_feat = c( 'user_order_recency', 
#              'prod_mean_add_to_cart_order', 
#              'prod_days_since_prior',
#              'user_product_diversity',
#              'prod_penetration',
#              'prod_double_penetration',
#              'weekly_orders')
#data <- data[!colnames(data) %in% feat[10:27]]

# Train / Test datasets ---------------------------------------------------

train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$user_id <- NULL
train$reordered[is.na(train$reordered)] <- 0

test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$reordered <- NULL



rm(data)
gc()


# Model -------------------------------------------------------------------

library(xgboost)
require(Ckmeans.1d.dp)
require(MLmetrics)

params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss",
  "eta"                 = 0.1,
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.77,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)


## 20% of the sample size
smp_size <- floor(0.4 * nrow(train))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

subtrain <- train[train_ind,]
valid <- train[-train_ind,] %>% sample_frac(0.3)
rm(train)

X <- xgb.DMatrix(as.matrix(subtrain %>% select(-reordered, -order_id, -product_id)), label = subtrain$reordered)
#model <- xgboost(data = X, params = params, nrounds = 90)
model <- xgb.load('xgboost.model')

importance <- xgb.importance(colnames(X), model = model)

#feat<-importance$Feature

xgb.ggplot.importance(importance)
gc()


## training model for threshold prediction


subtrain$prob<- predict(model, X)
rm(X)


source('include.R')
metrics <- by(subtrain[,c('reordered','prob')], subtrain[,'order_id'], calc_metrics)
metrics <- subtrain %>%
        group_by(order_id) %>%
        mutate(thresh = calc_metrics(reordered, prob))

saveRDS(metrics,file.path(path, "metrics.RDS"))


head(subtrain)




##training model2 for threshold prediction

X <- xgb.DMatrix(as.matrix(ord_train %>% select(-thresh, -order_id)), label = ord_train$thresh)

model2 <- xgboost(data = X, params = params, nrounds = 90)
#model2 <- xgb.load('xgboost.model2')


## validation
my_validation <- function (model, valid, threshold){
        
        V <- xgb.DMatrix(as.matrix(valid %>% select(-reordered, - order_id, -product_id)))
        valid$pred <- (predict(model, V) > threshold) * 1
        valid$percent <- predict(model, V)
        valid$product_id[is.na(valid$product_id) | valid$product_id=='None' | valid$product_id==0] <- 'None'
        
        pred <- valid %>%
                filter(pred == 1) %>%
                group_by(order_id) %>%
                arrange(desc(percent)) %>%
                summarise(
                        pred = paste(product_id, collapse = " "),
                        n_pred = n()*(1-sum(product_id=='None'))
                )
        #pred[pred$n_pred==0,]$pred<-'None'
        
        gt <- valid %>%
                filter(reordered == 1) %>%
                group_by(order_id) %>%
                summarise(
                        gt = paste(product_id, collapse = " "),
                        n_gt = n()*(1-sum(product_id=='None'))
                )
        #gt[gt$n_gt==0,]$gt<-'None'
        
        intsec <- valid %>%
                filter(pred == 1 & reordered == 1) %>%
                group_by(order_id) %>%
                summarise(
                        intsec = paste(product_id, collapse = " "),
                        n_int = n()*(1-sum(product_id=='None'))
                )
        #intsec[intsec$n_int==0,]$intsec<-'None'
        
        df <- merge(x = pred, y = gt, by = "order_id", all = TRUE)
        df <- merge(x = df, y = intsec, by = "order_id", all = TRUE)
        rm(pred, gt, intsec)
        
        write.csv(df, 'df.csv')
        
        # Compute F1
        df[is.na(df)] <- 0
        precision <- sum(df$n_int) / sum(df$n_pred)
        recall <- sum(df$n_int) / sum(df$n_gt)
        f1 <- 2*precision*recall/(precision+recall)
        output <- list(precision, recall, f1)
        return(output)
}

print(my_validation(model, valid, 0.10))
print(my_validation(model, valid, 0.19))

# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
test$reordered <- predict(model, X)

test$reordered <- (test$reordered > 0.20) * 1

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
    #n_prod = n()-sum(product_id=='None')
  )

#submission[submission$n_prod==0 | is.na(submission$n_prod),]$products<-'None'

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
#submission$n_prod<-NULL
write.csv(submission, file = "submit.csv", row.names = F)

