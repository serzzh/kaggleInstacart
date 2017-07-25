
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

# Feature tuning - remove features to study influence
data <- readRDS(file.path(path, "data.RDS"))
# rem_feat = c( 'user_order_recency', 
#              'prod_mean_add_to_cart_order', 
#              'prod_days_since_prior',
#              'user_product_diversity',
#              'prod_penetration',
#              'prod_double_penetration',
#              'weekly_orders')
# data <- data[!colnames(data) %in% rem_feat]

# Train / Test datasets ---------------------------------------------------

train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$user_id <- NULL
train$reordered[is.na(train$reordered)] <- 0

test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$reordered <- NULL

#saveRDS(data,file.path(path, "data.RDS"))

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
smp_size <- floor(0.2 * nrow(train))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

subtrain <- train[train_ind,]
valid <- train[-train_ind,] %>% sample_frac(0.1)
rm(train)

X <- xgb.DMatrix(as.matrix(subtrain %>% select(-reordered, -order_id, -product_id)), label = subtrain$reordered)
model <- xgboost(data = X, params = params, nrounds = 90)
#model <- xgb.load('xgboost.model')

importance <- xgb.importance(colnames(X), model = model)

xgb.ggplot.importance(importance)
rm(X, importance, subtrain)
gc()

V <- xgb.DMatrix(as.matrix(valid %>% select(-reordered, - order_id, -product_id)))

valid$pred <- (predict(model, V) > 0.22) * 1

# Validation script here -------------------------------------------------------------
# Formatting data to output shape

pred <- valid %>%
        filter(pred == 1) %>%
        group_by(order_id) %>%
        summarise(
                pred = paste(product_id, collapse = " "),
                n_pred = n()
        )

gt <- valid %>%
        filter(reordered == 1) %>%
        group_by(order_id) %>%
        summarise(
                gt = paste(product_id, collapse = " "),
                n_gt = n()
        )

intsec <- valid %>%
        filter(pred == 1 & reordered == 1) %>%
        group_by(order_id) %>%
        summarise(
                intsec = paste(product_id, collapse = " "),
                n_int = n()
        )

df <- merge(x = pred, y = gt, by = "order_id", all = TRUE)
df <- merge(x = df, y = intsec, by = "order_id", all = TRUE)
rm(pred, gt, intsec)

# Compute F1
df[is.na(df)] <- 0
precision <- sum(df$n_int) / sum(df$n_pred)
recall <- sum(df$n_int) / sum(df$n_gt)
f1 <- 2*precision*recall/(precision+recall)
print(precision)
print(recall)
print(f1)

rm(V,valid)

# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
test$reordered <- predict(model, X)

test$reordered <- (test$reordered > 0.22) * 1

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = "submit.csv", row.names = F)

