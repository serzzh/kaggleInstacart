
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

products <- rbind(products, data.table(product_id='None', product_name='None', aisle='None',department='None'))

# AisleVar = factor(products$aisle)
# DepVar = factor(products$department) 
# dumm = as.data.frame(model.matrix(~DepVar)[,-1])
# products = cbind(products, dumm) %>% select(-aisle,-department)


ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

products$product_id<-as.character(products$product_id)

orders <- orders %>%
        mutate(wdays=ifelse(order_dow>=2,1,0),
               wends=ifelse(order_dow<=1,1,0),
               night=ifelse((order_hour_of_day>=20 | order_hour_of_day<=6),1,0),
               day=ifelse(night!=1,1,0))


orders_products <- orders %>% 
        inner_join(orderp, by = "order_id") 

rm(orderp)
gc()

orders_products <- orders_products %>%
        left_join(products, by = "product_id")




# Products, departments, aisles ----------------------------------------------------------------
prd <- orders_products %>%
        arrange(user_id, order_number, product_id) %>%
        group_by(user_id, product_id) %>%
        mutate(product_time = row_number()) %>%
        ungroup() %>%
        group_by(product_id, aisle, department) %>%
        summarise(
                prod_orders = n(),
                prod_reorders = sum(reordered),
                #prod_wdays = sum(wdays==1),
                #prod_wends = sum(wends==1),
                #prod_day = sum(day==1),
                #prod_night = sum(night==1),
                prod_first_orders = sum(product_time == 1),
                prod_second_orders = sum(product_time == 2),
                prod_mean_add_to_cart_order = mean(add_to_cart_order),
                prod_days_since_prior = mean(days_since_prior_order, na.rm = T)) %>%
        ungroup()

prd_aisle <- prd %>%
        group_by(aisle) %>%
        summarise(
                aisle_orders = sum(prod_orders),
                aisle_reorders = sum(prod_reorders),
                aisle_first_orders = sum(prod_first_orders),
                aisle_second_orders = sum(prod_second_orders),
                aisle_days_since_prior = mean(prod_days_since_prior, na.rm = T)) %>%                
        ungroup()

prd_dep <- prd %>%
        group_by(department) %>%
        summarise(
                dep_orders = sum(prod_orders),
                dep_reorders = sum(prod_reorders)
                #dep_first_orders = sum(prod_first_orders),
                #dep_second_orders = sum(prod_second_orders)
                #dep_days_since_prior = mean(prod_days_since_prior, na.rm = T)
                ) %>%               
        ungroup()
        
prd <- prd %>% inner_join(prd_aisle) %>% inner_join(prd_dep)        

rm(prd_aisle,prd_dep)

#prd$prod_wend_ratio <- prd$prod_wends*2.5/prd$prod_wdays
#prd$prod_night_ratio <- prd$prod_night*5/prd$prod_day
prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
#prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders

prd$aisle_reorder_probability <- prd$aisle_second_orders / prd$aisle_first_orders
prd$aisle_reorder_times <- 1 + prd$aisle_reorders / prd$aisle_first_orders

#prd$dep_reorder_probability <- prd$dep_second_orders / prd$dep_first_orders
#prd$dep_reorder_times <- 1 + prd$dep_reorders / prd$dep_first_orders


#additional features---------------------------------------------------

prd$prod_penetration <- prd$prod_first_orders / max(orders_products$user_id)
prd$prod_double_pntr <- prd$prod_second_orders / max(orders_products$user_id)

# prd$aisle_penetration <- prd$aisle_first_orders / max(orders_products$user_id)
# prd$aisle_double_pntr <- prd$aisle_second_orders / max(orders_products$user_id)
# 
# prd$dep_penetration <- prd$dep_first_orders / max(orders_products$user_id)
# prd$dep_double_pntr <- prd$dep_second_orders / max(orders_products$user_id)

#aisles, departments-----------------------------------------------


#---------------------------------------------------

prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders, -aisle_orders, -dep_orders,
                      -dep_reorders, -aisle_reorders, -aisle_first_orders, -aisle_second_orders)

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
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1)
    #user_distinct_products = n_distinct(product_id)
  )

users <- users %>% inner_join(us)
users$user_average_basket <- users$user_total_products / users$user_orders
#users$user_product_diversity <- users$user_distinct_products / users$user_orders


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
                #up_wdays = sum(wdays==1),
                #up_wends = sum(wends==1),
                up_day = sum(day==1),
                #up_night = sum(night==1),
                up_first_order = min(order_number),
                up_last_order = max(order_number),
                up_average_cart_position = mean(add_to_cart_order))

ud <- orders_products %>%
        group_by(user_id, department) %>%
        summarise(
                ud_orders = length(unique(order_id)),
                ud_first_order = min(order_number),
                ud_last_order = max(order_number)
        )

ua <- orders_products %>%
        group_by(user_id, aisle) %>%
        summarise(
                ua_orders = length(unique(order_id)),
                ua_first_order = min(order_number),
                ua_last_order = max(order_number)
        )
        

rm(orders_products, orders)

data <- data %>%
        inner_join(prd, by = "product_id") %>%
        inner_join(users, by = "user_id") %>%
        inner_join(ud, by = c("user_id","department")) %>%
        inner_join(ua, by = c("user_id", "aisle"))
        
rm(ud,ua)

data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)
data$ua_order_rate <- data$ua_orders / data$user_orders
data$ua_orders_since_last_order <- data$user_orders - data$ua_last_order
data$ua_order_rate_since_first_order <- data$ua_orders / (data$user_orders - data$ua_first_order + 1)
data$ud_order_rate <- data$ud_orders / data$user_orders
data$ud_order_rate_since_first_order <- data$ud_orders / (data$user_orders - data$ud_first_order + 1)

data <- data %>%
        select(-ua_first_order, -ua_last_order, -ud_first_order, -ud_last_order, -ud_orders, -user_orders) %>%
        left_join(ordert %>% select(user_id, product_id, reordered), by = c("user_id", "product_id"))
rm(ordert)

rm(prd, users)
gc()

## One hot encoding for factors


saveRDS(data,file.path(path, "data.RDS"))
