# train as test


library(data.table)
library(dplyr)
library(tidyr)


# Load Data ---------------------------------------------------------------
path <- "../input"

aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp <- fread(file.path(path, "order_products__prior.csv"))
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"))


# Split users train test

users_test <- unique(orders[orders$eval_set=='test',]$user_id)
orders <- orders[orders$user_id %in% users_test,]
rm(users_test)
orders_test <- unique(orders$order_id)
orderp <- orderp[orderp$order_id %in% orders_test,]
rm(orders_test)

orders_train <- orders %>%
        filter(eval_set=='prior') %>%
        group_by(user_id) %>%
        arrange(desc(order_number)) %>%
        filter(1:n()==1) %>%
        ungroup()

orders[orders$order_id %in% orders_train$order_id,]$eval_set <- 'train'
ordert <- orderp[orderp$order_id %in% orders_train$order_id,]
orderp <- orderp[!(orderp$order_id %in% orders_train$order_id),]

rm(orders_train)
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
        select(-aisle_id, -department_id) %>%
        mutate(org = 1*grepl('organic',product_name,  ignore.case = TRUE))

rm(aisles, departments)

products <- rbind(products, data.table(product_id='None', product_name='None', aisle='None',department='None',org=0))


ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

products$product_id<-as.character(products$product_id)

# orders <- orders %>%
#         mutate(wdays=ifelse(order_dow>=2,1,0),
#                wends=ifelse(order_dow<=1,1,0),
#                night=ifelse((order_hour_of_day>=20 | order_hour_of_day<=6),1,0),
#                day=ifelse(night!=1,1,0))


orders <- data.table(orders)[order(user_id, order_number),]


orders <- orders %>%
        group_by(user_id) %>%
        arrange(order_number) %>%
        mutate(days_since_prior_order = ifelse(order_number==1, 0, days_since_prior_order)) %>%
        mutate(days_since_prior_order = ifelse(is.na(days_since_prior_order), mean(days_since_prior_order, na.rm=TRUE), 
                                               days_since_prior_order)) %>%
        mutate(days_since_first_order = cumsum(days_since_prior_order)) %>%
        ungroup()



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
        group_by(product_id, aisle, department, org) %>%
        summarise(
                prod_orders = n(),
                prod_reorders = sum(reordered),
                prod_first_orders = sum(product_time == 1),
                prod_second_orders = sum(product_time == 2),
                prod_third_orders = sum(product_time == 3),
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
                aisle_third_orders = sum(prod_third_orders),
                aisle_days_since_prior = mean(prod_days_since_prior, na.rm = T)) %>%                
        ungroup()

prd_dep <- prd %>%
        group_by(department) %>%
        summarise(
                dep_orders = sum(prod_orders),
                dep_reorders = sum(prod_reorders),
                dep_first_orders = sum(prod_first_orders),
                dep_second_orders = sum(prod_second_orders),
                dep_third_orders = sum(prod_third_orders),
                dep_days_since_prior = mean(prod_days_since_prior, na.rm = T)) %>%              
        ungroup()

prd <- prd %>% inner_join(prd_aisle) %>% inner_join(prd_dep)        

rm(prd_aisle,prd_dep)

prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_prob2 <- prd$prod_third_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders

prd$aisle_reorder_probability <- prd$aisle_second_orders / prd$aisle_first_orders
prd$aisle_reorder_prob2 <- prd$aisle_third_orders / prd$aisle_first_orders
prd$aisle_reorder_times <- 1 + prd$aisle_reorders / prd$aisle_first_orders

prd$dep_reorder_probability <- prd$dep_second_orders / prd$dep_first_orders
prd$dep_reorder_prob2 <- prd$dep_third_orders / prd$dep_first_orders
prd$dep_reorder_times <- 1 + prd$dep_reorders / prd$dep_first_orders


#additional features---------------------------------------------------

prd$prod_penetration <- prd$prod_first_orders / max(orders_products$user_id)
prd$prod_double_pntr <- prd$prod_second_orders / max(orders_products$user_id)


#---------------------------------------------------

prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders, -prod_third_orders,-aisle_orders, 
                      -dep_orders, -dep_reorders, -aisle_reorders, -aisle_first_orders, -aisle_second_orders, 
                      -aisle_third_orders, -dep_third_orders, -org)

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

users$user_weekly_orders <- users$user_orders*7/users$user_period

#---------------------------------------------------


us <- orders_products %>%
        group_by(user_id) %>%
        summarise(
                user_total_products = n(),
                user_organic = sum(org==1),
                user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
                user_distinct_products = n_distinct(product_id)
        )

users <- users %>% inner_join(us)
users$user_average_basket <- users$user_total_products / users$user_orders
users$user_org_ratio <- users$user_organic/users$user_total_products


us <- orders %>%
        filter(eval_set != "prior") %>%
        select(user_id, order_id, eval_set,
               time_since_last_order = days_since_prior_order)

users <- users %>% inner_join(us)

users$user_order_recency <- users$time_since_last_order / users$user_mean_days_since_prior

users <- users %>% select(-user_total_products, -user_organic) 

rm(us)
gc()



# Database ----------------------------------------------------------------
data <- orders_products %>%
        group_by(user_id, product_id) %>% 
        arrange(order_number) %>%
        summarise(
                up_orders = n(),
                up_first_order = min(order_number),
                up_last_order = max(order_number),
                x = lag(days_since_first_order),
                x = ifelse(is.na(x),0,x),
                up_date_of_last_order = last(days_since_first_order),
                up_date_of_prelast_order = last(x),
                #up_mean_days_since_prior_order = mean(days_since_first_order - x, rm.na=TRUE),
                # up_wdays = sum(wdays==1),
                # up_wends = sum(wends==1),
                # up_day = sum(day==1),
                # up_night = sum(night==1),
                up_average_cart_position = mean(add_to_cart_order))


ud <- orders_products %>%
        group_by(user_id, department) %>%
        summarise(
                ud_orders = length(unique(order_id)),
                ud_first_order = min(order_number),
                ud_last_order = max(order_number),
                x = lag(days_since_first_order),
                x = ifelse(is.na(x),0,x),
                ud_date_of_last_order = last(days_since_first_order),
                ud_date_of_prelast_order = last(x)
        )

ua <- orders_products %>%
        group_by(user_id, aisle) %>%
        summarise(
                ua_orders = length(unique(order_id)),
                ua_first_order = min(order_number),
                ua_last_order = max(order_number),
                x = lag(days_since_first_order),
                x = ifelse(is.na(x),0,x),
                ua_date_of_last_order = last(days_since_first_order),
                ua_date_of_prelast_order = last(x)
        )

# uo <- orders_products %>%
#         group_by(user_id, org) %>%
#         summarise(
#                 uo_orders = length(unique(order_id)),
#                 uo_first_order = min(order_number),
#                 uo_last_order = max(order_number)
#         )

rm(orders_products)

data <- data %>%
        inner_join(prd, by = "product_id") %>%
        inner_join(users, by = "user_id") %>%
        inner_join(ud, by = c("user_id","department")) %>%
        #inner_join(uo, by = c("user_id", "org")) %>%
        inner_join(ua, by = c("user_id", "aisle"))

rm(ud,ua)

data$up_order_rate <- data$up_orders / data$user_orders
data$up_reorder_ratio <- (data$up_orders-1) / data$up_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)
data$up_days_since_last_order <- data$up_date_of_last_order - data$up_date_of_prelast_order
data$up_rate_since_first_order <- data$up_orders / data$up_date_of_last_order
data <- data %>%
        select(-up_date_of_last_order, -up_date_of_prelast_order, -x, -x.x, -x.y)

data$ua_order_rate <- data$ua_orders / data$user_orders
data$ua_reorder_ratio <- (data$ua_orders-1) / data$ua_orders
data$ua_orders_since_last_order <- data$user_orders - data$ua_last_order
data$ua_order_rate_since_first_order <- data$ua_orders / (data$user_orders - data$ua_first_order + 1)
data$ua_days_since_last_order <- data$ua_date_of_last_order - data$ua_date_of_prelast_order
data$ua_rate_since_first_order <- data$ua_orders / data$ua_date_of_last_order
data <- data %>%
        select(-ua_date_of_last_order, -ua_date_of_prelast_order)

data$ud_order_rate <- data$ud_orders / data$user_orders
data$ud_reorder_ratio <- (data$ud_orders-1) / data$ud_orders
data$ud_orders_since_last_order <- data$user_orders - data$ud_last_order
data$ud_order_rate_since_first_order <- data$ud_orders / (data$user_orders - data$ud_first_order + 1)
data$ud_days_since_last_order <- data$ud_date_of_last_order - data$ud_date_of_prelast_order
data$ud_rate_since_first_order <- data$ud_orders / data$ud_date_of_last_order
data <- data %>%
        select(-ud_date_of_last_order, -ud_date_of_prelast_order)


ordert["eval_set"] <- "train"
data$eval_set <- as.factor(data$eval_set)
ordert$eval_set <- as.factor(ordert$eval_set)
levels(ordert$eval_set) <- levels(data$eval_set)

data <- data %>%
        select(-ua_first_order, -ua_last_order, -ud_first_order, -ud_last_order, -ud_orders, -ua_orders, -user_orders) %>%
        left_join(ordert %>% select(user_id, product_id, reordered, eval_set), by = c("user_id", "product_id", "eval_set"))
rm(ordert)

rm(prd, users)
gc()

## Add wdays, wends, day, night

# data <- data %>%
#         left_join(orders %>% 
#                           filter(eval_set=='train' | eval_set=='test')%>%
#                           select(user_id, wdays, wends, day, night), by = c("user_id"))
# 


rm(orders)
saveRDS(data,file.path(path, "data_test.RDS"))

