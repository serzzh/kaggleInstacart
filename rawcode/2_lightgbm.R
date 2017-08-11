
###########################################################################################################
#
# Kaggle Instacart competition
# Sergey Rakhmatullin, August 2017
# based on Fabien Vavrand script, June 2017
# Xgboost classifier, score 0.3974 on LB
# Products selection is based on product by product binary classification, with a optimized threshold
# F1 maximization
###########################################################################################################

library(data.table)
library(dplyr)
library(tidyr)

# Load Data ---------------------------------------------------------------
path <- "../input"
data <- readRDS(file.path(path, "data.RDS"))

data <- data %>% select(-x.x, -x.y, -dep_third_orders, -org, -user_weekly_orders, -dep_reorder_prob2, 
                        -ud_orders_since_last_order, -dep_reorder_times, -dep_second_orders, -dep_days_since_prior,
                        -ud_reorder_ratio, -aisle_reorder_prob2, -dep_first_orders)

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

library(lightgbm)
require(Ckmeans.1d.dp)
require(MLmetrics)


source('include.R')

## 20% of the sample size
smp_size <- floor(0.6 * nrow(train))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

subtrain <- train[train_ind,]
valid <- train[-train_ind,] %>% sample_frac(0.3)
rm(train)

# Load Word2Vec data ------------------------------------------------------
prod_emb <- read.csv(file.path(path, "product_embeddings.csv"))
prod_emb <- prod_emb %>% select(-X,-product_name,-aisle_id,-department_id)
prod_emb$product_id <- as.character(prod_emb$product_id)
subtrain <- subtrain %>% inner_join(prod_emb)
valid <- valid %>% inner_join(prod_emb)

X <- lgb.Dataset(as.matrix(subtrain %>% select(-reordered, -order_id, -product_id, -aisle, -department)),
                 label = subtrain$reordered, free_raw_data = FALSE)
# Y <- lgb.Dataset(as.matrix(valid %>% select(-reordered, -order_id, -product_id, -aisle, -department)), 
#                  label = valid$reordered, free_raw_data = FALSE )
# valids <- list(train = X, test = Y)
# 
model <- lightgbm(data = X, nrounds = 350, metric = "binary_logloss", learning_rate = 0.05,
                  verbosity = 2, min_data = 5, device='gpu', early_stopping_rounds = 10, #valids = valids,
                  objective = "binary")

#lgb.save(model,'lgb.model')
#model <- lgb.load('lgb.model')


#importance <- lgb.importance(model)
#xgb.ggplot.importance(importance)
# subtrain$prob <- predict(model, as.matrix(subtrain %>% select(-reordered, -order_id, -product_id, -aisle, -department)))
# rm(X, Y, subtrain)
# gc()

#train-logloss last 0.2434


## Threshold prediction-----------------------------------------------


#subtrain$prob <- predict(model, X)
#rm(X)



## adding metrics to training dataset
##subtrain<-add_metrics(subtrain)

# Validation, initial (threshold=0.21, Pc=0.389, Rc=0.51, f1=0.4418), last=0.4457864 (90) , 0.4468467 (350)
print(my_validation(model, valid, 0.21, method = 'lgbm'))

source('include.R')
source('f1.R')
# Apply models -------------------------------------------------------------
test$prob <- predict(model, as.matrix(test %>% select(-order_id, -product_id, -aisle, -department)))


# Exchange data
out_test <- test %>% select(order_id, prob, product_id)
colnames(out_test) <- c("order_id", "prediction", "product_id")
write.csv(out_test, file.path(path, "lgbm_my.csv"))
# ex_test <- read.csv(file.path(path, "lgbm_ew.csv"))
# colnames(ex_test)<-c('X', 'order_id', 'prob', 'product_id')


# Apply threshold

result <- apply_threshold(test)



submission <- result %>%
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
submission$n_prod<-NULL
submission[submission$products=='' | is.na(submission$products),]<-'None'
write.csv(submission, file = "submit.csv", row.names = F)

