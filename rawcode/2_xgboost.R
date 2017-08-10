
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

library(xgboost)
require(Ckmeans.1d.dp)
require(MLmetrics)


source('include.R')

## 20% of the sample size
smp_size <- floor(0.4 * nrow(train))

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

X <- xgb.DMatrix(as.matrix(subtrain %>% select(-reordered, -order_id, -product_id, -aisle, -department)), label = subtrain$reordered)
Y <- xgb.DMatrix(as.matrix(valid %>% select(-reordered, -order_id, -product_id, -aisle, -department)), label = valid$reordered)
watchlist <- list(train=X, test=Y)

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


#model_cv <-xgb.cv(data = X, params = params, nrounds = 90, nfold=5, early_stopping_rounds = 10) 
model <- xgb.train(data = X, params = params, nrounds = 150, watchlist = watchlist, early_stopping_rounds = 10)
#model <- xgb.load('xgb1.model')
xgb.save(model,'xgb1.model')

importance <- xgb.importance(colnames(X), model = model)

#feat<-importance$Feature

xgb.ggplot.importance(importance)
rm(X, Y, subtrain)
gc()

#train-logloss last [250]	train-logloss:0.238413	test-logloss:0.244279 

# Validation, initial (threshold=0.21, Pc=0.389, Rc=0.51, f1=0.4418), last=0.4454979 (250) 0.4440433
print(my_validation(model, valid, 0.21))

source('include.R')
source('f1.R')
source('f1cpp.R')

# Apply models -------------------------------------------------------------
test <- test %>% inner_join(prod_emb)
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id, -aisle, -department)))
test$prob <- predict(model, X)

# Apply threshold

result <- apply_threshold(test)

#print(test, n=200)
#test$reordered <- (test$reordered > 0.20) * 1

submission <- result %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

# submission <- test %>%
#         select(order_id, product_id, prob) %>%
#         group_by(order_id) %>%
#         summarise(products = exact_F1_max_none(prob, product_id))



missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
submission[submission$products=='' | is.na(submission$products),]<-'None'
write.csv(submission, file = "submit.csv", row.names = F)

