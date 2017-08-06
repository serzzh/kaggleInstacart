
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
data <- readRDS(file.path(path, "data.RDS"))

# Feature tuning - remove features to study influence
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

library(lightgbm)
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

params <- list(
        objective           = "binary",
        metric              = "binary_logloss",
        learning_rate       = 0.1,
        verbose             = 2,
        #nfold               = 5,
        min_data            =5
        #nrounds             =90
        #"eta"                 = 0.1,
        #"max_depth"           = 8
        # "min_child_weight"    = 10,
        # "gamma"               = 0.70,
        # "subsample"           = 0.77,
        # "colsample_bytree"    = 0.95,
        # "alpha"               = 2e-05,
        # "lambda"              = 10,
        # "watchlist"           = watchlist
)

X <- lgb.Dataset(as.matrix(subtrain %>% select(-reordered, -order_id, -product_id, -aisle, -department)), label = subtrain$reordered)
#rm(subtrain)
#model <- lgb.train(params, X, 90)

subtrain$prob <- predict(model,X)

#model <- xgb.load('xgboost.model')
lgb.save(model,'lgb.model')

importance <- lgb.importance(model)

#feat<-importance$Feature

#xgb.ggplot.importance(importance)
rm(X, Y, subtrain)
gc()

#train-logloss last 0.2434


## Threshold prediction-----------------------------------------------


#subtrain$prob <- predict(model, X)
#rm(X)



## adding metrics to training dataset
##subtrain<-add_metrics(subtrain)

# Validation, initial (threshold=0.21, Pc=0.389, Rc=0.51, f1=0.4418), last=0.4424329
print(my_validation(model, valid, 0.21, 'lgbm'))

source('include.R')
source('f1.R')
# Apply models -------------------------------------------------------------
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
    #n_prod = n()-sum(product_id=='None')
  )

#submission[submission$n_prod==0 | is.na(submission$n_prod),]$products<-'None'

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
submission$n_prod<-NULL
submission[submission$products=='' | is.na(submission$products),]<-'None'
write.csv(submission, file = "submit.csv", row.names = F)

