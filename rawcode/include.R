
## Adding precision, recall, F1 to training dataset

library(data.table)
library(dplyr)
library(tidyr)
library(rPython)

add_metrics <- function(df){
        df <- data.table(subtrain)[order(order_id,-prob),]
        df <- df %>%
                group_by(order_id) %>%
                mutate(gt=sum(reordered),
                       intc=cumsum(reordered),
                       pred=1:n()) %>%
                mutate(Ps=intc/pred, Rc=intc/gt) %>%
                mutate(f1=2*Ps*Rc/(Ps+Rc)) %>%
                ungroup()
        df[is.na(df[,'Rc']),'Rc']<-1
        df[is.na(df[,'f1']),'f1']<-0
        return(df)
}

##df<-test[test$order_id=='329954',]
## Apply threshold
apply_threshold <- function(df){
        df <- data.table(df)[order(order_id,-prob),]
        max_ns <- df %>%
                select(order_id,prob) %>%
                group_by(order_id) %>%
                summarise(max_n = unlist(max_expectations(prob)[1]))
        df <- df %>%
                inner_join(max_ns, by='order_id') %>%
                select(order_id, product_id, prob, max_n) %>%
                group_by(order_id) %>%
                mutate(count = row_number(),
                       reordered = (count<=max_n)*1) %>%
                ungroup()
        return(df)
}



        
## Validation of both models
my_validation <- function (model, valid, threshold, method = 'lgbm'){
        
        V <- as.matrix(valid %>% select(-reordered, - order_id, -product_id, -aisle, -department))        
        #V <- ifelse((method == 'xgb'), xgb.DMatrix(V), V)
        
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

        
        gt <- valid %>%
                filter(reordered == 1) %>%
                group_by(order_id) %>%
                summarise(
                        gt = paste(product_id, collapse = " "),
                        n_gt = n()*(1-sum(product_id=='None'))
                )

        
        intsec <- valid %>%
                filter(pred == 1 & reordered == 1) %>%
                group_by(order_id) %>%
                summarise(
                        intsec = paste(product_id, collapse = " "),
                        n_int = n()*(1-sum(product_id=='None'))
                )

        
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