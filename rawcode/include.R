calc_metrics <- function(df){
        df <- data.table(cbind(reordered=df$reordered, prob=df$prob))
        df <- df[order(-prob),]
        df <- cbind(df, gt=rep(sum(df[,'reordered']==1), nrow(df)))
        df <- cbind(df, intc=cumsum(df[,'reordered']))
        df <- cbind(df, pred=seq(1,nrow(df)))
        colnames(df)<-c('reordered', 'prob', 'gt','intc','pred')
        df <- df %>%
                mutate(Ps=intc/pred, Rc=intc/gt) %>%
                mutate(f1=2*Ps*Rc/(Ps+Rc))
        df[is.na(df[,'Rc']),'Rc']<-1
        df[is.na(df[,'f1']),'f1']<-0
        return (df)
}
##test_block
# for(i in subtrain$order_id){
#         df1 <- subtrain[subtrain$order_id==i,]
#         ord_train_y <- df1 %>%
#                 group_by(order_id) %>%
#                 summarise(thresh = opt_thres(reordered, prob))
#         print(ord_train_y)
# }
# ## summarising features
# ord_train <- subtrain %>%
#         group_by(order_id, 
#                  user_orders, user_period, user_mean_days_since_prior, weekly_orders, user_total_products, 
#                  user_reorder_ratio, user_distinct_products, user_average_basket, user_product_diversity,
#                  time_since_last_order, user_order_recency) %>%
#         summarise_at(vars(matches("up_"), matches("prod_")), funs(mean)) 
# ## Calculating optimal threshold for each order - it can take a while
# ord_train_y <- subtrain[0:1000,] %>%
#         group_by(order_id) %>%
#         summarise(thresh = opt_thres(reordered, prob))
# ## optimisation function for threshold

# ##Bad bad order - 3338932
# ord_train <- merge(ord_train, ord_train_y)
