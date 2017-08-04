get_expectations <- function(P, pNone=NULL){
        expectations = list()
        P <- as.vector(as.matrix(P))
        n <- length(P)
        DP_C = matrix(0L, nrow = n+2, ncol = n+1) 
        pNone <- ifelse(is.null(pNone), prod(1.0 - P), pNone)
        DP_C[1,1] <- 1.0
        for (j in 2:(n)) DP_C[1,j] <- (1.0 - P[j-1]) * DP_C[1, j-1]

        for (i in 2:(n+1)){
                DP_C[i,i] <- P[i-1] * DP_C[i-1, i-1]
                for (j in i:(n+1)) DP_C[i,j] <- P[j-1] * DP_C[i-1, j-1] + (1.0 - P[j-1]) * DP_C[i, j-1]
        } 
        
        DP_S = matrix(0L, nrow = 2*n+1, ncol = 1) 
        DP_SNone = matrix(0L, nrow = 2*n+1, ncol = 1)
        
        for (i in 2:(2*n+1)){
                DP_S[i] = 1. / (1. * (i-1))
                DP_SNone[i] = 1. / (1. * (i-1) + 1)
                
        }

        for (k in (n+1):1){
                f1 <-0
                f1None <- 0                
                for (k1 in 1:(n+1)){
                        f1 <- f1 + 2 * (k1-1) * DP_C[k1, k] * DP_S[k + k1 - 1]
                        f1None <- f1None + 2 * (k1-1) * DP_C[k1,k] * DP_SNone[k + k1 - 1]
                }
                if (k>2){
                        for (i in 2:(2*k-3)){
                                DP_S[i] <-  (1 - P[k - 1]) * DP_S[i] + P[k - 1] * DP_S[i + 1]
                                DP_SNone[i] <- (1 - P[k - 1]) * DP_SNone[i] + P[k - 1] * DP_SNone[i + 1]
                        }
                }
                expectations[[length(expectations)+1]] <- list(f1None + 2 * pNone / (1 + k), f1)
        }
        
        return(matrix(unlist(rev(expectations)), nrow = 2))
}


max_expectations <- function(P, pNone=NULL){
        expectations <- get_expectations(P, pNone)
        ix_max <- arrayInd(which.max(expectations), dim(expectations))
        max_f1 <- expectations[ix_max]
        predNone <- ifelse((ix_max[1] == 1), TRUE, FALSE)
        best_k = ix_max[2]-1
        return(list(best_k, predNone, max_f1))
}

print_best_pred <- function(P, pNone=NULL){
        
        P <- sort(P, TRUE)
        opt <- max_expectations(P, pNone)
        l <-  (0: (as.integer(opt[1])-1))
        #best_prediction <- paste0(best_prediction,'; v:', toString(l))
        f1_max <- as.numeric(opt[3])
        isnone <- unlist(unlist(opt[2]))
   
        return(list(isnone, l, f1_max))
}

print_best_pred(c(0.3, 0.2))
print_best_pred(c(0.3, 0.2), 0.57)
print_best_pred(c(0.9, 0.6))
print_best_pred(c(0.5, 0.4, 0.3, 0.35, 0.33, 0.31, 0.29, 0.27, 0.25, 0.20, 0.15, 0.10))
print_best_pred(c(0.5, 0.4, 0.3, 0.35, 0.33, 0.31, 0.29, 0.27, 0.25, 0.20, 0.15, 0.10), 0.2)



calc_approx_ef1 <- function(df_group){
        df <- df_group
        order_id <- df$order_id[1]
        df <- data.table(df)[order(-prob),c('product_id', 'prob', 'true')]
        df$true <- apply(df$true,1,as.integer)
        pred_none <- cumprod(1-df$prod)
        # add 'None' as product with p_none
        # ************************************************************
        
        # ************************************************************
        pi_sum <- sum(df$prod)
        len <- length(df$product_id)
        mask <- tril(ones(len, len))
        hi_sum <- sum(mask)
        
}
        
        

# df = df.sort_values('pred', ascending=False)[['product_id', 'pred', 'true']]
# products, preds = (zip(*df.sort_values('pred', ascending=False)[['product_id', 'pred']].values))
# _true = list(map(int, df['true'].values))
# pred_none = np.cumprod([1-x for x in preds])[-1]
# 
# # add 'None' as product with p_none
# # ************************************************************
# products = list(products)[::-1]
# preds = list(preds)[::-1]
# ii = bisect.bisect(preds, pred_none)
# bisect.insort(preds, pred_none)
# products.insert(ii, 65535)
# products = products[::-1]
# preds = preds[::-1]
# # ************************************************************
# 
# pi_sum = np.sum(preds)
# _len = len(products)
# mask = np.tril(np.ones((_len, _len)))
# hi_sum = mask.sum(1)
# 
# phi_sum = 2*np.dot(mask, preds)
# ef1 = phi_sum / (pi_sum+hi_sum)
# 
# ef1_max = np.max(ef1)
# 
# prod_max = ' '.join(map(str, map(int, filter(bool, mask[np.argmax(ef1)]*products))))
# prod_list = prod_max.replace('65535', 'None') # if ef1_max > pred_none else 'None'
# 
# return pd.DataFrame({'products':[prod_list], 'ef1':[ef1_max]})
