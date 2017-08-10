#Author: Faron, Lukasz Grad
#
# Quite fast implementation of Faron's expected F1 maximization using Rcpp and R
library(inline)
library(Rcpp)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

# Input: p: item reorder probabilities (sorted), p_none: none probability (0 if not specified)
# Output: matrix[2][n + 1] out: out[0][j] - F1 score with top j products and None
#                               out[1][j] - F1 score with top j products
cppFunction(
        'NumericMatrix get_expectations(NumericVector p, double p_none) {
        // Assuming p is sorted, p_none == 0 if not specified
        int n = p.size();
        NumericMatrix expectations = NumericMatrix(2, n + 1);
        double DP_C[n + 2][n + 1];
        std::fill(DP_C[0], DP_C[0] + (n + 2) * (n + 1), 0);
        if (p_none == 0.0) {
        p_none = std::accumulate(p.begin(), p.end(), 1.0, [](double &a, double &b) {return a * (1.0 - b);});
        }
        DP_C[0][0] = 1.0;
        for (int j = 1; j < n; ++j)
        DP_C[0][j] = (1.0 - p[j - 1]) * DP_C[0][j - 1];
        for (int i = 1; i < n + 1; ++i) {
        DP_C[i][i] = DP_C[i - 1][i - 1] * p[i - 1];
        for (int j = i + 1; j < n + 1; ++j)
        DP_C[i][j] = p[j - 1] * DP_C[i - 1][j - 1] + (1.0 - p[j - 1]) * DP_C[i][j - 1];
        }
        double DP_S[2 * n + 1];
        double DP_SNone[2 * n + 1];
        for (int i = 1; i < (2 * n + 1); ++i) {
        DP_S[i] = 1.0 / (1.0 * i);
        DP_SNone[i] = 1.0 / (1.0 * i + 1);
        }
        for (int k = n; k >= 0; --k) {
        double f1 = 0.0;
        double f1None = 0.0;
        for (int k1 = 0; k1 < (n + 1); ++k1) {
        f1 += 2 * k1 * DP_C[k1][k] * DP_S[k + k1];
        f1None += 2 * k1 * DP_C[k1][k] * DP_SNone[k + k1];
        }
        for (int i = 1; i < (2 * k - 1); ++i) {
        DP_S[i] = (1 - p[k - 1]) * DP_S[i] + p[k - 1] * DP_S[i + 1];
        DP_SNone[i] = (1 - p[k - 1]) * DP_SNone[i] + p[k - 1] * DP_SNone[i + 1];
        }
        expectations(0, k) = f1None + 2 * p_none / (2.0 + k);
        expectations(1, k) = f1;
        }
        return expectations;
        }'
)


max_expectations <- function(P, pNone=0.0){
        expectations <- get_expectations(as.vector(P), pNone)
        ix_max <- arrayInd(which.max(expectations), dim(expectations))
        max_f1 <- expectations[ix_max]
        predNone <- ifelse((ix_max[1] == 1), TRUE, FALSE)
        best_k = ix_max[2]-1
        return(list(best_k, predNone, max_f1))
}

print_best_pred <- function(P, pNone=0.0){
        
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


