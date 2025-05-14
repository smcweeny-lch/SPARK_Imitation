## This code is mostly defunct, but has some nice examples of 
# matrix multiplication to get factor scores manually if you ever need it
# Sean McWeeny, Nov 1, 2024


rm(list = ls())
# Sanity Checks
efa_results <- readRDS("./output/efa_iteration_4_mcweeny.rds")
data_2 <- readRDS("./data/imputation50_output_mcweeny_8_22_24.rds")
data_2 <- data_2 %>% select(rownames(efa_results$weights))

#test for any NA data
test.na <- apply(data_2, MARGIN = 2, FUN = is.na)
any(colSums(test.na) > 0)
test.na <- apply(data_2, MARGIN = 2, FUN = is.nan)
any(colSums(test.na) > 0)
any(data_2 == "")
any(as.matrix(data_2 == "NA"))
any(is.na(as.numeric(as.matrix(data_2))))
# There was none

#Next, check if original weights (saved rds) call changes when specifying scores
# efa_raw_results <- fa(data_2, nfactors = 17, scores = "regression")
# efa_raw_results$weights == efa_results$weights
#They do not. This is good. 
# Next, we check whether calculating the weights with a correlation matrix versus the raw data is replicable
# Also note that use = "pairwise" does change the result from use = "everything, but 
# only because of float numbers and it passes all.equal
R <- cor(data_2, use = "pairwise")
COV <- cov(data_2, use = "pairwise")
efa_mat_results <- fa(R, n.obs = nrow(data_2), nfactors = 17, scores = "regression")
all.equal(loadings(efa_results), loadings(efa_mat_results))
# They are the same out to 10 decimals

# S = f %*% Phi
S <- loadings(efa_results) %*% efa_results$Phi
W <- solve(R,S)
W2 <- solve(R) %*% loadings(efa_results) %*% efa_results$Phi 

all.equal(W, W2)




test <- factor.scores(data_2, efa_results, method = "regression")
all.equal(W, test$weights)

# SCALE!!! variables
scores <- scale(as.matrix(data_2)) %*% W
all.equal(scores, test$scores)



S2 <- factor.scores(data_2, efa_results, method = "Thurstone")

S2$scores == results$scores







W <- R_inv %*% loadings(efa_results)
W == S$weights


w <- solve(rmat, S)

scores <-  as.matrix(data_2) %*% w

efa_results$weights == w

efa_results$Phi == loadings(efa_results)

corPlot(efa_results$Phi)


#Then, let's try calculating the factor scores by hand 
cov_inv <- solve(covmat)
L <- unclass(efa_results$loadings)

weights <- t(t(L) %*% cov_inv) 


round(efa_results$weights,3) == round(weights, 3)


# Inputs:
# X: matrix of observed data (n observations x p variables)
# R: correlation matrix (p x p)
# L: factor loadings matrix (p x m), where m is the number of factors
R <- rmat
L <- unclass(efa_results$loadings)

# Step 1: Invert the correlation matrix R
R_inv <- solve(R)

# Step 2: Compute the weight matrix W for Thurstone's Regression Method
W <- R_inv %*% L %*% solve(t(L) %*% R_inv %*% L)




plot(colMeans(W - efa_mat_results$weights) / colMeans(W))


factor_scores <- as.matrix(data_2) %*% efa_results$weights 

efa_mat_results$
  
  
  
  
  
  
  dim(factor_scores)

round(factor_scores,2) == round(efa_results$scores,2)

dim(factor_scores)
dim(efa_results$weights)

efa_results$weights[,1]
W[,1]

# Step 3: Calculate factor scores
F_scores <- X %*% W

# Output:
F_scores  # Factor scores (n observations x m factors)


