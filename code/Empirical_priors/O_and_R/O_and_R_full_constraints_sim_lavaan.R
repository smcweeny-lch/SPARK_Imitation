# This is the primary script for generating the pseudo-constructs (and relations among them)
# for Onritz & Ritvo (1968). The data structures are from many sources, and need some explaining
# items_list was generated from CFAs_All_Constructs 1_30_25.R and contains the final items
# included on the final CFAs. For 2-factor constructs, they are stored separately on .F1 and .F2
# factors, which means we need to pull in the item relevance votes to exclude relevant items from the
# pseudoconstructs (available), in item_list_voted.

# This was modified from simulate_pseudoconstructs_O_and_R_full_constraints.R on April 21, 2025
# after discovering that lm was insufficient to estimate lavaan slopes, and sim_betas_lavaan.R was written



rm(list = ls())
library(tidyverse)
library(psych)

source("./code/Empirical_priors/sim_betas_lavaan.R")

items_list <- readRDS("./data/items_list_constructs.rds")
items_list[["Symbolism"]] <- c(items_list[["VL1.F1"]], items_list[["NVC.F1"]], items_list[["Imag.Play"]])

#items_list_lean <- items_list[-c(5:8, 10, 12, 14)]


# here, the "votes do not know about 2-factor constructs"
# you have to a) manually rename RC to RC.F1
# and create a new construct for RC.F2, with the same items as RC.F1
# this is somewhat unintuitive, but the items irrelevant to RC.F1 need to be
# the same items irrelevant to RC.F2, even if their item compositions are different


item_list_voted <- readRDS("./code/Empirical_priors/item_list.voted.rds")
names(item_list_voted)[6] <- "RC.F1"
item_list_voted[["RC.F2"]] <- item_list_voted[["RC.F1"]]
names(item_list_voted)[11] <- "VL2.F1"
item_list_voted[["VL2.F2"]] <- item_list_voted[["VL2.F1"]]
item_list_voted[["Imitation"]] <- 1:116
item_list_voted[["Imitation"]] <- item_list_voted[["Imitation"]][-c(95, 108)]
item_list_voted[["Symbolism"]] <- c(item_list_voted[["Symbolism"]], 17:20, 30)


data <- readRDS("./data/reverse_score_corrected_data_2_26_25.rds")



available <- list(
  scq = 75:114,
  rbs_r = 32:74,
  dcdq = 1:15,
  other = c(16:31, 115:116)
)

neg_corrs <- which(names(data) %in% c("q04_inappropriate_question", "q13_interests_intensity"))

construct <- "Imitation"
available_Imitation <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]

construct <- "Perceptual.Inconstancy"
available_Perceptual.Inconstancy_scq <- item_list_voted[[construct]][item_list_voted[[construct]] %in% available$scq]
available_Perceptual.Inconstancy_rbsr <- item_list_voted[[construct]][item_list_voted[[construct]] %in% available$rbs_r]
available_Perceptual.Inconstancy_scq <- setdiff(available_Perceptual.Inconstancy_scq, neg_corrs)

construct <- "Self.Nonself"
available_Self.Nonself <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]
available_Self.Nonself <- setdiff(available_Self.Nonself,  which(names(data) %in% c(items_list[["RC.F1"]],  items_list[["RC.F2"]])))
available_Self.Nonself <- setdiff(available_Self.Nonself,  neg_corrs)



construct <- "RC.F1"
available_RC.F1 <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]
available_RC.F1 <- setdiff(available_RC.F1,  which(names(data) %in% items_list[["Self.Nonself"]]))
available_RC.F1 <- setdiff(available_RC.F1,  neg_corrs)


construct <- "VL2.F1"
available_VL2.F1 <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq, available$other)]
available_VL2.F1 <- setdiff(available_VL2.F1,  which(names(data) %in% items_list[["Self.Nonself"]]))
available_VL2.F1 <- setdiff(available_VL2.F1,  neg_corrs)



# RC.F2 available should be identical to RC.F1 available, but VL won't be because 
# the pseudoitems for VL2.F2 will be exclusively SCQ rather than mixed demo/SCQ
# I just made them parallel for clarity 

construct <- "RC.F2"
available_RC.F2 <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]
available_RC.F2 <- setdiff(available_RC.F2,  which(names(data) %in% items_list[["Self.Nonself"]]))
available_RC.F2 <- setdiff(available_RC.F2,  neg_corrs)


# items_list[["VL2.F2"]]
# items_list[["VL2.F1"]]
construct <- "VL2.F2"
available_VL2.F2 <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]
available_VL2.F2 <- setdiff(available_VL2.F2,  which(names(data) %in% items_list[["Self.Nonself"]]))
available_VL2.F2 <- setdiff(available_VL2.F2,  neg_corrs)












#items_list[["Self.Nonself"]] %in% c(items_list[["VL2.F1"]], items_list[["VL2.F2"]])





real_reli <- vector()
for(construct in names(items_list)){
  
  
  reli <- alpha(data[,items_list[[construct]]], check.keys = F)$total$raw_alpha
  
  real_reli[construct] <- reli
  
}


niter <- 500
#niter <- 1000
#for(i in 501:niter)

betas_list <- list()
pseudo_items_list <- list()
relis_list <- list()
set.seed(123)
tic <- Sys.time()
for(i in 1:niter){
  relis_list[[i]] <- list()
  tic1 <- Sys.time()
  
  construct <- "Perceptual.Inconstancy"
  # available_Perceptual.Inconstancy <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$rbs_r, available$scq)]
  len <- length(items_list[[construct]])
  
  
  alpha_val <- 0
  while(alpha_val < real_reli[names(real_reli) == construct] - .15){
    perc.inc.scq.col_idx <- sample(available_Perceptual.Inconstancy_scq, size = 4)
    perc.inc.rbsr.col_idx <- sample(available_Perceptual.Inconstancy_rbsr, size = 2)
    perc.inc.col_idx <- c(perc.inc.rbsr.col_idx, perc.inc.scq.col_idx)
    temp <- psych::alpha(data[,perc.inc.col_idx])
    alpha_val <- temp$total$raw_alpha
  }
  
  pseudo_Perc.Inc <- rowMeans(data[,perc.inc.col_idx])
  #names(data[,perc.inc.col_idx])
  relis_list[[i]][[construct]] <- alpha_val
  
  
  
  
  
  
  
  
  construct <- "Imitation"
  # available_Imitation <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]
  len <- length(items_list[[construct]])
  
  
  alpha_val <- 0
  while(alpha_val < real_reli[names(real_reli) == construct] - .15){
    
    temp_available <- setdiff(available_Imitation, perc.inc.col_idx)
    
    Imi.col_idx <- sample(temp_available, size = len)
    temp <- psych::alpha(data[,Imi.col_idx])
    alpha_val <- temp$total$raw_alpha
  }
  
  pseudo_Imitation <- rowMeans(data[,Imi.col_idx])
  #names(data[,Imi.col_idx])
  relis_list[[i]][[construct]] <- alpha_val
  
  
  
  
  
  
  
  construct <- "Self.Nonself"
  # available_Self.Nonself <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]
  len <- length(items_list[[construct]])
  
  
  alpha_val <- 0
  while(alpha_val < real_reli[names(real_reli) == construct] - .15){
    
    temp_available <- setdiff(available_Self.Nonself, perc.inc.col_idx)
    
    self.nonself.col_idx <- sample(temp_available, size = len)
    
    temp <- psych::alpha(data[,self.nonself.col_idx])
    alpha_val <- temp$total$raw_alpha
  }
  
  pseudo_Self.Nonself <- rowMeans(data[,self.nonself.col_idx])
  #names(data[,self.nonself.col_idx])
  relis_list[[i]][[construct]] <- alpha_val
  
  
  
  
  construct <- "RC.F1"
  #items_list[["Self.Nonself"]] %in% items_list[["RC.F1"]]
  # available_RC <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]
  len <- length(items_list[[construct]])
  
  alpha_val <- 0
  while(alpha_val < real_reli[names(real_reli) == construct] - .25){
    rc1.overlap.item <- sample(self.nonself.col_idx, size = 1)
    
    temp_available <- setdiff(available_RC.F1, self.nonself.col_idx)
    temp_available <- setdiff(temp_available, perc.inc.col_idx)
    temp_available <- setdiff(temp_available, Imi.col_idx)
    
    
    rc1.col_idx <- sample(temp_available, size = len - 1)
    
    
    
    rc1.col_idx <- c(rc1.col_idx, rc1.overlap.item)
    
    temp <- psych::alpha(data[,rc1.col_idx])
    alpha_val <- temp$total$raw_alpha
  }
  
  pseudo_RC1.F1 <- rowMeans(data[,rc1.col_idx])
  #names(data[,rc.col_idx])
  relis_list[[i]][[construct]] <- alpha_val
  
  
  
  
  
  construct <- "RC.F2"
  # items_list[["Self.Nonself"]] %in% items_list[["RC.F2"]]
  # available_RC <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]
  len <- length(items_list[[construct]])
  
  RC.F2.col_idx <- NULL
  alpha_val <- 0
  while(alpha_val < real_reli[names(real_reli) == construct] - .25){
    # with 10 items, RC.F2 creates problems when restricting the available_items
    # if there are only 10 items, the reliability may be low enough to get it stuck in an endless loop
    # also high reli so hard to meet
    temp_available <- setdiff(available_RC.F2, self.nonself.col_idx)
    temp_available <- setdiff(temp_available, perc.inc.col_idx)
    temp_available <- setdiff(temp_available, Imi.col_idx)
    
    if(len >= length(temp_available)){
      rc2.col_idx <- sample(temp_available, size = len, replace = T)
    } else {
      rc2.col_idx <- sample(temp_available, size = len)
    }
    
    
    
    
    temp <- psych::alpha(data[,rc2.col_idx])
    alpha_val <- temp$total$raw_alpha
  }
  
  pseudo_RC.F2 <- rowMeans(data[,rc2.col_idx])
  #names(data[,rc.col_idx])
  relis_list[[i]][[construct]] <- alpha_val
  
  
  
  construct <- "VL2.F1"
  #items_list[["Self.Nonself"]] %in% items_list[["VL2.F1"]]
  # available_VL2 <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq, available$other)]
  len <- length(items_list[[construct]])
  
  alpha_val <- 0
  while(alpha_val < real_reli[names(real_reli) == construct] - .15){
    
    temp_available <- setdiff(available_VL2.F1, self.nonself.col_idx)
    temp_available <- setdiff(temp_available, perc.inc.col_idx)
    temp_available <- setdiff(temp_available, Imi.col_idx)
    
    
    
    VL2.F1.col_idx <- sample(temp_available, size = len)
    temp <- psych::alpha(data[,VL2.F1.col_idx])
    alpha_val <- temp$total$raw_alpha
  }
  
  pseudo_VL2.F1 <- rowMeans(data[,VL2.F1.col_idx])
  #names(data[,VL2.col_idx])
  relis_list[[i]][[construct]] <- alpha_val
  
  
  construct <- "VL2.F2"
  # available_VL2 <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq, available$other)]
  len <- length(items_list[[construct]])
  
  alpha_val <- 0
  while(alpha_val < real_reli[names(real_reli) == construct] - .15){
    
    VL2.F2.overlap.item <- sample(self.nonself.col_idx, size = 1)
    
    temp_available <- setdiff(available_VL2.F2, self.nonself.col_idx)
    temp_available <- setdiff(temp_available, perc.inc.col_idx)
    temp_available <- setdiff(temp_available, Imi.col_idx)
    
    
    
    VL2.F2.col_idx <- sample(temp_available, size = len - 1)
    VL2.F2.col_idx <- c(VL2.F2.overlap.item, VL2.F2.col_idx)
    
    temp <- psych::alpha(data[,VL2.F2.col_idx])
    alpha_val <- temp$total$raw_alpha
    #
  }
  
  pseudo_VL2.F2 <- rowMeans(data[,VL2.F2.col_idx])
  #names(data[,VL2.col_idx])
  relis_list[[i]][[construct]] <- alpha_val
  
  
  pseudo_items_list[[i]] <- list(Imitation = names(data[,Imi.col_idx]),
                                 Perceptual.Inconstancy = names(data[,perc.inc.col_idx]),
                                 RC.F1 = names(data[,rc1.col_idx]),
                                 RC.F2 = names(data[,rc2.col_idx]),
                                 Self.Nonself = names(data[,self.nonself.col_idx]),
                                 VL2.F1 = names(data[,VL2.F1.col_idx]),
                                 VL2.F2 = names(data[,VL2.F2.col_idx]))
  
  pseudo <- data.frame(Imitation = pseudo_Imitation,
                       Perceptual.Inconstancy = pseudo_Perc.Inc,
                       RC.F1 = pseudo_RC1.F1,
                       Self.Nonself = pseudo_Self.Nonself,
                       VL2.F1 = pseudo_VL2.F1,
                       RC.F2 = pseudo_RC.F2,
                       VL2.F2 = pseudo_VL2.F2)
  
  betas_list[[i]] <- sim_betas_lavaan(pseudo, theories = "O&R")
  print(i)
  toc1 <- Sys.time()
  print(toc1-tic1)
}
toc <- Sys.time()
toc-tic

relis_df <- as_tibble(do.call(rbind, relis_list))

relis_df <- relis_df %>% 
  mutate(across(everything(), ~as.numeric(.x)))




colMeans(relis_df)
class(relis_df)




nulls <- do.call(rbind, betas_list)


nulls$combo <- factor(paste0(nulls$measure_1, "*", nulls$measure_2))
nulls$correlation_value <- as.numeric(nulls$correlation_value)


nulls$draw <- rep(1:niter, each = 32)

row.names(nulls) <- NULL

# nulls$row.combo <- str_extract(row.names(nulls), regex("\\D\\D_row\\d"))


nulls %>% group_by(combo) %>% 
  filter(Fs == 11) %>%
  summarize(mean_beta = mean(correlation_value),
            median_beta = median(correlation_value),
            sd_beta = sd(correlation_value))




# Initialize an empty list to store the construct-based data frames
construct_items_list <- list()

# Get all construct names from the first iteration (assuming they are consistent across iterations)
construct_names <- names(pseudo_items_list[[1]])

# Iterate over each construct
for (construct in construct_names) {
  # Extract the construct vectors across all iterations
  construct_data <- lapply(pseudo_items_list, function(x) x[[construct]])
  
  # Convert the list of vectors into a data frame (rows = iterations, cols = vector elements)
  construct_df <- do.call(rbind, construct_data)
  
  # Store in the output list
  construct_items_list[[construct]] <- as.data.frame(construct_df)
}

# new_list now contains a data frame for each construct

items_list[["Self.Nonself"]] %in% items_list[["RC.F1"]]


nitem_vec <- c()
for(i in 1:nrow(construct_items_list[["Self.Nonself"]])){
  nitem_vec[i] <- sum(construct_items_list[["Imitation"]][i,] %in% construct_items_list[["RC.F2"]][i,])
  
}

median(nitem_vec)

construct_items_list[["Self.Nonself"]][1,] %in% construct_items_list[["RC.F1"]][1,]



saveRDS(nulls, "./code/Empirical_priors/O_and_R_pseudo_construct_all_constraints_4_21_25.rds")
saveRDS(pseudo_items_list, "./code/Empirical_priors/O_and_R_pseudo_construct_items_all_constraints_4_21_25.rds")
saveRDS(relis_df, "./code/Empirical_priors/O_and_R_pseudo_construct_reliabilities_all_constraints_4_21_25.rds")


nulls2 <- readRDS("./code/Empirical_priors/O_and_R_pseudo_construct_all_constraints_4_10_25.rds")

nulls2 %>% group_by(combo) %>%
  summarize(median = median(correlation_value)) %>%
  filter(!str_detect(combo, ".F2"))

nulls %>% filter(Fs == 11) %>%
  group_by(combo) %>% 
  summarize(median = median(correlation_value))








nulls %>% filter(draw == 1)



nulls <- readRDS("./code/Empirical_priors/O_and_R_pseudo_construct_all_constraints_3_24_25.rds")




combo_matrix <- combn(1:20, 2)

store <- c()
for(i in 1:ncol(combo_matrix)){
  
  a <- combo_matrix[,i][1] 
  b <- combo_matrix[,i][2]
  
  store[i] <- mean(pseudo_items_list[[a]]$Imitation %in% pseudo_items_list[[b]]$Imitation)
  
  
}

mean(store)


for(i in 1:ncol(combo_matrix)){
  
  a <- combo_matrix[,i][1] 
  b <- combo_matrix[,i][2]
  
  store[i] <- mean(pseudo_items_list[[a]]$Perceptual.Inconstancy %in% pseudo_items_list[[b]]$Imitation)
  
  
}




store <- c()
for(i in 1:ncol(combo_matrix)){
  
  a <- combo_matrix[,i][1] 
  b <- combo_matrix[,i][2]
  
  store[i] <- mean(pseudo_items_list[[a]]$Self.Nonself %in% pseudo_items_list[[b]]$Self.Nonself)
  
  
}

mean(store)



store <- c()
for(i in 1:ncol(combo_matrix)){
  
  a <- combo_matrix[,i][1] 
  b <- combo_matrix[,i][2]
  
  store[i] <- mean(pseudo_items_list[[a]]$Symbolism %in% pseudo_items_list[[b]]$Symbolism)
  
  
}

mean(store)



pseudo_items_list[[1]]







pseudo_items_list[[1]]