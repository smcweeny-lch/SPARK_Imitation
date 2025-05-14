# 
rm(list = ls())
library(tidyverse)
library(psych)

source("./code/Empirical_priors/sim_betas_lavaan.R")

items_list <- readRDS("./data/items_list_constructs.rds")
items_list[["Symbolism.11"]] <- c(items_list[["VL1.F1"]], items_list[["NVC.F1"]], items_list[["Imag.Play"]])
items_list[["Symbolism.12"]] <- c(items_list[["VL1.F1"]], items_list[["NVC.F2"]], items_list[["Imag.Play"]])
items_list[["Symbolism.21"]] <- c(items_list[["VL1.F2"]], items_list[["NVC.F1"]], items_list[["Imag.Play"]])
items_list[["Symbolism.22"]] <- c(items_list[["VL1.F2"]], items_list[["NVC.F2"]], items_list[["Imag.Play"]])
# 
# items_list[["VL1.F1"]]
# items_list[["VL1.F2"]]
# items_list[["NVC.F1"]]
# items_list[["NVC.F2"]]

item_list_voted <- readRDS("./code/Empirical_priors/item_list.voted.rds")
names(item_list_voted)[6] <- "RC.F1"
item_list_voted[["RC.F2"]] <- item_list_voted[["RC.F1"]]
names(item_list_voted)[11] <- "VL2.F1"
item_list_voted[["VL2.F2"]] <- item_list_voted[["VL2.F1"]]
item_list_voted[["Imitation"]] <- 1:116
item_list_voted[["Imitation"]] <- item_list_voted[["Imitation"]][-c(95, 108)]


item_list_voted[["Symbolism"]] <- c(item_list_voted[["Symbolism"]], 17:20, 30)



item_list_voted[["VL1"]]

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



construct <- "Affect.Sharing"
available_Affect.Sharing <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]




construct <- "Symbolism"
available_Symbolism <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq, available$other)]



construct <- "Social.Eng"
available_Social.Eng <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]
available_Social.Eng <- setdiff(available_Social.Eng,  which(names(data) %in% items_list[["Affect.Sharing"]]))


items_list[["Affect.Sharing"]] %in% items_list[["Social.Eng"]]
items_list[["Affect.Sharing"]] %in% items_list[["Symbolism.11"]]
items_list[["Social.Eng"]] %in% items_list[["Symbolism.11"]]
items_list[["Symbolism.11"]] %in% items_list[["Social.Eng"]]

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
  tic1 <- Sys.time()
  relis_list[[i]] <- list()
  
  
  
  construct <- "Imitation"
  
  len <- length(items_list[[construct]])
  

    Imi.col_idx <- sample(available_Imitation, size = len)
    temp <- psych::alpha(data[,Imi.col_idx])
    alpha_val <- temp$total$raw_alpha
  
  
  pseudo_Imitation <- rowMeans(data[,Imi.col_idx])
  #names(data[,Imi.col_idx])
  relis_list[[i]][[construct]] <- alpha_val
  
  
  
  
  construct <- "Affect.Sharing"
  # available_Affect.Sharing <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]
  len <- length(items_list[[construct]])
  

    affect.sharing.col_idx <- sample(available_Affect.Sharing, size = len)
    temp <- psych::alpha(data[,affect.sharing.col_idx])
    alpha_val <- temp$total$raw_alpha
  
  
  pseudo_Affect.Sharing <- rowMeans(data[,affect.sharing.col_idx])
  # names(data[,affect.sharing.col_idx])
  relis_list[[i]][[construct]] <- alpha_val
  
  
  construct <- "Social.Eng"
  # available_Social.Eng <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]
  len <- length(items_list[[construct]])
  
    
    
    social.eng.col_idx <- sample(available_Social.Eng, size = len)
    temp <- psych::alpha(data[,social.eng.col_idx])
    alpha_val <- temp$total$raw_alpha
  
  
  pseudo_Social.Eng <- rowMeans(data[,social.eng.col_idx])
  relis_list[[i]][[construct]] <- alpha_val
  
  
  
  
  construct <- "Symbolism"
  # available_Symbolism <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq, available$other)]
  len <- length(items_list[[paste0(construct, ".11")]])
  

    symbolism.11.col_idx <- sample(available_Symbolism, size = len)
    
    temp <- psych::alpha(data[,symbolism.11.col_idx])
    
    alpha_val <- temp$total$raw_alpha
    
  
  pseudo_Symbolism.11 <- rowMeans(data[,symbolism.11.col_idx])
  relis_list[[i]][[paste0(construct, "11")]] <- alpha_val
  
  
  len <- length(items_list[[paste0(construct, ".12")]])
  

    
    symbolism.12.col_idx <- sample(available_Symbolism, size = len)
    
    temp <- psych::alpha(data[,symbolism.12.col_idx])
    
    alpha_val <- temp$total$raw_alpha
    
  
  pseudo_Symbolism.12 <- rowMeans(data[,symbolism.12.col_idx])
  relis_list[[i]][[paste0(construct, "12")]] <- alpha_val  
  
  
  
  len <- length(items_list[[paste0(construct, ".21")]])
  
    
    symbolism.21.col_idx <- sample(available_Symbolism, size = len)
    
    temp <- psych::alpha(data[,symbolism.21.col_idx])
    alpha_val <- temp$total$raw_alpha
  
  
  pseudo_Symbolism.21 <- rowMeans(data[,symbolism.21.col_idx])
  relis_list[[i]][[paste0(construct, "21")]] <- alpha_val
  
  
  len <- length(items_list[[paste0(construct, ".22")]])
  

    symbolism.22.col_idx <- sample(available_Symbolism, size = len)
    
    temp <- psych::alpha(data[,symbolism.22.col_idx])
    alpha_val <- temp$total$raw_alpha

  pseudo_Symbolism.22 <- rowMeans(data[,symbolism.22.col_idx])
  relis_list[[i]][[paste0(construct, "22")]] <- alpha_val
  
  
  
  
  
  
  pseudo_items_list[[i]] <- list(Imitation = names(data[,Imi.col_idx]),
                                 Symbolism.11 = names(data[,symbolism.11.col_idx]),
                                 Symbolism.12 = names(data[,symbolism.12.col_idx]),
                                 Symbolism.21 = names(data[,symbolism.21.col_idx]),
                                 Symbolism.22 = names(data[,symbolism.22.col_idx]),
                                 Affect.Sharing = names(data[,affect.sharing.col_idx]),
                                 Social.Eng = names(data[,social.eng.col_idx]))
  
  pseudo <- data.frame(Imitation = pseudo_Imitation,
                       Symbolism.11 = pseudo_Symbolism.11,
                       Symbolism.12 = pseudo_Symbolism.12,
                       Symbolism.21 = pseudo_Symbolism.21,
                       Symbolism.22 = pseudo_Symbolism.22,
                       Affect.Sharing = pseudo_Affect.Sharing,
                       Social.Eng = pseudo_Social.Eng)
  
  betas_list[[i]] <- sim_betas_lavaan(pseudo, theories = "R&P")
  print(i)
  toc1 <- Sys.time()
  print(toc1 - tic1)
}
toc <- Sys.time()
print(toc - tic)


relis_df <- as_tibble(do.call(rbind, relis_list))

relis_df <- relis_df %>% 
  mutate(across(everything(), ~as.numeric(.x)))


colMeans(relis_df)
class(relis_df)


nulls <- do.call(rbind, betas_list)


nulls$combo <- (paste0(nulls$measure_1, "*", nulls$measure_2))
nulls$combo <- ifelse(nulls$combo == "Imitation*Social.Eng" & nulls$partialled == "No", "Imitation*Social.Eng.Raw", nulls$combo)
nulls$combo <- ifelse(nulls$combo == "Affect.Sharing*Social.Eng" & nulls$partialled == "No", "Affect.Sharing*Social.Eng.Raw", nulls$combo)
nulls$combo <- ifelse(nulls$combo == "Imitation*Affect.Sharing" & is.na(nulls$Fs), "Imitation*Affect.Sharing.Raw", nulls$combo)

nulls$combo <- factor(nulls$combo)


nulls$correlation_value <- as.numeric(nulls$correlation_value)


nulls$draw <- rep(1:niter, each = 19)

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

construct_items_list[["Imitation"]][1,]
construct_items_list[["Social.Eng"]]
construct_items_list[["Symbolism"]]
construct_items_list[["Affect.Sharing"]][2,]
construct_items_list[["Affect.Sharing"]][3,]
construct_items_list[["Affect.Sharing"]][4,]
construct_items_list[["Affect.Sharing"]][5,]
construct_items_list[["Social.Eng"]][8,]


construct_items_list[["Affect.Sharing"]][2,] %in% construct_items_list[["Social.Eng"]][2,]


# saveRDS(nulls, "./code/Empirical_priors/R_and_P_pseudo_construct_all_constraints_3_27_25.rds")
# saveRDS(pseudo_items_list, "./code/Empirical_priors/R_and_P_pseudo_construct_items_all_constraints_3_27_25.rds")
# saveRDS(relis_df, "./code/Empirical_priors/R_and_P_pseudo_construct_reliabilities_all_constraints_3_27_25.rds")

saveRDS(nulls, "./code/Empirical_priors/R_and_P_pseudo_construct_no_constraints_4_28_25.rds")
saveRDS(pseudo_items_list, "./code/Empirical_priors/R_and_P_pseudo_construct_items_no_constraints_4_28_25.rds")
saveRDS(relis_df, "./code/Empirical_priors/R_and_P_pseudo_construct_reliabilities_no_constraints_4_28_25.rds")



nulls %>% group_by(combo) %>%
  summarize(mean_beta = mean(correlation_value),
            median_beta = median(correlation_value),
            sd_beta = sd(correlation_value))







