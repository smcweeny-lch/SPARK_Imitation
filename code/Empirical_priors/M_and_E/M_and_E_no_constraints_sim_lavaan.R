# This script was modified from "simulate_pseudoconstructs_M_and_E_no_constraints.R 
# on April 18, 2025 after discovering that sim_betas.R used lm slopes that were not
# necessarily compatible with lavaan slopes (ie joint estimation)
# This should not matter for M and E as there are only 3 variables, and it passed a first
# sanity check by reproducing nearly identical beta values. 
# The original script and pseudoconstructs .rds files will be deprecated. 

rm(list = ls())
library(tidyverse)
library(psych)

source("./code/Empirical_priors/sim_betas_lavaan.R")

items_list <- readRDS("./data/items_list_constructs.rds")
items_list[["Symbolism"]] <- c(items_list[["VL1.F1"]], items_list[["NVC.F1"]], items_list[["Imag.Play"]])

items_list[["Perceptual.Inconstancy"]]


items_list_lean <- items_list[-c(5:8, 10, 12, 14)]




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


real_reli <- vector()
for(construct in names(items_list)){
  
  
  reli <- alpha(data[,items_list[[construct]]], check.keys = F)$total$raw_alpha
  
  real_reli[construct] <- reli
  
}

real_reli_lean <- real_reli[c(-5:-8)]

pseudo_MotorSkills <- rowMeans(as.data.frame(c(data[,13:15], data[,13:15])))

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
  
  
  construct <- "Imitation"
  available_Imitation <- item_list_voted[[construct]][item_list_voted[[construct]] %in% c(available$scq)]
  len <- length(items_list_lean[[construct]])
  
  
  Imi.col_idx <- sample(available_Imitation, size = len)
  temp <- psych::alpha(data[,Imi.col_idx])
  alpha_val <- temp$total$raw_alpha
  
  
  pseudo_Imitation <- rowMeans(data[,Imi.col_idx])
  #names(data[,Imi.col_idx])
  relis_list[[i]][[construct]] <- alpha_val
  
  
  
  construct <- "Social.Skills"
  #  Real alpha: 0.8245209
  available_Social.Skills <- item_list_voted[[construct]][item_list_voted[[construct]] %in% available$scq]
  len <- length(items_list_lean[[construct]])
  
  
  soc.skill.col_idx <- sample(available_Social.Skills, size = len)
  temp <- psych::alpha(data[,soc.skill.col_idx])
  alpha_val <- temp$total$raw_alpha
  
  
  pseudo_SocialSkills <- rowMeans(data[,soc.skill.col_idx])
  # names(data[,soc.skill.col_idx])
  relis_list[[i]][[construct]] <- alpha_val
  
  
  
  
  
  
  
  
  pseudo_items_list[[i]] <- list(Imitation = names(data[,Imi.col_idx]),
                                 Motor.Skills = c(names(data[,13:15]), names(data[,13:15])),
                                 Social.Skills = names(data[,soc.skill.col_idx]))
  
  
  pseudo <- data.frame(Imitation = pseudo_Imitation,
                       Motor.Skills = pseudo_MotorSkills,
                       Social.Skills = pseudo_SocialSkills)
  
  betas_list[[i]] <- sim_betas_lavaan(pseudo, theories = "M&E")
  print(i)
}

toc <- Sys.time()
toc-tic

betas_list
pseudo_items_list
relis_list

relis_df <- as_tibble(do.call(rbind, relis_list))

relis_df <- relis_df %>% 
  mutate(across(everything(), ~as.numeric(.x)))


colMeans(relis_df)
class(relis_df)




nulls <- do.call(rbind, betas_list)


nulls <- nulls %>% mutate(measure_2 = case_when( 
  measure_1 == "Social.Skills" & measure_2 == "Motor.Skills" & partialled == "No" ~ "Motor.Skills.raw",
  TRUE ~ measure_2))
nulls$combo <- factor(paste0(nulls$measure_1, "*", nulls$measure_2))
nulls$correlation_value <- as.numeric(nulls$correlation_value)



nulls$draw <- rep(1:niter, each = 4)

row.names(nulls) <- NULL

# nulls$row.combo <- str_extract(row.names(nulls), regex("\\D\\D_row\\d"))


nulls %>% group_by(combo) %>%
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


t(construct_items_list[["Social.Skills"]][1,])
t(construct_items_list[["Social.Skills"]][2,])
t(construct_items_list[["Social.Skills"]][3,])








length(betas_list)

saveRDS(nulls, "./code/Empirical_priors/M_and_E_pseudo_construct_no_constraints_4_18_25.rds")
saveRDS(pseudo_items_list, "./code/Empirical_priors/M_and_E_pseudo_construct_items_no_constraints_4_18_25.rds")
saveRDS(relis_df, "./code/Empirical_priors/M_and_E_pseudo_construct_reliabilities_no_constraints_4_18_25.rds")


nulls2 <- readRDS("./code/Empirical_priors/M_and_E_pseudo_construct_no_constraints_3_27_25.rds")

