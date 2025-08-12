# 
rm(list = ls())
library(tidyverse)
library(psych)

source("./code/Empirical_priors/sim_betas_lavaan.R")
source("./code/Empirical_priors/sim_betas_lavaan_std_all.R")


items_list <- readRDS("./data/items_list_constructs.rds")

items_list_lean <- items_list[-c(5:8, 10, 12, 14)]


item_list_voted <- readRDS("./code/Empirical_priors/item_list.voted.rds")
item_list_voted[["Imitation"]] <- 1:116
item_list_voted[["Imitation"]] <- item_list_voted[["Imitation"]][-c(95, 108)]


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

temp <- data.frame(
  variable_name = names(real_reli),
  reliability = round(as.numeric(real_reli),2),
  row.names = NULL
)

write.csv(temp, "./Manuscript/Supplemental Materials/alpha_reli.csv", row.names = F)

real_reli_lean <- real_reli[c(-5:-8)]

pseudo_MotorSkills <- rowMeans(as.data.frame(c(data[,13:15], data[,13:15])))

data2 <- read_rds("data/factor_score_estimates_constructs_symbolism_2_10_25.rds")

real_MotorSkills <- data2$Motor.Skills * -1
real_SocialSkills <- data2$Social.Skills * -1

available_Imitation <- item_list_voted[["Imitation"]][item_list_voted[["Imitation"]] %in% c(available$scq)]
available_Imitation <- setdiff(available_Imitation, which(names(data) %in% items_list[["Social.Skills"]]))



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
  len <- length(items_list_lean[[construct]])
  
  
  alpha_val <- 0
  while(alpha_val < real_reli_lean[names(real_reli_lean) == construct] - .15){
    Imi.col_idx <- sample(available_Imitation, size = len)
    temp <- psych::alpha(data[,Imi.col_idx])
    alpha_val <- temp$total$raw_alpha
  }
  
  pseudo_Imitation <- rowMeans(data[,Imi.col_idx])
  #names(data[,Imi.col_idx])
  relis_list[[i]][[construct]] <- alpha_val
  
  
  
  pseudo_items_list[[i]] <- list(Imitation = names(data[,Imi.col_idx]),
                                 Motor.Skills = items_list[["Motor.Skills"]],
                                 Social.Skills = items_list[["Social.Skills"]])
  
  
  pseudo <- data.frame(Imitation = pseudo_Imitation,
                       Motor.Skills = as.vector(real_MotorSkills),
                       Social.Skills = real_SocialSkills)
  
  # betas_list[[i]] <- sim_betas_lavaan(pseudo, theories = "M&E")
  betas_list[[i]] <- sim_betas_lavaan_std_all(pseudo, theories = "M&E")
  
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


construct_items_list[["Imitation"]][1,]

construct_items_list[["Imitation"]][2,]







length(betas_list)

# saveRDS(nulls, "./code/Empirical_priors/M_and_E_pseudoImi_realMotSoc_all_constraints_5_8_25.rds")
# saveRDS(pseudo_items_list, "./code/Empirical_priors/M_and_E_pseudo_MotSoc_items_all_constraints_5_8_25.rds")
# saveRDS(relis_df, "./code/Empirical_priors/M_and_E_pseudo_MotSoc_construct_reliabilities_all_constraints_5_8_25.rds")

saveRDS(nulls, "./code/Empirical_priors/M_and_E/output/M_and_E_pseudoImi_realMotSoc_all_constraints_8_11_25.rds")
saveRDS(pseudo_items_list, "./code/Empirical_priors/M_and_E/output/M_and_E_pseudoImi_realMotSoc_items_all_constraints_8_11_25.rds")
saveRDS(relis_df, "./code/Empirical_priors/M_and_E/output/M_and_E_pseudoImi_realMotSoc_construct_reliabilities_all_constraints_8_11_25.rds")



nulls %>% group_by(combo) %>%
  summarize(#mean_beta = mean(correlation_value),
            median_beta = median(correlation_value),
            sd_beta = sd(correlation_value))





items_list[["Social.Skills"]]




for(i in 1:20) print(pseudo_items_list[[i]]$Imitation)


nulls <- readRDS("./code/Empirical_priors/M_and_E_pseudoImi_realMotSoc_all_constraints_4_7_25.rds")
pseudo_items_list <- readRDS("./code/Empirical_priors/M_and_E_pseudo_MotSoc_items_all_constraints_4_7_25.rds")
relis_df <- readRDS("./code/Empirical_priors/M_and_E_pseudo_MotSoc_construct_reliabilities_all_constraints_4_7_25.rds")


summary_table <- nulls %>% group_by(combo) %>%
  summarize(mean_beta = mean(correlation_value),
            median_beta = median(correlation_value),
            sd_beta = sd(correlation_value))


summary_table



