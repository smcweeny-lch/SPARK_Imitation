library(readxl)

dict <- readRDS(file = "./data/data_dictionary.rds")
data <- readRDS("./data/reverse_score_corrected_data_2_26_25.rds")

W <- read_xlsx("./construct_voting/Item_Voting_Ericka_12_29_24.xlsx", col_names = T)
W$voter <- "W"

L <- read_xlsx("./construct_voting/Item_Voting_Current_NNL.xlsx", col_names = T)
L$voter <- "L"

E <- read_xlsx("./construct_voting/Ewen_Item_Voting_1_28_25.xlsx", col_names = T)
E$voter <- "E"




LEW <- bind_rows(E,W,L) %>% select(name, Social.Skills:Self.Nonself, voter)

LEW <- LEW %>% rename(VL1 = VL...10,
                      VL2 = VL...12,
                      Imag.Play = `Imag. Play`)

LEW_long <- LEW %>% pivot_longer(c(Social.Skills:Self.Nonself) , names_to = "construct", values_to = "score" )
LEW_long$composition <- case_when(LEW_long$construct %in% c("Affect.Sharing", "Social.Skills", "NVC", "Imag.Play", "Self.Nonself", "Relational.Capacity", "Social.Eng") ~ "scq",
                                  LEW_long$construct %in% c("VL1", "VL2") ~ "Other",
                                  LEW_long$construct == "Motor.Skills" ~ "dcdq",
                                  LEW_long$construct %in% c("Perceptual.Inconstancy") ~ "rbs-r")

LEW_long <- left_join(LEW_long, dict %>% select(var_name, from), by = c("name" = "var_name"))



#bLEW_long01 <- LEW_long %>% filter(score <= 1)



five_below <- LEW_long %>% group_by(construct,name, from, composition) %>% 
  summarize(sum = sum(score)) %>%
  filter(sum <= 5)

LEW_long %>% filter(construct == "Motor.Skills") %>% group_by(construct,name, from, composition) %>% 
  summarize(sum = sum(score)) %>%
  filter(sum <= 6)


LEW_long %>% filter(construct == "VL1") %>% group_by(construct,name, from, composition) %>% 
  summarize(sum = sum(score))



voted_items <- which(names(data) %in% E$name)

item_list_voted <- list()



for(construct in unique(LEW_long$construct)){
  
  eek <- which(names(data) %in% five_below$name[five_below$construct == construct])
  
  item_list_voted[[construct]] <- eek
  
}


item_list_voted[["Symbolism"]] <- base::intersect(item_list_voted[["Imag.Play"]] , (base::intersect(item_list_voted[["VL1"]],  item_list_voted[["NVC"]])))

saveRDS(item_list_voted, "./code/Empirical_priors/item_list.voted.rds")
