rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(tidySEM)

data2 <- read_rds("data/factor_score_estimates_constructs_symbolism_2_10_25.rds")

set.seed(123)
split <- sample(1:nrow(data2), size = nrow(data2)*.5,replace = F)

train <- data2[split,]
test <- data2[-split,]


#nulls <- readRDS("./code/Empirical_priors/O_and_R_pseudo_construct_all_constraints_3_24_25.rds")
nulls <- readRDS("./code/Empirical_priors/O_and_R/output/O_and_R_pseudo_construct_all_constraints_4_21_25.rds")


summary_table <- nulls %>% group_by(combo) %>%
  filter(Fs == 11) %>%
  summarize(mean_beta = mean(correlation_value),
            median_beta = median(correlation_value),
            sd_beta = sd(correlation_value)) %>%
  filter(!str_detect(combo, ".F2"))


#  combo                                mean_beta median_beta sd_beta
# <fct>                                     <dbl>       <dbl>   <dbl>
# 1 Imitation*Perceptual.Inconstancy      0.471       0.463  0.133 
# 2 Imitation*RC.F1                         0.140       0.112  0.159 
# 3 Imitation*VL2.F1                        0.290       0.285  0.177 
# 4 RC.F1*Perceptual.Inconstancy            0.202       0.192  0.0986
# 5 Self.Nonself*Perceptual.Inconstancy     0.373       0.367  0.108 
# 6 Self.Nonself*RC.F1                      0.570       0.577  0.104 
# 7 Self.Nonself*VL2.F1                     0.312       0.306  0.0868
# 8 VL2.F1*Perceptual.Inconstancy           0.351       0.343  0.136 
# 
# 
# OR.1968.syntax <- '
# Self.Nonself ~ b1*Perceptual.Inconstancy
# Imitation ~ b2*Perceptual.Inconstancy
# RC.F1 ~ b3*Imitation + b5*Self.Nonself + b7*Perceptual.Inconstancy
# VL2.F1 ~ b4*Imitation + b6*Self.Nonself + b8*Perceptual.Inconstancy 
# 
# b1_adj := b1 - .417
# b2_adj := b2 - .399
# b3_adj := b3 - .194
# b4_adj := b4 - .343
# b5_adj := b5 - .283
# b6_adj := b6 - .295
# b7_adj := b7 - .232
# b8_adj := b8 - .273
# '

# OR.1968.syntax <- '
# Self.Nonself ~ b1*Perceptual.Inconstancy
# Imitation ~ b2*Perceptual.Inconstancy
# RC.F1 ~ b3*Imitation + b5*Self.Nonself + b7*Perceptual.Inconstancy
# VL2.F1 ~ b4*Imitation + b6*Self.Nonself + b8*Perceptual.Inconstancy 
# 
# b1_adj := b1 - .348
# b2_adj := b2 - .407
# b3_adj := b3 - .101
# b4_adj := b4 - .252
# b5_adj := b5 - .410
# b6_adj := b6 - .114
# b7_adj := b7 - .233
# b8_adj := b8 - .347
# '



summary_table$b <- c(2, 3, 4, 7, 1, 5, 6, 8) 


summary_table %>% arrange(b)



OR.1968.syntax <- '
Self.Nonself ~ b1*Perceptual.Inconstancy
Imitation ~ b2*Perceptual.Inconstancy
RC.F1 ~ b3*Imitation + b5*Self.Nonself + b7*Perceptual.Inconstancy
VL2.F1 ~ b4*Imitation + b6*Self.Nonself + b8*Perceptual.Inconstancy 

b1_adj := b1 - .407
b2_adj := b2 - .348
b3_adj := b3 - .105
b4_adj := b4 - .265
b5_adj := b5 - .421
b6_adj := b6 - .108
b7_adj := b7 - .247
b8_adj := b8 - .396
'


OR.mod <- sem(model = OR.1968.syntax, 
              data = data2)


summary(OR.mod, fit.measures = T)

# So in the nulls from 4_21, I did not change the manually assigned names
# from F1 to F2, so when Fs == 21, RC.F1 is actually just RC.F2
summary_table <- nulls %>% group_by(combo) %>% 
  filter(Fs == 22) %>%
  summarize(median_beta = median(correlation_value),
            sd_beta = sd(correlation_value)) 

summary_table$b <- c(2, 3, 4, 7, 1, 5, 6, 8) 

summary_table %>% arrange(b)
# Imi --> RC.F2 .116 vs RC F1 





OR.1968.syntax.22 <- '
Self.Nonself ~ b1*Perceptual.Inconstancy
Imitation ~ b2*Perceptual.Inconstancy
RC.F2 ~ b3*Imitation + b5*Self.Nonself + b7*Perceptual.Inconstancy
VL2.F2 ~ b4*Imitation + b6*Self.Nonself + b8*Perceptual.Inconstancy 

b1_adj := b1 - .407
b2_adj := b2 - .348
b3_adj := b3 - .123 
b4_adj := b4 - .122
b5_adj := b5 - .281
b6_adj := b6 - .414
b7_adj := b7 - .286
b8_adj := b8 - .221
'


OR.mod.22 <- sem(model = OR.1968.syntax.22, 
              data = data2)


summary(OR.mod.22, fit.measures = T)






# So in the nulls from 4_21, I did not change the manually assigned names
# from F1 to F2, so when Fs == 21, RC.F1 is actually just RC.F2
summary_table <- nulls %>% group_by(combo) %>% 
  filter(Fs == 12) %>%
  summarize(median_beta = median(correlation_value),
            sd_beta = sd(correlation_value)) 

summary_table$b <- c(2, 3, 4, 7, 1, 5, 6, 8) 

summary_table %>% arrange(b)
# Imi --> RC.F2 .116 vs RC F1 





OR.1968.syntax.12 <- '
Self.Nonself ~ b1*Perceptual.Inconstancy
Imitation ~ b2*Perceptual.Inconstancy
RC.F1 ~ b3*Imitation + b5*Self.Nonself + b7*Perceptual.Inconstancy
VL2.F2 ~ b4*Imitation + b6*Self.Nonself + b8*Perceptual.Inconstancy 

b1_adj := b1 - .407
b2_adj := b2 - .348
b3_adj := b3 - .105 
b4_adj := b4 - .122
b5_adj := b5 - .421
b6_adj := b6 - .414
b7_adj := b7 - .247
b8_adj := b8 - .221
'


OR.mod.12 <- sem(model = OR.1968.syntax.12, 
                 data = data2)


summary(OR.mod.12, fit.measures = T)



summary_table <- nulls %>% group_by(combo) %>% 
  filter(Fs == 21) %>%
  summarize(median_beta = median(correlation_value),
            sd_beta = sd(correlation_value)) 

summary_table$b <- c(2, 3, 4, 7, 1, 5, 6, 8) 

summary_table %>% arrange(b)



OR.1968.syntax.21 <- '
Self.Nonself ~ b1*Perceptual.Inconstancy
Imitation ~ b2*Perceptual.Inconstancy
RC.F2 ~ b3*Imitation + b5*Self.Nonself + b7*Perceptual.Inconstancy
VL2.F1 ~ b4*Imitation + b6*Self.Nonself + b8*Perceptual.Inconstancy 

b1_adj := b1 - .407
b2_adj := b2 - .348
b3_adj := b3 - .123 
b4_adj := b4 - .265
b5_adj := b5 - .281
b6_adj := b6 - .108
b7_adj := b7 - .286
b8_adj := b8 - .396
'


OR.mod.21 <- sem(model = OR.1968.syntax.21, 
                 data = data2)


summary(OR.mod.21, fit.measures = T)








# OR.1968.syntax.drop1 <- '
# Self.Nonself ~ Perceptual.Inconstancy
# Imitation ~ 0*Perceptual.Inconstancy
# RC.F1 ~ Imitation +  Self.Nonself + Perceptual.Inconstancy
# VL1.F1 ~  Imitation + Self.Nonself + Perceptual.Inconstancy
# '
# 
# 
# OR.mod.drop1 <- sem(model = OR.1968.syntax.drop1, data = data2)
# 
# 
# summary(OR.mod.drop1, fit.measures = T)
# summary(OR.mod, fit.measures = T)
# 
# 
# anova(OR.mod, OR.mod.drop1)
# lavaan::partable(OR.mod)
# 
# D <- -2*lavaan::logLik(OR.mod.drop1) + 2*lavaan::logLik(OR.mod)
# pchisq(unclass(D), df = 1)



#####

OR_layout <- array(dim = c(5,5), NA)
OR_layout[3,1] <- "Perceptual.Inconstancy"
OR_layout[2,3] <- "Imitation"
OR_layout[4,3] <- "Self.Nonself"
OR_layout[1,5] <- "RC.F1"
OR_layout[5,5] <- "VL2.F1"

graph_data <- prepare_graph(OR.mod, layout = OR_layout)

graph_data$edges <- graph_data$edges %>% 
  filter(from != to) %>% 
  mutate(curvature = NA,
         linetype = 1,
         arrow = "last")



graph_data$edges$curvature[graph_data$edges$from == "Perceptual.Inconstancy" & graph_data$edges$to == "RC.F1"] <- 50
graph_data$edges$curvature[graph_data$edges$from == "Perceptual.Inconstancy" & graph_data$edges$to == "VL2.F1"] <- -50


# Self.Nonself ~ b1*Perceptual.Inconstancy
# Imitation ~ b2*Perceptual.Inconstancy
# RC.F1 ~ b3*Imitation + b5*Self.Nonself + b7*Perceptual.Inconstancy
# VL2.F1 ~ b4*Imitation + b6*Self.Nonself + b8*Perceptual.Inconstancy 



graph_data$edges$label[1:8]  <- format(round(parametertable(OR.mod)[c(15:17, 19, 21, 18, 20, 22),]$est, 2), nsmall = 2)
graph_data$edges$show[9] <- FALSE
graph_data$edges$label[7] <- paste0(graph_data$edges$label[7], "*")
graph_data$edges$label[3] <- paste0(graph_data$edges$label[3], "*")
graph_data$edges$label[c(1,2,4,5,6,8)] <- paste0(graph_data$edges$label[c(1,2,4,5,6,8)], "†") 


graph_data$edges$connect_to[4] <- "left"

graph_data$nodes$label[2] <- "Perceptual\nInconstancy"
graph_data$nodes$label[3] <- "Relational\nCapacity"
graph_data$nodes$label[4] <- "Self/Non-self"
graph_data$nodes$label[5] <- "Verbal\nLanguage"

graph_data$nodes$size <- 1.2
graph_data$edges$size <- 1.2

graph_data$edges$label_size <- 6.5
graph_data$nodes$label_size <- 6.5

scale <- 10
png(paste0("./Manuscript/plots/OR_1968_beta_adj_", Sys.Date(), ".png"), width = 1080*scale, height = 760*scale, res=72*scale)
plot(graph_data)
dev.off()









prior_graph_data <- graph_data

summary_table %>% arrange(b) %>% select(combo, median_beta)
prior_graph_data$edges %>% select(label_results, label)

summary_table

prior_graph_data$edges$label[prior_graph_data$edges$label_results == "Self.Nonself.ON.Perceptual.Inconstancy"] <- "0.41"
prior_graph_data$edges$label[prior_graph_data$edges$label_results == "Imitation.ON.Perceptual.Inconstancy"] <- "0.35"
prior_graph_data$edges$label[prior_graph_data$edges$label_results == "RC.F1.ON.Imitation"] <- ".11"
prior_graph_data$edges$label[prior_graph_data$edges$label_results == "RC.F1.ON.Self.Nonself"] <- "0.42"
prior_graph_data$edges$label[prior_graph_data$edges$label_results == "RC.F1.ON.Perceptual.Inconstancy"] <- "0.25"
prior_graph_data$edges$label[prior_graph_data$edges$label_results == "VL2.F1.ON.Self.Nonself"] <- "0.11"
prior_graph_data$edges$label[prior_graph_data$edges$label_results == "VL2.F1.ON.Imitation"] <- "0.27"
prior_graph_data$edges$label[prior_graph_data$edges$label_results == "VL2.F1.ON.Perceptual.Inconstancy"] <- "0.40"


prior_graph_data$edges$color <- "grey"
prior_graph_data$nodes$color <- "grey"

scale <- 10
png(paste0("./Manuscript/Supplemental Materials/OR_1968_AllConstraints_Null", Sys.Date(), ".png"), width = 1080*scale, height = 760*scale, res=72*scale)
plot(prior_graph_data)
dev.off()



#####

OR_layout <- array(dim = c(5,5), NA)
OR_layout[3,1] <- "Perceptual.Inconstancy"
OR_layout[2,3] <- "Imitation"
OR_layout[4,3] <- "Self.Nonself"
OR_layout[1,5] <- "RC.F1"
OR_layout[5,5] <- "VL2.F1"

graph_data <- prepare_graph(OR.mod, layout = OR_layout)

graph_data$edges <- graph_data$edges %>% 
  filter(from != to) %>% 
  mutate(curvature = NA,
         linetype = 1,
         arrow = "last")



graph_data$edges$curvature[graph_data$edges$from == "Perceptual.Inconstancy" & graph_data$edges$to == "RC.F1"] <- 40
graph_data$edges$curvature[graph_data$edges$from == "Perceptual.Inconstancy" & graph_data$edges$to == "VL2.F1"] <- -40




graph_data$edges$label[1:8]  <- format(round(parametertable(OR.mod)[c(15:17, 19, 21, 18, 20, 22),]$est, 2), nsmall = 2)
graph_data$edges$show[9] <- FALSE
graph_data$edges$label[7] <- paste0(graph_data$edges$label[7], "***")
graph_data$edges$label[3] <- paste0(graph_data$edges$label[3], "***")


graph_data$edges$connect_to[4] <- "left"

graph_data$nodes$label[2] <- "Perceptual\nInconstancy"
graph_data$nodes$label[3] <- "Relational\nCapacity"
graph_data$nodes$label[4] <- "Self/Non-self"
graph_data$nodes$label[5] <- "Verbal\nLanguage"



graph_data$nodes$node_xmin <- graph_data$nodes$node_xmin - .5
graph_data$nodes$node_xmax <- graph_data$nodes$node_xmax + .5


graph_data$nodes$node_ymin[2] <- graph_data$nodes$node_ymin[2] - .5
graph_data$nodes$node_ymin[3] <- graph_data$nodes$node_ymin[3] - .5
graph_data$nodes$node_ymin[5] <- graph_data$nodes$node_ymin[5] - .5

graph_data$nodes$node_ymax[2] <- graph_data$nodes$node_ymax[2] + .5
graph_data$nodes$node_ymax[3] <- graph_data$nodes$node_ymax[3] + .5
graph_data$nodes$node_ymax[5] <- graph_data$nodes$node_ymax[5] + .5



# graph_data$nodes$node_xmin <- graph_data$nodes$node_xmin - .5
# graph_data$nodes$node_xmax <- graph_data$nodes$node_xmax + .5





graph_data$nodes$size <- 2.5
graph_data$edges$size <- 2.5

graph_data$edges$label_size <- 8.5
graph_data$nodes$label_size <- 8.5


graph_data$edges$color <- c("#AAADB0", "#AAADB0", "black", "#AAADB0", "#AAADB0" ,"#AAADB0", "black", "#AAADB0", "#AAADB0" )  
graph_data$edges$label_color <- c("#AAADB0", "#AAADB0", "black", "#AAADB0", "#AAADB0","#AAADB0", "black", "#AAADB0", "#AAADB0" )  

graph_data$nodes$color <- c("black", "#AAADB0", "black", "black",  "black")  
graph_data$nodes$label_color <- c("black", "#AAADB0", "black", "black",  "black")  


scale <- 10
png(paste0("./INSAR2025/OR_poster_plot_greyed", Sys.Date(), ".png"), width = 1080*scale, height = 760*scale, res=72*scale)
plot(graph_data)
dev.off()



