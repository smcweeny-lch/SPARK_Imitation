rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(tidySEM)

data2 <- read_rds("data/factor_score_estimates_constructs_symbolism_2_10_25.rds")

data2 <- data2 %>% select(-Symbolism)

set.seed(123)
split <- sample(1:nrow(data2), size = nrow(data2)*.5,replace = F)

train <- data2[split,]
test <- data2[-split,]


# alternatives to the symbolism construct
symbolism_syntax11 <- 'Symbolism=~Imag.Play+VL1.F1+NVC.F1'
symbolism_syntax12 <- 'Symbolism=~Imag.Play+VL1.F1+NVC.F2'
symbolism_syntax21 <- 'Symbolism=~Imag.Play+VL1.F2+NVC.F1'
symbolism_syntax22 <- 'Symbolism=~Imag.Play+VL1.F2+NVC.F2'



symbolism11.cfa <- lavaan::cfa(symbolism_syntax11, data = train, meanstructure = T, std.lv = T)
symbolism12.cfa <- lavaan::cfa(symbolism_syntax12, data = train, meanstructure = T, std.lv = T)


symbolism21.cfa <- lavaan::cfa(symbolism_syntax21, data = train, meanstructure = T, std.lv = T)
symbolism22.cfa <- lavaan::cfa(symbolism_syntax22, data = train, meanstructure = T, std.lv = T)


# semPaths(
#   symbolism11.cfa, 
#   nCharNodes = 3, 
#   layout = "tree3", 
#   sizeMan = 6,       # Reduce size of manifest variables
#   sizeMan2 = 4,      # Further reduce size of the rectangles
#   label.cex = 0.7,   # Shrink label size to fit text better 
#   what = "std", 
#   intercepts = F, 
#   residuals = F, 
#   thresholds = F
# )
# semPaths(
#   symbolism12.cfa, 
#   nCharNodes = 3, 
#   layout = "tree3", 
#   sizeMan = 6,       # Reduce size of manifest variables
#   sizeMan2 = 4,      # Further reduce size of the rectangles
#   label.cex = 0.7,   # Shrink label size to fit text better 
#   what = "std", 
#   intercepts = F, 
#   residuals = F, 
#   thresholds = F
# )

#nulls <- readRDS("./code/Empirical_priors/R_and_P_pseudo_construct_all_constraints_3_27_25.rds")
nulls <- readRDS("./code/Empirical_priors/R_and_P/output/R_and_P_pseudo_construct_all_constraints_4_22_25.rds")
summary_table <- nulls %>% group_by(combo) %>%
  filter(Fs == 11) %>%
  summarize(median_beta = median(correlation_value),
            sd_beta = sd(correlation_value))

summary_table$b <- c(1,4,2,3)
summary_table %>% arrange(b)

# RP.1991.syntax <- '
# Affect.Sharing ~ b1*Imitation
# Symbolism ~ b2*Imitation
# Social.Eng ~ b3*Affect.Sharing + b4*Imitation
# 
# b1_adj := b1 - .360
# b2_adj := b2 - .405
# b3_adj := b3 - .711
# b4_adj := b4 - .275
# '

data2$Symbolism <- lavPredict(symbolism11.cfa, data2)  

data2$Symbolism <- scale(data2$Symbolism)


RP.1991.syntax <- '
Social.Eng ~ b3*Affect.Sharing + b4*Imitation
Affect.Sharing ~ b1*Imitation
Symbolism ~ b2*Imitation

b1_adj := b1 - .356
b2_adj := b2 - .384
b3_adj := b3 - .567
b4_adj := b4 - .249
'

RP.mod <- sem(model = RP.1991.syntax, 
              data = data2 )


summary(RP.mod, standardized = T)

# med.mod <- psych::mediate(Social.Eng ~ Imitation + (Affect.Sharing), data = data2)
# summary(med.mod)
# 
# 
# lm.mod <- lm(Social.Eng ~ Affect.Sharing + Imitation, data = data2)
# summary(lm.mod)
# estimates <- parameterEstimates(RP.mod)
summary(RP.mod, fit.measures = T)



data2$Symbolism <- lavPredict(symbolism12.cfa, data2)  

data2$Symbolism <- scale(data2$Symbolism)


summary_table <- nulls %>% group_by(combo) %>% 
  filter(Fs == 12) %>%
  summarize(median_beta = round(median(correlation_value),3),
            sd_beta = round(sd(correlation_value),3))


summary_table$b <- c(1,4,2,3)
summary_table %>% arrange(b)

# RP.1991.syntax <- '
# Affect.Sharing ~ b1*Imitation
# Symbolism ~ b2*Imitation
# Social.Eng ~ b3*Affect.Sharing + b4*Imitation
# 
# b1_adj := b1 - .360
# b2_adj := b2 - .405
# b3_adj := b3 - .711
# b4_adj := b4 - .275
# '

RP.1991.syntax.12 <- '
Affect.Sharing ~ b1*Imitation
Symbolism ~ b2*Imitation
Social.Eng ~ b3*Affect.Sharing + b4*Imitation

b1_adj := b1 - .356
b2_adj := b2 - .357
b3_adj := b3 - .595
b4_adj := b4 - .238
'

RP.mod.12 <- sem(model = RP.1991.syntax.12, 
              data = data2 )


summary(RP.mod.12, standardized = T)

estimates <- parameterEstimates(RP.mod.12)
summary(RP.mod.12, fit.measures = T)








data2$Symbolism <- lavPredict(symbolism21.cfa, data2)  

data2$Symbolism <- scale(data2$Symbolism)


summary_table <- nulls %>% group_by(combo) %>%
  filter(Fs == 21) %>%
  summarize(median_beta = median(correlation_value),
            sd_beta = sd(correlation_value))

summary_table$b <- c(1,4,2,3)
summary_table %>% arrange(b)

# RP.1991.syntax <- '
# Affect.Sharing ~ b1*Imitation
# Symbolism ~ b2*Imitation
# Social.Eng ~ b3*Affect.Sharing + b4*Imitation
# 
# b1_adj := b1 - .360
# b2_adj := b2 - .405
# b3_adj := b3 - .711
# b4_adj := b4 - .275
# '

RP.1991.syntax.21 <- '
Affect.Sharing ~ b1*Imitation
Symbolism ~ b2*Imitation
Social.Eng ~ b3*Affect.Sharing + b4*Imitation

b1_adj := b1 - .356
b2_adj := b2 - .365
b3_adj := b3 - .589
b4_adj := b4 - .236
'



RP.mod.21 <- sem(model = RP.1991.syntax.21, 
                 data = data2 )


summary(RP.mod.21, standardized = T)

estimates <- parameterEstimates(RP.mod.21)
summary(RP.mod.21, fit.measures = T)





data2$Symbolism <- lavPredict(symbolism22.cfa, data2)  

data2$Symbolism <- scale(data2$Symbolism)


summary_table <- nulls %>% group_by(combo) %>%
  filter(Fs == 22) %>%
  summarize(median_beta = median(correlation_value),
            sd_beta = sd(correlation_value))

summary_table$b <- c(1,4,2,3)
summary_table %>% arrange(b)

# RP.1991.syntax <- '
# Affect.Sharing ~ b1*Imitation
# Symbolism ~ b2*Imitation
# Social.Eng ~ b3*Affect.Sharing + b4*Imitation
# 
# b1_adj := b1 - .360
# b2_adj := b2 - .405
# b3_adj := b3 - .711
# b4_adj := b4 - .275
# '

RP.1991.syntax.22 <- '
Affect.Sharing ~ b1*Imitation
Symbolism ~ b2*Imitation
Social.Eng ~ b3*Affect.Sharing + b4*Imitation

b1_adj := b1 - .356
b2_adj := b2 - .353
b3_adj := b3 - .608
b4_adj := b4 - .225
'



RP.mod.22 <- sem(model = RP.1991.syntax.22, 
                 data = data2 )



estimates <- parameterEstimates(RP.mod.22)
summary(RP.mod.22, fit.measures = T)






# 
# RP_layout <- array(dim = c(5,5), NA)
# RP_layout[3,1] <- "Imitation"
# RP_layout[3,3] <- "Affect.Sharing"
# RP_layout[1,5] <- "Symbolism"
# RP_layout[5,5] <- "Social.Eng"
# 
# graph_data <- prepare_graph(RP.mod, layout = RP_layout, text_size = 8)
# 
# graph_data$edges <- graph_data$edges %>%
#   mutate(show = ifelse(from == to, F, T))
#   # %>% mutate(curvature = NA,
#   #        linetype = 1,
#   #        arrow = "last")
# 
# # graph_data$edges$curvature[graph_data$edges$from == "Perception" & graph_data$edges$to == "RC"] <- 50
# # graph_data$edges$curvature[graph_data$edges$from == "Perception" & graph_data$edges$to == "VL"] <- -50
# 
# graph_data$edges$curvature[graph_data$edges$from == "Social.Eng" & graph_data$edges$to == "Symbolism"] <- 100
# graph_data$edges$linetype[graph_data$edges$from == "Social.Eng" & graph_data$edges$to == "Symbolism"] <- 2
# graph_data$edges$arrow[graph_data$edges$from == "Social.Eng" & graph_data$edges$to == "Symbolism"] <- "both"
# 
# graph_data$edges$size <- 1
# 
# 
# graph_data$nodes$shape <- "" 
# graph_data$nodes$label <- ifelse(graph_data$nodes$label  == "Affect.Sharing", "Affect\nSharing", graph_data$nodes$label) 
# 
# 
# graph_data$edges$label[graph_data$edges$label_results == "Affect.Sharing.ON.Imitation"] <- "0.43"
# graph_data$edges$label[graph_data$edges$label_results == "Social.Eng.ON.Imitation"] <- "0.31"
# graph_data$edges$label[graph_data$edges$label_results == "Symbolism.ON.Imitation"] <- "0.49"
# graph_data$edges$label[graph_data$edges$label_results == "Social.Eng.ON.Affect.Sharing"] <- "0.47"
# graph_data$edges$label[graph_data$edges$label_results == "Symbolism.WITH.Social.Eng.WITH"] <- "0.35"
# 
# 
# 
# graph_data$edges

# graph_data$edges$label[graph_data$edges$label_results == "Affect.Sharing.ON.Imitation"] <- "0.35"
# graph_data$edges$label[graph_data$edges$label_results == "Social.Eng.ON.Imitation"] <- "0.25"
# graph_data$edges$label[graph_data$edges$label_results == "Symbolism.ON.Imitation"] <- "0.38"
# graph_data$edges$label[graph_data$edges$label_results == "Social.Eng.ON.Affect.Sharing"] <- "0.46"
# graph_data$edges$label[graph_data$edges$label_results == "Social.Eng.WITH.Symbolism"] <- "0.48"

#


RP_layout <- array(dim = c(5,5), NA)
RP_layout[3,1] <- "Imitation"
RP_layout[3,3] <- "Affect.Sharing"
RP_layout[2,5] <- "Symbolism"
RP_layout[4,5] <- "Social.Eng"


graph_data <- prepare_graph(RP.mod, layout = RP_layout)

graph_data$edges <- graph_data$edges %>%
  mutate(show = ifelse(from == to, F, T))

graph_data$edges$show[8] <- FALSE
graph_data$edges$size <- 2.5

graph_data$edges$label_size <- 9.2

graph_data$edges$connect_from[3] <- "right"

 
graph_data$nodes$label <- ifelse(graph_data$nodes$label  == "Affect.Sharing", "Affect\nSharing", graph_data$nodes$label) 
#graph_data$nodes$label <- ifelse(graph_data$nodes$label  == "Social.Eng", "Social\nEngagement", graph_data$nodes$label) 
graph_data$nodes$label <- ifelse(graph_data$nodes$label  == "Social.Eng", "Joint Attn +\nPragmatics", graph_data$nodes$label) 


graph_data$edges$label[1] <- as.character(round(parametertable(RP.mod)$est[12], 2))
graph_data$edges$label[2] <- as.character(round(parametertable(RP.mod)$est[13], 2))
graph_data$edges$label[3] <- as.character(round(parametertable(RP.mod)$est[10], 2))
graph_data$edges$label[4] <- as.character(round(parametertable(RP.mod)$est[11], 2))

graph_data$edges$label <- ifelse(graph_data$edges$label %in% c("0.1", "-0.1"), paste0(graph_data$edges$label, "0"), graph_data$edges$label)

# graph_data$edges$label[1:4] <- paste0(graph_data$edges$label[1:4], c("***", "", "", "***"))
graph_data$edges$label[1:4] <- paste0(graph_data$edges$label[1:4], c("†", "", "", ""))


graph_data$nodes$size <- 2.5
graph_data$nodes$label_size <- 9.2



scale<-10
png(paste0("./Manuscript/plots/RP_1991_beta_adj_", Sys.Date(), ".png"), width = 1440*scale, height = 720*scale, res=72*scale)
plot(graph_data)
dev.off()


prior_graph_data <- graph_data

round(summary_table$median_beta, 2)

prior_graph_data$edges$label[prior_graph_data$edges$label_results == "Affect.Sharing.ON.Imitation"] <- "0.36"
prior_graph_data$edges$label[prior_graph_data$edges$label_results == "Social.Eng.ON.Imitation"] <- "0.25"
prior_graph_data$edges$label[prior_graph_data$edges$label_results == "Symbolism.ON.Imitation"] <- "0.38"
prior_graph_data$edges$label[prior_graph_data$edges$label_results == "Social.Eng.ON.Affect.Sharing"] <- "0.57"
#prior_graph_data$edges$label[prior_graph_data$edges$label_results == "Social.Eng.WITH.Symbolism"] <- "0.48"

prior_graph_data$edges$color <- "grey"
prior_graph_data$nodes$color <- "grey"


scale <- 10
png(paste0("./Manuscript/Supplemental Materials/RP_1991_AllConstraints_Null", Sys.Date(), ".png"), width = 1440*scale, height = 720*scale, res=72*scale)
plot(prior_graph_data)
dev.off()


#####

RP_layout <- array(dim = c(5,5), NA)
RP_layout[3,1] <- "Imitation"
RP_layout[3,3] <- "Affect.Sharing"
RP_layout[2,5] <- "Symbolism"
RP_layout[4,5] <- "Social.Eng"

graph_data <- prepare_graph(RP.mod, layout = RP_layout)

graph_data$edges <- graph_data$edges %>%
  mutate(show = ifelse(from == to, F, T))

graph_data$edges$show[8] <- FALSE
graph_data$edges$size <- 3.5

graph_data$edges$label_size <- 12

graph_data$edges$connect_from[3] <- "right"


graph_data$nodes$label <- ifelse(graph_data$nodes$label  == "Affect.Sharing", "Affect\nSharing", graph_data$nodes$label) 
graph_data$nodes$label <- ifelse(graph_data$nodes$label  == "Social.Eng", "Social\nEngagement", graph_data$nodes$label) 


graph_data$edges$label[1:4] <- as.character(round(parametertable(RP.mod)$est[10:13], 2))

graph_data$edges$label <- ifelse(graph_data$edges$label %in% c("0.1", "-0.1"), paste0(graph_data$edges$label, "0"), graph_data$edges$label)

# graph_data$edges$label[1:4] <- paste0(graph_data$edges$label[1:4], c("***", "", "", "***"))
graph_data$edges$label[1:4] <- paste0(graph_data$edges$label[1:4], c("*", "*", "", "*"))



graph_data$nodes$x[1] <- graph_data$nodes$x[1] + 1
graph_data$nodes$node_xmin[1] <- graph_data$nodes$node_xmin[1] + 1
graph_data$nodes$node_xmax[1] <- graph_data$nodes$node_xmax[1] + 1

graph_data$nodes$node_xmin <- graph_data$nodes$node_xmin -.5
graph_data$nodes$node_xmax <- graph_data$nodes$node_xmax +.5








graph_data$nodes$size <- 3.5
graph_data$nodes$label_size <- 12

graph_data$edges$color <- c("black", "black", "#AAADB0", "black", rep("", 5))

graph_data$edges$label_color <- c("black", "black", "#AAADB0", "black", rep("", 5))

graph_data$nodes$color <- c(rep("black", 4))
graph_data$nodes$label_color  <- c(rep("black", 4))



scale <- 10
png(paste0("./INSAR2025/RP_1991_poster_plot_greyed", Sys.Date(), ".png"), width = 1440*scale, height = 720*scale, res=72*scale)
plot(graph_data)
dev.off()


