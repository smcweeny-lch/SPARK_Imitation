rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(tidySEM)


data2 <- read_rds("data/factor_score_estimates_constructs_symbolism_2_10_25.rds")



# nulls <- readRDS("./code/Empirical_priors/M_and_E/output/M_and_E_pseudo_construct_full_constraints_4_21_25.rds")
nulls <- readRDS("./code/Empirical_priors/M_and_E/output/M_and_E_pseudo_construct_full_constraints_8_11_25.rds")
# nulls <- readRDS("./code/Empirical_priors/M_and_E/M_and_E_pseudo_construct_reliability_constraints_4_21_25.rds")


# nulls <- readRDS("./code/Empirical_priors/M_and_E_pseudo_construct_all_constraints_3_27_25.rds")

#nulls <- readRDS("./code/Empirical_priors/M_and_E_pseudoImi_realMotSoc_all_constraints_4_7_25.rds")
summary_table <- nulls %>% group_by(combo) %>%
  summarize(#mean_beta = mean(correlation_value),
            median_beta = median(correlation_value),
            sd_beta = sd(correlation_value))


ME.2011.syntax <- 'Motor.Skills ~ b1*Imitation
                  Social.Skills ~ b2*Imitation
                  Motor.Skills ~~ phi*Social.Skills
                  
                  b1_adj := b1 - 0.093       
                  b2_adj := b2 - 0.307       
                  phi_adj := phi - 0.212       
                  
'
# .212 was .198


ME.mod <- sem(model = ME.2011.syntax, 
              data = data2)


saveRDS(ME.mod, "./code/Results_Analyses/lavaan_models/M_and_E/ME_mod.rds")

estimates <- parameterEstimates(ME.mod, standardized = T)


summary(ME.mod, standardized = T, fit.measures = T)



# hist((nulls %>% filter(combo == "Motor.Skills*Imitation"))$correlation_value, xlim = c(0, .3),
#      main = "Motor Skills * Imitation Pseudo-Betas", xlab = "Pseudo-Beta")
# abline(v = .264, col = "red")
# text("Real Beta ->", x = .23, y = 10, col = "red")
# abline(v = quantile((nulls %>% filter(combo == "Motor.Skills*Imitation"))$correlation_value, .95), lty = 2, lwd = 2)
# text("<- alpha = .05", x = .2, y = 30)
# 
# 
# 
# hist((nulls %>% filter(combo == "Social.Skills*Imitation"))$correlation_value, xlim = c(0, .8),
#      main = "Social Skills * Imitation Pseudo-Betas", xlab = "Pseudo-Beta")
# abline(v = estimates$est[estimates$label == "b2"], col = "red")
# text("<- Real Beta", x = .62, y = 50, col = "red")
# abline(v = quantile((nulls %>% filter(combo == "Social.Skills*Imitation"))$correlation_value, .95), lty = 2, lwd = 2)
# text("<- alpha = .05", x = .7, y = 70)
# 



# get_layout(ME.mod, layout_algorithm = "layout_on_grid")
# ME_layout <- array(dim = c(5,5), NA)
# ME_layout[3,1] <- "Imitation"
# ME_layout[1,5] <- "Motor.Skills"
# ME_layout[5,5] <- "Social.Skills"
# 
# graph_data <- prepare_graph(ME.mod, layout = ME_layout)
# 
# graph_data$edges <- graph_data$edges %>% 
#   filter(from != to) 
# 
# 
# cor.test(data2$Motor.Skills, data2$Social.Skills)
# 
# summary(ME.mod, fit.measures = T)
# 
# 
# 
# get_layout(ME.mod, layout_algorithm = "layout_on_grid")
# ME_layout <- array(dim = c(5,5), NA)
# ME_layout[3,1] <- "Imitation"
# ME_layout[1,5] <- "Motor.Skills"
# ME_layout[5,5] <- "Social.Skills"
# 
# graph_data <- prepare_graph(ME.mod, layout = ME_layout)
# 
# graph_data$edges <- graph_data$edges %>%
#   filter(from != to)
# 
# # manual change for graph re: residual covariance issue (lavaan is NOT residual cov)
# # see corr proof that the residual corr is .26
# cor(lm(Motor.Skills ~ Imitation, data = data2)$residuals, lm(Social.Skills ~ Imitation, data = data2)$residuals)
# cor(lm(Motor.Skills ~ Imitation, data = data2)$residuals, data2$Social.Skills)
# 
# 
# 
# # graph_data$edges$curvature[graph_data$edges$from == "RC" & graph_data$edges$to == "VL"] <- 50
# # graph_data$edges$linetype[graph_data$edges$from == "RC" & graph_data$edges$to == "VL"] <- 2
# # graph_data$edges$arrow[graph_data$edges$from == "RC" & graph_data$edges$to == "VL"] <- "both"
# 
# 
# scale<-5
# png("./Model_DAG_evaluations/ME_2011_DAG.png", width = 720*scale, height = 480*scale, res=72*scale)
# plot(graph_data)
# dev.off()





get_layout(ME.mod, layout_algorithm = "layout_on_grid")
ME_layout <- array(dim = c(5,5), NA)
ME_layout[2,1] <- "Imitation"
ME_layout[1,3] <- "Motor.Skills"
ME_layout[3,3] <- "Social.Skills"

graph_data <- prepare_graph(ME.mod, layout = ME_layout)

graph_data$edges <- graph_data$edges %>%
  filter(from != to)

graph_data$edges$label <- as.character(round(parameterestimates(ME.mod, standardized = T)$std.all[7:9], 2))
graph_data$edges$label <- paste0(graph_data$edges$label, c("*", "*", "*"))

# raw_corr_row <- c(from = "Motor.Skills", to = "Social.Skills", arrow = "none", label = "0.12***", connect_from = "bottom", connect_to = "top", curvature = 10,
#                           linetype = 1, lhs = "Motor.Skills", op = "~~", rhs = "Social.Skills", est = .12, se = .01, pval = 0.00, 
#                           confint = NA, est_sig = NA, est_std = NA, se_std = NA, pval_std = NA,  confint_std = NA, est_sig_std = NA, label_results = "Motor.Skills.WITH.Social.Skills", lavaan_label = NA, show = TRUE)

graph_data$edges$connect_from[3] <- "right"
graph_data$edges$connect_to[3] <- "right"



graph_data$edges <- rbind(graph_data$edges, graph_data$edges[3,])
graph_data$edges$connect_from[4] <- "right"
graph_data$edges$connect_to[4] <- "right"
graph_data$edges$curvature[4] <- 10
graph_data$edges$linetype[4] <- 1
# .35 (real) - .23 (pseudo)
graph_data$edges$label[4] <- "0.12*"


graph_data$edges$size <- 1.5
graph_data$edges$label_size <- 5


graph_data$nodes$size <- 1.5
graph_data$nodes$label_size <- 5.5
graph_data$nodes$label[2] <- "Motor Skills"
graph_data$nodes$label[3] <- "Social Skills"

scale <- 10
png(paste0("./Manuscript/plots/ME_2011_beta_adj_", Sys.Date(), ".png"), width = 720*scale, height = 360*scale, res=72*scale)
plot(graph_data)
dev.off()




# 
# 
# 
# 
# 
# 
# 
# cor(lm(Social.Skills ~ Imitation, data=data2)$residuals, 
#     lm(Motor.Skills ~ Imitation, data=data2)$residuals)
# 
# 
# summary_table$median_beta
# prior_graph_data <- graph_data
# 
# prior_graph_data$edges$label[1] <- "0.09"
# prior_graph_data$edges$label[2] <- "0.31"
# prior_graph_data$edges$label[3] <- "0.20"
# prior_graph_data$edges$label[4] <- "0.23"
# 
# prior_graph_data$edges$color <- "grey"
# prior_graph_data$nodes$color <- "grey"
# 
# png(paste0("./Manuscript/Supplemental Materials/ME_2011_AllConstraints_Null", Sys.Date(), ".png"), width = 720*scale, height = 480*scale, res=72*scale)
# plot(prior_graph_data)
# dev.off()
# 
# 
# 
# 
# ####
# 
# ME_layout <- array(dim = c(5,5), NA)
# ME_layout[2,1] <- "Imitation"
# ME_layout[1,3] <- "Motor.Skills"
# ME_layout[3,3] <- "Social.Skills"
# 
# graph_data <- prepare_graph(ME.mod, layout = ME_layout)
# 
# graph_data$edges <- graph_data$edges %>%
#   filter(from != to)
# 
# graph_data$edges$label <- as.character(round(parametertable(ME.mod)$est[7:9], 2))
# graph_data$edges$label <- paste0(graph_data$edges$label, c("***","***",""))
# 
# # raw_corr_row <- c(from = "Motor.Skills", to = "Social.Skills", arrow = "none", label = "0.12***", connect_from = "bottom", connect_to = "top", curvature = 10,
# #                           linetype = 1, lhs = "Motor.Skills", op = "~~", rhs = "Social.Skills", est = .12, se = .01, pval = 0.00, 
# #                           confint = NA, est_sig = NA, est_std = NA, se_std = NA, pval_std = NA,  confint_std = NA, est_sig_std = NA, label_results = "Motor.Skills.WITH.Social.Skills", lavaan_label = NA, show = TRUE)
# 
# graph_data$edges$connect_from[3] <- "right"
# graph_data$edges$connect_to[3] <- "right"
# 
# 
# 
# graph_data$edges <- rbind(graph_data$edges, graph_data$edges[3,])
# graph_data$edges$connect_from[4] <- "right"
# graph_data$edges$connect_to[4] <- "right"
# graph_data$edges$curvature[4] <- 10
# graph_data$edges$linetype[4] <- 1
# graph_data$edges$label[4] <- "0.12***"
# 
# 
# graph_data$nodes$node_xmin <- graph_data$nodes$node_xmin - .3
# graph_data$nodes$node_xmax <- graph_data$nodes$node_xmax + .3
# 
# 
# 
# graph_data$edges$size <- 2
# graph_data$edges$label_size <- 7.5
# 
# 
# 
# graph_data$nodes$size <- 2
# graph_data$nodes$label_size <- 7.5
# graph_data$nodes$label[2] <- "Motor Skills"
# graph_data$nodes$label[3] <- "Social Skills"
# 
# scale <- 10
# png(paste0("./INSAR2025/ME_2011_poster_plot", Sys.Date(), ".png"), width = 720*scale, height = 360*scale, res=72*scale)
# plot(graph_data)
# dev.off()


# 
# 
# 
# ########
# get_layout(OR.mod.drop1, layout_algorithm = "layout_on_grid")
# OR_layout <- array(dim = c(5,5), NA)
# OR_layout[3,1] <- "Perception"
# OR_layout[2,3] <- "Imitation"
# OR_layout[4,3] <- "Self"
# OR_layout[1,5] <- "RC"
# OR_layout[5,5] <- "VL"
# 
# graph_data <- prepare_graph(OR.mod.drop1, layout = OR_layout)
# 
# graph_data$edges <- graph_data$edges %>% 
#   filter(from != to) %>% 
#   mutate(curvature = NA,
#          linetype = 1,
#          arrow = "last")
# 
# graph_data$edges$curvature[graph_data$edges$from == "Perception" & graph_data$edges$to == "RC"] <- 50
# graph_data$edges$curvature[graph_data$edges$from == "Perception" & graph_data$edges$to == "VL"] <- -50
# 
# graph_data$edges$curvature[graph_data$edges$from == "RC" & graph_data$edges$to == "VL"] <- 50
# graph_data$edges$linetype[graph_data$edges$from == "RC" & graph_data$edges$to == "VL"] <- 2
# graph_data$edges$arrow[graph_data$edges$from == "RC" & graph_data$edges$to == "VL"] <- "both"
# 
# 
# plot(graph_data)
# 
# dev.new()