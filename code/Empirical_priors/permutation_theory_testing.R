rm(list = ls())
library(tidyverse)
#nulls <- readRDS("./code/Empirical_priors/R_and_P_pseudo_construct_all_constraints_3_27_25.rds")
nulls <- readRDS("./code/Empirical_priors/R_and_P/R_and_P_pseudo_construct_all_constraints_4_22_25.rds")

nulls <- readRDS("./code/Empirical_priors/R_and_P/R_and_P_pseudo_construct_no_constraints_4_28_25.rds")
nulls <- readRDS("./code/Empirical_priors/R_and_P/R_and_P_pseudo_construct_reliability_constraints_4_28_25.rds")

#nulls <- readRDS("./code/Empirical_priors/R_and_P_pseudo_construct_all_constraints_4_10_25.rds")


summary_table <- nulls %>% group_by(combo) %>% 
  filter(Fs == 11) %>%
  summarize(median_beta = round(median(correlation_value),2),
            sd_beta = round(sd(correlation_value),2))

summary_table

real_b_imi_aff <- .433 # Imi -> Affect.Sharing
real_b_imi_sym <- .397 # Imi -> Symbolism
real_b_aff_soc <- .467 # Aff -> Social.Eng
real_b_imi_soc <- .306 # Imi -> Social.Eng


mean((nulls %>% filter(combo == "Imitation*Affect.Sharing" & Fs == "11"))$correlation_value > real_b_imi_aff)
mean((nulls %>% filter(combo == "Imitation*Symbolism.11" & Fs == "11"))$correlation_value > real_b_imi_sym)
mean((nulls %>% filter(combo == "Social.Eng*Affect.Sharing" & Fs == "11"))$correlation_value > real_b_aff_soc)
mean((nulls %>% filter(combo == "Imitation*Social.Eng" & Fs == "11"))$correlation_value > real_b_imi_soc)




mean((nulls %>% filter(combo == "Imitation*Affect.Sharing" & Fs == "12"))$correlation_value > parameterestimates(RP.mod.12)$est[parameterestimates(RP.mod.12)$label == "b1"])
mean((nulls %>% filter(combo == "Imitation*Symbolism.12" & Fs == "12"))$correlation_value > parameterestimates(RP.mod.12)$est[parameterestimates(RP.mod.12)$label == "b2"])
mean((nulls %>% filter(combo == "Social.Eng*Affect.Sharing" & Fs == "12"))$correlation_value > parameterestimates(RP.mod.12)$est[parameterestimates(RP.mod.12)$label == "b3"])
mean((nulls %>% filter(combo == "Imitation*Social.Eng" & Fs == "12"))$correlation_value > parameterestimates(RP.mod.12)$est[parameterestimates(RP.mod.12)$label == "b4"])



mean((nulls %>% filter(combo == "Imitation*Affect.Sharing" & Fs == "21"))$correlation_value > parameterestimates(RP.mod.21)$est[parameterestimates(RP.mod.21)$label == "b1"])
mean((nulls %>% filter(combo == "Imitation*Symbolism.21" & Fs == "21"))$correlation_value > parameterestimates(RP.mod.21)$est[parameterestimates(RP.mod.21)$label == "b2"])
mean((nulls %>% filter(combo == "Social.Eng*Affect.Sharing" & Fs == "21"))$correlation_value > parameterestimates(RP.mod.21)$est[parameterestimates(RP.mod.21)$label == "b3"])
mean((nulls %>% filter(combo == "Imitation*Social.Eng" & Fs == "21"))$correlation_value > parameterestimates(RP.mod.21)$est[parameterestimates(RP.mod.21)$label == "b4"])



mean((nulls %>% filter(combo == "Imitation*Affect.Sharing" & Fs == "22"))$correlation_value > parameterestimates(RP.mod.22)$est[parameterestimates(RP.mod.22)$label == "b1"])
mean((nulls %>% filter(combo == "Imitation*Symbolism.22" & Fs == "22"))$correlation_value > parameterestimates(RP.mod.22)$est[parameterestimates(RP.mod.22)$label == "b2"])
mean((nulls %>% filter(combo == "Social.Eng*Affect.Sharing" & Fs == "22"))$correlation_value > parameterestimates(RP.mod.22)$est[parameterestimates(RP.mod.22)$label == "b3"])
mean((nulls %>% filter(combo == "Imitation*Social.Eng" & Fs == "22"))$correlation_value > parameterestimates(RP.mod.22)$est[parameterestimates(RP.mod.22)$label == "b4"])





ggplot(nulls, aes(x = correlation_value)) +
  geom_histogram() +
  theme_bw() + 
  facet_wrap(~factor(combo), nrow = 1)


RP1_hist <- ggplot(nulls %>% filter(combo == "Imitation*Social.Eng" & Fs == "11"),
                   aes(x = correlation_value)) +
  geom_histogram(binwidth = 0.01, fill = "grey", color = "black") +
  geom_vline(aes(xintercept = quantile(correlation_value, 0.95), color = "p = .05", linetype = "p = .05"), linewidth = .5) +
  geom_vline(aes(xintercept = quantile(correlation_value, 0.5), color = "p = .50", linetype = "p = .50"), linewidth = 1) +
  geom_vline(aes(xintercept = real_b_imi_soc, color = "Real slope", linetype = "Real slope"), linewidth = 1) +
  scale_color_manual(name = "Legend", values = c("p = .05" = "black", "p = .50" = "black", "Real slope" = "red")) +
  scale_linetype_manual(name = "Legend", values = c("p = .05" = "dashed", "p = .50" = "solid", "Real slope" = "solid")) +
  theme_minimal()

print(RP1_hist)


permutation_gg <-  function(nulls, real_param, combos, Fss, title, 
                            strip.axis = T, strip.legend = T, title.size = 16, binwidth = .01){
  graph <- ggplot(nulls %>% filter(combo == combos & Fs == Fss),
         aes(x = correlation_value)) +
    geom_histogram(binwidth = binwidth, fill = "grey", color = "black") +
    geom_vline(aes(xintercept = quantile(correlation_value, 0.95), color = "p = .05", linetype = "p = .05"), linewidth = 1.2) +
    geom_vline(aes(xintercept = quantile(correlation_value, 0.5), color = "p = .50", linetype = "p = .50"), linewidth = 1.2) +
    geom_vline(aes(xintercept = real_param, color = "Real slope", linetype = "Real slope"), linewidth = 1.2) +
    scale_color_manual(name = "Legend", values = c("p = .05" = "black", "p = .50" = "black", "Real slope" = "red")) +
    scale_linetype_manual(name = "Legend", values = c("p = .05" = "dashed", "p = .50" = "solid", "Real slope" = "solid")) +
    theme_minimal() +
    ggtitle(title)+
    theme(title = element_text(size = title.size))
  

  
  if(strip.axis == T){
   graph <- graph + 
     theme(axis.title.x=element_blank(), 
           axis.title.y=element_blank()) 
  }
  if(strip.legend == T) {
    graph <- graph + theme(legend.position = "none")
    
  }
  return(graph)
}

gg1 <- permutation_gg(nulls = nulls, real_b_aff_soc, combos = "Social.Eng*Affect.Sharing",
                      Fss = "11", title = "Affect Sharing → Joint Attention + Pragmatics")
gg2 <- permutation_gg(nulls = nulls, real_b_imi_aff, combos = "Imitation*Affect.Sharing", 
                      Fss = "11", title = "Imitation → Affect Sharing")
gg3 <- permutation_gg(nulls = nulls, real_b_imi_soc, combos = "Imitation*Social.Eng",  
                      Fss = "11", title = "Imitation → Joint Attention + Pragmatics")
gg4 <- permutation_gg(nulls = nulls, real_b_imi_sym, combos = "Imitation*Symbolism.11", 
                      Fss = "11", title = "Imitation → Symbolism")

library(gridExtra)
library(grid)

xaxis.grob <- grid::textGrob("β", gp = gpar(fontsize = 14, fontface = "italic"))
yaxis.grob <- grid::textGrob("Frequency", rot = 90, gp = gpar(fontsize = 14))
full_legend <- cowplot::get_legend(RP1_hist)
title.grob <- grid::textGrob("Rogers & Pennington: Permutation Testing versus Fully Constrained Monte Carlo Simulations", gp = gpar(fontsize = 18))

plot_grid <- grid.arrange(nrow = 2, ncol = 2, grobs = list(gg1, gg2, gg3, gg4))

scale <- 10
png("./Manuscript/Supplemental Materials/R_P_Permutation_Plot.png", width = 1080*scale, height = 720*scale, res = 72*scale)
grid.arrange(
  arrangeGrob(
    title.grob
  ),
  
  arrangeGrob(
    yaxis.grob, plot_grid, full_legend, ncol = 3, widths = c(0.05, 1, .1)
  ),
  arrangeGrob(
    nullGrob(), xaxis.grob, ncol = 2, widths = c(0.05, 1)
  ),
  heights = c(.05, 1, 0.05)
)
dev.off()











# nulls <- readRDS("./code/Empirical_priors/O_and_R_pseudo_construct_all_constraints_3_24_25.rds")
nulls <- readRDS("./code/Empirical_priors/O_and_R/O_and_R_pseudo_construct_all_constraints_4_21_25.rds")
nulls <- readRDS("./code/Empirical_priors/O_and_R/O_and_R_pseudo_construct_reliability_constraints_4_28_25.rds")
nulls <- readRDS("./code/Empirical_priors/O_and_R/O_and_R_pseudo_construct_no_constraints_4_28_25.rds")

summary_table <- nulls %>% group_by(combo) %>%
  filter(Fs == 11) %>%
  summarize(median_beta = median(correlation_value),
            sd_beta = sd(correlation_value)) %>%
  filter(!str_detect(combo, ".F2"))

summary_table$b <- c(2, 3, 4, 7, 1, 5, 6, 8) 
summary_table %>% arrange(b)


b_self_perc <- .310
b_imi_perc <- .107
b_imi_RC <- .413
b_imi_VL <- .037 
b_self_RC <- .389
b_self_VL <- .333
b_VL_perc <- .153
b_RC_perc <- .132





# b_self_perc <- .310
# b_imi_perc <- .107
# b_imi_RC <- .413
# b_self_RC <- .389
# b_RC_perc <- .132
# b_imi_VL <- .037 
# b_self_VL <- .333
# b_VL_perc <- .153





mean((nulls %>% filter(combo == "Self.Nonself*Perceptual.Inconstancy" & Fs == "11"))$correlation_value > b_self_perc)
mean((nulls %>% filter(combo == "Imitation*Perceptual.Inconstancy"& Fs == "11"))$correlation_value > b_imi_perc)

mean((nulls %>% filter(combo == "Imitation*RC.F1" & Fs == "11"))$correlation_value > b_imi_RC)
mean((nulls %>% filter(combo == "Imitation*VL2.F1" & Fs == "11"))$correlation_value > b_imi_VL)

mean((nulls %>% filter(combo == "Self.Nonself*RC.F1" & Fs == "11"))$correlation_value > b_self_RC)
mean((nulls %>% filter(combo == "Self.Nonself*VL2.F1" & Fs == "11"))$correlation_value > b_self_VL)

mean((nulls %>% filter(combo == "RC.F1*Perceptual.Inconstancy" & Fs == "11"))$correlation_value > b_RC_perc)
mean((nulls %>% filter(combo == "VL2.F1*Perceptual.Inconstancy" & Fs == "11"))$correlation_value > b_VL_perc)



# In the simulation, I did not rename to .F2 but did properly use both Fs and fit the model of the pseudo-construct

summary_table <- nulls %>% group_by(combo) %>%
  filter(Fs == 12) %>%
  summarize(median_beta = median(correlation_value),
            sd_beta = sd(correlation_value))
summary_table$b <- c(2, 3, 4, 7, 1, 5, 6, 8) 
summary_table %>% arrange(b)



mean((nulls %>% filter(combo == "Self.Nonself*Perceptual.Inconstancy" & Fs == "12"))$correlation_value > parameterestimates(OR.mod.12)$est[parameterestimates(OR.mod.12)$label == "b1"])
mean((nulls %>% filter(combo == "Imitation*Perceptual.Inconstancy"& Fs == "12"))$correlation_value > parameterestimates(OR.mod.12)$est[parameterestimates(OR.mod.12)$label == "b2"])

mean((nulls %>% filter(combo == "Imitation*RC.F1" & Fs == "12"))$correlation_value > parameterestimates(OR.mod.12)$est[parameterestimates(OR.mod.12)$label == "b3"])
mean((nulls %>% filter(combo == "Imitation*VL2.F1" & Fs == "12"))$correlation_value > parameterestimates(OR.mod.12)$est[parameterestimates(OR.mod.12)$label == "b4"])

mean((nulls %>% filter(combo == "Self.Nonself*RC.F1" & Fs == "12"))$correlation_value > parameterestimates(OR.mod.12)$est[parameterestimates(OR.mod.12)$label == "b5"])
mean((nulls %>% filter(combo == "Self.Nonself*VL2.F1" & Fs == "12"))$correlation_value > parameterestimates(OR.mod.12)$est[parameterestimates(OR.mod.12)$label == "b6"])

mean((nulls %>% filter(combo == "RC.F1*Perceptual.Inconstancy" & Fs == "12"))$correlation_value > parameterestimates(OR.mod.12)$est[parameterestimates(OR.mod.12)$label == "b7"])
mean((nulls %>% filter(combo == "VL2.F1*Perceptual.Inconstancy" & Fs == "12"))$correlation_value > parameterestimates(OR.mod.12)$est[parameterestimates(OR.mod.12)$label == "b8"])




#####
summary_table <- nulls %>% group_by(combo) %>%
  filter(Fs == 21) %>%
  summarize(median_beta = median(correlation_value),
            sd_beta = sd(correlation_value))
summary_table$b <- c(2, 3, 4, 7, 1, 5, 6, 8) 
summary_table %>% arrange(b)



mean((nulls %>% filter(combo == "Self.Nonself*Perceptual.Inconstancy" & Fs == "21"))$correlation_value > parameterestimates(OR.mod.21)$est[parameterestimates(OR.mod.21)$label == "b1"])
mean((nulls %>% filter(combo == "Imitation*Perceptual.Inconstancy"& Fs == "21"))$correlation_value > parameterestimates(OR.mod.21)$est[parameterestimates(OR.mod.21)$label == "b2"])

mean((nulls %>% filter(combo == "Imitation*RC.F1" & Fs == "21"))$correlation_value > parameterestimates(OR.mod.21)$est[parameterestimates(OR.mod.21)$label == "b3"])
mean((nulls %>% filter(combo == "Imitation*VL2.F1" & Fs == "21"))$correlation_value > parameterestimates(OR.mod.21)$est[parameterestimates(OR.mod.21)$label == "b4"])

mean((nulls %>% filter(combo == "Self.Nonself*RC.F1" & Fs == "21"))$correlation_value > parameterestimates(OR.mod.21)$est[parameterestimates(OR.mod.21)$label == "b5"])
mean((nulls %>% filter(combo == "Self.Nonself*VL2.F1" & Fs == "21"))$correlation_value > parameterestimates(OR.mod.21)$est[parameterestimates(OR.mod.21)$label == "b6"])

mean((nulls %>% filter(combo == "RC.F1*Perceptual.Inconstancy" & Fs == "21"))$correlation_value > parameterestimates(OR.mod.21)$est[parameterestimates(OR.mod.21)$label == "b7"])
mean((nulls %>% filter(combo == "VL2.F1*Perceptual.Inconstancy" & Fs == "21"))$correlation_value > parameterestimates(OR.mod.21)$est[parameterestimates(OR.mod.21)$label == "b8"])


#####




summary_table <- nulls %>% group_by(combo) %>%
  filter(Fs == 22) %>%
  summarize(median_beta = median(correlation_value),
            sd_beta = sd(correlation_value))
summary_table$b <- c(2, 3, 4, 7, 1, 5, 6, 8) 
summary_table %>% arrange(b)


mean((nulls %>% filter(combo == "Self.Nonself*Perceptual.Inconstancy" & Fs == "22"))$correlation_value > parameterestimates(OR.mod.22)$est[parameterestimates(OR.mod.22)$label == "b1"])
mean((nulls %>% filter(combo == "Imitation*Perceptual.Inconstancy"& Fs == "22"))$correlation_value > parameterestimates(OR.mod.22)$est[parameterestimates(OR.mod.22)$label == "b2"])

mean((nulls %>% filter(combo == "Imitation*RC.F1" & Fs == "22"))$correlation_value > parameterestimates(OR.mod.22)$est[parameterestimates(OR.mod.22)$label == "b3"])
mean((nulls %>% filter(combo == "Imitation*VL2.F1" & Fs == "22"))$correlation_value > parameterestimates(OR.mod.22)$est[parameterestimates(OR.mod.22)$label == "b4"])

mean((nulls %>% filter(combo == "Self.Nonself*RC.F1" & Fs == "22"))$correlation_value > parameterestimates(OR.mod.22)$est[parameterestimates(OR.mod.22)$label == "b5"])
mean((nulls %>% filter(combo == "Self.Nonself*VL2.F1" & Fs == "22"))$correlation_value > parameterestimates(OR.mod.22)$est[parameterestimates(OR.mod.22)$label == "b6"])

mean((nulls %>% filter(combo == "RC.F1*Perceptual.Inconstancy" & Fs == "22"))$correlation_value > parameterestimates(OR.mod.22)$est[parameterestimates(OR.mod.22)$label == "b7"])
mean((nulls %>% filter(combo == "VL2.F1*Perceptual.Inconstancy" & Fs == "22"))$correlation_value > parameterestimates(OR.mod.22)$est[parameterestimates(OR.mod.22)$label == "b8"])

















# 
# ggplot(nulls %>% filter(combo %in% c("Self.Nonself*Perceptual.Inconstancy", "Imitation*Perceptual.Inconstancy",
#                                      "Imitation*RC.F1", "Imitation*VL2.F1",
#                                      "Self.Nonself*RC.F1", "Self.Nonself*VL2.F1")), 
#        aes(x = correlation_value)) +
#   geom_histogram() +
#   theme_bw() + 
#   facet_wrap(~factor(combo), nrow = 1)


gg1 <- permutation_gg(nulls = nulls, b_self_perc, combos = "Self.Nonself*Perceptual.Inconstancy",
                      Fss = "11", title = "Perc.Inc → Self/Non-Self", title.size = 12)
gg2 <- permutation_gg(nulls = nulls, b_imi_perc, combos = "Imitation*Perceptual.Inconstancy", 
                      Fss = "11", title = "Perc.Inc → Imitation", title.size = 12)
gg3 <- permutation_gg(nulls = nulls, b_imi_RC, combos = "Imitation*RC.F1",  
                      Fss = "11", title = "Imitation → RC", title.size = 12)
gg4 <- permutation_gg(nulls = nulls, b_imi_VL, combos = "Imitation*VL2.F1", 
                      Fss = "11", title = "Imitation → VL", title.size = 12)
gg5 <- permutation_gg(nulls = nulls, b_self_RC, combos = "Self.Nonself*RC.F1",
                      Fss = "11", title = "Self/Non-Self → RC", title.size = 12)
gg6 <- permutation_gg(nulls = nulls, b_self_VL, combos = "Self.Nonself*VL2.F1", 
                      Fss = "11", title = "Self/Non-Self → VL", title.size = 12)
gg7 <- permutation_gg(nulls = nulls, b_RC_perc, combos = "RC.F1*Perceptual.Inconstancy",  
                      Fss = "11", title = "Perc.Inc → RC", title.size = 12)
gg8 <- permutation_gg(nulls = nulls, b_VL_perc, combos = "VL2.F1*Perceptual.Inconstancy", 
                      Fss = "11", title = "Perc.Inc → VL", title.size = 12)


xaxis.grob <- grid::textGrob("β", gp = gpar(fontsize = 14, fontface = "italic"))
yaxis.grob <- grid::textGrob("Frequency", rot = 90, gp = gpar(fontsize = 14))
full_legend <- cowplot::get_legend(RP1_hist)
title.grob <- grid::textGrob("Ornitz & Ritvo: Permutation Testing versus Fully Constrained Monte Carlo Simulations", gp = gpar(fontsize = 18))

plot_grid <- grid.arrange(nrow = 2, ncol = 4, grobs = list(gg1, gg2, gg3, gg4, gg5, gg6, gg7, gg8))

scale <- 10
png("./Manuscript/Supplemental Materials/O_R_Permutation_Plot.png", width = 1080*scale, height = 720*scale, res = 72*scale)
grid.arrange(
  arrangeGrob(
    title.grob
  ),
  
  arrangeGrob(
    yaxis.grob, plot_grid, full_legend, ncol = 3, widths = c(0.05, 1, .1)
  ),
  arrangeGrob(
    nullGrob(), xaxis.grob, ncol = 2, widths = c(0.05, 1)
  ),
  heights = c(.05, 1, 0.05)
)
dev.off()












#nulls <- readRDS("./code/Empirical_priors/M_and_E_pseudo_construct_all_constraints_3_27_25.rds")
nulls <- readRDS("./code/Empirical_priors/M_and_E/M_and_E_pseudo_construct_full_constraints_4_21_25.rds")
nulls <- readRDS("./code/Empirical_priors/M_and_E/M_and_E_pseudo_construct_reliability_constraints_4_21_25.rds")
nulls <- readRDS("./code/Empirical_priors/M_and_E/M_and_E_pseudo_construct_no_constraints_4_18_25.rds")
#nulls <- readRDS("./code/Empirical_priors/M_and_E_pseudoImi_realMotSoc_all_constraints_4_7_25.rds")

nulls <- readRDS("./code/Empirical_priors/M_and_E_pseudoImi_realMotSoc_all_constraints_5_8_25.rds")


summary_table <- nulls %>% group_by(combo) %>%
  summarize(#mean_beta = mean(correlation_value),
    median_beta = median(correlation_value),
    sd_beta = sd(correlation_value))

summary_table


b_imi_motor <- .264
b_imi_social <- .533
phi_mot_soc <- .213
r_mot_soc <- .354


mean((nulls %>% filter(combo == "Motor.Skills*Imitation"))$correlation_value > b_imi_motor)
mean((nulls %>% filter(combo == "Social.Skills*Imitation"))$correlation_value > b_imi_social)

mean((nulls %>% filter(combo == "Social.Skills*Motor.Skills"))$correlation_value > phi_mot_soc)
mean((nulls %>% filter(combo == "Social.Skills*Motor.Skills.raw"))$correlation_value > r_mot_soc)

b_imi_motor - .191
b_imi_social - .217 
phi_mot_soc - .312
r_mot_soc - .354

ggplot(nulls, aes(x = correlation_value)) +
  geom_histogram() +
  theme_bw() + 
  facet_wrap(~factor(combo), nrow = 1)


nulls$Fs <- "11"

gg1 <- permutation_gg(nulls = nulls, b_imi_motor, combos = "Motor.Skills*Imitation",
                      Fss = "11", title = "Imitation → Motor", title.size = 12, binwidth = .005)
gg2 <- permutation_gg(nulls = nulls, b_imi_social, combos = "Social.Skills*Imitation", 
                      Fss = "11", title = "Imitation → Social", title.size = 12)
gg3 <- permutation_gg(nulls = nulls, phi_mot_soc, combos = "Social.Skills*Motor.Skills",  
                      Fss = "11", title = "Social ↔ Motor (residual)", title.size = 12, binwidth = .005)
gg4 <- permutation_gg(nulls = nulls, r_mot_soc, combos = "Social.Skills*Motor.Skills.raw", 
                      Fss = "11", title = "Social ↔ Motor (raw)", title.size = 12, binwidth = .005)

xaxis.grob <- grid::textGrob("β", gp = gpar(fontsize = 14, fontface = "italic"))
yaxis.grob <- grid::textGrob("Frequency", rot = 90, gp = gpar(fontsize = 14))
full_legend <- cowplot::get_legend(RP1_hist)
title.grob <- grid::textGrob("Mostofsky & Ewen: Permutation Testing versus Fully Constrained Monte Carlo Simulations", gp = gpar(fontsize = 18))

plot_grid <- grid.arrange(nrow = 2, ncol = 2, grobs = list(gg1, gg2, gg3, gg4))

scale <- 10
png("./Manuscript/Supplemental Materials/M_E_Permutation_Plot.png", width = 1080*scale, height = 720*scale, res = 72*scale)
grid.arrange(
  arrangeGrob(
    title.grob
  ),
  
  arrangeGrob(
    yaxis.grob, plot_grid, full_legend, ncol = 3, widths = c(0.05, 1, .1)
  ),
  arrangeGrob(
    nullGrob(), xaxis.grob, ncol = 2, widths = c(0.05, 1)
  ),
  heights = c(.05, 1, 0.05)
)
dev.off()


