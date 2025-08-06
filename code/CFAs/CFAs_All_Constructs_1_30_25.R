rm(list=ls())
library(tidyverse)
library(psych)
library(lavaan)
library(semPlot)
source("./code/Utilities/create_lavaan_syntax.R")
source("./code/Utilities/efa_to_cfa_prep.R")
source("./code/Utilities/efa_to_cfa_items.R")
source("./code/Utilities/mixed_cor_utils.R")
source("./code/Utilities/onefactor_syntax.R")


data <- readRDS("./data/reverse_score_corrected_data_2_26_25.rds")


syntax_list <- read_rds("./code/CFAs/CFA_syntax.rds")
# imag.play, self.nonself


i <- 1
for(i in 1:length(syntax_list)){
  print(syntax_list[[i]][["n_items"]])
  print(syntax_list)
}

set.seed(123)
split <- sample(1:(nrow(data)), size = (nrow(data))*.5,replace = F)
split2 <- (nrow(data))+1:nrow(data)

train <- data[split,]
test <- data[c(-split, -split2),]
data_full <- rbind(train, test)


items_sources <- readxl::read_xlsx("./code/construct_voting/old/Item_Voting_SM.xlsx") %>%
  select(-Imitation:-`O&R Sum`)
items_sources$dichotomous <- items_sources$from %in% c("background_history-child, background_history",
                                                       "background_history-child, background_history-sibling",
                                                       "background_history-child, background_history-adult",
                                                       "basic_medical_screening",
                                                       "core_descriptive_variables, background_history-child, background_history-sibling",
                                                       "scq",
                                                       "other","NA")



construct <- "Motor.Skills"
# syntax_list[[construct]][["n_items"]]
# 
# 
# train_motor <- train %>% select(syntax_list[[construct]][["items"]])
#   
# syntax_list[[construct]][["items2"]] <- syntax_list[[construct]][["items"]][-1]
# 
# syntax_list[[construct]][["second_syntax"]] <- onefactor_syntax(syntax_list[[construct]][["items2"]])
# 
#   
# # FIRST FIT
# #########
# motor.firstfit.cfa <- lavaan::cfa(syntax_list[[construct]][["syntax"]], data = test, meanstructure = T, std.lv = T,
#                                  ordered = items_sources$name[items_sources$dichotomous])
# 
# motor.firstfit.summary <- summary(motor.firstfit.cfa, fit.measures = TRUE, standardized = TRUE)
# 
# fitMeasures(motor.firstfit.cfa)[["rmsea.scaled"]]
# fitMeasures(motor.firstfit.cfa)[["cfi.scaled"]]
# fitMeasures(motor.firstfit.cfa)[["tli.scaled"]]
# fitMeasures(motor.firstfit.cfa)[["srmr"]]
# 
# semPaths(
#   motor.firstfit.cfa, 
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
# 
# 
# motor.secondfit.cfa <- lavaan::cfa(syntax_list[[construct]][["second_syntax"]], data = test, meanstructure = T, std.lv = T,
#                                   ordered = items_sources$name[items_sources$dichotomous])
# 
# motor.secondfit.summary <- summary(motor.secondfit.cfa, fit.measures = TRUE, standardized = TRUE)
# 
# fitMeasures(motor.secondfit.cfa)[["rmsea"]]
# fitMeasures(motor.secondfit.cfa)[["cfi"]]
# fitMeasures(motor.secondfit.cfa)[["tli"]]
# fitMeasures(motor.secondfit.cfa)[["srmr"]]
# 
# semPaths(
#   motor.secondfit.cfa, 
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
# 
# 
# train_motor <- train %>% select(all_of(syntax_list[[construct]][["items2"]]))
# 
# 
# 
# 
# 
# alt_syntax <- efa_to_cfa_prep(train_motor, 
#                               nfactors = 2, 
#                               hierarchical = F, 
#                               bifactor = F, 
#                               unique = F,
#                               drop_2_item_factors = F,
#                               cor = "cor")
# 
# 
# motor.2.cfa <- lavaan::cfa(alt_syntax, data = test, meanstructure = T, std.lv = T,
#                           ordered = items_sources$name[items_sources$dichotomous])
# 
# motor.2.summary <- summary(motor.2.cfa, fit.measures = TRUE, standardized = TRUE)
# 
# fitMeasures(motor.2.cfa)[["rmsea.scaled"]]
# fitMeasures(motor.2.cfa)[["cfi.scaled"]]
# fitMeasures(motor.2.cfa)[["tli.scaled"]]
# fitMeasures(motor.2.cfa)[["srmr"]]
# 
# semPaths(
#   motor.2.cfa, 
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


syntax_list[[construct]][["items3"]] <- syntax_list[[construct]][["items"]][-c(1, 5)]
syntax_list[[construct]][["third_syntax"]] <- onefactor_syntax(syntax_list[[construct]][["items3"]])

motor.items.final <- syntax_list[[construct]][["items3"]]
motor.syntax.final <- syntax_list[[construct]][["third_syntax"]]

motor.thirdfit.cfa <- lavaan::cfa(motor.syntax.final, data = test, meanstructure = T, std.lv = T,
                                   ordered = items_sources$name[items_sources$dichotomous])

motor.thirdfit.summary <- summary(motor.thirdfit.cfa, fit.measures = TRUE, standardized = TRUE)

fitMeasures(motor.thirdfit.cfa)[["rmsea"]]
fitMeasures(motor.thirdfit.cfa)[["cfi"]]
fitMeasures(motor.thirdfit.cfa)[["tli"]]
fitMeasures(motor.thirdfit.cfa)[["srmr"]]


scale <- 5
png("./plots/CFAs/Motor_Final_CFA.png", width = 720*scale, height = 720*scale, res=72*scale)
semPaths(
  motor.thirdfit.cfa, 
  nCharNodes = 3, 
  layout = "tree3", 
  sizeMan = 6,       # Reduce size of manifest variables
  sizeMan2 = 4,      # Further reduce size of the rectangles
  label.cex = 0.7,   # Shrink label size to fit text better 
  what = "std", 
  intercepts = F, 
  residuals = F, 
  thresholds = F
)
dev.off()








construct <- "Social.Skills"
syntax_list[[construct]][["n_items"]]

social.skills.syntax.final <- syntax_list[[construct]][["syntax"]]
social.skills.items.final <- syntax_list[[construct]][["items"]]

#syntax_list[[construct]][["items2"]] <- syntax_list[[construct]][["items"]][-1]

#syntax_list[[construct]][["second_syntax"]] <- onefactor_syntax(syntax_list[[construct]][["items2"]])


# FIRST FIT
#########
socskill.firstfit.cfa <- lavaan::cfa(social.skills.syntax.final, data = test, meanstructure = T, std.lv = T,
                                  ordered = items_sources$name[items_sources$dichotomous])

socskill.firstfit.summary <- summary(socskill.firstfit.cfa, fit.measures = TRUE, standardized = TRUE)

fitMeasures(socskill.firstfit.cfa)[["rmsea.scaled"]]
fitMeasures(socskill.firstfit.cfa)[["cfi.scaled"]]
fitMeasures(socskill.firstfit.cfa)[["tli.scaled"]]
fitMeasures(socskill.firstfit.cfa)[["srmr"]]


png("./plots/CFAs/SocialSkills_Final_CFA.png", width = 720*scale, height = 720*scale, res=72*scale)
semPaths(
  socskill.firstfit.cfa, 
  nCharNodes = 3, 
  layout = "tree3", 
  sizeMan = 6,       # Reduce size of manifest variables
  sizeMan2 = 4,      # Further reduce size of the rectangles
  label.cex = 0.7,   # Shrink label size to fit text better 
  what = "std", 
  intercepts = F, 
  residuals = F, 
  thresholds = F
)
dev.off()



construct <- "Affect.Sharing"

affect.sharing.final.syntax <- syntax_list[[construct]][["syntax"]]
affect.sharing.items.final <- syntax_list[[construct]][["items"]]

# syntax_list[[construct]][["items2"]] <- syntax_list[[construct]][["items"]][-6]
# syntax_list[[construct]][["second_syntax"]] <- onefactor_syntax(syntax_list[[construct]][["items2"]])


# manual <- 'Affect.Sharing=~q09_expressions_appropriate+q27_smile_back+q30_join_enjoyment+q31_comfort+q33_range_expressions+smiled_age_mos'

affect.firstfit.cfa <- lavaan::cfa(affect.sharing.final.syntax, data = train, meanstructure = T, std.lv = T,
                                  ordered = items_sources$name[items_sources$dichotomous])


affect.firstfit.summary <- summary(affect.firstfit.cfa, fit.measures = TRUE, standardized = TRUE)

fitMeasures(affect.firstfit.cfa)[["rmsea.scaled"]]
fitMeasures(affect.firstfit.cfa)[["cfi.scaled"]]
fitMeasures(affect.firstfit.cfa)[["tli.scaled"]]
fitMeasures(affect.firstfit.cfa)[["srmr"]]

png("./plots/CFAs/AffectSharing_Final_CFA.png", width = 720*scale, height = 720*scale, res=72*scale)
semPaths(
  affect.firstfit.cfa, 
  nCharNodes = 3, 
  layout = "tree3", 
  sizeMan = 6,       # Reduce size of manifest variables
  sizeMan2 = 4,      # Further reduce size of the rectangles
  label.cex = 0.7,   # Shrink label size to fit text better 
  what = "std", 
  intercepts = F, 
  residuals = F, 
  thresholds = F
)
dev.off()

construct <- "NVC"

# 
# syntax_list[[construct]][["items2"]] <- syntax_list[[construct]][["items"]][-2]
# syntax_list[[construct]][["second_syntax"]] <- onefactor_syntax(syntax_list[[construct]][["items2"]])
# 
# 
# # manual <- 'Affect.Sharing=~q09_expressions_appropriate+q27_smile_back+q30_join_enjoyment+q31_comfort+q33_range_expressions+smiled_age_mos'
# 
# NVC.firstfit.cfa <- lavaan::cfa(syntax_list[[construct]][["syntax"]], data = train, meanstructure = T, std.lv = T,
#                                    ordered = items_sources$name[items_sources$dichotomous])
# 
# 
# NVC.firstfit.summary <- summary(NVC.firstfit.cfa, fit.measures = TRUE, standardized = TRUE)
# 
# fitMeasures(NVC.firstfit.cfa)[["rmsea.scaled"]]
# fitMeasures(NVC.firstfit.cfa)[["cfi.scaled"]]
# fitMeasures(NVC.firstfit.cfa)[["tli.scaled"]]
# fitMeasures(NVC.firstfit.cfa)[["srmr"]]
# 
# semPaths(
#   NVC.firstfit.cfa, 
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
# 
# 
# NVC.secondfit.cfa <- lavaan::cfa(syntax_list[[construct]][["second_syntax"]], data = train, meanstructure = T, std.lv = T,
#                                 ordered = items_sources$name[items_sources$dichotomous])
# 
# 
# NVC.secondfit.summary <- summary(NVC.secondfit.cfa, fit.measures = TRUE, standardized = TRUE)
# 
# fitMeasures(NVC.secondfit.cfa)[["rmsea.scaled"]]
# fitMeasures(NVC.secondfit.cfa)[["cfi.scaled"]]
# fitMeasures(NVC.secondfit.cfa)[["tli.scaled"]]
# fitMeasures(NVC.secondfit.cfa)[["srmr"]]
# 
# semPaths(
#   NVC.secondfit.cfa, 
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
# 
# 
# syntax_list[[construct]][["items3"]] <- syntax_list[[construct]][["items"]][-c(1,2,4)]
# syntax_list[[construct]][["third_syntax"]] <- onefactor_syntax(syntax_list[[construct]][["items3"]])
# 
# 
# NVC.thirdfit.cfa <- lavaan::cfa(syntax_list[[construct]][["third_syntax"]], data = train, meanstructure = T, std.lv = T,
#                                  ordered = items_sources$name[items_sources$dichotomous])
# 
# 
# NVC.thirdfit.summary <- summary(NVC.thirdfit.cfa, fit.measures = TRUE, standardized = TRUE)
# 
# fitMeasures(NVC.thirdfit.cfa)[["rmsea.scaled"]]
# fitMeasures(NVC.thirdfit.cfa)[["cfi.scaled"]]
# fitMeasures(NVC.thirdfit.cfa)[["tli.scaled"]]
# fitMeasures(NVC.thirdfit.cfa)[["srmr"]]
# 
# semPaths(
#   NVC.thirdfit.cfa, 
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


train_NVC <- train %>% select(all_of(syntax_list[[construct]][["items"]]))

NVC.final.syntax <- efa_to_cfa_prep(train_NVC, 
                              nfactors = 2, 
                              hierarchical = F, 
                              bifactor = F, 
                              unique = F,
                              drop_2_item_factors = F,
                              cor = "cor")


NVC.final.items <- efa_to_cfa_items(train_NVC, 
                                    nfactors = 2, 
                                    unique = F,
                                    cor = "cor")



NVC.2fact.cfa <- lavaan::cfa(NVC.final.syntax, data = test, meanstructure = T, std.lv = T,
                                ordered = items_sources$name[items_sources$dichotomous])


NVC.2fact.summary <- summary(NVC.2fact.cfa, fit.measures = TRUE, standardized = TRUE)

fitMeasures(NVC.2fact.cfa)[["rmsea.scaled"]]
fitMeasures(NVC.2fact.cfa)[["cfi.scaled"]]
fitMeasures(NVC.2fact.cfa)[["tli.scaled"]]
fitMeasures(NVC.2fact.cfa)[["srmr"]]

png("./plots/CFAs/NVC_Final_CFA.png", width = 720*scale, height = 720*scale, res=72*scale)
semPaths(
  NVC.2fact.cfa, 
  nCharNodes = 3, 
  layout = "tree3", 
  sizeMan = 6,       # Reduce size of manifest variables
  sizeMan2 = 4,      # Further reduce size of the rectangles
  label.cex = 0.7,   # Shrink label size to fit text better 
  what = "std", 
  intercepts = F, 
  residuals = F, 
  thresholds = F
)
dev.off()




construct <- "Social.Eng"

social.eng.final.syntax <- syntax_list[[construct]][["syntax"]]
social.eng.final.items <- syntax_list[[construct]][["items"]]

#syntax_list[[construct]][["items2"]] <- syntax_list[[construct]][["items"]][-2]
#syntax_list[[construct]][["second_syntax"]] <- onefactor_syntax(syntax_list[[construct]][["items2"]])


# manual <- 'Affect.Sharing=~q09_expressions_appropriate+q27_smile_back+q30_join_enjoyment+q31_comfort+q33_range_expressions+smiled_age_mos'

socialeng.firstfit.cfa <- lavaan::cfa(social.eng.final.syntax, data = train, meanstructure = T, std.lv = T,
                                      ordered = items_sources$name[items_sources$dichotomous])


socialeng.firstfit.summary <- summary(socialeng.firstfit.cfa, fit.measures = TRUE, standardized = TRUE)

fitMeasures(socialeng.firstfit.cfa)[["rmsea.scaled"]]
fitMeasures(socialeng.firstfit.cfa)[["cfi.scaled"]]
fitMeasures(socialeng.firstfit.cfa)[["tli.scaled"]]
fitMeasures(socialeng.firstfit.cfa)[["srmr"]]

png("./plots/CFAs/SocialEng_Final_CFA.png", width = 720*scale, height = 720*scale, res=72*scale)
semPaths(
  socialeng.firstfit.cfa, 
  nCharNodes = 3, 
  layout = "tree3", 
  sizeMan = 6,       # Reduce size of manifest variables
  sizeMan2 = 4,      # Further reduce size of the rectangles
  label.cex = 0.7,   # Shrink label size to fit text better 
  what = "std", 
  intercepts = F, 
  residuals = F, 
  thresholds = F
)
dev.off()

construct <- "VL1"


# 
# syntax_list[[construct]][["items2"]] <- syntax_list[[construct]][["items"]][-c(9)]
# syntax_list[[construct]][["second_syntax"]] <- onefactor_syntax(syntax_list[[construct]][["items2"]])
# 
# 
# # manual <- 'Affect.Sharing=~q09_expressions_appropriate+q27_smile_back+q30_join_enjoyment+q31_comfort+q33_range_expressions+smiled_age_mos'
# 
# VL.firstfit.cfa <- lavaan::cfa(syntax_list[[construct]][["syntax"]], data = train, meanstructure = T, std.lv = T,
#                                 ordered = items_sources$name[items_sources$dichotomous])
# 
# 
# VL.firstfit.summary <- summary(VL.firstfit.cfa, fit.measures = TRUE, standardized = TRUE)
# 
# fitMeasures(VL.firstfit.cfa)[["rmsea.scaled"]]
# fitMeasures(VL.firstfit.cfa)[["cfi.scaled"]]
# fitMeasures(VL.firstfit.cfa)[["tli.scaled"]]
# fitMeasures(VL.firstfit.cfa)[["srmr"]]
# 
# semPaths(
#   VL.firstfit.cfa, 
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
# 
# 
# # manual <- 'Affect.Sharing=~q09_expressions_appropriate+q27_smile_back+q30_join_enjoyment+q31_comfort+q33_range_expressions+smiled_age_mos'
# 
# VL.secondfit.cfa <- lavaan::cfa(syntax_list[[construct]][["second_syntax"]], data = train, meanstructure = T, std.lv = T,
#                                ordered = items_sources$name[items_sources$dichotomous])
# 
# 
# VL.secondfit.summary <- summary(VL.secondfit.cfa, fit.measures = TRUE, standardized = TRUE)
# 
# fitMeasures(VL.secondfit.cfa)[["rmsea.scaled"]]
# fitMeasures(VL.secondfit.cfa)[["cfi.scaled"]]
# fitMeasures(VL.secondfit.cfa)[["tli.scaled"]]
# fitMeasures(VL.secondfit.cfa)[["srmr"]]
# 
# semPaths(
#   VL.secondfit.cfa, 
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


train_VL1 <- train %>% select(all_of(syntax_list[[construct]][["items"]]))

VL1.final.syntax <- efa_to_cfa_prep(train_VL1, 
                              nfactors = 2, 
                              hierarchical = F, 
                              bifactor = F, 
                              unique = F,
                              drop_2_item_factors = F,
                              cor = "cor")


VL1.final.items <- efa_to_cfa_items(train_VL1, 
                                    nfactors = 2, 
                                    unique = F,
                                    cor = "cor")




VL1.2fact.cfa <- lavaan::cfa(VL1.final.syntax, data = test, meanstructure = T, std.lv = T,
                             ordered = items_sources$name[items_sources$dichotomous])


VL1.2fact.summary <- summary(VL1.2fact.cfa, fit.measures = TRUE, standardized = TRUE)

fitMeasures(VL1.2fact.cfa)[["rmsea.scaled"]]
fitMeasures(VL1.2fact.cfa)[["cfi.scaled"]]
fitMeasures(VL1.2fact.cfa)[["tli.scaled"]]
fitMeasures(VL1.2fact.cfa)[["srmr"]]

png("./plots/CFAs/VL1_Final_CFA.png", width = 720*scale, height = 720*scale, res=72*scale)
semPaths(
  VL1.2fact.cfa, 
  nCharNodes = 3, 
  layout = "tree3", 
  sizeMan = 6,       # Reduce size of manifest variables
  sizeMan2 = 4,      # Further reduce size of the rectangles
  label.cex = 0.7,   # Shrink label size to fit text better 
  what = "std", 
  intercepts = F, 
  residuals = F, 
  thresholds = F
)
dev.off()







construct <- "Relational.Capacity"

# 
# syntax_list[[construct]][["items2"]] <- syntax_list[[construct]][["items"]][-19]
# syntax_list[[construct]][["second_syntax"]] <- onefactor_syntax(syntax_list[[construct]][["items2"]])
# 
# 
# # manual <- 'Affect.Sharing=~q09_expressions_appropriate+q27_smile_back+q30_join_enjoyment+q31_comfort+q33_range_expressions+smiled_age_mos'
# 
# RC.firstfit.cfa <- lavaan::cfa(syntax_list[[construct]][["syntax"]], data = train, meanstructure = T, std.lv = T,
#                                ordered = items_sources$name[items_sources$dichotomous])
# 
# 
# RC.firstfit.summary <- summary(RC.firstfit.cfa, fit.measures = TRUE, standardized = TRUE)
# 
# fitMeasures(RC.firstfit.cfa)[["rmsea.scaled"]]
# fitMeasures(RC.firstfit.cfa)[["cfi.scaled"]]
# fitMeasures(RC.firstfit.cfa)[["tli.scaled"]]
# fitMeasures(RC.firstfit.cfa)[["srmr"]]
# 
# semPaths(
#   RC.firstfit.cfa, 
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
# 
# # manual <- 'Affect.Sharing=~q09_expressions_appropriate+q27_smile_back+q30_join_enjoyment+q31_comfort+q33_range_expressions+smiled_age_mos'
# 
# RC.secondfit.cfa <- lavaan::cfa(syntax_list[[construct]][["second_syntax"]], data = train, meanstructure = T, std.lv = T,
#                                ordered = items_sources$name[items_sources$dichotomous])
# 
# 
# RC.secondfit.summary <- summary(RC.secondfit.cfa, fit.measures = TRUE, standardized = TRUE)
# 
# fitMeasures(RC.secondfit.cfa)[["rmsea.scaled"]]
# fitMeasures(RC.secondfit.cfa)[["cfi.scaled"]]
# fitMeasures(RC.secondfit.cfa)[["tli.scaled"]]
# fitMeasures(RC.secondfit.cfa)[["srmr"]]
# 
# semPaths(
#   RC.secondfit.cfa, 
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
# 

train_RC <- train %>% select(all_of(syntax_list[[construct]][["items"]]))

RC.final.syntax <- efa_to_cfa_prep(train_RC, 
                              nfactors = 2, 
                              hierarchical = F, 
                              bifactor = F, 
                              unique = F,
                              drop_2_item_factors = F,
                              cor = "cor")

RC.final.items <- efa_to_cfa_items(train_RC, 
                                    nfactors = 2, 
                                    unique = F,
                                    cor = "cor")



RC.2fact.cfa <- lavaan::cfa(RC.final.syntax, data = test, meanstructure = T, std.lv = T,
                             ordered = items_sources$name[items_sources$dichotomous])


RC.2fact.summary <- summary(RC.2fact.cfa, fit.measures = TRUE, standardized = TRUE)

fitMeasures(RC.2fact.cfa)[["rmsea.scaled"]]
fitMeasures(RC.2fact.cfa)[["cfi.scaled"]]
fitMeasures(RC.2fact.cfa)[["tli.scaled"]]
fitMeasures(RC.2fact.cfa)[["srmr"]]

png("./plots/CFAs/RC_Final_CFA.png", width = 720*scale, height = 720*scale, res=72*scale)
semPaths(
  RC.2fact.cfa, 
  nCharNodes = 3, 
  layout = "tree3", 
  sizeMan = 6,       # Reduce size of manifest variables
  sizeMan2 = 4,      # Further reduce size of the rectangles
  label.cex = 0.7,   # Shrink label size to fit text better 
  what = "std", 
  intercepts = F, 
  residuals = F, 
  thresholds = F
)
dev.off()








construct <- "VL2"


syntax_list[[construct]][["items2"]] <- syntax_list[[construct]][["items"]][-c(2,10,11)]
syntax_list[[construct]][["second_syntax"]] <- onefactor_syntax(syntax_list[[construct]][["items2"]])
# 
# 
# # manual <- 'Affect.Sharing=~q09_expressions_appropriate+q27_smile_back+q30_join_enjoyment+q31_comfort+q33_range_expressions+smiled_age_mos'
# 
# VL2.firstfit.cfa <- lavaan::cfa(syntax_list[[construct]][["syntax"]], data = train, meanstructure = T, std.lv = T,
#                                ordered = items_sources$name[items_sources$dichotomous])
# 
# 
# VL2.firstfit.summary <- summary(VL2.firstfit.cfa, fit.measures = TRUE, standardized = TRUE)
# 
# fitMeasures(VL2.firstfit.cfa)[["rmsea.scaled"]]
# fitMeasures(VL2.firstfit.cfa)[["cfi.scaled"]]
# fitMeasures(VL2.firstfit.cfa)[["tli.scaled"]]
# fitMeasures(VL2.firstfit.cfa)[["srmr"]]
# 
# semPaths(
#   VL2.firstfit.cfa, 
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


train_VL2 <- train %>% select(all_of(syntax_list[[construct]][["items2"]]))

VL2.final.syntax <- efa_to_cfa_prep(train_VL2, 
                              nfactors = 2, 
                              hierarchical = F, 
                              bifactor = F, 
                              unique = F,
                              drop_2_item_factors = F,
                              cor = "cor")


VL2.final.items <- efa_to_cfa_items(train_VL2, 
                                   nfactors = 2, 
                                   unique = F,
                                   cor = "cor")




VL2.2fact.cfa <- lavaan::cfa(VL2.final.syntax, data = test, meanstructure = T, std.lv = T,
                             ordered = items_sources$name[items_sources$dichotomous])


VL2.2fact.summary <- summary(VL2.2fact.cfa, fit.measures = TRUE, standardized = TRUE)

fitMeasures(VL2.2fact.cfa)[["rmsea.scaled"]]
fitMeasures(VL2.2fact.cfa)[["cfi.scaled"]]
fitMeasures(VL2.2fact.cfa)[["tli.scaled"]]
fitMeasures(VL2.2fact.cfa)[["srmr"]]

png("./plots/CFAs/VL2_Final_CFA.png", width = 720*scale, height = 720*scale, res=72*scale)

semPaths(
  VL2.2fact.cfa, 
  nCharNodes = 3, 
  layout = "tree3", 
  sizeMan = 6,       # Reduce size of manifest variables
  sizeMan2 = 4,      # Further reduce size of the rectangles
  label.cex = 0.7,   # Shrink label size to fit text better 
  what = "std", 
  intercepts = F, 
  residuals = F, 
  thresholds = F
)
dev.off()





construct <- "Perceptual.Inconstancy"

perc.final.syntax <- syntax_list[[construct]][["syntax"]]
perc.final.items <- syntax_list[[construct]][["items"]]





#syntax_list[[construct]][["items2"]] <- syntax_list[[construct]][["items"]][-2]
#syntax_list[[construct]][["second_syntax"]] <- onefactor_syntax(syntax_list[[construct]][["items2"]])


# manual <- 'Affect.Sharing=~q09_expressions_appropriate+q27_smile_back+q30_join_enjoyment+q31_comfort+q33_range_expressions+smiled_age_mos'

perc.firstfit.cfa <- lavaan::cfa(perc.final.syntax, data = train, meanstructure = T, std.lv = T,
                                ordered = items_sources$name[items_sources$dichotomous])


perc.firstfit.summary <- summary(perc.firstfit.cfa, fit.measures = TRUE, standardized = TRUE)

fitMeasures(perc.firstfit.cfa)[["rmsea.scaled"]]
fitMeasures(perc.firstfit.cfa)[["cfi.scaled"]]
fitMeasures(perc.firstfit.cfa)[["tli.scaled"]]
fitMeasures(perc.firstfit.cfa)[["srmr"]]

png("./plots/CFAs/PerceptualInconstancy_Final_CFA.png", width = 720*scale, height = 720*scale, res=72*scale)

semPaths(
  perc.firstfit.cfa, 
  nCharNodes = 3, 
  layout = "tree3", 
  sizeMan = 6,       # Reduce size of manifest variables
  sizeMan2 = 4,      # Further reduce size of the rectangles
  label.cex = 0.7,   # Shrink label size to fit text better 
  what = "std", 
  intercepts = F, 
  residuals = F, 
  thresholds = F
)
dev.off()

VL1.final.items
VL2.final.items
RC.final.items
NVC.final.items


items_list <- list(Motor.Skills = motor.items.final,
                   Social.Skills = social.skills.items.final,
                   Affect.Sharing = affect.sharing.items.final,
                   Social.Eng = social.eng.final.items,
                   VL1.F1 = VL1.final.items$name[VL1.final.items$MR.name == "MR1"],
                   VL1.F2 = VL1.final.items$name[VL1.final.items$MR.name == "MR2"],
                   NVC.F1 = NVC.final.items$name[NVC.final.items$MR.name == "MR1"],
                   NVC.F2 = NVC.final.items$name[NVC.final.items$MR.name == "MR2"],
                   RC.F1 = RC.final.items$name[RC.final.items$MR.name == "MR1"],
                   RC.F2 = RC.final.items$name[RC.final.items$MR.name == "MR2"],
                   VL2.F1 = VL2.final.items$name[VL2.final.items$MR.name == "MR1"],
                   VL2.F2 = VL2.final.items$name[VL2.final.items$MR.name == "MR2"],
                   Perceptual.Inconstancy = perc.final.items,
                   Imag.Play = syntax_list[["Imag.Play"]][["items"]],
                   Self.Nonself = syntax_list[["Self.Nonself"]][["items"]],
                   Imitation = c("q21_copy_you", "q34_copy_actions")
                   )

saveRDS(items_list, "./data/items_list_constructs.rds")









vl2 <- lavPredict(VL2.2fact.cfa, data)
rc2 <- lavPredict(RC.2fact.cfa, data)
vl1 <- lavPredict(VL1.2fact.cfa, data)
NVC2 <- lavPredict(NVC.2fact.cfa, data)

data$VL1.F1 <- vl1[,1]
data$VL1.F2 <- vl1[,2]

data$NVC.F1 <- NVC2[,1]
data$NVC.F2 <- NVC2[,2]

data$RC.F1 <- rc2[,1] 
data$RC.F2 <- rc2[,2] 

data$VL2.F1 <- vl2[,1]
data$VL2.F2 <- vl2[,2]

data$Motor.Skills <- lavPredict(motor.thirdfit.cfa, data)
data$Social.Skills <- lavPredict(socskill.firstfit.cfa, data)
data$Affect.Sharing <- lavPredict(affect.firstfit.cfa, data)
data$Social.Eng <- lavPredict(socialeng.firstfit.cfa, data)
data$Perceptual.Inconstancy <- lavPredict(perc.firstfit.cfa, data)

data$Imag.Play <- apply(data %>% select(q35_make_believe, q39_imaginative_games), MARGIN = 1, FUN = mean)
data$Self.Nonself <- apply(data %>% select(q05_pronouns_mixed, q10_hand_tool, q27_smile_back), MARGIN = 1, FUN = mean)
data$Imitation <- apply(data %>% select(q21_copy_you, q34_copy_actions), MARGIN = 1, FUN = mean)

# data <- data %>% rename(Motor.Skills = `F`)

data2 <- data %>% select(Perceptual.Inconstancy:Social.Eng)
data2 <- data %>% select(VL1.F1:Imitation)
names(data2)

#write_rds(data2, "./data/factor_score_estimates_constructs_1_30_25.rds")

###

symbolism_syntax <- 'Symbolism=~VL1.F1+NVC.F1+Imag.Play'
symbolism.cfa <- lavaan::cfa(symbolism_syntax, data = data2, meanstructure = T, std.lv = T)
data2$Symbolism <- lavPredict(symbolism.cfa)

data2 <- data2 %>% mutate(across(everything(), ~ scale(.x)))
#saveRDS(data2, "data/factor_score_estimates_constructs_symbolism_2_10_25.rds")
#saveRDS(data2, "data/factor_score_estimates_constructs_symbolism_7_30_25_replication.rds")
