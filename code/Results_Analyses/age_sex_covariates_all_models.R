rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(tidySEM)


data2 <- read_rds("data/factor_score_estimates_constructs_symbolism_2_10_25.rds")
data2$ids <- read_rds("./data/all_ids.rds")

data_reg <- read.csv("./data/individuals_registration.csv") %>%
  select(subject_sp_id, age_at_registration_years, sex)

data2 <- left_join(data2, data_reg, by = c("ids" = "subject_sp_id"))
rm(data_reg)

set.seed(123)
split <- sample(1:nrow(data2), size = nrow(data2)*.5,replace = F)

train <- data2[split,]
test <- data2[-split,]


symbolism_syntax11 <- 'Symbolism11=~Imag.Play+VL1.F1+NVC.F1'
symbolism11.cfa <- lavaan::cfa(symbolism_syntax11, data = train, meanstructure = T, std.lv = T)
data2$Symbolism11 <- lavPredict(symbolism11.cfa, data2)
data2$Symbolism11 <- scale(data2$Symbolism11)


data2_male <- data2 %>% filter(sex == "Male")
data2_female <- data2 %>% filter(sex == "Female")
data2_young <- data2 %>% filter(age_at_registration_years <= 7)
data2_old <- data2 %>% filter(age_at_registration_years > 7)



ME.2011.syntax <- 'Motor.Skills ~ b1*Imitation
                  Social.Skills ~ b2*Imitation
                  Motor.Skills ~~ phi*Social.Skills
                  
                  b1_adj := b1 - 0.093       
                  b2_adj := b2 - 0.307       
                  phi_adj := phi - 0.213       
                  
'



ME.mod <- sem(model = ME.2011.syntax, 
              data = data2)


ME.mod.male <- sem(model = ME.2011.syntax, 
              data = data2_male)
ME.mod.female <- sem(model = ME.2011.syntax, 
                   data = data2_female)

ME.mod.young <- sem(model = ME.2011.syntax, 
                   data = data2_young)
ME.mod.old <- sem(model = ME.2011.syntax, 
                     data = data2_old)


parameterEstimates(ME.mod.male, standardized = T)[1:3,]
parameterEstimates(ME.mod.female, standardized = T)[1:3,]

parameterEstimates(ME.mod.young, standardized = T)[1:3,]
parameterEstimates(ME.mod.old, standardized = T)[1:3,]

cor(data2_young$Motor.Skills, data2_young$Social.Skills)
cor(data2_old$Motor.Skills, data2_old$Social.Skills)

cor(data2_male$Motor.Skills, data2_male$Social.Skills)
cor(data2_female$Motor.Skills, data2_female$Social.Skills)

saveRDS(ME.mod.male, "./code/Results_Analyses/lavaan_models/M_and_E/ME_mod_male.rds")
saveRDS(ME.mod.female, "./code/Results_Analyses/lavaan_models/M_and_E/ME_mod_female.rds")
saveRDS(ME.mod.young, "./code/Results_Analyses/lavaan_models/M_and_E/ME_mod_young.rds")
saveRDS(ME.mod.old, "./code/Results_Analyses/lavaan_models/M_and_E/ME_mod_old.rds")


########
OR.1968.syntax <- '
Self.Nonself ~ b1*Perceptual.Inconstancy
Imitation ~ b2*Perceptual.Inconstancy
RC.F1 ~ b3*Imitation + b5*Self.Nonself + b7*Perceptual.Inconstancy
VL2.F1 ~ b4*Imitation + b6*Self.Nonself + b8*Perceptual.Inconstancy 

b1_adj := b1 - .407
b2_adj := b2 - .348
b3_adj := b3 - .106
b4_adj := b4 - .266
b5_adj := b5 - .425
b6_adj := b6 - .108
b7_adj := b7 - .249
b8_adj := b8 - .398
'

OR.mod.male <- sem(model = OR.1968.syntax, 
                   data = data2_male)
OR.mod.female <- sem(model = OR.1968.syntax, 
                     data = data2_female)

OR.mod.young <- sem(model = OR.1968.syntax, 
                    data = data2_young)
OR.mod.old <- sem(model = OR.1968.syntax, 
                  data = data2_old)


parameterEstimates(OR.mod.male, standardized = T)[1:8,]
parameterEstimates(OR.mod.female, standardized = T)[1:8,]

parameterEstimates(OR.mod.young, standardized = T)[1:8,]
parameterEstimates(OR.mod.old, standardized = T)[1:8,]


saveRDS(OR.mod.male, "./code/Results_Analyses/lavaan_models/O_and_R/OR_mod_male.rds")
saveRDS(OR.mod.female, "./code/Results_Analyses/lavaan_models/O_and_R/OR_mod_female.rds")
saveRDS(OR.mod.young, "./code/Results_Analyses/lavaan_models/O_and_R/OR_mod_young.rds")
saveRDS(OR.mod.old, "./code/Results_Analyses/lavaan_models/O_and_R/OR_mod_old.rds")



###########


RP.1991.syntax <- '
Social.Eng ~ b3*Affect.Sharing + b4*Imitation
Affect.Sharing ~ b1*Imitation
Symbolism11 ~ b2*Imitation

b1_adj := b1 - .356
b2_adj := b2 - .384
b3_adj := b3 - .617
b4_adj := b4 - .267
'

RP.mod <- sem(model = RP.1991.syntax, 
              data = data2 )
summary(RP.mod, standardized = T)

RP.mod.male <- sem(model = RP.1991.syntax, 
                   data = data2_male)
RP.mod.female <- sem(model = RP.1991.syntax, 
                     data = data2_female)

RP.mod.young <- sem(model = RP.1991.syntax, 
                    data = data2_young)
RP.mod.old <- sem(model = RP.1991.syntax, 
                  data = data2_old)


parameterEstimates(RP.mod.male, standardized = T)[c(3,2,4,1),]
parameterEstimates(RP.mod.female, standardized = T)[c(3,2,4,1),]

parameterEstimates(RP.mod.young, standardized = T)[c(3,2,4,1),]
parameterEstimates(RP.mod.old, standardized = T)[c(3,2,4,1),]


saveRDS(RP.mod.male, "./code/Results_Analyses/lavaan_models/R_and_P/RP_mod_male.rds")
saveRDS(RP.mod.female, "./code/Results_Analyses/lavaan_models/R_and_P/RP_mod_female.rds")
saveRDS(RP.mod.young, "./code/Results_Analyses/lavaan_models/R_and_P/RP_mod_young.rds")
saveRDS(RP.mod.old, "./code/Results_Analyses/lavaan_models/R_and_P/RP_mod_old.rds")

