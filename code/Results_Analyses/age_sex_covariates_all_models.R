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


symbolism_syntax11 <- 'Symbolism11=~Imag.Play+VL1.F1+NVC.F1'
symbolism11.cfa <- lavaan::cfa(symbolism_syntax11, data = data2, meanstructure = T, std.lv = T)
data2$Symbolism11 <- lavPredict(symbolism11.cfa, data2)


data2_male <- data2 %>% filter(sex == "Male")
data2_female <- data2 %>% filter(sex == "Female")
data2_young <- data2 %>% filter(age_at_registration_years <= 7)
data2_old <- data2 %>% filter(age_at_registration_years > 7)



ME.2011.syntax <- 'Motor.Skills ~ b1*Imitation
                  Social.Skills ~ b2*Imitation
                  Motor.Skills ~~ phi*Social.Skills
                  
                  b1_adj := b1 - 0.093       
                  b2_adj := b2 - 0.307       
                  phi_adj := phi - 0.199       
                  
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


parameterEstimates(ME.mod.male)[1:3,]
parameterEstimates(ME.mod.female)[1:3,]

parameterEstimates(ME.mod.young)[1:3,]
parameterEstimates(ME.mod.old)[1:3,]

cor(data2_young$Motor.Skills, data2_young$Social.Skills)
cor(data2_old$Motor.Skills, data2_old$Social.Skills)

cor(data2_male$Motor.Skills, data2_male$Social.Skills)
cor(data2_female$Motor.Skills, data2_female$Social.Skills)



########
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

OR.mod.male <- sem(model = OR.1968.syntax, 
                   data = data2_male)
OR.mod.female <- sem(model = OR.1968.syntax, 
                     data = data2_female)

OR.mod.young <- sem(model = OR.1968.syntax, 
                    data = data2_young)
OR.mod.old <- sem(model = OR.1968.syntax, 
                  data = data2_old)


parameterEstimates(OR.mod.male)[1:8,]
parameterEstimates(OR.mod.female)[1:8,]

parameterEstimates(OR.mod.young)[1:8,]
parameterEstimates(OR.mod.old)[1:8,]

###########


RP.1991.syntax <- '
Social.Eng ~ b3*Affect.Sharing + b4*Imitation
Affect.Sharing ~ b1*Imitation
Symbolism11 ~ b2*Imitation

b1_adj := b1 - .356
b2_adj := b2 - .384
b3_adj := b3 - .567
b4_adj := b4 - .249
'

RP.mod.male <- sem(model = RP.1991.syntax, 
                   data = data2_male)
RP.mod.female <- sem(model = RP.1991.syntax, 
                     data = data2_female)

RP.mod.young <- sem(model = RP.1991.syntax, 
                    data = data2_young)
RP.mod.old <- sem(model = RP.1991.syntax, 
                  data = data2_old)


parameterEstimates(RP.mod.male)[c(3,4,1,2),]
parameterEstimates(RP.mod.female)[c(3,4,1,2),]

parameterEstimates(RP.mod.young)[c(3,4,1,2),]
parameterEstimates(RP.mod.old)[c(3,4,1,2),]
