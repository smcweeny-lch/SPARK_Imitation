rm(list = ls())
subject_sp_id <- read.csv("./data/included_subjects.csv")
subject_sp_id <- subject_sp_id$x 
data_reg <- read.csv("./data/individuals_registration.csv")
data_bhc <- read.csv("./data/background_history_child.csv")




data_bhc <- data_bhc[data_bhc$subject_sp_id %in% subject_sp_id,]

data_reg <- data_reg[data_reg$subject_sp_id %in% subject_sp_id,]


data_reg %>% group_by(sex) %>% count()
14862/19053

apply(data_reg %>% select(starts_with("race")), MARGIN = 2, FUN = function(x)sum(x, na.rm = T))

sum(data_reg$hispanic, na.rm = T)


sum(data_reg$cognitive_impairment_at_enrollment, na.rm = T)
sum(data_reg$cognitive_impairment_at_enrollment, na.rm = T) / nrow(data_reg)

mean(data_reg$age_at_registration_months) / 12
sum(data_reg$sex == "Male") / nrow(data_reg)

mean(data_reg$diagnosis_age)
sd(data_reg$diagnosis_age)
max(data_reg$age_at_registration_months)

data_bhc %>% count(annual_household_income)
