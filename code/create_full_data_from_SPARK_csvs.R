rm(list = ls())
library(tidyverse)

data_bha <- read.csv("./data/background_history_adult.csv")
data_bhc <- read.csv("./data/background_history_child.csv")
data_bhs <- read.csv("./data/background_history_sibling.csv")
data_bms <- read.csv("./data/basic_medical_screening.csv")
data_clr <- read.csv("./data/clinical_lab_report.csv")
data_cdv <- read.csv("./data/core_descriptive_variables.csv")
data_dcdq <- read.csv("./data/dcdq.csv")
data_iq <- read.csv("./data/iq.csv")
data_rbsr <- read.csv("./data/rbsr.csv")
data_scq <- read.csv("./data/scq.csv")
data_reg <- read.csv("./data/individuals_registration.csv")
data_vineland <- read.csv("./data/vineland.csv")
data_dictionary <- read_rds("./data/data_dictionary.rds")


################################################################################
#isolate columns from each dataframe. columns are the union of fpsyg & Natasha
################################################################################
dcdq_sub <- data_dcdq[,c("subject_sp_id","q01_throw_ball","q02_catch_ball","q03_hit_ball","q04_jump_obstacles",
                         "q05_run_fast_similar","q06_plan_motor_activity","q07_printing_writing_drawing_fast",
                         "q08_printing_letters_legible","q09_appropriate_tension_printing_writing",
                         "q10_cuts_pictures_shapes","q11_likes_sports_motors_skills",
                         "q12_learns_new_motor_tasks","q13_quick_competent_tidying_up",
                         "q14_bull_in_china_shop","q15_fatigue_easily")]

bhc_sub <- data_bhc[,c("subject_sp_id","age_at_eval_months","crawled_age_mos","sped_ot",
                       "sped_pt","intervention_ot_fine_mot","intervention_pt","sped_soc_skills",
                       "intervention_social_skills","smiled_age_mos","used_words_age_mos",
                       "combined_words_age_mos","combined_phrases_age_mos","language_age_level",
                       "sped_speech","intervention_speech_language","intervention_ot_sensory",
                       "cog_age_level", 
                       
                       "age_onset_mos")] #added 8/20 to filter


#bha_sub <- data_bha[,c("subject_sp_id","sped_ot","sped_pt","intervention_ot_fine_mot","intervention_pt",
#                       "supports_social","supports_social_need","intervention_social_skills",
#                       "free_time_friends","free_time_family","free_time_online_comm",
#                       "free_time_groups","sped_speech","intervention_speech_language",
#                       "intervention_ot_sensory")]

scq_sub <- data_scq[,c("subject_sp_id","q01_phrases","q02_conversation","q03_odd_phrase","q04_inappropriate_question",
                       "q05_pronouns_mixed","q06_invented_words","q07_same_over","q08_particular_way",
                       "q09_expressions_appropriate","q10_hand_tool","q11_interest_preoccupy",
                       "q12_parts_object","q13_interests_intensity","q14_senses","q15_odd_ways",
                       "q16_complicated_movements","q17_injured_deliberately","q18_objects_carry",
                       "q19_best_friend","q20_talk_friendly","q21_copy_you","q22_point_things",
                       "q23_gestures_wanted","q24_nod_head","q25_shake_head","q26_look_directly",
                       "q27_smile_back","q28_things_interested","q29_share","q30_join_enjoyment",
                       "q31_comfort","q32_help_attention","q33_range_expressions",
                       "q34_copy_actions","q35_make_believe","q36_same_age","q37_respond_positively",
                       "q38_pay_attention","q39_imaginative_games","q40_cooperatively_games")]

rbsr_sub <- data_rbsr[,c("subject_sp_id","q01_whole_body","q02_head","q03_hand_finger","q04_locomotion",
                         "q05_object_usage","q06_sensory","q07_hits_self_body","q08_hits_self_against_object",
                         "q09_hits_self_with_object","q10_bites_self","q11_pulls",
                         "q12_rubs","q13_inserts_finger","q14_skin_picking","q15_arranging",
                         "q17_washing","q18_checking","q21_repeating","q22_touch_tap","q23_eating",
                         "q24_sleep","q25_self_care","q26_travel","q27_play","q28_communication",
                         "q29_things_same_place","q30_objects","q31_becomes_upset",
                         "q32_insists_walking","q33_insists_sitting","q34_dislikes_changes",
                         "q35_insists_door","q36_likes_piece_music","q37_resists_change",
                         "q38_insists_routine","q39_insists_time","q40_fascination_subject",
                         "q41_strongly_attached","q42_preoccupation","q43_fascination_movement"
#                         )]
                         ,"q16_complete", "q20_hoarding", "q19_counting")]
#NOTE - 16, 19, 20 WERE ADDED TEMPORARILY 10/14/24



bms_sub <- data_bms[,c("subject_sp_id","dev_lang_dis","dev_lang")]

#combine into single dataframe
data_asd <- inner_join(dcdq_sub,bhc_sub,by = "subject_sp_id") %>%
  inner_join(rbsr_sub,by = "subject_sp_id") %>% 
  inner_join(scq_sub,by = "subject_sp_id") %>%
  inner_join(bms_sub,by = "subject_sp_id") %>% 
  left_join(data_reg[,c("subject_sp_id", "diagnosis_age", "age_validity_flag", "age_at_registration_months")], by = "subject_sp_id")

# names(data_bhs)[which(!names(data_bhs) %in% names(data_bhc))]
# View(data_dictionary %>% filter(str_detect(var_name, "age")) %>% dplyr::select(var_name, from))

#Ensure that children under 18 months at ASD Eval were excluded previously 
print(paste0(sum(data_asd$age_at_eval_months <= 18), " participants were diagnosed with ASD before 18 months"))
#Ensure Adults were excluded
print(paste0(sum(data_asd$age_at_registration_months > 18*12, na.rm = T), " participants were older than 18 years old at the time of registration"))
# print(paste0(sum(data_sibs$age_at_eval_months <= 18), " Siblings were assessed for ASD before 18 months"))

sum(data_asd$combined_words_age_mos < 6, na.rm = T)





# Age flags of varying types - NAs
print(paste0(sum(!is.na(data_asd$age_validity_flag)), " observations were flagged and removed for age validity"))
print(paste0(sum(is.na(data_asd$diagnosis_age)), " observations didn't have a diagnosis age listed"))
print(paste0(sum(is.na(data_asd$age_onset_mos)), " observations didn't have an onset age listed"))
print(paste0(sum(data_asd$age_onset_mos > data_asd$diagnosis_age, na.rm = T) , " observations had listed the age of first concern / onset as after the age of diagnosis"))
print(paste0(sum(data_asd$combined_words_age_mos <= 6, na.rm = T), " children were reported to be combining words with a verb before 7 months"))

#26083, 967 (vs 965), 25118
data_asd <- data_asd  %>%
  filter(!is.na(age_onset_mos)) %>%
  filter(age_onset_mos <= diagnosis_age) %>% 
  filter(is.na(age_validity_flag)) %>%
  filter(!is.na(diagnosis_age)) %>% 
  filter(combined_words_age_mos > 6)

data_asd  <- data_asd %>% dplyr::select(-diagnosis_age, -age_validity_flag, -age_onset_mos)


data <- data_asd


# These questions pertain to children who didn't have spoken language q01_phrases
# If they did, how would they score on a variety of questions shown below
# Using mean values for age-matched children, we generated the likelihood that a child would have q2-7 if they did have q01_phrases
# Rather than transform the variable from binary, we sampled from a binomial distribution.

################################################################################
#begin cleaning dataframe
################################################################################

#map categorical data to numerical
mapping <- c("signif_below_age"=1,"slight_below_age"=2,"at_age"=3,"above_age"=4)
data <- data %>%
  mutate(language_age_level=mapping[language_age_level])
data <- data %>%
  mutate(cog_age_level=mapping[cog_age_level])

#identify imputation value for missing data by calculating percent existing by speech age
q02percent <- data.frame(age=numeric(0),value=numeric(0))
q03percent <- data.frame(age=numeric(0),value=numeric(0))
q04percent <- data.frame(age=numeric(0),value=numeric(0))
q05percent <- data.frame(age=numeric(0),value=numeric(0))
q06percent <- data.frame(age=numeric(0),value=numeric(0))
q07percent <- data.frame(age=numeric(0),value=numeric(0))
for (age in unique(na.omit(data$combined_words_age_mos))) {
  if (age != 888){
    tempdata <- data %>% dplyr::select(combined_words_age_mos,q02_conversation,q03_odd_phrase,
                                       q04_inappropriate_question,q05_pronouns_mixed,q06_invented_words,
                                       q07_same_over) %>%
      filter(combined_words_age_mos==age)
    
    q02percent <- rbind(q02percent,data.frame(q = "Q2", age=age,value=100*sum(tempdata$q02_conversation==1,na.rm=TRUE)/(sum(tempdata$q02_conversation==1,na.rm=TRUE)+sum(tempdata$q02_conversation==0,na.rm=TRUE))))
    q03percent <- rbind(q03percent,data.frame(q = "Q3", age=age,value=100*sum(tempdata$q03_odd_phrase==1,na.rm=TRUE)/(sum(tempdata$q03_odd_phrase==1,na.rm=TRUE)+sum(tempdata$q03_odd_phrase==0,na.rm=TRUE))))
    q04percent <- rbind(q04percent,data.frame(q = "Q4", age=age,value=100*sum(tempdata$q04_inappropriate_question==1,na.rm=TRUE)/(sum(tempdata$q04_inappropriate_question==1,na.rm=TRUE)+sum(tempdata$q04_inappropriate_question==0,na.rm=TRUE))))
    q05percent <- rbind(q05percent,data.frame(q = "Q5", age=age,value=100*sum(tempdata$q05_pronouns_mixed==1,na.rm=TRUE)/(sum(tempdata$q05_pronouns_mixed==1,na.rm=TRUE)+sum(tempdata$q05_pronouns_mixed==0,na.rm=TRUE))))
    q06percent <- rbind(q06percent,data.frame(q = "Q6", age=age,value=100*sum(tempdata$q06_invented_words==1,na.rm=TRUE)/(sum(tempdata$q06_invented_words==1,na.rm=TRUE)+sum(tempdata$q06_invented_words==0,na.rm=TRUE))))
    q07percent <- rbind(q07percent,data.frame(q = "Q7", age=age,value=100*sum(tempdata$q07_same_over==1,na.rm=TRUE)/(sum(tempdata$q07_same_over==1,na.rm=TRUE)+sum(tempdata$q07_same_over==0,na.rm=TRUE))))
  }
}
plotpercents <- rbind(q02percent, q03percent, q04percent, q05percent, q06percent, q07percent)
plotpercents <- plotpercents %>% 
  rename(Age.Started.Combining.Words = age,
         Percent.Endorsed = value)

scale <- 10
png("./Manuscript/Supplemental Materials/Imputed_SCQ_by_Age.png", width = 720*scale, height = 520*scale, res = 72*scale)
ggplot(plotpercents, aes(x = Age.Started.Combining.Words, y = Percent.Endorsed))+
  geom_point(size = 2, alpha = .4) +
  theme_bw() +
  facet_wrap(~q) +
  ylim(c(0,100)) +
  xlab("Age Started Combining Words") + 
  ylab("Percentage Endorsed") +
  theme(axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20),
        strip.text.x = element_text(size = 16),
        axis.text = element_text(size = 12))
dev.off()

# sum(data$combined_words_age_mos == 888 ,na.rm=T)
# sum(data$combined_words_age_mos == 85 ,na.rm=T)


# 2410 kids not yet speaking, wrt combined_words
# sum(data$combined_words_age_mos == 888, na.rm = T)

# 2207 kids for imputation (no phrases, not yet combining words) 
#sum(data$combined_words_age_mos == 888 & data$q01_phrases == F, na.rm = T)

# 1454 kids who are older than 7, not yet speaking for imputation
#sum(data$combined_words_age_mos == 888 & data$q01_phrases == F & data$age_at_registration_months <= 84, na.rm = T)
imputed_IDS <- data$subject_sp_id[which(data$combined_words_age_mos == 888 & data$q01_phrases == F & data$age_at_registration_months <= 84)]

set.seed(123)

data <- data %>% 
  rowwise() %>% 
  mutate(q02_conversation = 
           case_when(q01_phrases == 0 & age_at_registration_months >= 85 & combined_words_age_mos == 888 ~ sample(c(0,1),size=1, prob=c(1-q02percent$value[q02percent$age == 85]/100,
                                                                                                                                                                    q02percent$value[q02percent$age == 85]/100)),
                     q01_phrases == 0 & combined_words_age_mos < 85 ~ ifelse(is.na(combined_words_age_mos), NA, sample(c(0,1),size=1,prob=c(1-q02percent$value[which.min(abs(q02percent$age-combined_words_age_mos))]/100,q02percent$value[which.min(abs(q02percent$age-combined_words_age_mos))]/100))), 
                     q01_phrases == 0 & combined_words_age_mos == 888 & age_at_registration_months < 85  ~ ifelse(is.na(combined_words_age_mos), NA, sample(c(0,1),size=1,prob=c(1-q02percent$value[which.min(abs(q02percent$age-combined_words_age_mos))]/100,q02percent$value[which.min(abs(q02percent$age-combined_words_age_mos))]/100))), 
                     q01_phrases == 1 ~ q02_conversation))



data <- data %>% 
  rowwise() %>% 
  mutate(q03_odd_phrase = 
           case_when(q01_phrases == 0 & age_at_registration_months >= 85 & combined_words_age_mos == 888 ~ sample(c(0,1),size=1, prob=c(1-q03percent$value[q03percent$age == 85]/100,
                                                                                                                                        q03percent$value[q03percent$age == 85]/100)),
                     q01_phrases == 0 & combined_words_age_mos < 85 ~ ifelse(is.na(combined_words_age_mos), NA, sample(c(0,1),size=1,prob=c(1-q03percent$value[which.min(abs(q03percent$age-combined_words_age_mos))]/100,q03percent$value[which.min(abs(q03percent$age-combined_words_age_mos))]/100))), 
                     q01_phrases == 0 & combined_words_age_mos == 888 & age_at_registration_months < 85  ~ ifelse(is.na(combined_words_age_mos), NA, sample(c(0,1),size=1,prob=c(1-q03percent$value[which.min(abs(q03percent$age-combined_words_age_mos))]/100,q03percent$value[which.min(abs(q03percent$age-combined_words_age_mos))]/100))), 
                     q01_phrases == 1 ~ q03_odd_phrase))



data <- data %>% 
  rowwise() %>% 
  mutate(q04_inappropriate_question = 
           case_when(q01_phrases == 0 & age_at_registration_months >= 85 & combined_words_age_mos == 888 ~ sample(c(0,1),size=1, prob=c(1-q04percent$value[q04percent$age == 85]/100,
                                                                                                                                        q04percent$value[q04percent$age == 85]/100)),
                     q01_phrases == 0 & combined_words_age_mos < 85 ~ ifelse(is.na(combined_words_age_mos), NA, sample(c(0,1),size=1,prob=c(1-q04percent$value[which.min(abs(q04percent$age-combined_words_age_mos))]/100,q04percent$value[which.min(abs(q04percent$age-combined_words_age_mos))]/100))), 
                     q01_phrases == 0 & combined_words_age_mos == 888 & age_at_registration_months < 85  ~ ifelse(is.na(combined_words_age_mos), NA, sample(c(0,1),size=1,prob=c(1-q04percent$value[which.min(abs(q04percent$age-combined_words_age_mos))]/100,q04percent$value[which.min(abs(q04percent$age-combined_words_age_mos))]/100))), 
                     q01_phrases == 1 ~ q04_inappropriate_question))


data <- data %>% 
  rowwise() %>% 
  mutate(q05_pronouns_mixed = 
           case_when(q01_phrases == 0 & age_at_registration_months >= 85 & combined_words_age_mos == 888 ~ sample(c(0,1),size=1, prob=c(1-q05percent$value[q05percent$age == 85]/100,
                                                                                                                                        q05percent$value[q05percent$age == 85]/100)),
                     q01_phrases == 0 & combined_words_age_mos < 85 ~ ifelse(is.na(combined_words_age_mos), NA, sample(c(0,1),size=1,prob=c(1-q05percent$value[which.min(abs(q05percent$age-combined_words_age_mos))]/100,q05percent$value[which.min(abs(q05percent$age-combined_words_age_mos))]/100))), 
                     q01_phrases == 0 & combined_words_age_mos == 888 & age_at_registration_months < 85  ~ ifelse(is.na(combined_words_age_mos), NA, sample(c(0,1),size=1,prob=c(1-q05percent$value[which.min(abs(q05percent$age-combined_words_age_mos))]/100,q05percent$value[which.min(abs(q05percent$age-combined_words_age_mos))]/100))), 
                     q01_phrases == 1 ~ q05_pronouns_mixed))



data <- data %>% 
  rowwise() %>% 
  mutate(q06_invented_words = 
           case_when(q01_phrases == 0 & age_at_registration_months >= 85 & combined_words_age_mos == 888 ~ sample(c(0,1),size=1, prob=c(1-q06percent$value[q06percent$age == 85]/100,
                                                                                                                                        q06percent$value[q06percent$age == 85]/100)),
                     q01_phrases == 0 & combined_words_age_mos < 85 ~ ifelse(is.na(combined_words_age_mos), NA, sample(c(0,1),size=1,prob=c(1-q06percent$value[which.min(abs(q06percent$age-combined_words_age_mos))]/100,q06percent$value[which.min(abs(q06percent$age-combined_words_age_mos))]/100))), 
                     q01_phrases == 0 & combined_words_age_mos == 888 & age_at_registration_months < 85  ~ ifelse(is.na(combined_words_age_mos), NA, sample(c(0,1),size=1,prob=c(1-q06percent$value[which.min(abs(q06percent$age-combined_words_age_mos))]/100,q06percent$value[which.min(abs(q06percent$age-combined_words_age_mos))]/100))), 
                     q01_phrases == 1 ~ q06_invented_words))

data <- data %>% 
  rowwise() %>% 
  mutate(q07_same_over = 
           case_when(q01_phrases == 0 & age_at_registration_months >= 85 & combined_words_age_mos == 888 ~ sample(c(0,1),size=1, prob=c(1-q07percent$value[q07percent$age == 85]/100,
                                                                                                                                        q07percent$value[q07percent$age == 85]/100)),
                     q01_phrases == 0 & combined_words_age_mos < 85 ~ ifelse(is.na(combined_words_age_mos), NA, sample(c(0,1),size=1,prob=c(1-q07percent$value[which.min(abs(q07percent$age-combined_words_age_mos))]/100,q07percent$value[which.min(abs(q07percent$age-combined_words_age_mos))]/100))), 
                     q01_phrases == 0 & combined_words_age_mos == 888 & age_at_registration_months < 85  ~ ifelse(is.na(combined_words_age_mos), NA, sample(c(0,1),size=1,prob=c(1-q07percent$value[which.min(abs(q07percent$age-combined_words_age_mos))]/100,q07percent$value[which.min(abs(q07percent$age-combined_words_age_mos))]/100))), 
                     q01_phrases == 1 ~ q07_same_over))

#compare empirical to simulated/imputed

data %>% ungroup %>% filter(subject_sp_id %in% imputed_IDS) %>%
  summarize(q2 = mean(q02_conversation,na.rm=T),
            q3 = mean(q03_odd_phrase,na.rm=T),
            q4 = mean(q04_inappropriate_question,na.rm=T),
            q5 = mean(q05_pronouns_mixed,na.rm=T),
            q6 = mean(q06_invented_words,na.rm=T),
            q7 = mean(q07_same_over,na.rm=T))

round((plotpercents %>% filter(age == 85))$value, 3)
# matches pretty well

#binarize the following columns to deal with the 888 problem. adjust age in months to normal age of occurrence
data$crawled_age_mos <- ifelse(data$crawled_age_mos >12, 1, 0)
data$smiled_age_mos <- ifelse(data$smiled_age_mos >3, 1, 0)
data$used_words_age_mos <- ifelse(data$used_words_age_mos >18, 1, 0)
data$combined_words_age_mos <- ifelse(data$combined_words_age_mos >25, 1, 0)
data$combined_phrases_age_mos <- ifelse(data$combined_phrases_age_mos >36, 1, 0)

#adjust columns that are binary but use NA for 0
binary_columns <- data_dictionary %>%
  filter(`answer choices/type` == "1|null") %>%
  pull(var_name)
binary_columns <- intersect(names(data),binary_columns)
data[binary_columns][is.na(data[binary_columns])] <- 0

#isolate data with no missing data
data_no_missing <- data[complete.cases(data),]

#make sure the data type of the replaced columns is right for later steps
for (col_name in names(data_no_missing)[-1]) {
  if (!is.numeric(data_no_missing[[col_name]])) {
    data_no_missing[[col_name]] <- as.numeric(data_no_missing[[col_name]])
  }
}


# write.csv(data_no_missing$subject_sp_id, "./data/included_subjects.csv", row.names = F)


################################################################################
#standardize columns
################################################################################
#start by removing age column that is no longer needed
data_no_missing <- data_no_missing %>% select(-age_at_eval_months, -age_at_registration_months)
#scale 0-1
#scaled_data <- data_no_missing %>%
#  mutate_at(vars(2:ncol(data_no_missing)), ~ (.-min(.))/(max(.)-min(.)))


scaled_data <- data_no_missing[,-1] %>%
  apply(2,function(x) (x-min(x))/(max(x)-min(x)))

scaled_data <- as.data.frame(cbind(data_no_missing$subject_sp_id,scaled_data))
for (col_name in names(scaled_data)[-1]) {
  if (!is.numeric(scaled_data[[col_name]])) {
    scaled_data[[col_name]] <- as.numeric(scaled_data[[col_name]])
  }
}


saveRDS(scaled_data, "./data/data_12_10_24.rds")

#
scaled_data <- readRDS("./data/data_12_10_24.rds")

reverse_score_corrected_data <- scaled_data %>%
  mutate(across(c(q01_phrases:q02_conversation, q09_expressions_appropriate, q19_best_friend:q40_cooperatively_games, # SCQ items
                  q01_throw_ball:q15_fatigue_easily, # whole DCDQ
                  cog_age_level, language_age_level), # 
                ~ (-1*.x + 1))) %>% 
  select(-V1)


saveRDS(reverse_score_corrected_data, "./data/reverse_score_corrected_data_2_26_25.rds")

