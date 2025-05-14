#This script tests the fit of the data to the original psychometric paper's factors (without a bifactor in all cases - should check if original did)
# THE SCQ FIT IS NOT VALID - IMPUTED LANGUAGE ITEMS DISRUPTED MULTIPLE SCQ FACTORS
# ALL REVERSE SCORED ITEMS HAVE ALREADY BEEN PROPERLY REVERSE SCORED 


rm(list = ls())
library(psych)
library(tidyverse)
library(readxl)
library(lavaanPlot)
library(lavaan)
library(semPlot)

data <- readRDS("./data/imputation50_FULL_output_mcweeny_10_15_24.rds")
set.seed(123)
data_2 <- data %>% slice_sample(n = 10000)

SM <- read_xlsx("./Item_Voting_SM.xlsx")
SM <- SM %>% filter(!is.na(name))

dichotomous_items <- names(which(unlist(lapply(data_2, function(col){length(levels(factor(col)))})) == 2))

# SM_2 <- SM %>% filter(!name %in% c('q16_complete', 'q20_hoarding'))


data_scq <- data_2 %>% select(SM$name[SM$from == "scq"], -q01_phrases)
data_dcdq <- data_2 %>% select(SM$name[SM$from == "dcdq"])
data_rbsr <- data_2 %>% select(SM$name[SM$from == "rbs-r"])



#####
test_syntax <- 'm1=~q01_whole_body+q01_throw_ball+q06_plan_motor_activity'

test_cfa <- lavaan::cfa(test_syntax, data = data_2, meanstructure = T, std.lv = T)
summary(test_cfa, fit.measures = TRUE, standardized = TRUE)


# paste0(names(data_rbsr)[c(1, 2, 3, 4, 5, 6, 19, 39, 40)], collapse= "+")
# paste0(names(data_rbsr)[7:14], collapse= "+")
# paste0(names(data_rbsr)[c(15, 16, 17, 41, 42, 43)], collapse= "+")
# paste0(names(data_rbsr)[23:36], collapse= "+")
# paste0(names(data_rbsr)[c(33, 37, 38)], collapse= "+")

rbsr_syntax <- 'Stereotypic=~q01_whole_body+q02_head+q03_hand_finger+q04_locomotion+q05_object_usage+q06_sensory+q22_touch_tap+q42_preoccupation+q43_fascination_movement
                SelfInjurious=~q07_hits_self_body+q08_hits_self_against_object+q09_hits_self_with_object+q10_bites_self+q11_pulls+q12_rubs+q13_inserts_finger+q14_skin_picking
                Compulsive=~q15_arranging+q17_washing+q18_checking+q16_complete+q20_hoarding+q19_counting
                Ritualistic=~q26_travel+q27_play+q28_communication+q29_things_same_place+q30_objects+q31_becomes_upset+q32_insists_walking+q33_insists_sitting+q34_dislikes_changes+q35_insists_door+q37_resists_change+q38_insists_routine+q39_insists_time
                RestrictedInterests=~q36_likes_piece_music+q40_fascination_subject+q41_strongly_attached'


rbsr_cfa <- lavaan::cfa(rbsr_syntax, data = data_2, meanstructure = T, std.lv = T)
modificationindices(rbsr_cfa) %>% arrange(desc(mi))
summary(rbsr_cfa, fit.measures = TRUE, standardized = TRUE)
semPaths(
  rbsr_cfa, 
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

psych::alpha(fitted(rbsr_cfa)$cov)
psych::omega(fitted(dcdq_cfa)$cov, nfactors = 5)




# paste0(names(data_dcdq)[1:6], collapse = "+")
# paste0(names(data_dcdq)[7:10], collapse = "+")
# paste0(names(data_dcdq)[11:15], collapse = "+")


dcdq_syntax <- 'Control=~q01_throw_ball+q02_catch_ball+q03_hit_ball+q04_jump_obstacles+q05_run_fast_similar+q06_plan_motor_activity
                FineMotor=~q07_printing_writing_drawing_fast+q08_printing_letters_legible+q09_appropriate_tension_printing_writing+q10_cuts_pictures_shapes
                Coordination=~q11_likes_sports_motors_skills+q12_learns_new_motor_tasks+q13_quick_competent_tidying_up+q14_bull_in_china_shop+q15_fatigue_easily'

# dcdq_syntax <- 'Control=~q01_throw_ball+q02_catch_ball+q03_hit_ball+q04_jump_obstacles+q05_run_fast_similar+q06_plan_motor_activity
#                 FineMotor=~q07_printing_writing_drawing_fast+q08_printing_letters_legible+q09_appropriate_tension_printing_writing+q10_cuts_pictures_shapes
#                 Coordination=~q11_likes_sports_motors_skills+q12_learns_new_motor_tasks+q13_quick_competent_tidying_up'

dcdq_cfa <- lavaan::cfa(dcdq_syntax, data = data_2, meanstructure = T, std.lv = T)
modificationindices(dcdq_cfa) %>% arrange(desc(mi))
summary(dcdq_cfa, fit.measures = TRUE, standardized = TRUE)
semPlot::semPaths(dcdq_cfa, 
                  intercepts = F,
                  layout = "tree3",
                  what = "std",
                  residuals = F)


psych::alpha(fitted(dcdq_cfa)$cov)
psych::omega(fitted(dcdq_cfa)$cov, nfactors = 3)

# paste0(names(data_scq)[c(9, 10, 19, 19, 26, 27:33, 36, 37, 39, 40) - 1], collapse = '+')
# paste0(names(data_scq)[c(2:6, 20:25, 34) - 1], collapse = '+')
# paste0(names(data_scq)[c(35, 7,8,11:16) - 1], collapse = '+')

# paste0(names(data_scq)[c(29, 36, 40, 37, 34, 31, 28, 30, 21, 39, 22, 27, 26, 35, 33, 38, 23,32,20,18) - 1], collapse = '+')
# paste0(names(data_scq)[c(2, 3, 24,25, 10, 16) - 1], collapse = '+')
# paste0(names(data_scq)[c(4:8) - 1], collapse = '+')
# paste0(names(data_scq)[c(9, 11:15, 17, 19) - 1], collapse = '+')

# scq_syntax <- 'Soc.Int=~q09_expressions_appropriate+q10_hand_tool+q19_best_friend+q19_best_friend+q26_look_directly+q27_smile_back+q28_things_interested+q29_share+q30_join_enjoyment+q31_comfort+q32_help_attention+q33_range_expressions+q36_same_age+q37_respond_positively+q39_imaginative_games+q40_cooperatively_games
#               Communication=~q02_conversation+q03_odd_phrase+q04_inappropriate_question+q05_pronouns_mixed+q06_invented_words+q20_talk_friendly+q21_copy_you+q22_point_things+q23_gestures_wanted+q24_nod_head+q25_shake_head+q34_copy_actions
#               RRB=~q35_make_believe+q07_same_over+q08_particular_way+q11_interest_preoccupy+q12_parts_object+q13_interests_intensity+q14_senses+q15_odd_ways+q16_complicated_movements'


data_lang <- data_2 %>% filter(q01_phrases == 1)
data_scq <- data_2 %>% filter(q01_phrases == 1) %>% select(SM$name[SM$from == "scq"], -q01_phrases)

# scq_syntax <- 'Soc.Int=~q29_share+q36_same_age+q40_cooperatively_games+q37_respond_positively+q34_copy_actions+q31_comfort+q28_things_interested+q30_join_enjoyment+q21_copy_you+q39_imaginative_games+q22_point_things+q27_smile_back+q26_look_directly+q35_make_believe+q33_range_expressions+q38_pay_attention+q23_gestures_wanted+q32_help_attention+q20_talk_friendly+q18_objects_carry
#               Communication=~q02_conversation+q03_odd_phrase+q24_nod_head+q25_shake_head+q10_hand_tool+q16_complicated_movements              
#               Lang=~q04_inappropriate_question+q05_pronouns_mixed+q06_invented_words+q07_same_over+q08_particular_way
#               RRB=~q09_expressions_appropriate+q11_interest_preoccupy+q12_parts_object+q13_interests_intensity+q14_senses+q15_odd_ways+q17_injured_deliberately+q19_best_friend'

test <- tetrachoric(data_scq)$rho
corPlot(test)

scq_syntax <- 'Soc.Int=~q29_share+q36_same_age+q40_cooperatively_games+q37_respond_positively+q31_comfort+q28_things_interested+q30_join_enjoyment+q39_imaginative_games+q27_smile_back+q26_look_directly+q33_range_expressions+q32_help_attention+q20_talk_friendly
              Communication=~q34_copy_actions+q21_copy_you+q22_point_things+q35_make_believe+
              
              q02_conversation+q03_odd_phrase+q24_nod_head+q25_shake_head+q10_hand_tool+q16_complicated_movements
              Lang=~q04_inappropriate_question+q05_pronouns_mixed+q06_invented_words+q07_same_over+q08_particular_way
              RRB=~q09_expressions_appropriate+q11_interest_preoccupy+q12_parts_object+q13_interests_intensity+q14_senses+q15_odd_ways+q17_injured_deliberately+q18_objects_carry'






scq_cfa <- lavaan::cfa(scq_syntax, data = data_lang, meanstructure = T, std.lv = T, ordered = T)
modificationindices(scq_cfa) %>% arrange(desc(mi))
summary(scq_cfa, fit.measures = TRUE, standardized = TRUE)
semPlot::semPaths(scq_cfa, 
                  intercepts = F,
                  layout = "tree3",
                  what = "std",
                  residuals = F)


###################################

rbsr_syntax_bifactor <- 'Stereotypic=~q01_whole_body+q02_head+q03_hand_finger+q04_locomotion+q05_object_usage+q06_sensory+q22_touch_tap+q42_preoccupation+q43_fascination_movement
                SelfInjurious=~q07_hits_self_body+q08_hits_self_against_object+q09_hits_self_with_object+q10_bites_self+q11_pulls+q12_rubs+q13_inserts_finger+q14_skin_picking
                Compulsive=~q15_arranging+q17_washing+q18_checking+q16_complete+q20_hoarding+q19_counting
                Ritualistic=~q26_travel+q27_play+q28_communication+q29_things_same_place+q30_objects+q31_becomes_upset+q32_insists_walking+q33_insists_sitting+q34_dislikes_changes+q35_insists_door+q37_resists_change+q38_insists_routine+q39_insists_time
                RestrictedInterests=~q36_likes_piece_music+q40_fascination_subject+q41_strongly_attached
                g=~q01_whole_body+q02_head+q03_hand_finger+q04_locomotion+q05_object_usage+q06_sensory+q22_touch_tap+q42_preoccupation+q43_fascination_movement+q07_hits_self_body+q08_hits_self_against_object+q09_hits_self_with_object+q10_bites_self+q11_pulls+q12_rubs+q13_inserts_finger+q14_skin_picking+q15_arranging+q17_washing+q18_checking+q16_complete+q20_hoarding+q19_counting+q26_travel+q27_play+q28_communication+q29_things_same_place+q30_objects+q31_becomes_upset+q32_insists_walking+q33_insists_sitting+q34_dislikes_changes+q35_insists_door+q37_resists_change+q38_insists_routine+q39_insists_time+q36_likes_piece_music+q40_fascination_subject+q41_strongly_attached
                g~~0*Stereotypic
                g~~0*SelfInjurious
                g~~0*Ritualistic
                g~~0*RestrictedInterests
                g~~0*Compulsive'


rbsr_cfa_bifactor <- lavaan::cfa(rbsr_syntax_bifactor, data = data_2, meanstructure = T, std.lv = T)
modificationindices(rbsr_cfa_bifactor) %>% arrange(desc(mi))
summary(rbsr_cfa_bifactor, fit.measures = TRUE, standardized = TRUE)
semPlot::semPaths(rbsr_cfa_bifactor, 
                  intercepts = F,
                  layout = "tree3",
                  bifactor = "g",
                  what = "std",
                  residuals = F)



dcdq_bifactor_syntax <- 'Control=~q01_throw_ball+q02_catch_ball+q03_hit_ball+q04_jump_obstacles+q05_run_fast_similar+q06_plan_motor_activity
                FineMotor=~q07_printing_writing_drawing_fast+q08_printing_letters_legible+q09_appropriate_tension_printing_writing+q10_cuts_pictures_shapes
                Coordination=~q11_likes_sports_motors_skills+q12_learns_new_motor_tasks+q13_quick_competent_tidying_up+q14_bull_in_china_shop+q15_fatigue_easily
                g=~q01_throw_ball+q02_catch_ball+q03_hit_ball+q04_jump_obstacles+q05_run_fast_similar+q06_plan_motor_activity+q07_printing_writing_drawing_fast+q08_printing_letters_legible+q09_appropriate_tension_printing_writing+q10_cuts_pictures_shapes+q11_likes_sports_motors_skills+q12_learns_new_motor_tasks+q13_quick_competent_tidying_up+q14_bull_in_china_shop+q15_fatigue_easily
                g~~0*Control
                g~~0*FineMotor
                g~~0*Coordination'


dcdq_cfa_bifactor <- lavaan::cfa(dcdq_bifactor_syntax, data = data_2, meanstructure = T, std.lv = T)
modificationindices(dcdq_cfa_bifactor) %>% arrange(desc(mi))
summary(dcdq_cfa_bifactor, fit.measures = TRUE, standardized = TRUE)
semPlot::semPaths(dcdq_cfa_bifactor, 
                  intercepts = F,
                  layout = "tree3",
                  bifactor = "g",
                  what = "std",
                  residuals = F)

# paste0(names(data_scq)[c(9, 10, 19, 19, 26, 27:33, 36, 37, 39, 40) - 1], collapse = '+')
# paste0(names(data_scq)[c(2:6, 20:25, 34) - 1], collapse = '+')
# paste0(names(data_scq)[c(35, 7,8,11:16) - 1], collapse = '+')

# paste0(names(data_scq)[c(29, 36, 40, 37, 34, 31, 28, 30, 21, 39, 22, 27, 26, 35, 33, 38, 23,32,20,18) - 1], collapse = '+')
# paste0(names(data_scq)[c(2, 3, 24,25, 10, 16) - 1], collapse = '+')
# paste0(names(data_scq)[c(4:8) - 1], collapse = '+')
# paste0(names(data_scq)[c(9, 11:15, 17, 19) - 1], collapse = '+')
# 
# scq_syntax <- 'Soc.Int=~q09_expressions_appropriate+q10_hand_tool+q19_best_friend+q19_best_friend+q26_look_directly+q27_smile_back+q28_things_interested+q29_share+q30_join_enjoyment+q31_comfort+q32_help_attention+q33_range_expressions+q36_same_age+q37_respond_positively+q39_imaginative_games+q40_cooperatively_games
#               Communication=~q02_conversation+q03_odd_phrase+q04_inappropriate_question+q05_pronouns_mixed+q06_invented_words+q20_talk_friendly+q21_copy_you+q22_point_things+q23_gestures_wanted+q24_nod_head+q25_shake_head+q34_copy_actions
#               RRB=~q35_make_believe+q07_same_over+q08_particular_way+q11_interest_preoccupy+q12_parts_object+q13_interests_intensity+q14_senses+q15_odd_ways+q16_complicated_movements'

scq_bifactor_syntax <- 'Soc.Int=~q29_share+q36_same_age+q40_cooperatively_games+q37_respond_positively+q34_copy_actions+q31_comfort+q28_things_interested+q30_join_enjoyment+q21_copy_you+q39_imaginative_games+q22_point_things+q27_smile_back+q26_look_directly+q35_make_believe+q33_range_expressions+q38_pay_attention+q23_gestures_wanted+q32_help_attention+q20_talk_friendly+q18_objects_carry
              Communication=~q02_conversation+q03_odd_phrase+q24_nod_head+q25_shake_head+q10_hand_tool+q16_complicated_movements              
              #Lang=~q04_inappropriate_question+q05_pronouns_mixed+q06_invented_words+q07_same_over+q08_particular_way
              RRB=~q09_expressions_appropriate+q11_interest_preoccupy+q12_parts_object+q13_interests_intensity+q14_senses+q15_odd_ways+q17_injured_deliberately+q19_best_friend
              g=~q29_share+q36_same_age+q40_cooperatively_games+q37_respond_positively+q34_copy_actions+q31_comfort+q28_things_interested+q30_join_enjoyment+q21_copy_you+q39_imaginative_games+q22_point_things+q27_smile_back+q26_look_directly+q35_make_believe+q33_range_expressions+q38_pay_attention+q23_gestures_wanted+q32_help_attention+q20_talk_friendly+q18_objects_carry+q02_conversation+q03_odd_phrase+q24_nod_head+q25_shake_head+q10_hand_tool+q16_complicated_movements+q04_inappropriate_question+q05_pronouns_mixed+q06_invented_words+q07_same_over+q08_particular_way+q09_expressions_appropriate+q11_interest_preoccupy+q12_parts_object+q13_interests_intensity+q14_senses+q15_odd_ways+q17_injured_deliberately+q19_best_friend
              g~~0*Soc.Int
              g~~0*Communication
              g~~0*RRB
              #g~~0*Lang'


scq_cfa_bifactor <- lavaan::cfa(scq_bifactor_syntax, data = data_2, meanstructure = T, std.lv = T, ordered = T)
modificationindices(scq_cfa_bifactor) %>% arrange(desc(mi))
summary(scq_cfa_bifactor, fit.measures = TRUE, standardized = TRUE)
semPlot::semPaths(scq_cfa_bifactor, 
                  intercepts = F,
                  layout = "tree3",
                  sizeMan = .3,
                  sizeMan2 = .3,
                  bifactor = "g",
                  what = "std",
                  residuals = F)

##############


rbsr_syntax_bifactor_hier <- 'Stereotypic=~q01_whole_body+q02_head+q03_hand_finger+q04_locomotion+q05_object_usage+q06_sensory+q22_touch_tap+q42_preoccupation+q43_fascination_movement
                SelfInjurious=~q07_hits_self_body+q08_hits_self_against_object+q09_hits_self_with_object+q10_bites_self+q11_pulls+q12_rubs+q13_inserts_finger+q14_skin_picking
                Compulsive=~q15_arranging+q17_washing+q18_checking+q16_complete+q20_hoarding+q19_counting
                Ritualistic=~q26_travel+q27_play+q28_communication+q29_things_same_place+q30_objects+q31_becomes_upset+q32_insists_walking+q33_insists_sitting+q34_dislikes_changes+q35_insists_door+q37_resists_change+q38_insists_routine+q39_insists_time
                RestrictedInterests=~q36_likes_piece_music+q40_fascination_subject+q41_strongly_attached
                g=~Stereotypic+SelfInjurious+Ritualistic+RestrictedInterests+Compulsive
                Stereotypic~~0*SelfInjurious
                Stereotypic~~0*Compulsive
                Stereotypic~~0*Ritualistic
                Stereotypic~~0*RestrictedInterests
                SelfInjurious~~0*Compulsive
                SelfInjurious~~0*Ritualistic
                SelfInjurious~~0*RestrictedInterests
                Compulsive~~0*Ritualistic
                Compulsive~~0*RestrictedInterests
                Ritualistic~~0*RestrictedInterests'

# t <- combn(c("Stereotypic", 'SelfInjurious', 'Compulsive', 'Ritualistic', 'RestrictedInterests'), 2, FUN = function(x) paste0(x[1], "~~0*", x[2]))
# paste0(t, collapse = "\n")


rbsr_cfa_bifactor_hier <- lavaan::cfa(rbsr_syntax_bifactor_hier, data = data_2, meanstructure = T, std.lv = T)
modificationindices(rbsr_cfa_bifactor_hier) %>% arrange(desc(mi))
summary(rbsr_cfa_bifactor_hier, fit.measures = TRUE, standardized = TRUE)
semPlot::semPaths(rbsr_cfa_bifactor_hier, 
                  intercepts = F,
                  layout = "tree3",
                  what = "std",
                  residuals = F)



# t <- combn(c("Control", 'FineMotor', 'Coordination'), 2, FUN = function(x) paste0(x[1], "~~0*", x[2]))
# paste0(t, collapse = "\n")

dcdq_bifactor_syntax_hier <- 'Control=~q01_throw_ball+q02_catch_ball+q03_hit_ball+q04_jump_obstacles+q05_run_fast_similar+q06_plan_motor_activity
                FineMotor=~q07_printing_writing_drawing_fast+q08_printing_letters_legible+q09_appropriate_tension_printing_writing+q10_cuts_pictures_shapes
                Coordination=~q11_likes_sports_motors_skills+q12_learns_new_motor_tasks+q13_quick_competent_tidying_up+q14_bull_in_china_shop+q15_fatigue_easily
                g=~Control+FineMotor+Coordination
                Control~~0*FineMotor
                Control~~0*Coordination
                FineMotor~~0*Coordination'


dcdq_cfa_bifactor_hier <- lavaan::cfa(dcdq_bifactor_syntax_hier, data = data_2, meanstructure = T, std.lv = T)
modificationindices(dcdq_cfa_bifactor_hier) %>% arrange(desc(mi))
summary(dcdq_cfa_bifactor_hier, fit.measures = TRUE, standardized = TRUE)
semPlot::semPaths(dcdq_cfa_bifactor_hier, 
                  intercepts = F,
                  layout = "tree3",
                  what = "std",
                  residuals = F)


# t <- combn(c("Soc.Int", 'Communication', 'Lang','RRB'), 2, FUN = function(x) paste0(x[1], "~~0*", x[2]))
# paste0(t, collapse = "\n")


scq_bifactor_syntax_hier <- 'Soc.Int=~q29_share+q36_same_age+q40_cooperatively_games+q37_respond_positively+q34_copy_actions+q31_comfort+q28_things_interested+q30_join_enjoyment+q21_copy_you+q39_imaginative_games+q22_point_things+q27_smile_back+q26_look_directly+q35_make_believe+q33_range_expressions+q38_pay_attention+q23_gestures_wanted+q32_help_attention+q20_talk_friendly+q18_objects_carry
              Communication=~q02_conversation+q03_odd_phrase+q24_nod_head+q25_shake_head+q10_hand_tool+q16_complicated_movements              
              Lang=~q04_inappropriate_question+q05_pronouns_mixed+q06_invented_words+q07_same_over+q08_particular_way
              RRB=~q09_expressions_appropriate+q11_interest_preoccupy+q12_parts_object+q13_interests_intensity+q14_senses+q15_odd_ways+q17_injured_deliberately+q19_best_friend
              g=~Soc.Int+Communication+RRB+Lang
              Soc.Int~~0*Communication 
              Soc.Int~~0*Lang 
              Soc.Int~~0*RRB 
              Communication~~0*Lang 
              Communication~~0*RRB 
              Lang~~0*RRB'


scq_cfa_bifactor_hier <- lavaan::cfa(scq_bifactor_syntax_hier, data = data_2, meanstructure = T, std.lv = T, ordered = T)
modificationindices(scq_cfa_bifactor_hier) %>% arrange(desc(mi))
summary(scq_cfa_bifactor_hier, fit.measures = TRUE, standardized = TRUE)
semPlot::semPaths(scq_cfa_bifactor_hier, 
                  intercepts = F,
                  layout = "tree3",
                  what = "std",
                  residuals = F)
