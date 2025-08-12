sim_betas_lavaan_std_all <- function(dat, theories){
  #takes in a pseudo-dataframe and returns the list of relevant betas/cors
  pseudo <- dat %>% mutate(across(everything(), ~scale(.x)))
  
  
  if(theories == "M&E"){
    
    ME.2011.syntax <- 'Motor.Skills ~ b1*Imitation
                  Social.Skills ~ b2*Imitation
                  Motor.Skills ~~ phi*Social.Skills'
    
    ME.mod <- sem(model = ME.2011.syntax, 
                  data = pseudo)
    
    
    ME1.Mot.Imi <- parameterestimates(ME.mod, standardized = T)$std.all[parameterestimates(ME.mod)$label == "b1"] 
    ME2.Soc.Imi <- parameterestimates(ME.mod, standardized = T)$std.all[parameterestimates(ME.mod)$label == "b2"]
    
    ME3.Mot.Soc <- parameterestimates(ME.mod, standardized = T)$std.all[parameterestimates(ME.mod)$label == "phi"]
    ME4.Mot.Soc <- cor(pseudo$Social.Skills, pseudo$Motor.Skills)
    
    
    ME_row1 <- c(measure_1 = "Motor.Skills", measure_2 = "Imitation", correlation_value = ME1.Mot.Imi, partialled = "No", theory = "M&E")
    ME_row2 <- c(measure_1 = "Social.Skills", measure_2 = "Imitation", correlation_value = ME2.Soc.Imi, partialled = "No", theory = "M&E")
    ME_row3 <- c(measure_1 = "Social.Skills", measure_2 = "Motor.Skills", correlation_value = ME3.Mot.Soc, partialled = "Imitation", theory = "M&E")
    ME_row4 <- c(measure_1 = "Social.Skills", measure_2 = "Motor.Skills", correlation_value = ME4.Mot.Soc, partialled = "No", theory = "M&E")
    
    
    
    df_long <- data.frame(
      rbind(ME_row1, ME_row2, ME_row3, ME_row4))
    
  } else if(theories == "R&P"){
    
    
    RP.1991.syntax.11 <- 'Affect.Sharing ~ b1*Imitation
                        Symbolism.11 ~ b2*Imitation
                        Social.Eng ~ b3*Affect.Sharing + b4*Imitation'
    
    RP.mod.11 <- sem(model = RP.1991.syntax.11, 
                     data = pseudo )
    
    
    
    
    
    RP1.Imi.AS.11 <- parameterestimates(RP.mod.11, standardized = T)$std.all[parameterestimates(RP.mod.11)$label == "b1"]
    RP2.Imi.Sym.11 <- parameterestimates(RP.mod.11, standardized = T)$std.all[parameterestimates(RP.mod.11)$label == "b2"]
    
    RP3.AS.Soc.11 <- parameterestimates(RP.mod.11, standardized = T)$std.all[parameterestimates(RP.mod.11)$label == "b3"]
    RP4.Imi.Soc.11 <- parameterestimates(RP.mod.11, standardized = T)$std.all[parameterestimates(RP.mod.11)$label == "b4"]
    
    RP5.Sym.Soc.11 <- parameterestimates(RP.mod.11, standardized = T)$std.all[8]
    
    
    RP_row1.11 <- c(measure_1 = "Imitation", measure_2 = "Affect.Sharing", correlation_value = RP1.Imi.AS.11, partialled = "No", theory = "R&P", Fs = 11)
    RP_row2.11 <- c(measure_1 = "Imitation", measure_2 = "Symbolism.11", correlation_value = RP2.Imi.Sym.11, partialled = "No", theory = "R&P", Fs = 11)
    RP_row3.11 <- c(measure_1 = "Social.Eng", measure_2 = "Affect.Sharing", correlation_value = RP3.AS.Soc.11, partialled = "Imitation", theory = "R&P", Fs = 11)
    RP_row4.11 <- c(measure_1 = "Imitation", measure_2 = "Social.Eng", correlation_value = RP4.Imi.Soc.11, partialled = "Affect.Sharing", theory = "R&P", Fs = 11)
    RP_row5.11 <- c(measure_1 = "Symbolism.11", measure_2 = "Social.Eng", correlation_value = RP5.Sym.Soc.11, partialled = "Imitation", theory = "R&P", Fs = 11)
    
    
    RP.1991.syntax.12 <- 'Affect.Sharing ~ b1*Imitation
                        Symbolism.12 ~ b2*Imitation
                        Social.Eng ~ b3*Affect.Sharing + b4*Imitation'
    
    RP.mod.12 <- sem(model = RP.1991.syntax.12, 
                     data = pseudo )
    
    
    
    
    
    RP1.Imi.AS.12 <- parameterestimates(RP.mod.12, standardized = T)$std.all[parameterestimates(RP.mod.12)$label == "b1"]
    RP2.Imi.Sym.12 <- parameterestimates(RP.mod.12, standardized = T)$std.all[parameterestimates(RP.mod.12)$label == "b2"]
    
    RP3.AS.Soc.12 <- parameterestimates(RP.mod.12, standardized = T)$std.all[parameterestimates(RP.mod.12)$label == "b3"]
    RP4.Imi.Soc.12 <- parameterestimates(RP.mod.12, standardized = T)$std.all[parameterestimates(RP.mod.12)$label == "b4"]
    
    RP5.Sym.Soc.12 <- parameterestimates(RP.mod.12, standardized = T)$std.all[8]
    
    
    RP_row1.12 <- c(measure_1 = "Imitation", measure_2 = "Affect.Sharing", correlation_value = RP1.Imi.AS.12, partialled = "No", theory = "R&P", Fs = 12)
    RP_row2.12 <- c(measure_1 = "Imitation", measure_2 = "Symbolism.12", correlation_value = RP2.Imi.Sym.12, partialled = "No", theory = "R&P", Fs = 12)
    RP_row3.12 <- c(measure_1 = "Social.Eng", measure_2 = "Affect.Sharing", correlation_value = RP3.AS.Soc.12, partialled = "Imitation", theory = "R&P", Fs = 12)
    RP_row4.12 <- c(measure_1 = "Imitation", measure_2 = "Social.Eng", correlation_value = RP4.Imi.Soc.12, partialled = "Affect.Sharing", theory = "R&P", Fs = 12)
    RP_row5.12 <- c(measure_1 = "Symbolism.12", measure_2 = "Social.Eng", correlation_value = RP5.Sym.Soc.12, partialled = "Imitation", theory = "R&P", Fs = 12)
    
    
    
    RP.1991.syntax.21 <- 'Affect.Sharing ~ b1*Imitation
                        Symbolism.21 ~ b2*Imitation
                        Social.Eng ~ b3*Affect.Sharing + b4*Imitation'
    
    RP.mod.21 <- sem(model = RP.1991.syntax.21, 
                     data = pseudo )
    
    
    
    
    
    RP1.Imi.AS.21 <- parameterestimates(RP.mod.21, standardized = T)$std.all[parameterestimates(RP.mod.21)$label == "b1"]
    RP2.Imi.Sym.21 <- parameterestimates(RP.mod.21, standardized = T)$std.all[parameterestimates(RP.mod.21)$label == "b2"]
    
    RP3.AS.Soc.21 <- parameterestimates(RP.mod.21, standardized = T)$std.all[parameterestimates(RP.mod.21)$label == "b3"]
    RP4.Imi.Soc.21 <- parameterestimates(RP.mod.21, standardized = T)$std.all[parameterestimates(RP.mod.21)$label == "b4"]
    
    RP5.Sym.Soc.21 <- parameterestimates(RP.mod.21, standardized = T)$std.all[8]
    
    
    RP_row1.21 <- c(measure_1 = "Imitation", measure_2 = "Affect.Sharing", correlation_value = RP1.Imi.AS.21, partialled = "No", theory = "R&P", Fs = 21)
    RP_row2.21 <- c(measure_1 = "Imitation", measure_2 = "Symbolism.21", correlation_value = RP2.Imi.Sym.21, partialled = "No", theory = "R&P", Fs = 21)
    RP_row3.21 <- c(measure_1 = "Social.Eng", measure_2 = "Affect.Sharing", correlation_value = RP3.AS.Soc.21, partialled = "Imitation", theory = "R&P", Fs = 21)
    RP_row4.21 <- c(measure_1 = "Imitation", measure_2 = "Social.Eng", correlation_value = RP4.Imi.Soc.21, partialled = "Affect.Sharing", theory = "R&P", Fs = 21)
    RP_row5.21 <- c(measure_1 = "Symbolism.21", measure_2 = "Social.Eng", correlation_value = RP5.Sym.Soc.21, partialled = "Imitation", theory = "R&P", Fs = 21)
    
    
    RP.1991.syntax.22 <- 'Affect.Sharing ~ b1*Imitation
                        Symbolism.22 ~ b2*Imitation
                        Social.Eng ~ b3*Affect.Sharing + b4*Imitation'
    
    RP.mod.22 <- sem(model = RP.1991.syntax.22, 
                     data = pseudo )
    
    
    
    
    
    RP1.Imi.AS.22 <- parameterestimates(RP.mod.22, standardized = T)$std.all[parameterestimates(RP.mod.22)$label == "b1"]
    RP2.Imi.Sym.22 <- parameterestimates(RP.mod.22, standardized = T)$std.all[parameterestimates(RP.mod.22)$label == "b2"]
    
    RP3.AS.Soc.22 <- parameterestimates(RP.mod.22, standardized = T)$std.all[parameterestimates(RP.mod.22)$label == "b3"]
    RP4.Imi.Soc.22 <- parameterestimates(RP.mod.22, standardized = T)$std.all[parameterestimates(RP.mod.22)$label == "b4"]
    
    RP5.Sym.Soc.22 <- parameterestimates(RP.mod.22, standardized = T)$std.all[8]
    
    
    RP_row1.22 <- c(measure_1 = "Imitation", measure_2 = "Affect.Sharing", correlation_value = RP1.Imi.AS.22, partialled = "No", theory = "R&P", Fs = 22)
    RP_row2.22 <- c(measure_1 = "Imitation", measure_2 = "Symbolism.22", correlation_value = RP2.Imi.Sym.22, partialled = "No", theory = "R&P", Fs = 22)
    RP_row3.22 <- c(measure_1 = "Social.Eng", measure_2 = "Affect.Sharing", correlation_value = RP3.AS.Soc.22, partialled = "Imitation", theory = "R&P", Fs = 22)
    RP_row4.22 <- c(measure_1 = "Imitation", measure_2 = "Social.Eng", correlation_value = RP4.Imi.Soc.22, partialled = "Affect.Sharing", theory = "R&P", Fs = 22)
    RP_row5.22 <- c(measure_1 = "Symbolism.22", measure_2 = "Social.Eng", correlation_value = RP5.Sym.Soc.22, partialled = "Imitation", theory = "R&P", Fs = 22)
    
    
    RP_row6 <- c(measure_1 = "Affect.Sharing", measure_2 = "Social.Eng", correlation_value = cor(pseudo$Affect.Sharing, pseudo$Social.Eng), partialled = "No", theory = "R&P", Fs = NA)
    RP_row7 <- c(measure_1 = "Imitation", measure_2 = "Social.Eng", correlation_value = cor(pseudo$Imitation, pseudo$Social.Eng), partialled = "No", theory = "R&P", Fs = NA)
    RP_row8 <- c(measure_1 = "Imitation", measure_2 = "Affect.Sharing", correlation_value = cor(pseudo$Imitation, pseudo$Affect.Sharing), partialled = "No", theory = "R&P", Fs = NA)
    
    
    df_long <- data.frame(rbind(RP_row1.11, RP_row2.11, RP_row3.11, RP_row4.11,
                                RP_row1.12, RP_row2.12, RP_row3.12, RP_row4.12,
                                RP_row1.21, RP_row2.21, RP_row3.21, RP_row4.21,
                                RP_row1.22, RP_row2.22, RP_row3.22, RP_row4.22,
                                RP_row6, RP_row7, RP_row8))
    
    
    
    
  } else if(theories == "O&R"){
    
    
    OR.1968.syntax.11 <- '
                      Self.Nonself ~ b1*Perceptual.Inconstancy
                      Imitation ~ b2*Perceptual.Inconstancy
                      RC.F1 ~ b3*Imitation + b5*Self.Nonself + b7*Perceptual.Inconstancy
                      VL2.F1 ~ b4*Imitation + b6*Self.Nonself + b8*Perceptual.Inconstancy' 
    
    
    OR.mod.11 <- sem(model = OR.1968.syntax.11, 
                     data = pseudo)
    
    
    
    OR1.Perc.Imi.11 <- parameterestimates(OR.mod.11, standardized = T)$std.all[parameterestimates(OR.mod.11)$label == "b1"]
    OR2.Perc.Self.11 <- parameterestimates(OR.mod.11, standardized = T)$std.all[parameterestimates(OR.mod.11)$label == "b2"]
    
    OR3.Perc.RC.F1.11 <- parameterestimates(OR.mod.11, standardized = T)$std.all[parameterestimates(OR.mod.11)$label == "b7"]
    
    OR4.Perc.VL2.F1.11 <- parameterestimates(OR.mod.11, standardized = T)$std.all[parameterestimates(OR.mod.11)$label == "b8"]
    
    OR5.Imi.RC.F1.11 <- parameterestimates(OR.mod.11, standardized = T)$std.all[parameterestimates(OR.mod.11)$label == "b3"]
    
    OR6.Imi.VL2.F1.11 <- parameterestimates(OR.mod.11, standardized = T)$std.all[parameterestimates(OR.mod.11)$label == "b4"]
    
    OR7.Self.RC.F1.11 <- parameterestimates(OR.mod.11, standardized = T)$std.all[parameterestimates(OR.mod.11)$label == "b5"]
    
    OR8.Self.VL2.F1.11 <- parameterestimates(OR.mod.11, standardized = T)$std.all[parameterestimates(OR.mod.11)$label == "b6"]
    
    
    
    
    
    OR_row1.11 <- c(measure_1 = "Imitation", measure_2 = "Perceptual.Inconstancy", correlation_value = OR1.Perc.Imi.11, partialled = "No", theory = "O&R", Fs = "11")
    OR_row2.11 <- c(measure_1 = "Self.Nonself", measure_2 = "Perceptual.Inconstancy", correlation_value = OR2.Perc.Self.11, partialled = "No", theory = "O&R", Fs = "11")
    OR_row3.11 <- c(measure_1 = "RC.F1", measure_2 = "Perceptual.Inconstancy", correlation_value = OR3.Perc.RC.F1.11, partialled = "Imitation, Self.Nonself", theory = "O&R", Fs = "11")
    OR_row4.11 <- c(measure_1 = "VL2.F1", measure_2 = "Perceptual.Inconstancy", correlation_value = OR4.Perc.VL2.F1.11, partialled = "Imitation, Self.Nonself", theory = "O&R", Fs = "11")
    OR_row5.11 <- c(measure_1 = "Imitation", measure_2 = "RC.F1", correlation_value = OR5.Imi.RC.F1.11, partialled = "Perception, Self.Nonself", theory = "O&R", Fs = "11")
    OR_row6.11 <- c(measure_1 = "Imitation", measure_2 = "VL2.F1", correlation_value = OR6.Imi.VL2.F1.11, partialled = "Perception, Self.Nonself", theory = "O&R", Fs = "11")
    OR_row7.11 <- c(measure_1 = "Self.Nonself", measure_2 = "RC.F1", correlation_value = OR7.Self.RC.F1.11, partialled = "Perception, Imitation", theory = "O&R", Fs = "11")
    OR_row8.11 <- c(measure_1 = "Self.Nonself", measure_2 = "VL2.F1", correlation_value = OR8.Self.VL2.F1.11, partialled = "Perception, Imitation", theory = "O&R", Fs = "11")
    
    
    OR.1968.syntax.12 <- '
                      Self.Nonself ~ b1*Perceptual.Inconstancy
                      Imitation ~ b2*Perceptual.Inconstancy
                      RC.F1 ~ b3*Imitation + b5*Self.Nonself + b7*Perceptual.Inconstancy
                      VL2.F2 ~ b4*Imitation + b6*Self.Nonself + b8*Perceptual.Inconstancy' 
    
    
    OR.mod.12 <- sem(model = OR.1968.syntax.12, 
                     data = pseudo)
    
    
    
    OR1.Perc.Imi.12 <- parameterestimates(OR.mod.12, standardized = T)$std.all[parameterestimates(OR.mod.12)$label == "b1"]
    OR2.Perc.Self.12 <- parameterestimates(OR.mod.12, standardized = T)$std.all[parameterestimates(OR.mod.12)$label == "b2"]
    
    OR3.Perc.RC.F1.12 <- parameterestimates(OR.mod.12, standardized = T)$std.all[parameterestimates(OR.mod.12)$label == "b7"]
    
    OR4.Perc.VL2.F1.12 <- parameterestimates(OR.mod.12, standardized = T)$std.all[parameterestimates(OR.mod.12)$label == "b8"]
    
    OR5.Imi.RC.F1.12 <- parameterestimates(OR.mod.12, standardized = T)$std.all[parameterestimates(OR.mod.12)$label == "b3"]
    
    OR6.Imi.VL2.F1.12 <- parameterestimates(OR.mod.12, standardized = T)$std.all[parameterestimates(OR.mod.12)$label == "b4"]
    
    OR7.Self.RC.F1.12 <- parameterestimates(OR.mod.12, standardized = T)$std.all[parameterestimates(OR.mod.12)$label == "b5"]
    
    OR8.Self.VL2.F1.12 <- parameterestimates(OR.mod.12, standardized = T)$std.all[parameterestimates(OR.mod.12)$label == "b6"]
    
    
    
    
    
    
    OR_row1.12 <- c(measure_1 = "Imitation", measure_2 = "Perceptual.Inconstancy", correlation_value = OR1.Perc.Imi.12, partialled = "No", theory = "O&R", Fs = "12")
    OR_row2.12 <- c(measure_1 = "Self.Nonself", measure_2 = "Perceptual.Inconstancy", correlation_value = OR2.Perc.Self.12, partialled = "No", theory = "O&R", Fs = "12")
    OR_row3.12 <- c(measure_1 = "RC.F1", measure_2 = "Perceptual.Inconstancy", correlation_value = OR3.Perc.RC.F1.12, partialled = "Imitation, Self.Nonself", theory = "O&R", Fs = "12")
    OR_row4.12 <- c(measure_1 = "VL2.F1", measure_2 = "Perceptual.Inconstancy", correlation_value = OR4.Perc.VL2.F1.12, partialled = "Imitation, Self.Nonself", theory = "O&R", Fs = "12")
    OR_row5.12 <- c(measure_1 = "Imitation", measure_2 = "RC.F1", correlation_value = OR5.Imi.RC.F1.12, partialled = "Perception, Self.Nonself", theory = "O&R", Fs = "12")
    OR_row6.12 <- c(measure_1 = "Imitation", measure_2 = "VL2.F1", correlation_value = OR6.Imi.VL2.F1.12, partialled = "Perception, Self.Nonself", theory = "O&R", Fs = "12")
    OR_row7.12 <- c(measure_1 = "Self.Nonself", measure_2 = "RC.F1", correlation_value = OR7.Self.RC.F1.12, partialled = "Perception, Imitation", theory = "O&R", Fs = "12")
    OR_row8.12 <- c(measure_1 = "Self.Nonself", measure_2 = "VL2.F1", correlation_value = OR8.Self.VL2.F1.12, partialled = "Perception, Imitation", theory = "O&R", Fs = "12")
    
    
    OR.1968.syntax.21 <- '
                      Self.Nonself ~ b1*Perceptual.Inconstancy
                      Imitation ~ b2*Perceptual.Inconstancy
                      RC.F2 ~ b3*Imitation + b5*Self.Nonself + b7*Perceptual.Inconstancy
                      VL2.F1 ~ b4*Imitation + b6*Self.Nonself + b8*Perceptual.Inconstancy' 
    
    
    OR.mod.21 <- sem(model = OR.1968.syntax.21, 
                     data = pseudo)
    
    
    
    OR1.Perc.Imi.21 <- parameterestimates(OR.mod.21, standardized = T)$std.all[parameterestimates(OR.mod.21)$label == "b1"]
    OR2.Perc.Self.21 <- parameterestimates(OR.mod.21, standardized = T)$std.all[parameterestimates(OR.mod.21)$label == "b2"]
    
    OR3.Perc.RC.F1.21 <- parameterestimates(OR.mod.21, standardized = T)$std.all[parameterestimates(OR.mod.21)$label == "b7"]
    
    OR4.Perc.VL2.F1.21 <- parameterestimates(OR.mod.21, standardized = T)$std.all[parameterestimates(OR.mod.21)$label == "b8"]
    
    OR5.Imi.RC.F1.21 <- parameterestimates(OR.mod.21, standardized = T)$std.all[parameterestimates(OR.mod.21)$label == "b3"]
    
    OR6.Imi.VL2.F1.21 <- parameterestimates(OR.mod.21, standardized = T)$std.all[parameterestimates(OR.mod.21)$label == "b4"]
    
    OR7.Self.RC.F1.21 <- parameterestimates(OR.mod.21, standardized = T)$std.all[parameterestimates(OR.mod.21)$label == "b5"]
    
    OR8.Self.VL2.F1.21 <- parameterestimates(OR.mod.21, standardized = T)$std.all[parameterestimates(OR.mod.21)$label == "b6"]
    
    
    
    
    
    
    OR_row1.21 <- c(measure_1 = "Imitation", measure_2 = "Perceptual.Inconstancy", correlation_value = OR1.Perc.Imi.21, partialled = "No", theory = "O&R", Fs = "21")
    OR_row2.21 <- c(measure_1 = "Self.Nonself", measure_2 = "Perceptual.Inconstancy", correlation_value = OR2.Perc.Self.21, partialled = "No", theory = "O&R", Fs = "21")
    OR_row3.21 <- c(measure_1 = "RC.F1", measure_2 = "Perceptual.Inconstancy", correlation_value = OR3.Perc.RC.F1.21, partialled = "Imitation, Self.Nonself", theory = "O&R", Fs = "21")
    OR_row4.21 <- c(measure_1 = "VL2.F1", measure_2 = "Perceptual.Inconstancy", correlation_value = OR4.Perc.VL2.F1.21, partialled = "Imitation, Self.Nonself", theory = "O&R", Fs = "21")
    OR_row5.21 <- c(measure_1 = "Imitation", measure_2 = "RC.F1", correlation_value = OR5.Imi.RC.F1.21, partialled = "Perception, Self.Nonself", theory = "O&R", Fs = "21")
    OR_row6.21 <- c(measure_1 = "Imitation", measure_2 = "VL2.F1", correlation_value = OR6.Imi.VL2.F1.21, partialled = "Perception, Self.Nonself", theory = "O&R", Fs = "21")
    OR_row7.21 <- c(measure_1 = "Self.Nonself", measure_2 = "RC.F1", correlation_value = OR7.Self.RC.F1.21, partialled = "Perception, Imitation", theory = "O&R", Fs = "21")
    OR_row8.21 <- c(measure_1 = "Self.Nonself", measure_2 = "VL2.F1", correlation_value = OR8.Self.VL2.F1.21, partialled = "Perception, Imitation", theory = "O&R", Fs = "21")
    
    
    OR.1968.syntax.22 <- '
                      Self.Nonself ~ b1*Perceptual.Inconstancy
                      Imitation ~ b2*Perceptual.Inconstancy
                      RC.F2 ~ b3*Imitation + b5*Self.Nonself + b7*Perceptual.Inconstancy
                      VL2.F2 ~ b4*Imitation + b6*Self.Nonself + b8*Perceptual.Inconstancy' 
    
    
    OR.mod.22 <- sem(model = OR.1968.syntax.22, 
                     data = pseudo)
    
    
    
    OR1.Perc.Imi.22 <- parameterestimates(OR.mod.22, standardized = T)$std.all[parameterestimates(OR.mod.22)$label == "b1"]
    OR2.Perc.Self.22 <- parameterestimates(OR.mod.22, standardized = T)$std.all[parameterestimates(OR.mod.22)$label == "b2"]
    
    OR3.Perc.RC.F1.22 <- parameterestimates(OR.mod.22, standardized = T)$std.all[parameterestimates(OR.mod.22)$label == "b7"]
    
    OR4.Perc.VL2.F1.22 <- parameterestimates(OR.mod.22, standardized = T)$std.all[parameterestimates(OR.mod.22)$label == "b8"]
    
    OR5.Imi.RC.F1.22 <- parameterestimates(OR.mod.22, standardized = T)$std.all[parameterestimates(OR.mod.22)$label == "b3"]
    
    OR6.Imi.VL2.F1.22 <- parameterestimates(OR.mod.22, standardized = T)$std.all[parameterestimates(OR.mod.22)$label == "b4"]
    
    OR7.Self.RC.F1.22 <- parameterestimates(OR.mod.22, standardized = T)$std.all[parameterestimates(OR.mod.22)$label == "b5"]
    
    OR8.Self.VL2.F1.22 <- parameterestimates(OR.mod.22, standardized = T)$std.all[parameterestimates(OR.mod.22)$label == "b6"]
    
    
    
    
    
    
    OR_row1.22 <- c(measure_1 = "Imitation", measure_2 = "Perceptual.Inconstancy", correlation_value = OR1.Perc.Imi.22, partialled = "No", theory = "O&R", Fs = "22")
    OR_row2.22 <- c(measure_1 = "Self.Nonself", measure_2 = "Perceptual.Inconstancy", correlation_value = OR2.Perc.Self.22, partialled = "No", theory = "O&R", Fs = "22")
    OR_row3.22 <- c(measure_1 = "RC.F1", measure_2 = "Perceptual.Inconstancy", correlation_value = OR3.Perc.RC.F1.22, partialled = "Imitation, Self.Nonself", theory = "O&R", Fs = "22")
    OR_row4.22 <- c(measure_1 = "VL2.F1", measure_2 = "Perceptual.Inconstancy", correlation_value = OR4.Perc.VL2.F1.22, partialled = "Imitation, Self.Nonself", theory = "O&R", Fs = "22")
    OR_row5.22 <- c(measure_1 = "Imitation", measure_2 = "RC.F1", correlation_value = OR5.Imi.RC.F1.22, partialled = "Perception, Self.Nonself", theory = "O&R", Fs = "22")
    OR_row6.22 <- c(measure_1 = "Imitation", measure_2 = "VL2.F1", correlation_value = OR6.Imi.VL2.F1.22, partialled = "Perception, Self.Nonself", theory = "O&R", Fs = "22")
    OR_row7.22 <- c(measure_1 = "Self.Nonself", measure_2 = "RC.F1", correlation_value = OR7.Self.RC.F1.22, partialled = "Perception, Imitation", theory = "O&R", Fs = "22")
    OR_row8.22 <- c(measure_1 = "Self.Nonself", measure_2 = "VL2.F1", correlation_value = OR8.Self.VL2.F1.22, partialled = "Perception, Imitation", theory = "O&R", Fs = "22")
    
    
    
    
    
    
    
    
    df_long <- data.frame(rbind(OR_row1.11, OR_row2.11, OR_row3.11, OR_row4.11, OR_row5.11, OR_row6.11, OR_row7.11, OR_row8.11,
                                OR_row1.12, OR_row2.12, OR_row3.12, OR_row4.12, OR_row5.12, OR_row6.12, OR_row7.12, OR_row8.12,
                                OR_row1.21, OR_row2.21, OR_row3.21, OR_row4.21, OR_row5.21, OR_row6.21, OR_row7.21, OR_row8.21,
                                OR_row1.22, OR_row2.22, OR_row3.22, OR_row4.22, OR_row5.22, OR_row6.22, OR_row7.22, OR_row8.22))
    #, OR_row9, OR_row10, OR_row11, OR_row12, OR_row13, OR_row14))
    
  }
  
  
  
  return(df_long)
}
