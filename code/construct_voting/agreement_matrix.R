rm(list = ls())
library(readxl)
library(tidyverse)
library(irr)
library(psych)


W <- read_xlsx("./code/construct_voting/Item_Voting_Ericka_12_29_24.xlsx", col_names = T)
W$voter <- "W"

L <- read_xlsx("./code/construct_voting/Item_Voting_Current_NNL.xlsx", col_names = T)
L$voter <- "L"

E <- read_xlsx("./code/construct_voting/Ewen_Item_Voting_1_28_25.xlsx", col_names = T)
E$voter <- "E"


LEW <- bind_rows(E,W,L) %>% select(name, Social.Skills:Self.Nonself, voter)

LEW <- LEW %>% rename(VL1 = VL...10,
                      VL2 = VL...12,
                      Imag.Play = `Imag. Play`)

LEW_long <- LEW %>% pivot_longer(c(Social.Skills:Self.Nonself) , names_to = "construct", values_to = "score" )

LEW_long3 <- LEW_long %>% filter(score == 3)


final <- LEW_long3 %>% group_by(construct, name) %>%
  count() %>% filter(n >= 2)

final_perc <- final %>% filter(construct == "Perceptual.Inconstancy") %>% 
   filter(n == 3)


final <- final %>% filter(!construct == "Perceptual.Inconstancy") %>%
  bind_rows(final_perc)



constructs <- unique(final$construct)


final %>% group_by(construct) %>% count()

paste0((final %>% filter(construct == "VL2"))$name, collapse = "+")


syntax_list <- list()



for(con in constructs){
  first_fit_syntax <- paste0(con, "=~", paste0((final %>% filter(construct == con))$name, collapse = "+"))
  items <- (final %>% filter(construct == con))$name
  n_items <- length(items)
  con2 <- list(syntax = first_fit_syntax, items = items, n_items = n_items)
  syntax_list[[con]] <- con2
}


write_rds(syntax_list, "./CFA_syntax.rds")





LEW_long %>% group_by(construct, name) %>% 
  summarize(sum = sum(score)) %>%
  filter(sum >= 7) %>%
  count()

constructs <- unique(LEW_long$construct)

absoLEWt <- LEW_long %>% mutate(score = case_match(score, 
                                        3 ~ 1,
                                       c(0,1,2) ~ 0))




LEW_long_modded <- LEW_long %>% mutate(score = case_match(score, 
                                                          3 ~ 5,
                                                          2 ~ 3,
                                                          1 ~ 1,
                                                          0 ~ 0))


## General cleaning function to pull the m x n vote matrix from long format for a given construct
vote.matrix <- function(x, cat){
 x %>% filter(construct == cat) %>%
    pivot_wider(values_from = score, names_from = voter) %>%
    select(E:L)
}

#### When treating data as ordinal
#### Use Kendall W 
#### Mean Cohen's Weighted Kappa 

for(construct in constructs){
  print(construct)
  print(round(kendall(correct = T, ratings = vote.matrix(LEW_long, construct))$value, 2))
}
  

# kappa2(vote.matrix(LEW_long, "Motor.Skills") %>% select(E,W), weight = "equal")
# kappa2(vote.matrix(LEW_long, "Motor.Skills") %>% select(E,L), weight = "equal")
# kappa2(vote.matrix(LEW_long, "Motor.Skills") %>% select(L,W), weight = "equal")

# No function exists directly for weighted kappa among multiple observers in IRR, so utilized the Light's Kappa code
# To build custom function
# Returns all 3, just in case we're interested
weighted.kappam <- function(x, weight = "equal"){
  ratings <- as.matrix(na.omit(x))
  ns <- nrow(ratings)
  nr <- ncol(ratings)
  for (i in 1:(nr - 1)) for (j in (i + 1):nr) {
    if ((i == 1) & (j == (i + 1))) 
      kappas <- kappa2(ratings[, c(i, j)], weight = "equal")$value
    else kappas <- c(kappas, kappa2(ratings[, c(i, j)], weight = "equal")$value)
  }
  kappas
}

kappa_constructs <- data.frame(matrix(nrow = 0, ncol = 5))
for(construct in constructs){
  x <- vote.matrix(LEW_long, construct)
  print(construct)
  kappas <- weighted.kappam(x)
  print(round(kappas, 2))
  
  kappa_constructs[construct,] <- c(round(kappas,2), round(mean(kappas),2), construct)
  
}

names(kappa_constructs) <- c("EW", "EL", "LW", "mean", "construct")
row.names(kappa_constructs) <- NULL
kappa_constructs[,1:4] <- apply(kappa_constructs[,1:4], MARGIN = 2, FUN = as.numeric)

colMeans(kappa_constructs[,1:4])


votes <- c()
for(construct in constructs){
  print(construct)
  votes[construct]<- colSums(vote.matrix(LEW_long, construct))
  print(colSums(vote.matrix(LEW_long, construct)))
  print(sum(colSums(vote.matrix(LEW_long, construct))))
}





### When dichotomizing (0,3)

# Fleiss's Kappa
for(construct in constructs){
  x <- vote.matrix(absoLEWt, construct)
  assign(paste0(construct, "_fleiss_kappa"), kappam.fleiss(x))
  print(construct)
  print(round(kappam.fleiss(x)$value, 2))
}

# Cohen's Kappa (Light's, but it's just avg pairwise)
for(construct in constructs){
  x <- vote.matrix(absoLEWt, construct)
  assign(paste0(construct, "_cohen_kappa"), kappam.light(x))
  print(construct)
  print(round(kappam.light(x)$value, 2))
}


#Agreement, straight up
####
vote.matrix(absoLEWt, "Imag.Play")







# Continuous (ICC) - don't use (see https://www.john-uebersax.com/stat/icc.htm)
#######

ICC.calc <- function(cat){
  x <- LEW_long %>% filter(construct == cat) %>% pivot_wider(values_from = score, names_from = voter)
  x2 <- ICC(x %>% select(E:L), lmer = F)
  round((x2$results %>% filter(type == "ICC3k"))$ICC, 3)
}

for(construct in constructs){
  assign(paste0(construct, "_ICC"), ICC.calc(construct))
  print(construct)
  #print(ICC.calc(construct))
  print(ICC.calc(construct))
}



light.kappa <- function(cat){
  x <- LEW_long %>% filter(construct == cat) %>% pivot_wider(values_from = score, names_from = voter)
  round(kappam.light(x %>% select(E:L))$value, 3)
}





p.i <- vote.matrix(LEW_long,  "Perceptual.Inconstancy")

LEW_long %>% group_by(construct, voter) %>% 
  summarize(x = sum(score))

LEW_long %>% group_by(voter) %>% 
  summarize(x = sum(score))


LEW_long %>% group_by(voter, score) %>% 
  count()


LEW_long %>% group_by(voter) %>% 
  summarize(x = sum(score == 3))



colSums(p.i %>% select(E:L))















construct_by_item <- absoLEWt %>% group_by(construct, name) %>%
  summarize(yes = sum(score == 1),
            no = sum(score == 0))


print(construct_by_item %>% filter(construct == "Motor.Skills"), n = 91)

xx <- kappam.fleiss(ratings = construct_by_item %>% 
                      filter(construct == "Affect.Sharing") %>% 
                      select(no,yes))


xx <- kappam.fleiss(ratings = construct_by_item %>% 
                      filter(construct == "Motor.Skills") %>% 
                      select(no,yes))









E$name[which(as.numeric(W$Social.Skills == 3) + as.numeric(E$Social.Skills == 3) + as.numeric(L$Social.Skills == 3) >= 2)]

E$name[which(as.numeric(W$Motor.Skills == 3) + as.numeric(E$Motor.Skills == 3) + as.numeric(L$Motor.Skills == 3) >= 2)]

E$name[which(as.numeric(W$NVC == 3) + as.numeric(E$NVC == 3) + as.numeric(L$NVC == 3) >= 2)]

E$name[which(as.numeric(W$Affect.Sharing == 3) + as.numeric(E$Affect.Sharing == 3) + as.numeric(L$Affect.Sharing == 3) >= 2)]

E$name[which(as.numeric(W$`Imag. Play` == 3) + as.numeric(E$`Imag. Play` == 3) + as.numeric(L$`Imag. Play` == 3) >= 2)]

E$name[which(as.numeric(W$NVC == 3) + as.numeric(E$NVC == 3) + as.numeric(L$NVC == 3) >= 2)]

E$name[which(as.numeric(W$Social.Eng == 3) + as.numeric(E$Social.Eng == 3) + as.numeric(L$Social.Eng == 3) >= 2)]

E$name[which(as.numeric(W$VL...10 == 3) + as.numeric(E$VL...10 == 3) + as.numeric(L$VL...10 == 3) >= 2)]

E$name[which(as.numeric(W$Social.Eng == 3) + as.numeric(E$Social.Eng == 3) + as.numeric(L$Social.Eng == 3) >= 2)]

E$name[which(as.numeric(W$VL...12 == 3) + as.numeric(E$VL...12 == 3) + as.numeric(L$VL...12 == 3) >= 2)]

E$name[which(as.numeric(W$Perceptual.Inconstancy == 3) + as.numeric(E$Perceptual.Inconstancy == 3) + as.numeric(L$Perceptual.Inconstancy == 3) >= 2)]

E$name[which(as.numeric(W$Self.Nonself == 3) + as.numeric(E$Self.Nonself == 3) + as.numeric(L$Self.Nonself == 3) >= 2)]





WL <- data.frame(mapply(paste0, W, L))
WL$MR <- W$MR

cols <- names(WL)

union <- list()

for(i in 2:ncol(WL)){
  union[[i-1]] <- WL$MR[str_detect(WL[,i], "X")]
}

names(union) <- cols[2:length(cols)]

#saveRDS(union, "./output/union.rds")

intersection <- list()
for(i in 2:ncol(WL)){
  intersection[[i-1]] <- WL$MR[str_detect(WL[,i], "XX")]
}

names(intersection) <- cols[2:length(cols)]

#saveRDS(intersection, "./output/intersection.rds")

rm(W, L)


# TF <- WL %>% mutate(across(everything(), ~ ifelse((.x == "XX" | .x == "OO"), T, F)))
# 
# data.frame(var = names(L) , agreement = as.vector(round(colSums(TF) / 17, 2)))
# 
# intersection.n <- colSums(WL %>% mutate(across(everything(), ~ .x == "XX")))
# union.n <- colSums(WL %>% mutate(across(everything(), ~ str_detect(.x, "X"))))
# df <- data.frame(AND = intersection.n, OR = union.n)
# 
# disagreements <- as.data.frame(colSums(WL %>% mutate(across(everything(), ~ str_detect(.x, "XO|OX")))))
# names(disagreements) <- c("disagree")
# 
# data.frame(W = colSums(W %>% mutate(across(everything(), ~ .x == "X"))),
#            L = colSums(L %>% mutate(across(everything(), ~ .x == "X"))))


