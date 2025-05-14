# This is a function that pulls the items for a data-driven CFA, and is a companion to
# efa_to_cfa_prep.R 
# It runs an EFA with the pre-specified number of factors desired, 
# Written by SM 2/5/24

efa_to_cfa_items <- function(df, 
                            nfactors, 
                            unique = FALSE,
                            cor = "cor", 
                            cutoff = .3){
  
  if(cor == "mixed"){
    mixed.mat <- mixedCor.2(df)
    efa <- fa(mixed.mat, nfactors = nfactors, n.obs = nrow(df))
    
  }else{
    efa <- fa(df, nfactors = nfactors, cor = cor)
    
  }
  
  
  
  filtered_loadings <- loadings(efa)
  filtered_loadings[abs(filtered_loadings)<=cutoff] <-0
  
  max_loadings<-apply(abs(filtered_loadings),1,which.max)
  unique_loadings <- filtered_loadings
  for(i in seq_along(max_loadings)){
    unique_loadings[i,-max_loadings[i]]<-0
  }
  
  if(unique == TRUE){ 
    load.mat <- data.frame(unclass(unique_loadings))
  } else {
    load.mat <- data.frame(unclass(filtered_loadings))
  }  
  
  load.mat$name <- rownames(load.mat)
  
  load.mat <- load.mat %>% filter(!if_all(contains("MR"), ~ .x == 0))
  
  load.mat.long <- pivot_longer(load.mat, cols = contains("MR"), values_to = "val", names_to = "MR.name") %>%
    filter(val != 0)  %>%
    group_by(MR.name) %>%
    filter(n() >= 2) %>%
    ungroup()
  
  return(load.mat.long)
}
