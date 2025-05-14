create_lavaan_syntax <- function(tbl, bifactor = F, hierarchical = T, drop_2_item_factors = FALSE) {
  
  #Which factors only have two items? Will not be specified if not fixed
  tmp <- tbl %>% group_by(MR.name) %>% count() %>% filter(n < 3)
  two_item_factors <- tmp$MR.name
  loadings_needed_for <- tbl %>% filter(MR.name %in% two_item_factors)
  tbl <- tbl %>% filter(!MR.name %in% two_item_factors)
  #if one of these two-item factors is included, write the val*name fixed syntax
  if(nrow(loadings_needed_for)  > 0 & drop_2_item_factors == F){ 
    MR_syntax_fixed <- loadings_needed_for %>%
      group_by(MR.name) %>%
      summarize(loadings_syntax = paste(paste0(val, "*", name), collapse = "+")) %>%
      mutate(MR_syntax = paste0(MR.name, "=~", loadings_syntax)) %>%
      pull(MR_syntax)
  } else {MR_syntax_fixed <- "" } #if not, string is blank
  
  #regular syntax of factors and items
  MR_syntax <- tbl %>%
    group_by(MR.name) %>%
    summarize(items = paste(name, collapse = "+")) %>%
    mutate(MR_syntax = paste0(MR.name, "=~", items)) %>%
    pull(MR_syntax)
  
  
  # if(g == TRUE){ 
  #   #Create the higher-level factor 'g' if there's more than one factor (without covariances)
  #   if(length(unique(tbl$MR.name)) > 1){
  #     MR_names <- tbl %>%
  #       distinct(MR.name) %>%
  #       pull(MR.name)
  #     #for the weird All model (might delete later)
  #     if(nrow(tbl) < 84){
  #       g_syntax <- paste0("g=~", paste(MR_names, collapse = "+"))
  #     } else{  g_syntax <- ""}
  #     
  #   } else{ g_syntax <- ""}
  # } else {
  #   g_syntax <- ""
  # }
  
  #uncorrelated_factors_syntax <- combn(MR_names, 2, FUN = function(x) paste0(x[1], "~~0*", x[2]))
  
  
  MR_names <- tbl %>%
    distinct(MR.name) %>%
    pull(MR.name)
  
  if(bifactor == T & hierarchical == T){
    
    g_syntax <- paste0("g=~", paste(MR_names, collapse = "+"), "\n")
    g_syntax2 <- combn(MR_names, 2, FUN = function(x) paste0(x[1], "~~0*", x[2]))
    g_syntax2 <- paste0(g_syntax2, collapse = "\n")
    g_syntax <- paste0(g_syntax, g_syntax2, collapse = "\n")
    
    bifactor_syntax <- "\n"
    
  } else if (bifactor == T & hierarchical == F){
    
    bifactor_syntax  <- tbl %>%
      ungroup() %>%
      summarize(items = paste(name, collapse = "+")) %>%
      mutate(bifactor_syntax = paste0("g=~", items)) %>%
      pull(bifactor_syntax)
    
    g_syntax <- paste0("g~~0*", MR_names, collapse = "\n")
    
  } else{
    g_syntax <- ""
    bifactor_syntax <- ""
  }
  
  # Combine everything into the final lavaan syntax
  final_syntax <- paste(c(MR_syntax,MR_syntax_fixed,bifactor_syntax, g_syntax), collapse = "\n")
  return(final_syntax)
}
