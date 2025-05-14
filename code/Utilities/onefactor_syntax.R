onefactor_syntax <- function(items){
  paste0("F=~", paste0(items, collapse = "+"))
}
