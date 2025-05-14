is.dichotomous <- function(x){
  length(levels(as.factor(x))) == 2
}


mixedCor.2 <- function(df){
  dd <- which(apply(df, MARGIN = 2, FUN = is.dichotomous))
  xx <- which(!apply(df, MARGIN = 2, FUN = is.dichotomous))
  mixedCor(data = df, c = xx, d = dd)$rho
}
