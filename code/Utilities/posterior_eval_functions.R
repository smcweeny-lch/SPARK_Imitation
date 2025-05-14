# posterior evaluation functions
# Written by SM week of 2/10/25
# Takes samples from posterior and prior and compares them

overlap <- function(prior, posterior, parameter){
  set.seed(123)
  ndraws <- min(nrow(prior %>% filter(combo == parameter)), nrow(posterior %>% filter(combo == parameter)))
  prior <- prior %>% filter(combo == parameter) %>% sample_n(ndraws)
  posterior <- posterior %>% filter(combo == parameter) %>% sample_n(ndraws)
  2 * pnorm(-abs(mean(posterior$beta) - mean(prior$beta)) / sqrt(2*(var(posterior$beta) + var(prior$beta))))  
}

superiority <- function(prior, posterior, parameter){
  set.seed(123)
  ndraws <- min(nrow(prior %>% filter(combo == parameter)), nrow(posterior %>% filter(combo == parameter)))
  prior <- prior %>% filter(combo == parameter) %>% sample_n(ndraws)
  posterior <- posterior %>% filter(combo == parameter) %>% sample_n(ndraws)
  
  prob_superior_mc <- mean(posterior$beta > prior$beta)
  #cat("Probability that posterior > null =", prob_superior_mc, "\n")
  return(prob_superior_mc)
}

ci.diff <- function(prior, posterior, parameter){
  ndraws <- min(nrow(prior %>% filter(combo == parameter)), nrow(posterior %>% filter(combo == parameter)))
  prior <- prior %>% filter(combo == parameter) %>% sample_n(ndraws)
  posterior <- posterior %>% filter(combo == parameter) %>% sample_n(ndraws)
  
  diff_samples <- posterior$beta - prior$beta
  ci_diff <- quantile(diff_samples, probs = c(0.025, 0.975))
  return(ci_diff)
  #cat("95% credible interval for the difference =", ci_diff, "\n")
}

compare.post.prior <- function(prior, posterior, parameter){
  prior <- prior %>% filter(combo == parameter )
  posterior <- posterior %>% filter(combo == parameter )
  median.prior <- median(prior$beta)
  median.posterior <- median(posterior$beta)
  overlap.dist <- overlap(prior, posterior, parameter)
  p.exceed.975.percentile <- pnorm(quantile(prior$beta, .975), mean(posterior$beta), sd(posterior$beta))
  p.exceed.median <- pnorm(quantile(prior$beta, .5), mean(posterior$beta), sd(posterior$beta))
  superiority.dist <- superiority(prior, posterior, parameter)
  ci.diff.dist <- ci.diff(prior, posterior, parameter)
  return(list(median.prior = median.prior, 
              median.posterior = median.posterior, 
              OVL = overlap.dist, 
              p.exceed.975 = p.exceed.975.percentile, 
              p.value = p.exceed.median, 
              prob.post.gt.prior = superiority.dist, 
              credible.interval.difference = ci.diff.dist))
}

# KL <- function(mean_prior, mean_post, sd_prior, sd_post){
#   log(sd_prior/sd_post) + (sd_post^2 + (mean_post - mean_prior)^2) / (2 * sd_prior^2) - 0.5
# }
