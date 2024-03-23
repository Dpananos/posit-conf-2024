
create_experiment_data <- function() {
  
  set.seed(2)
  device <- c('Desktop', 'Mobile')
  lang <- c('English', 'Non-English')
  treat <- c('Control', 'Treatment')
  
  
  base <- crossing(
    device, 
    lang
    ) %>% 
    mutate(
      prevalence = c(0.7, 0.15, 0.1, 0.05),
      conversion = c(0.11, 0.06, 0.04, 0.02),
      treat_conversion = c(0.14, 0.02, 0.06, 0.02)
      ) 
  
  base %>% 
    sample_n(size = 30000, weight = prevalence, replace = T) %>% 
    mutate(
      treat = sample(treat, size=n(), replace=T),
      conversion = rbinom(n(), 1, if_else(treat=='Control', conversion, treat_conversion))
    )
    

}

summarized_experiment_data <- function(rollup_vars=NULL) {
  
  create_experiment_data() %>% 
    group_by(treat, across({{rollup_vars}})) %>% 
    summarise(
      N = n(),
      conversions = sum(conversion)
    )
  
}

