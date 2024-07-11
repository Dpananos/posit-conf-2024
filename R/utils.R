
create_experiment_data <- function() {
  
  set.seed(3)

  
  d <- readr::read_csv('data/data.csv') %>% 
    sample_n(size = 40000, weight = weight, replace = TRUE) %>% 
    group_by(device, lang) %>% 
    mutate(
      treat = sample(c("Treatment","Control"), size = n(), replace = T),
      p = if_else(treat == 'Treatment', p_treat, p_control),
      conversion = rbinom(n(), 1, p)
    )
  
  
  d
  
  
  
}

