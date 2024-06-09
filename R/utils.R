
create_experiment_data <- function() {
  
  set.seed(2)
  device <- c('Desktop', 'Mobile')
  treat <- c('Control', 'Treatment')
  
  
  dgrid <- crossing(
    device, 
    treat
  ) %>% 
    mutate(wt = c(0.4, 0.4, 0.1, 0.1))
  
  grid <- dgrid %>% 
    sample_n(20000, replace=T, weight = wt) %>% 
    mutate(
      tenure = pmin(6.0, rpois(n(), 2.5))
    )
  
  X <- model.matrix( ~ device*treat + treat*splines::ns(tenure,Boundary.knots = c(2, 5), knots = c(2.5, 4.5)),
                     data=grid
                    )
  

  b <- c(0.10, -0.03, 0.015, 
         -1/20, -1/20, -1/20, 
         -0.013,
         -0.005, -0.005, -0.005)
  
  
  
  
 grid %>% 
   mutate(
     p = as.numeric(X %*% b),
     conversion = rbinom(n(), 1, p)
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