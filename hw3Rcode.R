


normal_ci_sim_fn = function(mu=2, sigma=sqrt(5), n = 200, nsim = 100, alpha = 0.05){
  
  
  ci_results_sim_df = data.frame(NULL)
  
      for( i in 1:nsim){
        
            
            data = rnorm(n, mu, sigma)
            
            mu_estimate = mean(data)
            std_error = sd(data)/sqrt(n)
            me = std_error * qnorm(1-alpha/2)
            
            CI_Lower_bound = mu_estimate - me
            CI_Upper_bound = mu_estimate + me
            
            ci = data.frame(CI_Lower_bound, CI_Upper_bound)
            
            ci_results_sim_df = rbind(ci_results_sim_df, ci)
        
      }
  
  return(ci_results_sim_df)
  
}

sim1 = normal_ci_sim_fn()


name_fn = function(mu, ci_dataframe){
  
        
  
  
}
  
  