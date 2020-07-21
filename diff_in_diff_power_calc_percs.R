diff_in_diff_power_calc_percs <- function(pre_test_perc = 0.40,
                                       post_test_perc = 0.47,
                                       pre_control_perc = 0.40,
                                       post_control_perc = 0.42,
                                       n_sample_test = 3200,
                                       n_sample_control = 3200,
                                       alpha=0.05,
                                       n_bootstrap_iterations=500,
                                       set_seed_value=1234
){

    # Function used to estimate power of your difference in difference test given expected lift (via pre/post test/control percentages) and sample sizes.
    # Contrary to a/b testing power analysis scenario where you input test/control percentages, significance level (alpha) & 
    # power (beta) and get minimum sample size, this function expects test/control percentages for pre & post, significance level (alpha)
    # and sample sizes that will be supplied and outputs the power (beta) that can be achieved given these inputs. Note: Function is built
    # for power analysis when the metrics under consideration are percentages

  
  # Throw error if percentage values are not between 0 & 1
  if(!all(sapply(c(pre_test_perc, post_test_perc, pre_control_perc, post_control_perc), function(x) x >= 0 & x <= 1))) {
    stop('This is a function to estimate power for diff in diff for percentage metrics. 
         Expecting values between 0 & 1 for percentages')
  }
  
  
  # n_sample parameters need to be positive integer
  if(!all(sapply(c(n_sample_test, n_sample_control, n_bootstrap_iterations), function(x) x %% 1 == 0 & x > 0))) {
    stop('n_samples parameters need to be a positive integer')
  }
  
  # bootsrap iteration parameter needs to be positive integer
  if(!all(sapply(c(n_bootstrap_iterations), function(x) x %% 1 == 0 & x > 0))) {
    stop('n_bootstrap_iterations needs to be a positive integer')
  }
  
  # alpha value needs to be less than 1
  if(!(alpha > 0 & alpha < 1)){
    stop('alpha needs to 0 < alpha < 1. Typical values are 0.1, 0.05 & 0.01')
  }
  

  ## helper function
  simulate_conf_intervals <- function(pre_test_perc,
                                      post_test_perc,
                                      pre_control_perc,
                                      post_control_perc,
                                      n_sample_test,
                                      n_sample_control,
                                      alpha){
    
    ## draw from "known" distributions
    pre_test <- rbinom(n_sample_test, 1, pre_test_perc)
    post_test <- rbinom(n_sample_test, 1, post_test_perc)
    
    pre_control <- rbinom(n_sample_control, 1, pre_control_perc)
    post_control <- rbinom(n_sample_control, 1, post_control_perc)
    
    calc_did <- function(pre_test_,
                         post_test_,
                         pre_control_,
                         post_control_,
                         n_sample_test_,
                         n_sample_control_){
      
      test_inds <- sample(1:n_sample_test_, n_sample_test_, replace=T)
      control_inds <- sample(1:n_sample_control_, n_sample_control_, replace=T)
      
      return(mean(post_test_[test_inds]) - mean(pre_test_[test_inds]) - mean(post_control_[control_inds]) + mean(pre_control_[control_inds]))
    }
    
    dids <- replicate(500, calc_did(pre_test_ = pre_test,
                            post_test_ = post_test,
                            pre_control_ = pre_control,
                            post_control_ = post_control,
                            n_sample_test_ = n_sample_test,
                            n_sample_control_ = n_sample_control))
    
    return(c(mean(dids), quantile(dids, alpha/2))*100)
  }
  
  ## Set seed for reproducible results
  set.seed(set_seed_value)
  output_holder <- replicate(n_bootstrap_iterations,
                             simulate_conf_intervals(pre_test_perc = pre_test_perc,
                                                     post_test_perc = post_test_perc,
                                                     pre_control_perc = pre_control_perc,
                                                     post_control_perc = post_control_perc,
                                                     n_sample_test = n_sample_test,
                                                     n_sample_control = n_sample_control,
                                                     alpha=alpha)
  )
  # avg lift & avg 95% lower CI & power
  print(paste0("Average Delta: ", mean(output_holder[1,])))
  print(paste0("Average 95% Lower Confidence Value: ", mean(output_holder[2,])))
  print(paste0("Power estimated for given lift & sample sizes: ", mean(output_holder[2,] > 0)))
}
