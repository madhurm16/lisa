gob_finder = function(data, param, gamma_specification = ".cst"){
  # Select omega and beta associated
  param[,"gamma"] = param.gamma %>%
    select(gamma = paste0("gamma", gamma_specification))
  param[,c("omega", "beta")] =  param.pref %>%
    select(omega = paste0("omega", gamma_specification),
           beta = paste0("beta", gamma_specification))
  
  ## Merge param and data
  data = merge(data, param)
  
  # Return
  return(data)
}