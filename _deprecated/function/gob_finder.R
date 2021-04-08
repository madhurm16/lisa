gob_finder = function(data, param, gamma_spe = ".cst"){
  # Select omega and beta associated
  param[,"gamma"] = param.gamma %>%
    select(gamma = paste0("gamma", gamma_spe))
  param[,c("omega", "beta")] =  param.pref %>%
    select(omega = paste0("omega", gamma_spe),
           beta = paste0("beta", gamma_spe))
  
  ## Merge param and data
  data = merge(data, param, by = c("Country", "Year"))
  
  # Return
  return(data)
}