demo_changer = function(data = data, break_year = 1970,
                        PG = "TPG", SR = "TSR", DE = "TDE", IE = "TIE",
                        p1_equals_p_fix = FALSE,
                        init_spe = FALSE, AFinder = FALSE,
                        ret_spe = 0){
  
  if(init_spe == TRUE | AFinder == TRUE){
    # Compute population
    for(seq in 1:4){
      for(t in 2:3){
        # Young population dynamics
        data$Ny[data$Period == t & data$Sequence == seq] = 
          data$Ny[data$Period == t-1 & data$Sequence == seq] * data$n[data$Period == t & data$Sequence == seq]
        # Old population dynamics
        data$No[data$Period == t & data$Sequence == seq] = 
          data$Ny[data$Period == t-1 & data$Sequence == seq] * data$p[data$Period == t & data$Sequence == seq]
      }
    }
    
    if(AFinder == TRUE){
      # Compute eta
      data = data %>% mutate(eta = n / p * (1 + alpha*p1) / omega)
    }
    
  } else {
    
    
   ## Fix variables
   data = breakers %>%
     subset(Year == break_year) %>% 
     select(Country, n_fix = n, p_fix = p, p1_fix = p1, Ny_fix = Ny, No_fix = No, eta_fix = eta) %>% 
     merge(data, .)
  
  ## Constant population growth : not necessary to change K_0 for FPG
  if(PG == "FPG"){
    data = data %>% mutate(Ny = ifelse(Year >= break_year, Ny*n_fix/n, Ny),
                           n = ifelse(Year >= break_year, n_fix, n))
  }
  
  ## Constant survival rate : also change K_0
  if(SR == "FSR"){
    data = data %>% mutate(No = ifelse(Year >= break_year, No*p_fix/p, No),
                           # Also changing initial capital stock 
                           K = ifelse(Year > break_year, K*p_fix/p*(1+alpha*p)/(1+alpha*p_fix), K),
                           p = ifelse(Year >= break_year, p_fix, p),
                           # Same initial labor income share required : same fixed point
                           p1 = ifelse(Year >= break_year, p1_fix, p1))
    
    if(p1_equals_p_fix == TRUE){
      # p1 only has > instead of >= to attribute the difference with counterfactual 
      # to 2010 instead of 1970
      data = data %>% mutate(p1 = ifelse(Year > break_year, p_fix, p1))
    }
  }
   
  # Compute eta
  data = data %>% mutate(eta = n / p * (1 + alpha*p1) / omega)
  
  ## Constant population structure
  if(DE == "FDE"){
    data = data %>% mutate(Ny = ifelse(Year >= break_year, Ny*n_fix/n, Ny),
                           No = ifelse(Year >= break_year, No*p_fix/p, No),
                           # Also changing initial capital stock 
                           K = ifelse(Year > break_year, K*p_fix/p*(1+alpha*p)/(1+alpha*p_fix), K),
                           n = ifelse(Year >= break_year, n_fix, n),
                           p = ifelse(Year >= break_year, p_fix, p),
                           # p1 only has > instead of >= to attribute the difference with counterfactual 
                           # to 2010 instead of 1970
                           # Same initial labor income share required : same fixed point
                           p1 = ifelse(Year >= break_year, p1_fix, p1),
                           # Ny = ifelse(Year >= break_year, Ny_fix, Ny),
                           # No = ifelse(Year >= break_year, No_fix, No)
                           )
    
    if(p1_equals_p_fix == TRUE){
      data = data %>% mutate(p1 = ifelse(Year > break_year, p_fix, p1))
    }
  }
   
  ## Computation
  # Compute population
  for(seq in 1:4){
   for(t in 2:3){
     # Young population dynamics
     data$Ny[data$Period == t & data$Sequence == seq] = 
       data$Ny[data$Period == t-1 & data$Sequence == seq] * data$n[data$Period == t & data$Sequence == seq]
     # Old population dynamics
     data$No[data$Period == t & data$Sequence == seq] = 
       data$Ny[data$Period == t-1 & data$Sequence == seq] * data$p[data$Period == t & data$Sequence == seq]
   }
  }
  
  ## Constant political power (eta)
  if(IE == "FIE"){
    data = data %>% mutate(eta = ifelse(Year >= break_year, eta_fix, eta))
  }
  
  }
  
  ## Remove fixed variables
  data = data %>% select(-contains("fix"))
  
  return(data)
}