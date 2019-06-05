demo_changer = function(data = data, break_year = 1970,
                        PG = "TPG", SR = "TSR", DE = "TDE", IE = "TIE",
                        init_spe = FALSE, AFinder = FALSE){
  
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
    
  # Compute eta
  data = data %>% mutate(eta = n / p * (1 + alpha*p1) / omega)
  
  ## Fix variables
  data = data %>% subset(Year == break_year) %>% 
    select(Country, n_fix = n, p_fix = p, Ny_fix = Ny, No_fix = No, eta_fix = eta) %>% 
    merge(data, .)
  
  ## Constant population growth
  if(PG == "FPG"){
    data = data %>% mutate(n = ifelse(Year >= break_year, n_fix, n))
  }
  
  ## Constant survival rate
  if(SR == "FSR"){
    data = data %>% mutate(p = ifelse(Year >= break_year, p_fix, p),
                           # p1 only has > instead of >= to attribute the difference with counterfactual 
                           # to 2010 instead of 1970
                           # Same initial labor income share required : same fixed point
                           p1 = ifelse(Year > break_year, p_fix, p1))
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
  # Compute eta
  data = data %>% mutate(eta = n / p * (1 + alpha*p1) / omega)
  
  ## Constant population structure
  if(DE == "FDE"){
    data = data %>% mutate(n = ifelse(Year >= break_year, 1, n),
                           p = ifelse(Year >= break_year, p_fix, p),
                           # p1 only has > instead of >= to attribute the difference with counterfactual 
                           # to 2010 instead of 1970
                           # Same initial labor income share required : same fixed point
                           p1 = ifelse(Year > break_year, p_fix, p1),
                           Ny = ifelse(Year >= break_year, Ny_fix, Ny),
                           No = ifelse(Year >= break_year, No_fix, No))
  }
  
  ## Constant political power (eta)
  if(IE == "FIE"){
    data = data %>% mutate(eta = ifelse(Year >= break_year, eta_fix, eta))
  }
  
  }
  
  data = data %>% select(-contains("fix"))
  
  return(data)
}