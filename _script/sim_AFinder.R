# Loop on countries
  for(country in levels(data$Country)){
    
    target = df1 %>% 
      subset(Country == country) %>% 
      mutate(MatchYear = FirstYear+40) %>%
      .[which(.$Year %in% c({unique(.$MatchYear)-2}:{unique(.$MatchYear)+2})),] %>% 
      pull("theta") %>% 
      mean(na.rm = T) %>% 
      round(3)
    
    A_bound = c(1,200) # Grid search interval
    precision = nchar(strsplit(as.character(target), "\\.")[[1]][2]) # Precision
    
    # Loop on precision level
    for(preci in c(1:precision)){
      
      A_interval = seq(A_bound[1], A_bound[2], by = 0.1^(preci))
      est = NA
      
      for(Ai in c(1:length(A_interval))){
        
        est_add = data %>%
          subset(Country == country) %>% 
          mutate(A = A_interval[Ai]) %>% 
          # model(time = 2, AFinder = TRUE) %>% 
          model3(., AFinder = TRUE) %>%
          subset(Year == FirstYear+40) %>% 
          pull("theta")
        
        est = c(est, est_add)
      
      }
      
      est = est[-1]
      names(est) = A_interval
      dist = (est - target)^2
      A_bound = sort(as.numeric(c(names(dist)[dist == min(dist)],
                                  names(dist)[dist == min(dist[dist!=min(dist)])])))
    }
    
    # Select the A value
    A_answer = as.numeric(names(dist)[dist == min(dist)])[1]
    
    # Save the result
    data$A[data$Country == country] = A_answer
    
  }

# Remove
rm(est, est_add, dist, preci, precision, target, A_answer, A_bound, A_interval, Ai, country)
