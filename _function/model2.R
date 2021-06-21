# Model function
model2 <- function(data, time, AFinder = FALSE){
  
  all_seq = unique(data$Sequence)
  if(AFinder == TRUE){all_seq = 1}
  
  for(seq in all_seq){
    
    # Row position in data : according to the sequence
    position = seq(seq, 4*time, 4)
    
    for(t in position){
      
      ## Solver
      # Sigma = 1
      if(data$sigma[t] == 1){
        # Net replacement rate in unemployment
        data$X[t] = exp(-(1+(1-data$phi[t])/data$phi[t]/data$gamma[t])^(-1))
        # Capital-per-worker
        data$k[t] = data$K[t]/data$Ny[t]*data$eta[t]*(1/data$X[t]-1)
        # Labor
        data$L[t] = data$K[t]/data$k[t]
        # Wage under Cobb Douglas
        data$w[t] = data$A[t]*(1-data$phi[t])*data$AL[t]*(data$k[t]*data$AK[t]/data$AL[t])^(data$phi[t])
        # Rental rate under Cobb Douglas
        data$r[t] = data$A[t]*data$phi[t]*data$AK[t]*(data$k[t]*data$AK[t]/data$AL[t])^(data$phi[t]-1)
        # Output under Cobb Douglas
        data$Y[t] = data$A[t]*((data$AK[t]*data$K[t])^(data$phi[t])*(data$AL[t]*data$L[t])^(1-data$phi[t]))
        
      } else {
        
        # Function
        to_solve = function(k){
          F1 = (data$sigma[t]+(1-data$phi[t])/data$phi[t]*(1-data$gamma[t]*(1-data$sigma[t]))/data$gamma[t]*(data$AK[t]/data$AL[t]*k)^((1-data$sigma[t])/data$sigma[t]))^(-1) + 
            log((1+data$phi[t]/(1-data$phi[t])*k^((data$sigma[t]-1)/data$sigma[t]))/(1+k/data$eta[t]*data$Ny[t]/data$K[t]))
        }
        
        data$k[t] = round(uniroot(to_solve, interval = c(0.001, 10000))$root, 3)
        
        ## Other variables
        # Labor
        data$L[t] = data$K[t]/data$k[t]
        if(data$L[t] > data$Ny[t]){
          data$L[t] = data$Ny[t]
          data$k[t] = data$K[t]/data$Ny[t]
        } else {
          data$L[t] = data$K[t]/data$k[t]
        }
        # Unemployment replacement rate
        data$X[t] = exp(-(data$sigma[t] + (1-data$phi[t])/data$phi[t] * (1-data$gamma[t]*(1-data$sigma[t]))/data$gamma[t] * (data$AK[t]/data$AL[t]*data$k[t])^((1-data$sigma[t])/data$sigma[t]))^(-1))
        # Wage
        data$w[t] = data$A[t]*(1-data$phi[t])*data$AL[t]*(data$phi[t]*(data$k[t]*data$AK[t]/data$AL[t])^((data$sigma[t]-1)/data$sigma[t])+1-data$phi[t])^(1/(data$sigma[t]-1))
        # Rental rate
        data$r[t] = data$A[t]*data$phi[t]*data$AK[t]*(data$phi[t]+(1-data$phi[t])*(data$k[t]*data$AK[t]/data$AL[t])^((1-data$sigma[t])/data$sigma[t]))^(1/(data$sigma[t]-1))
        # Output
        data$Y[t] = data$A[t]*(data$phi[t]*(data$AK[t]*data$K[t])^((data$sigma[t]-1)/data$sigma[t])+(1-data$phi[t])*(data$AL[t]*data$L[t])^((data$sigma[t]-1)/data$sigma[t]))^(data$sigma[t]/(data$sigma[t]-1))
        
      }
      
      # Unemployment rate
      data$u[t] = 1 - data$L[t]/data$Ny[t]
      # Labor share
      data$theta[t] = (data$phi[t]/(1-data$phi[t])*(data$AK[t]/data$AL[t]*data$k[t])^((data$sigma[t]-1)/data$sigma[t])+1)^(-1)
      # Tax rate
      data$tau[t] = (data$beta[t]+data$u[t]*data$eta[t])/(1+data$beta[t]+data$eta[t])
      # Unemployment benefits per capita
      # data$b[t] = (1-data$tau[t])*data$w[t]*data$X[t]
      data$b[t] = data$eta[t]/(1+data$beta[t]+data$eta[t])*data$Y[t]/data$Ny[t]
      # Health spending per capita
      # data$h[t] = (data$tau[t]*data$Y[t]-data$b[t]*data$u[t]*data$Ny[t])/data$No[t]
      data$h[t] = data$beta[t]/(1+data$beta[t]+data$eta[t])*data$Y[t]/data$No[t]
      # Savings
      data$S[t] = data$alpha[t]*data$p1[t]/(1+data$alpha[t]*data$p1[t])*((1-data$tau[t])*data$w[t]*(1-data$u[t])+data$b[t]*data$u[t])*data$Ny[t]
      # Capital accumulation
      if(t != last(position)){
        data$K[t+4] = data$S[t]
      }
      
    }
    
  }
  # Return data
  return(data)
}
