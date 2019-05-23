# Model function
model <- function(time, A = A){
  
    for(t in 1:time){
      
      # Limits
      data$k1[t] = data$K[t]/data$Ny[t]
      data$k2[t] = data$AL[t]/data$AK[t]*((1-phi)/phi/data$eta[t])^(sigma/(sigma-1))
      
      # Functions
      to_solve = function(k){
        F1 = (sigma+(1-phi)/phi*(1-gamma*(1-sigma))/gamma*(data$AK[t]/data$AL[t]*k)^((1-sigma)/sigma))^(-1) - 
          log((data$Ny[t]/data$K[t]*k-1)/(phi/(1-phi)*(data$AK[t]/data$AL[t]*k)^((sigma-1)/sigma)*data$eta[t]-1))
      }
      
      # Solver
      if(sigma<1){
        if(data$k1[t]<data$k2[t]){
          data$k[t] = round(uniroot(to_solve, interval = c(ceiling_digit(data$k1[t],3),flooring_digit(data$k2[t],3)))$root,3)
          data$X[t] = (sigma + (1-phi)/phi * (1-gamma*(1-sigma))/gamma * (data$AK[t]/data$AL[t]*data$k[t])^((1-sigma)/sigma))^(-1)
        } else {
          data$k[t] = data$k1[t]
          data$X[t] = 0
        }
      } else {
        if(data$k1[t]<data$k2[t]){
          data$k[t] = data$k1[t]
          data$X[t] = 0
        } else {
          data$k[t] = round(uniroot(to_solve, interval = c(ceiling_digit(data$k1[t],3), (data$k1[t]*100)))$root,1000)
          data$X[t] = (sigma + (1-phi)/phi * (1-gamma*(1-sigma))/gamma * (data$AK[t]/data$AL[t]*data$k[t])^((1-sigma)/sigma))^(-1)
        }
      }
      
      # Other variables
      
      data$L[t] = data$K[t]/data$k[t]
      data$w[t] = A*(1-phi)*data$AL[t]*(phi*(data$k[t]*data$AK[t]/data$AL[t])^((sigma-1)/sigma)+1-phi)^(1/(sigma-1))
      data$Y[t] = A*(phi*(data$AK[t]*data$K[t])^((sigma-1)/sigma)+(1-phi)*(data$AL[t]*data$L[t])^((sigma-1)/sigma))^(sigma/(sigma-1))
      data$u[t] = 1 - data$L[t]/data$Ny[t]
      data$theta[t] = (phi/(1-phi)*(data$AK[t]/data$AL[t]*data$k[t])^((sigma-1)/sigma)+1)^(-1)
      data$tau[t] = 1-((1-data$theta[t])*(1+beta+data$eta[t]))^(-1)
      data$b[t] = (1-data$tau[t])*data$w[t]*exp(-data$X[t])
      data$h[t] = (data$tau[t]*data$Y[t]/data$Ny[t]-data$b[t]*data$u[t])*data$n[t]/data$p[t]
      data$S[t] = alpha*data$p1[t]/(1+alpha*data$p1[t])*((1-data$tau[t])*data$w[t]*(1-data$u[t])+data$b[t]*data$u[t])*data$Ny[t]
      data$K[t+1] = data$S[t]
      
    }
  
  return(data)
  
}
