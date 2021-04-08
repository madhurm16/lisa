# Create empty vector before solving the model
empty_vector <- function(){
  
  # Population
  Ny = rep(NA, time)
  No = rep(NA, time)
  n = rep(NA, time)
  p = rep(NA, time)
  p1 = rep(NA, time)
  eta = rep(NA, time)
  
  # Technology
  AK = rep(NA, time)
  AL = rep(NA, time)
  
  # Limits
  k1 = rep(NA, time)
  k2 = rep(NA, time)
  
  # Solver
  k = rep(NA, time)
  X = rep(NA, time)
  
  # Variables
  L = rep(NA, time)
  w = rep(NA, time)
  Y = rep(NA, time)
  u = rep(NA, time)
  theta = rep(NA, time)
  tau = rep(NA, time)
  b = rep(NA, time)
  h = rep(NA, time)
  S = rep(NA, time)
  K = rep(NA, time)
  
  output = list(Ny, No, n, p, p1, eta, AK, AL, k1, k2,  k, X, L, w, Y, u, theta, tau, b, h, S, K)
  
  names(output) = c("Ny", "No", "n", "p", "p1", "eta",
                    "AK", "AL",
                    "k1", "k2", "k", "X",
                    "L", "w", "Y", "u", "theta", "tau", "b", "h", "S", "K")
  
  return(output)
  
}
