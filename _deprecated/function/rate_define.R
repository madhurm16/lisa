rate_define <- function(var_init, SR = "ISR", PG = "DPG", DE = "TDE", IE = "TIE"){
  
  var_init = var_init_save[var_init$Country == Country,]
  
  n_fix = 1
  p_fix = 1
  p1_fix = 1
  
  eta_fix = n_fix/p_fix * (1+alpha*p1_fix) / omega
  
  Ny_nor_fix = var_init$Ny_nor_plus4*n_fix
  No_nor_fix = var_init$Ny_nor_plus4*p_fix
  
  # Column locations in var_init
  Ny_col = rep(1:5)*6-2
  No_col = rep(1:5)*6-1
  n_col = rep(1:5)*6
  p_col = rep(1:5)*6+1
  p1_col = rep(1:5)*6+2
  eta_col = rep(1:5)*6+3
  
  # Initial year of the break_year
  fix_x = break_year %% generation + (start - 10)
  
  # Column of each variable
  Ny_fix_y = Ny_col[1] + 6 * (break_year - fix_x) / generation
  No_fix_y = No_col[1] + 6 * (break_year - fix_x) / generation
  
  n_fix_y = n_col[1] + 6 * (break_year - fix_x) / generation
  p_fix_y = p_col[1] + 6 * (break_year - fix_x) / generation
  p1_fix_y = p1_col[1] + 6 * (break_year - fix_x) / generation
  
  eta_fix_y = eta_col[1] + 6 * (break_year - fix_x) / generation
  
  # Block the direct channel
  if(DE == "FDE"){
    
    n_fix = 1
    p_fix = var_init[var_init$Year == fix_x, p_fix_y]
    #p1_fix = var_init[var_init$Year == fix_x, p1_fix_y]
    p1_fix = p_fix
    
    Ny_fix = var_init[var_init$Year == fix_x, Ny_fix_y]
    No_fix = var_init[var_init$Year == fix_x, No_fix_y]
    
    var_init[,n_col[n_col > n_fix_y]] <- n_fix
    var_init[var_init$Year >= fix_x, n_fix_y] <- n_fix
    
    var_init[, p_col[p_col > p_fix_y]] <- p_fix
    var_init[var_init$Year >= fix_x, p_fix_y] <- p_fix
    
    var_init[, p1_col[p1_col > p1_fix_y]] <- p1_fix
    var_init[var_init$Year >= fix_x, p1_fix_y] <- p1_fix
    
    var_init[, Ny_col[Ny_col > Ny_fix_y]] <- Ny_fix
    var_init[var_init$Year > fix_x, Ny_fix_y] <- Ny_fix
    
    var_init[, No_col[No_col > No_fix_y]] <- No_fix
    var_init[var_init$Year > fix_x, No_fix_y] <- No_fix
    
  }
  
  # Constant population growth at break_year
  if(PG == "CPG"){
    
    n_fix = var_init[var_init$Year == fix_x, n_fix_y]
    
    var_init[,n_col[n_col > n_fix_y]] <- n_fix
    var_init[var_init$Year >= fix_x, n_fix_y] <- n_fix
    
    # Modify variables after CPG application
    var_init$eta        = var_init$n/var_init$p*(1+alpha*var_init$p1)/omega
    
    var_init$Ny_nor_plus1 = var_init$Ny_nor*var_init$n_plus1
    var_init$No_nor_plus1 = var_init$Ny_nor*var_init$p_plus1
    var_init$eta_plus1  = var_init$n_plus1/var_init$p_plus1*(1+alpha*var_init$p1_plus1)/omega
    
    var_init$Ny_nor_plus2 = var_init$Ny_nor_plus1*var_init$n_plus2
    var_init$No_nor_plus2 = var_init$Ny_nor_plus1*var_init$p_plus2
    var_init$eta_plus2  = var_init$n_plus2/var_init$p_plus2*(1+alpha*var_init$p1_plus2)/omega
    
    var_init$Ny_nor_plus3 = var_init$Ny_nor_plus2*var_init$n_plus3
    var_init$No_nor_plus3 = var_init$Ny_nor_plus2*var_init$p_plus3
    var_init$eta_plus3  = var_init$n_plus3/var_init$p_plus3*(1+alpha*var_init$p1_plus3)/omega
    
    var_init$Ny_nor_plus4 = var_init$Ny_nor_plus3*var_init$n_plus4
    var_init$No_nor_plus4 = var_init$Ny_nor_plus3*var_init$p_plus4
    var_init$eta_plus4  = var_init$n_plus4/var_init$p_plus4*(1+alpha*var_init$p1_plus4)/omega
    
  }
  
  # Constant survival rate at break_year
  if(SR == "CSR"){
    
    p_fix = var_init[var_init$Year == fix_x, p_fix_y]
    #p1_fix = var_init[var_init$Year == fix_x, p1_fix_y]
    p1_fix = p_fix
    
    var_init[, p_col[p_col > p_fix_y]] <- p_fix
    var_init[var_init$Year >= fix_x, p_fix_y] <- p_fix
    
    var_init[, p1_col[p1_col > p1_fix_y]] <- p1_fix
    var_init[var_init$Year >= fix_x, p1_fix_y] <- p1_fix
    
    # Modify variables after CSR application
    var_init$eta        = var_init$n/var_init$p*(1+alpha*var_init$p1)/omega
    
    var_init$Ny_nor_plus1 = var_init$Ny_nor*var_init$n_plus1
    var_init$No_nor_plus1 = var_init$Ny_nor*var_init$p_plus1
    var_init$eta_plus1  = var_init$n_plus1/var_init$p_plus1*(1+alpha*var_init$p1_plus1)/omega
    
    var_init$Ny_nor_plus2 = var_init$Ny_nor_plus1*var_init$n_plus2
    var_init$No_nor_plus2 = var_init$Ny_nor_plus1*var_init$p_plus2
    var_init$eta_plus2  = var_init$n_plus2/var_init$p_plus2*(1+alpha*var_init$p1_plus2)/omega
    
    var_init$Ny_nor_plus3 = var_init$Ny_nor_plus2*var_init$n_plus3
    var_init$No_nor_plus3 = var_init$Ny_nor_plus2*var_init$p_plus3
    var_init$eta_plus3  = var_init$n_plus3/var_init$p_plus3*(1+alpha*var_init$p1_plus3)/omega
    
    var_init$Ny_nor_plus4 = var_init$Ny_nor_plus3*var_init$n_plus4
    var_init$No_nor_plus4 = var_init$Ny_nor_plus3*var_init$p_plus4
    var_init$eta_plus4  = var_init$n_plus4/var_init$p_plus4*(1+alpha*var_init$p1_plus4)/omega
    
  }
  
  # Block the indirect channel
  if(IE == "FIE"){
    
    eta_fix = var_init[var_init$Year == fix_x, eta_fix_y]
    
    var_init[,eta_col[eta_col > eta_fix_y]] <- eta_fix
    var_init[var_init$Year >= fix_x, eta_fix_y] <- eta_fix
    
  }
  
  return(list(var_init, Ny_nor_fix, No_nor_fix, n_fix, p_fix, p1_fix, eta_fix))
  
}