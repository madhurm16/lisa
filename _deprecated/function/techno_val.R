techno_val = function(aK){
  
  if(TC == "0TC"){
    data$AK = rep(1, time)
    data$AL = rep(1, time)
  }
  
  if(TC %in% c("BTC", "BTC_aK0", "NTC")){
    t = 1:time
    year_a = 10*(4*t + i - 5)
    aL = a + aK
    data$AK = exp(aK*year_a)
    data$AL = exp(aL*year_a)
  }
  
  return(data)
  
}