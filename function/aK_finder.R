aK_finder = function(target = target, var_target = var_target){
  
  data_reset = data
  aK_bound = c(0,10)
  precision = nchar(strsplit(as.character(target), "\\.")[[1]][2])
  
  for (preci in c(0:precision)){
    aK_interval = seq(aK_bound[1], aK_bound[2], by = 0.1^(preci))
    est = NA
    for(aKi in c(1:length(aK_interval))){
      data = data_reset
      aK = aK_interval[aKi]
      data = techno_val(aK = aK)
      
      est_add = model_base(2, A = A)
      est_add = est_add[names(est_add) == var_target]
      est_add = sapply(est_add, "[[", 2)
      est = c(est, est_add)
    }
    est = est[-1]
    names(est) = aK_interval
    dist = abs(est - target)
    aK_bound = sort(as.numeric(c(names(dist)[dist == min(dist)], names(dist)[dist == min(dist[dist!=min(dist)])])))
  }
  aK_answer = as.numeric(names(dist)[dist == min(dist)])[1]
  return(aK_answer)
  
  
  
}