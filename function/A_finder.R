A_finder <- function(target, var_target){
   
   A_bound = c(0,100)
   precision = nchar(strsplit(as.character(target), "\\.")[[1]][2])
   
   for (preci in c(0:precision)){
      A_interval = seq(A_bound[1], A_bound[2], by = 0.1^(preci))
      est = NA
      for(Ai in c(1:length(A_interval))){
        A = A_interval[Ai]
        est_add = model(2, A = A)
        est_add = est_add[names(est_add) == var_target]
        est_add = sapply(est_add, "[[", 2)
        est = c(est, est_add)
      }
      est = est[-1]
      names(est) = A_interval
      dist = (est - target)^2
      A_bound = sort(as.numeric(c(names(dist)[dist == min(dist)], names(dist)[dist == min(dist[dist!=min(dist)])])))
   }
   A_answer = as.numeric(names(dist)[dist == min(dist)])[1]
   return(A_answer)
}