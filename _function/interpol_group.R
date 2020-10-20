require(reshape2)
require(dplyr)
require(zoo)

interpol_group <- function(df, method_use='linear'){
   for (i in 3:ncol(df)){
      varnames = names(df)
      names(df)[i] = 'var'
      num_NA = sum(is.na(df$var))
      df = df %>% group_by(Country) %>% mutate(var = na.approx(var, method = method_use, na.rm = FALSE))
      num_NA2 = sum(is.na(df$var))
      names(df) = varnames
      print(paste0("Number of NA values removed for the variable ", names(df)[i],": ",
                   num_NA-num_NA2," on ",num_NA," (",round((1-num_NA2/num_NA)*100,1),"%)"))
      df = as.data.frame(df %>% ungroup())
   }
   return(df)
}