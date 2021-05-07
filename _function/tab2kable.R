tab2kable = function(tab_tex, 
                     caption = NULL, label = NULL, note = NULL, 
                     full_width = TRUE,
                     ...){
  
  # MIDRULE POSITION
  midrule_pos = grep("\\\\midrule", tab_tex)
  
  # SCRAP TABULAR
  begin_tab = tab_tex[1:(midrule_pos[1]-2)]
  model_names = tab_tex[midrule_pos[1]-1]
  core = tab_tex[(midrule_pos[1]+1):(last(midrule_pos)-1)]
  gof = tab_tex[(last(midrule_pos)+1):(length(tab_tex)-2)]
  
  # MIDRULE POSITION in CORE
  midrule_pos_in_core = grep("\\\\midrule", core)
  
  # GROUP MODELS
  cmidrule_pos = grep("\\\\cmidrule", begin_tab)
  
  if(!is.null(cmidrule_pos)){
    
    all.header = str_split(begin_tab[cmidrule_pos-1], "&") %>% 
      lapply(., FUN = function(x){x %>% .[-1] %>% data.frame(X = .) %>% 
          separate(col = "X", into = c("colspan", "group"), sep = "\\{c\\}") %>% 
          mutate_all(~ gsub("\\multicolumn", "", .) %>% 
                       gsub("\\multicolumn", "", .) %>% 
                       gsub("\\{|\\}|\\^", "", .) %>% 
                       gsub("\\\\", "", .)) %>% 
          mutate_at(vars(group), ~ if_else(str_trim(.) != "", str_trim(.), " "))
        })
    
    # Header in vector format (list to loop on)
    header.kable = lapply(all.header, function(x){as.integer(x$colspan)} %>% setNames(x$group))
    
  }
  
  # GROUPS (not implemented so far) add them manually after tab2kable
  # core[midrule_pos_in_core]
  
  # Retreat model names
  model_names = str_split(model_names, " & ", simplify = TRUE) %>% 
    gsub("\\multicolumn", "", .) %>% 
    gsub("\\{1\\}\\{c\\}", "", .) %>%
    gsub("\\{|\\}|\\^", "", .) %>% 
    gsub("\\\\", "", .)
  
  # New table
  new_table = core
  new_table = data.frame(X = c(new_table, gof), stringsAsFactors = FALSE)
  
  # Remove remaining midrule
  if(!(length(midrule_pos_in_core)==0)){
    new_table = data.frame(X = new_table[-which(startsWith(str_trim(new_table$X, "left"), 
                                                       "\\midrule")),])
    names(new_table) = "X"
  }
  row.names(new_table) = NULL
  
  # Count the number of columns
  nb_col = new_table[1,] %>% str_count(" & ") %>% {.+1}
  
  # Clean TeX characters
  new_table = new_table %>% 
    separate(col = "X", into = paste0("col", 1:nb_col), sep = " & ") %>% 
    mutate_if(is.character, ~ gsub("\\\\quad", "", .)) %>% 
    mutate_if(is.character, ~ gsub("\\\\\\\\", "", .)) %>% 
    mutate_if(is.character, ~ gsub("\\multicolumn", "", .)) %>% 
    mutate_if(is.character, ~ gsub("\\{1\\}\\{c\\}", "", .)) %>% 
    mutate_if(is.character, ~ gsub("\\^\\{", "", .)) %>% 
    mutate_if(is.character, ~ gsub("\\{|\\}|\\\\", "", .)) %>% 
    mutate_if(is.character, ~ str_trim(.))
  
  # Column names
  names(new_table) <- model_names
  
  # Kable it
  new_table = kableExtra::kable(new_table, ...)
  
  # GROUP MODEL
  if(!is.null(cmidrule_pos)){
    for(header_i in rev(header.kable)){
      new_table = add_header_above(new_table, c(" ", header_i))
    }
  }
  
  # Style it
  new_table = kableExtra::kable_styling(new_table,
    bootstrap_options = c("striped", "hover", "condensed"), full_width = full_width)
  
  # Footnote
  if(!is.null(note)){
    new_table = new_table %>% 
      footnote(
        general = note, 
        footnote_as_chunk = TRUE)
  }
  
  return(new_table)
}
