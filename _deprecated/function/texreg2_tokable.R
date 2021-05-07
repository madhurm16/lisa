texreg2_tokable = function(list_to_display, 
                   group.models = NULL, model_map = NULL,
                   coef_map = NULL, gof.row = NULL,
                   caption = NULL, label = NULL, 
                   note = NULL, 
                   full_width = TRUE,
                   ...){
  
  table_split = texreg::texreg(
    list_to_display,
    stars = c(0.01, 0.05, 0.1),
    custom.coef.map = coef_map,
    custom.model.names = model_map,
    custom.note = "",
    table = FALSE,
    dcolumn = TRUE, ...
  ) %>% 
    strsplit("\n") %>% 
    unlist
  
  # HLINE POSITION
  hline_pos = grep("hline", table_split)
  
  # SCRAP TEXREG
  begin_tab_align = table_split[hline_pos[1]-1]
  model_names = table_split[hline_pos[2]-1]
  core = table_split[(hline_pos[2]+1):(hline_pos[3]-1)]
  gof = table_split[(hline_pos[3]+1):(hline_pos[4]-1)]
  
  # GOF ROW
  if(!is.null(gof.row)){
    string <- NA
    for(i in 1:length(names(gof.row))){
      string[i] <- gof.row[[i]] %>% 
        paste0(collapse = "} & \\multicolumn{1}{c}{") %>% 
        c(names(gof.row)[i], .) %>% 
        paste0(collapse = " & \\multicolumn{1}{c}{")
    }
    gof.row.table = paste0(string, "} \\\\")
  }
  
  # GROUPS MODEL
  if(!is.null(group.models)){
    header.models <- NULL
    for(i in 1:length(group.models)){
      header.models = c(header.models, length(group.models[[i]]))
    }
    names(header.models) = names(group.models)
  }
  
  # Retreat model names
  model_names = str_split(model_names, " & ", simplify = TRUE) %>% 
    gsub("\\multicolumn", "", .) %>% 
    gsub("\\{1\\}\\{c\\}", "", .) %>%
    gsub("\\{|\\}|\\^", "", .) %>% 
    gsub("\\\\", "", .)
  
  # New table
  new_table = core
  
  # GOF ROW
  if(!is.null(gof.row)){
    new_table = c(new_table, gof.row.table)
  }
  
  new_table = c(new_table, gof) %>% 
    data.frame(stringsAsFactors = FALSE)
  
  # Count the number of columns
  nb_col = new_table[1,] %>% str_count(" & ") %>% {.+1}
  
  # Clean TeX characters
  new_table = new_table %>% 
    separate(col = ".", into = paste0("col", 1:nb_col), sep = " & ") %>% 
    mutate_if(is.character, ~ gsub("\\\\\\\\", "", .)) %>% 
    mutate_if(is.character, ~ gsub("\\\\multicolumn", "", .)) %>% 
    # mutate_if(is.character, ~ gsub("\\\\quad", "", .)) %>% 
    mutate_if(is.character, ~ gsub("\\{1\\}\\{c\\}", "", .)) %>% 
    mutate_if(is.character, ~ gsub("\\^\\{", "", .)) %>% 
    mutate_if(is.character, ~ gsub("\\{|\\}", "", .)) %>% 
    mutate_if(is.character, ~ str_trim(.))
  names(new_table) <- model_names
  
  # Kable it
  new_table = kableExtra::kable(new_table, caption = caption, label = label, ...) 
  # %>% 
  #   row_spec(row = 0, align = "c")
  
  if(!is.null(gof.row)){
    new_table = new_table %>% 
      pack_rows("Controls", 
                length(core)+1, 
                length(core)+length(gof.row.table)) %>% 
      pack_rows("Goodness-of-fit statistics", 
                length(core)+length(gof.row.table)+1, 
                length(core)+length(gof.row.table)+length(gof))
  }else{
    new_table = new_table %>% 
      pack_rows("Goodness-of-fit statistics", 
                length(core)+1, 
                length(core)+length(gof))
    
  }
  
  # GROUPS MODEL
  if(!is.null(group.models)){
  new_table = new_table %>% 
    add_header_above(c(" ", header.models))
  }
  
  # Style it
  new_table = new_table %>% 
    kableExtra::kable_styling(
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
