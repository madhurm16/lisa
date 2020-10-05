tabular = function(list_to_display, 
         group.models = NULL, model_map = NULL,
         coef_map = NULL, gof.row = NULL,
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
  
  # GOF MULTICOLUMN CENTER
  gof = gof %>% str_replace(" & ", " &\\\\multicolumn{1}{c}{") %>% 
    str_replace_all(" & ", " } &\\\\multicolumn{1}{c}{") %>% 
    str_replace_all("\\\\\\\\", "}\\\\\\\\") %>% 
    str_remove_all(" ")
  
  # GROUPS MODEL
  if(!is.null(group.models)){
    group.models.names <- ""
    group.models.midrule <- NULL
    for(i in 1:length(group.models)){
      group.models.names = c(group.models.names, 
                             paste0("\\multicolumn{", length(group.models[[i]]), "}{c}{", 
                                    names(group.models)[i], "}")) %>% 
        paste0(collapse = " & ")
      
      group.models.midrule = c(group.models.midrule, 
                               paste0("\\cmidrule(lr){", min(group.models[[i]])+1,"-", 
                                      max(group.models[[i]])+1, "}")
      )
    }
    group.models.names = paste0(group.models.names, " \\\\")
    group.models.midrule = paste0(group.models.midrule, collapse = "")
  }
  
  # NEW TABLE
  new_table = c(begin_tab_align,
                "\\toprule")
  if(!is.null(group.models)){
    new_table = c(new_table, group.models.names, group.models.midrule)
  }
  new_table = c(new_table,
                model_names,
                "\\midrule",
                core,
                "\\midrule"
  )
  if(!is.null(gof.row)){
    new_table = c(new_table, gof.row.table, "\\midrule")
  }
  new_table = c(new_table,
                gof,
                "\\bottomrule")
  # End of the table
  new_table = c(new_table, "\\end{tabular}")
  
  return(new_table)
  
}
