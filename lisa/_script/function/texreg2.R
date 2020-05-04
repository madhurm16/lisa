texreg2 = function(list_to_display, 
                   group.models = NULL,
                   coef_map = NULL, 
                   caption = NULL, label = NULL, 
                   gof.row = NULL, note = NULL, par_box_size = 0.7, ...){
  
  table_split = texreg::texreg(
    list_to_display,
    stars = c(0.01, 0.05, 0.1),
    custom.coef.map = coef_map,
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
    group.models.names <- ""
    group.models.midrule <- NULL
    for(i in 1:length(group.models)){
      group.models.names = c(group.models.names, 
                             paste0("\\multicolumn{", length(group.models[[i]]), "}{c}{", 
                                    names(group.models)[i], "}")) %>% 
        paste0(collapse = " & ")
      
      group.models.midrule = c(group.models.midrule, 
                               paste0("\\cmidrule(lr){", min(group.models[[i]]),"-", 
                                      max(group.models[[i]]), "}")
      )
    }
    group.models.names = paste0(group.models.names, " \\\\")
    group.models.midrule = paste0(group.models.midrule, collapse = "")
  }
  
  # NEW TABLE
  new_table = c("\\begin{table}[!ht]\n\\centering")
  if(!is.null(caption)){
    new_table = c(new_table, paste0("\\caption{", caption,"}"))
  }
  if(!is.null(label)){
    new_table = c(new_table, paste0("\\label{", label,"}"))
  }
  new_table = c(new_table,
                begin_tab_align,
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
  if(!is.null(note)){
    new_table = c(new_table,
                  paste0("\\multicolumn{", length(list_to_display)+1, "}{l}{\\parbox{", par_box_size, "\\linewidth}{\\vspace{2pt}\\scriptsize{", note, "}}}")
    )
  }
  # End of the table
  new_table = c(new_table, "\\end{tabular}\n\\end{table}")
  
  # Collapse and display
  new_table %>% 
    paste0(collapse = "\n") %>% 
    cat()
  
}
