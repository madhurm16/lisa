# The tabular function is used to produce a regression table
tabular = function(list_to_display, 
         group.models = NULL, model_map = NULL,
         coef_map = NULL, gof.row = NULL,
         reg.type = NULL, groups = NULL,
         ...){
  
  table_split = texreg::texreg(
    list_to_display,
    stars = c(0.01, 0.05, 0.1),
    custom.coef.map = coef_map,
    custom.model.names = model_map,
    custom.note = "",
    groups = groups,
    table = FALSE,
    dcolumn = TRUE,
    include.aic = FALSE,
    include.bic = FALSE,
    include.dev = FALSE, # beside = T, include.group = F, include.loglik = F
    ...
  ) %>% 
    strsplit("\n") %>% 
    unlist
  
  # Count the number of & in column [Because length(list_to_display) does is reduced for multinomial reg]
  esperluette_number = lengths(regmatches(table_split[6], gregexpr("&", table_split[6])))
  
  # HLINE POSITION
  hline_pos = grep("hline", table_split)
  
  # SCRAP TEXREG
  begin_tab_align = table_split[hline_pos[1]-1]
  model_names = table_split[hline_pos[2]-1]
  core = table_split[(hline_pos[2]+1):(hline_pos[3]-1)]
  gof = table_split[(hline_pos[3]+1):(hline_pos[4]-1)]
  
  # BEGIN TAB include REGRESSION TYPE
  if(!is.null(reg.type)){
    begin_tab_align = c(begin_tab_align, "\\toprule",
                        paste0(" & \\multicolumn{", esperluette_number,"}{c}{", reg.type, "} \\\\"),
                        paste0("\\cmidrule(lr){2-", esperluette_number+1, "}"))
  }
  
  # CORE GROUPS
  if(!is.null(groups)){
    empty_line = NULL
    for(i in 1:length(groups)){
    core[(groups[[i]][1]+i-1)*2-1] = paste0("\\midrule\\multicolumn{", esperluette_number+1,"}{l}{", 
                                      names(groups[i]), "} \\\\ \\midrule")
    empty_line = c(empty_line, (groups[[i]][1]+i-1)*2)
    }
    core = core[-empty_line]
  }
  
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
  
  # PSEUDO R2 in BINOMIAL LOGISTIC REGRESSION
  if(sum(grep("Log", gof[1]))==1){
    gof = c(pseudoR2(list_to_display), gof)
  }
  
  # GOF MULTICOLUMN CENTER
  gof = gof %>% 
    str_replace_all("\\s+", " ") %>% 
    str_replace(" & ", " &\\\\multicolumn{1}{c}{") %>% 
    str_replace_all(" & ", "} &\\\\multicolumn{1}{c}{") %>% 
    str_replace_all(" \\\\\\\\", "}\\\\\\\\") %>% 
    str_replace_all("&\\\\multicolumn", "& \\\\multicolumn")
  
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
  new_table = begin_tab_align
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
