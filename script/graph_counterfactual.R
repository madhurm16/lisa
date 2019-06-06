##### INITIALIZATION #####

# Define required packages
lapply(c("dplyr", "ggplot2", "ggrepel", "RColorBrewer", "reshape2"), require, character.only = TRUE)

# Define paths
loc_final = file.path("data", "final")
loc_result = file.path("result")
  loc_counter = file.path(loc_result, "counter")
  loc_sim = file.path(loc_result, "sim")

# Define countries to plot
country_set = c("France", "United States")

# Load data
final = read.csv(file.path(loc_sim, "final_counter.csv"), header = TRUE)
data = read.csv(file.path(loc_sim, "data.csv"), header = TRUE)

# Graphic parameters
breaks_10_years = seq(1970, 2080, 10)
labs_20_years = as.vector(rbind(seq(1970, 2080, 20), rep("", length(breaks_10_years)/2)))
scale_graph = 1920/1080

##### FUNCTIONS : GRAPH PRODUCERS #####

## Function to produce PGSR counterfactual graphs
PGSR_graph = function(data_decomp){
  
  # Graph
  ggplot(data = data_decomp, aes(x = Year, y = theta, color = Spe_PGSR, linetype = from)) +
    geom_line(size = 0.5) +
    geom_label_repel(data = data_decomp %>% subset(Year == 2080 & break_year == b_year),
                     aes(label = round(theta, 3)),
                     nudge_x = 5, na.rm = TRUE, segment.color = "transparent") +
    scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years, lim = c(NA, 2090)) +
    scale_y_continuous(lim = c(ymin, ymax)) +
    scale_linetype_manual(name = "From",
                          breaks = c("data", "sim"),
                          labels = c("Data", "Model"),
                          values = c("solid", "dashed")) +
    scale_color_manual(name = "Specification",
                       breaks = c("TPG.TSR", "TPG.FSR",
                                  "FPG.TSR", "FPG.FSR"),
                       labels = c("Benchmark", "Constant Survival Rate",
                                  "Constant Pop. Growth", "Constant Dep. Ratio"),
                       values = brewer.pal(8, "Set1")[c(1:4)] %>% 
                         setNames(c("TPG.TSR", "TPG.FSR", "FPG.TSR", "FPG.FSR")),
                       na.value = "black") +
    theme_classic(base_size = 14) +
    guides(color = guide_legend(order = 0), linetype = guide_legend(order = 1)) +
    theme(legend.direction = "vertical", legend.box = "horizontal", 
          legend.position = c(0.5,1), legend.justification = c(0, 1)) +
    labs(x = "Year", y = "Labor income share") +
    ggsave(file.path(loc_counter, paste0("PGSR_", country, b_year,"_", i, ".png")),
           width = scale_graph*5, height = 5)
  
}

## Function to produce DEIE counterfactual graphs
DEIE_graph = function(data_decomp){
  
  # Graph
  ggplot(data = data_decomp, aes(x = Year, y = theta, color = Spe_DEIE, linetype = from)) +
    geom_line(size = 0.5) +
    geom_label_repel(data = data_decomp %>% subset(Year == 2080 & break_year == b_year),
                     aes(label = round(theta, 3)),
                     nudge_x = 5, na.rm = TRUE, segment.color = "transparent") +
    scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years, lim = c(NA, 2090)) +
    scale_y_continuous(lim = c(ymin, ymax)) +
    scale_linetype_manual(name = "From",
                          breaks = c("data", "sim"),
                          labels = c("Data", "Model"),
                          values = c("solid", "dashed")) +
    scale_color_manual(name = "Specification",
                       breaks = c("TDE.TIE", "TDE.FIE",
                                  "FDE.TIE", "FDE.FIE"),
                       labels = c("Benchmark", "w/o Indirect Effect",
                                  "w/o Direct Effect", "w/o Demographic Effect"),
                       values = brewer.pal(8, "Set1")[c(1,5,8,4)] %>% 
                         setNames(c("TDE.TIE", "TDE.FIE", "FDE.TIE", "FDE.FIE")),
                       na.value = "black") +
    theme_classic(base_size = 14) +
    guides(color = guide_legend(order = 0), linetype = guide_legend(order = 1)) +
    theme(legend.direction = "vertical", legend.box = "horizontal", 
          legend.position = c(0.5,1), legend.justification = c(0, 1)) +
    labs(x = "Year", y = "Labor income share") +
    ggsave(file.path(loc_counter, paste0("DEIE_", country, b_year, "_", i, ".png")),
           width = scale_graph*5, height = 5)
  
}

##### DATA PREPARATION #####

## Reshape dataframe
decomp = final %>% 
  select(Country, Specification, Year, theta) %>% 
  rbind(., 
        data %>% 
          mutate(Specification = "data_1970") %>%
          select(Specification, Country, Year, theta)) %>% 
  rbind(.,
        data %>% 
          mutate(Specification = "data_2010") %>%
          select(Specification, Country, Year, theta)) %>% 
  mutate(from = ifelse(grepl("data", Specification), "data", "sim")) %>%
  filter(complete.cases(.)) %>% 
  mutate(PG = ifelse(from == "data", NA, ifelse(grepl("TPG", Specification), "TPG", "FPG")),
         SR = ifelse(from == "data", NA, ifelse(grepl("TSR", Specification), "TSR", "FSR")),
         DE = ifelse(from == "data", NA, ifelse(grepl("TDE", Specification), "TDE", "FDE")),
         IE = ifelse(from == "data", NA, ifelse(grepl("TIE", Specification), "TIE", "FIE")),
         break_year = as.numeric(gsub("[^0-9]", "\\1", Specification)),
         Spe_PGSR = interaction(PG, SR),
         Spe_DEIE = interaction(DE, IE)) %>% 
  select(Specification, break_year, from, Spe_PGSR, Spe_DEIE, PG, SR, DE, IE, everything())

##### PGSR GRAPHS #####

# List of selected graphs
PGSR_list = list(NA, "TPG.TSR", 
                 c("TPG.TSR","TPG.FSR"), 
                 c("TPG.TSR","FPG.TSR"),
                 c("TPG.TSR", "FPG.TSR", "TPG.FSR"), 
                 c("TPG.TSR", "FPG.TSR", "TPG.FSR", "FPG.FSR"))

# Break_year loop
for(b_year in c(1970, 2010)){
  # Country loop
  for(country in country_set){
    
    decomp_temp = decomp %>% 
      subset(DE %in% c("TDE", NA) & IE %in% c("TIE", NA) & Country == country & break_year == b_year)
    
    # Limits on graph
    ymin = decomp_temp %>% pull("theta") %>% min()
    ymax = decomp_temp %>% pull("theta") %>% max()
    
    # Loop for graphs
    for(i in c(1:length(PGSR_list))){
      decomp_temp %>% 
        subset(from == "data" | Spe_PGSR %in% PGSR_list[[i]]) %>% 
        PGSR_graph()
    }
  }
}

##### DEIE GRAPHS #####

# List of selected graphs
DEIE_list = list(NA, "TDE.TIE", 
                 c("TDE.TIE","TDE.FIE"), 
                 c("TDE.TIE","FDE.TIE"),
                 c("TDE.TIE", "FDE.TIE", "TDE.FIE"), 
                 c("TDE.TIE", "FDE.TIE", "TDE.FIE", "FDE.FIE"))

# Break_year loop
for(b_year in c(1970, 2010)){
  # Country loop
  for(country in country_set){
    
    decomp_temp = decomp %>% 
      subset(PG %in% c("TPG", NA) & SR %in% c("TSR", NA) & Country == country & break_year == b_year)
    
    # Limits on graph
    ymin = decomp_temp %>% pull("theta") %>% min()
    ymax = decomp_temp %>% pull("theta") %>% max()
    
    # Loop for graphs
    for(i in c(1:length(DEIE_list))){
      decomp_temp %>% 
        subset(from == "data" | Spe_DEIE %in% DEIE_list[[i]]) %>% 
        DEIE_graph()
    }
  }
}

