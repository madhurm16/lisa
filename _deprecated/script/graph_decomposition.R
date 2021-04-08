##### INITIALIZATION #####

# Define required packages
lapply(c("dplyr", "ggplot2", "ggrepel", "RColorBrewer", "reshape2"), require, character.only = TRUE)

# Define paths
loc_final = file.path("data", "final")
loc_result = file.path("result")
loc_decomposition = file.path(loc_result, "decomposition")
loc_sim = file.path(loc_result, "sim")

# Define countries to plot
country_set = c("France", "United States")

# Load data
final = read.csv(file.path(loc_sim, "final_counter.csv"), header = TRUE)

# Graphic parameters
breaks_10_years = seq(1970, 2080, 10)
labs_20_years = as.vector(rbind(seq(1970, 2080, 20), rep("", length(breaks_10_years)/2)))
scale_graph = 1920/1080

##### FUNCTIONS : GRAPH PRODUCERS #####

## Function to produce PGSR decomposition graphs
PGSR_decomp = function(data_decomp){
  
  ggplot(data = data_decomp, aes(x = Year)) +
    geom_col(aes(y = value, fill = variable),
             color = "black", size = .7, alpha = .5) +
    geom_line(aes(y = position_PGSR),
              color = brewer.pal(8, "Set1")[1], size = 1) +
    geom_hline(yintercept = 0, size = 1) +
    scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
    scale_fill_manual(name = "",
                      breaks = c("TPG.FSR", "FPG.TSR"),
                      labels = c("Survival rate", "Population growth"),
                      values = brewer.pal(8, "Set1")[c(2,3)] %>% 
                        setNames(c("TPG.FSR", "FPG.TSR")),
                      na.value = "black") +
    theme_classic(base_size = 14) +
    theme(legend.direction = "vertical", legend.box = "horizontal", 
          legend.position = c(1,1), legend.justification = c(1, 1)) +
    labs(x = "Year", y = "Difference with counterfactual (in pp.)") +
    ggsave(file.path(loc_decomposition, paste0("PGSR_decomp_", country, ".png")),
           width = scale_graph*5, height = 5)
}
## Function to produce DEIE decomposition graphs
DEIE_decomp = function(data_decomp){
  
  ggplot(data = data_decomp, aes(x = Year)) +
    geom_col(aes(y = value, fill = variable),
             color = "black", size = .7, alpha = .5) +
    geom_line(aes(y = position_DEIE),
              color = brewer.pal(8, "Set1")[1], size = 1) +
    geom_hline(yintercept = 0, size = 1) +
    scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
    scale_fill_manual(name = "",
                      breaks = c("FDE.TIE", "TDE.FIE"),
                      labels = c("Direct effect", "Indirect effect"),
                      values = brewer.pal(8, "Set1")[c(5,8)] %>% 
                        setNames(c("FDE.TIE", "TDE.FIE")),
                      na.value = "black") +
    theme_classic(base_size = 14) +
    theme(legend.direction = "vertical", legend.box = "horizontal", 
          legend.position = c(0.01,1), legend.justification = c(0, 1)) +
    labs(x = "Year", y = "Difference with counterfactual (in pp.)") +
    ggsave(file.path(loc_decomposition, paste0("DEIE_decomp_", country, ".png")),
           width = scale_graph*5, height = 5)
}

##### DATA PREPARATION #####

## Reshape dataframe
decomp = final %>% 
  select(Country, Specification, Year, theta) %>% 
  mutate(PG = ifelse(Specification == "data", NA, ifelse(grepl("TPG", Specification), "TPG", "FPG")),
         SR = ifelse(Specification == "data", NA, ifelse(grepl("TSR", Specification), "TSR", "FSR")),
         DE = ifelse(Specification == "data", NA, ifelse(grepl("TDE", Specification), "TDE", "FDE")),
         IE = ifelse(Specification == "data", NA, ifelse(grepl("TIE", Specification), "TIE", "FIE")),
         break_year = as.numeric(gsub("[^0-9]", "\\1", Specification)),
         Spe_PGSR = interaction(PG, SR),
         Spe_DEIE = interaction(DE, IE)) %>% 
  select(Country, Year, Spe_PGSR, Spe_DEIE, theta) %>% 
  dcast(Country + Year ~ Spe_PGSR + Spe_DEIE, value.var = "theta") %>% 
  setNames(c("Country", "Year",
             "FPG.FSR", "TPG.FSR", "FPG.TSR", 
             "FDE.FIE", "TDE.FIE", "FDE.TIE",
             "benchmark")) %>% 
  mutate(FPG.TSR = (benchmark - FPG.TSR)*100,
         TPG.FSR = (benchmark - TPG.FSR)*100,
         FPG.TSR.share = abs(FPG.TSR)/(abs(FPG.TSR) + abs(TPG.FSR)),
         TPG.FSR.share = abs(TPG.FSR)/(abs(FPG.TSR) + abs(TPG.FSR)),
         position_PGSR = (FPG.TSR + TPG.FSR),
         FDE.TIE = (benchmark - FDE.TIE)*100,
         TDE.FIE = (benchmark - TDE.FIE)*100,
         FDE.TIE.share = abs(FDE.TIE)/(abs(FDE.TIE) + abs(TDE.FIE)),
         TDE.FIE.share = abs(TDE.FIE)/(abs(FDE.TIE) + abs(TDE.FIE)),
         position_DEIE = (FDE.TIE + TDE.FIE)) %>% 
  melt(id.vars = c("Country", "Year", "position_PGSR", "position_DEIE"))

##### PGSR GRAPH #####

# Country loop
for(country in country_set){
  decomp %>% 
    subset(Country == country & variable %in% c("TPG.FSR", "FPG.TSR")) %>% 
    PGSR_decomp()
}

##### DEIE GRAPH #####

# Country loop
for(country in country_set){
  decomp %>% 
    subset(Country == country & variable %in% c("TDE.FIE", "FDE.TIE")) %>% 
    DEIE_decomp()
}





