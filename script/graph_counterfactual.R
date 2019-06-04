##### INITIALIZATION #####

# Define required packages
lapply(c("dplyr", "ggplot2", "ggrepel", "RColorBrewer", "reshape2"), require, character.only = TRUE)

# Define paths
loc_final = file.path("data", "final")
loc_result = file.path("result")
  loc_counter = file.path(loc_result, "counter")
  loc_sim = file.path(loc_result, "sim")

# Define countries to plot
country = c("France", "United States")

# Load data
final = read.csv(file.path(loc_sim, "final_counter.csv"), header = TRUE)
data = read.csv(file.path(loc_sim, "data.csv"), header = TRUE)

# Graphic parameters
breaks_10_years = seq(1970, 2080, 10)
labs_20_years = as.vector(rbind(seq(1970, 2080, 20), rep("", length(breaks_10_years)/2)))

##### GRAPH SEQUENCE #####

# Reshape dataframe
decomp = final %>% 
  select(Country, Specification, Year, theta) %>% 
  rbind(., 
        data %>% 
          mutate(Specification = "data") %>%
          select(Specification, Country, Year, theta)) %>% 
  mutate(from = ifelse(Specification == "data", "data", "sim")) %>% 
  filter(complete.cases(.)) %>% 
  mutate(PG = ifelse(Specification == "data", NA, ifelse(grepl("TPG", Specification), "TPG", "FPG")),
         SR = ifelse(Specification == "data", NA, ifelse(grepl("TSR", Specification), "TSR", "FSR")),
         DE = ifelse(Specification == "data", NA, ifelse(grepl("TDE", Specification), "TDE", "FDE")),
         IE = ifelse(Specification == "data", NA, ifelse(grepl("TIE", Specification), "TIE", "FIE")),
         break_year = as.numeric(gsub("[^0-9]", "\\1", Specification))) %>% 
  select(Specification, break_year, PG, SR, DE, IE, everything())

## PGSR

country = "France"

## Loop on country

decomp_temp = decomp %>% 
  subset(DE %in% c("TDE", NA) & IE %in% c("TIE", NA) & Country == country) %>% 
  mutate(Spe_SRPG = interaction(PG, SR) %>% factor(., levels = rev(levels(.))))

# Limits on graph
ymin = decomp_temp %>% pull("theta") %>% min()
ymax = decomp_temp %>% pull("theta") %>% max()

# Graph 1 : DATA
decomp_temp %>% 
  subset(Specification == "data") %>% 
  PGSR_graph()

# Graph 2 : DATA + MODEL (RED)
decomp_temp %>% 
  subset(Specification == "data" | Spe_SRPG == "TPG.TSR") %>%
  PGSR_graph()

# GRAPH 3 : DATA + MODEL (RED/GREEN)
decomp_temp %>% 
  subset(Specification == "data" | Spe_SRPG %in% c("TPG.TSR","FPG.TSR")) %>% 
  PGSR_graph()


# GRAPH 3 : DATA + MODEL (RED/BLUE)
decomp_temp %>% 
  subset(Specification == "data" | Spe_SRPG %in% c("TPG.TSR","TPG.FSR")) %>% 
  PGSR_graph()

# GRAPH 4 : DATA + MODEL (RED/GREEN/BLUE)
decomp_temp %>% 
  subset(Specification == "data" | Spe_SRPG %in% c("TPG.TSR", "FPG.TSR", "TPG.FSR")) %>% 
  PGSR_graph()


# GRAPH 5 : DATA + MODEL (ALL)
decomp_temp %>% 
  subset(Specification == "data" | Spe_SRPG %in% c("TPG.TSR", "FPG.TSR", "TPG.FSR", "FPG.FSR")) %>% 
  PGSR_graph()


## Function to produce counterfactual graphs
PGSR_graph = function(data_decomp){
  
  col_palette = brewer.pal(8, "Set1")[c(1,3,2,4)]
  if(levels(as.factor(as.character(data_decomp$Spe_SRPG)))[1] == "TPG.FSR"){
    col_palette = brewer.pal(8, "Set1")[c(1,2,4)]
  }

  ggplot(data = data_decomp, aes(x = Year, y = theta, color = Spe_SRPG, linetype = from)) +
    geom_line(size = 0.5) +
    geom_label_repel(data = data_decomp %>% subset(Year == 2080) , aes(label = round(theta, 3)),
                     nudge_x = 5, na.rm = TRUE, segment.color = "transparent") +
    scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years, lim = c(NA, 2090)) +
    scale_y_continuous(lim = c(ymin, ymax)) +
    scale_linetype_manual(name = "From",
                          breaks = c("data", "sim"),
                          labels = c("Data", "Model"),
                          values = c("solid", "dashed")) +
    scale_color_manual(name = "Specification",
                       breaks = c("TPG.TSR", "FPG.TSR",
                                  "TPG.FSR", "FPG.FSR"),
                       labels = c("Benchmark", "Constant Pop. Growth",
                                  "Constant Survival Rate", "Constant Dep. Ratio"),
                       values = brewer.pal(8, "Set1")[c(1,3,2,4)],
                       na.value = "black") +
    theme_classic(base_size = 14) +
    guides(color = guide_legend(order = 0), linetype = guide_legend(order = 1)) +
    theme(legend.direction = "vertical", legend.box = "horizontal", 
          legend.position = c(0.5,1), legend.justification = c(0, 1)) +
    labs(x = "Year", y = "Labor income share")
    
}




  ggplot(aes(x = Year,
             y = theta,
             color = interaction(PG, SR) %>%
               factor(., levels = rev(levels(.))),
             linetype = from)) +
  geom_line(size = 0.5) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years, lim = c(NA, 2090)) +
  scale_y_continuous(lim = c(ymin, ymax)) +
  scale_linetype_manual(name = "From",
                        breaks = c("data", "sim"),
                        labels = c("Data", "Model"),
                        values = c("solid", "dashed")) +
  scale_color_manual(name = "Specification",
                     breaks = c("TPG.TSR", "FPG.TSR",
                                "TPG.TSR", "FPG.FSR"),
                     labels = c("Benchmark", "Constant Pop. Growth",
                                "Constant Survival Rate", "Constant Dep. Ratio"),
                     values = brewer.pal(8, "Set1")[c(1,3,2,4)],
                     na.value = "black") +
  theme_classic(base_size = 14) +
  guides(color = guide_legend(order = 0), linetype = guide_legend(order = 1)) +
  theme(legend.direction = "vertical", legend.box = "horizontal", 
        legend.position = c(0.5,1), legend.justification = c(0, 1)) +
  labs(x = "Year", y = "Labor income share")
  
  
  

# TEMPLATE
  
  ggplot(aes(x = Year, y = theta, color = interaction(PG, SR), linetype = from)) +
  geom_line(size = 0.5) +
  geom_label_repel(aes(label = round(theta, 3)),
                   data = subset(decomp, Country == country & Year == 2080 &
                                   DE == "TDE" & IE == "TIE"), 
                   nudge_x = 5, na.rm = TRUE,
                   segment.color = "transparent") +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years, lim = c(NA, 2090)) +
  scale_linetype_manual(name = "From",
                        breaks = c("data", "sim"),
                        labels = c("Data", "Model"),
                        values = c("solid", "dashed")) +
  scale_color_manual(name = "Specification",
                     breaks = c("TPG.TSR", "TPG.FSR",
                                "FPG.TSR", "FPG.FSR"),
                     labels = c("TPG & TSR", "FPG & TSR",
                                "TPG & FSR", "FPG & FSR"),
                     values = brewer.pal(8, "Set1")[c(4,3,2,1)],
                     na.value = "black") +
  theme_classic(base_size = 14) +
  guides(color = guide_legend(order = 0), linetype = guide_legend(order = 1)) +
  theme(legend.position = "right", legend.direction = "vertical") +
  labs(x = "Year", y = "Labor income share")
  ggsave(file.path(loc_counter, "PGSR_France"), width = 1920/1080*5, height = 5)
