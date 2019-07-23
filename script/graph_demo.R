# Define required packages
lapply(c("dplyr", "ggplot2", "ggrepel", "RColorBrewer", "reshape2"), require, character.only = TRUE)

# Define paths
loc_final = file.path("data", "final")
loc_result = file.path("result")

# Define countries to plot
country = c("France", "United States")

# Load data
demo = read.csv(file.path(loc_final, "demo.csv"), header = TRUE)

# Plot n, p and dep // Red = France // Blue = United States
demo %>% 
  select("Country", "Year", "n", "p", "dep") %>% 
  subset(Country %in% country & Year %in% seq(1970, 2080, 10)) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, color = Country)) +
    geom_line(size = 0.5) +
    facet_wrap(variable ~ . , ncol = 3, scales = "free",
               labeller = labeller(variable = c(p = "p",
                                                n = "n",
                                                dep = "p/n"))) +
    scale_color_brewer(name = "", palette = "Set1") +
    scale_x_continuous(breaks = seq(1970, 2080, 10),
                       labels = c(1970, "", "", 2000, "", "", 2030, "", "", 2060, "","")) +
    theme_classic(base_size = 14) +
    theme(legend.direction = "vertical", legend.box = "horizontal", legend.position = "none") +
    labs(x = "", y = "") +
    ggsave(file.path(loc_result, "demo_npdep.png"), width = 1920/1080*5, height = 1920/1080*5/3)
