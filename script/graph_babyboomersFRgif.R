# Define required packages
require(purrr)
require(magick)
require(dplyr)
require(reshape2)
require(ggplot2)
require(RColorBrewer)

# Define paths
loc_final = file.path("data", "final")
loc_result = file.path("result")
loc_result_agestruct = file.path(loc_result, "agestruct")

# Define countries to plot
country = "France"

# Load data
esaun = read.csv(file.path(loc_final, "esaun.csv"), header = TRUE)

# Load only demographic data
pop = esaun %>% 
  select("Country", "Year", starts_with("X")) %>% 
  subset(Country %in% country & Year %in% c(1950:2020))

# Rename AgeGroups
names(pop) = names(pop) %>% gsub("X", "", .) %>% gsub("\\.", "-", .) %>% gsub("80-", "80+", .)

# Melt data
pop = pop %>% melt(id.vars = c("Country", "Year"), variable.name = "AgeGrp", value.name = "PopTotal")

## AgeGrp bounds
# Min of the AgeGrp
pop$AgeGrp_min = as.numeric(substr(as.character(pop$AgeGrp), 1, 2))
# Max of the AgeGrp
pop$AgeGrp_max = as.numeric(substr(as.character(pop$AgeGrp), 4, 5))

## AgeGrp birth date bounds
# Min birth
pop$Born_min = pop$Year - pop$AgeGrp_min
# Max birth
pop$Born_max = pop$Year - pop$AgeGrp_max

# Graphic parameter : max PopTotal for each country
max_pop = pop %>%
  group_by(Country) %>%
  summarise(Year = Year[PopTotal == max(PopTotal)], 
            AgeGrp = AgeGrp[PopTotal == max(PopTotal)], 
            PopTotal_max = max(PopTotal))

##### FRANCE #####

# Define baby boomers cohort : those who are born between 1945 and 1965
bb_core = c(1946:1965)

# Create a dummy variable if the AgeGroup corresponds to baby boomers
pop$bb = ifelse(pop$Born_min %in% bb_core | pop$Born_max %in% bb_core, "BabyBoomer", "Standard") %>% 
  as.factor()

# Generate graphs
for(year in c(1950:2020)){
  
  pop %>% 
    subset(Year == year) %>% 
    
    ggplot(aes(x = AgeGrp, y = PopTotal, fill = bb)) +
    geom_col(size = 0.8, color = "black", alpha = 0.5) +
    scale_y_continuous(limits = c(0,max_pop$PopTotal_max[max_pop$Country == country])) +
    scale_fill_manual(name = "",
                      breaks = c("BabyBoomer", "Standard"),
                      labels = c("Baby Boomers", "Standard Cohorts"),
                      values = brewer.pal(8, "Set1")[c(1,2)],
                      na.value = "black") +
    labs(x = "Age group", y = "Population (thousands)") +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(size = 22)) +
    theme(legend.direction = "horizontal", legend.box = "horizontal", legend.position = "none") +
    ggtitle(paste(country, "in", year)) +
    ggsave(file.path(loc_result_agestruct, country, paste0(year,".png")), width = 1920/1080*5, height = 5)
}

## Animation

# List all graphs
input_gif = list.files(file.path(loc_result_agestruct, country), pattern = "*.png", full.names = TRUE)

# Animation with all graphs // 4 images per second
list.files(file.path(loc_result_agestruct, country), pattern = "*.png", full.names = TRUE) %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 4) %>% 
  image_write(file.path(loc_result, paste0("agestruct_1y_", country, ".gif")))

# Animation with graphs each 5 years from 1950 to 2020
input_gif[input_gif %>% grep(paste(seq(1950, 2020, 5), collapse = "|"), .)] %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(file.path(loc_result, paste0("agestruct_5y_", country, ".gif")))



