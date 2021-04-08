# Load init script
source("_script/init.R")
require(purrr)
require(magick)

# Define countries to plot
country = "France"

# Load data
esaun = read.csv(file.path(loc_data, "esaun.csv"), header = TRUE)

# Load only demographic data
pop = esaun %>% 
  select("Country", "Year", starts_with("X")) %>% 
  subset(Country %in% country & Year %in% c(1950:2020))

# Rename AgeGroups
names(pop) = names(pop) %>% gsub("X", "", .) %>% gsub("\\.", "-", .) %>% gsub("80-", "80+", .)

# Melt data
pop = pop %>% 
  setDT %>% 
  melt(id.vars = c("Country", "Year"), variable.name = "AgeGrp", value.name = "PopTotal") %>% 
  mutate(
    AgeGrp = AgeGrp %>% 
      factor(labels = c("0-4", "5-9", levels(.)[-c(1,2)]))
  ) %>%
## AgeGrp bounds
  separate(col = "AgeGrp", into = c("AgeGrp_min", "AgeGrp_max"), remove = F) %>% 
  mutate_at(vars(starts_with("AgeGrp_")), as.integer)

# Compute share
pop = pop %>% 
  add_count(Country, Year, wt = PopTotal, name = "Total") %>% 
  mutate(PopShare = PopTotal/Total*100) %>% 
  ungroup

## AgeGrp birth date bounds
# Min birth
pop$Born_min = pop$Year - pop$AgeGrp_min
# Max birth
pop$Born_max = pop$Year - pop$AgeGrp_max

# Graphic parameter : max PopTotal for each country
max_pop = pop %>%
  group_by(Country) %>%
  summarise(PopTotal_Year = Year[PopTotal == max(PopTotal)], 
            PopTotal_AgeGrp = AgeGrp[PopTotal == max(PopTotal)], 
            PopTotal_max = max(PopTotal),
            PopShare_Year = Year[PopShare == max(PopShare)], 
            PopShare_AgeGrp = AgeGrp[PopShare == max(PopShare)], 
            PopShare_max = max(PopShare),
            )

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
    geom_col(size = 1, alpha = .7) +
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
    ggsave(file.path(loc_graphic, "_agestructure", country, "count", paste0(year,".png")), 
           width = 1920/1080*5, height = 5)
  
  pop %>% 
    subset(Year == year) %>% 
    
    ggplot(aes(x = AgeGrp, y = PopShare, fill = bb)) +
    geom_col(size = 1, alpha = .7) +
    scale_y_continuous(limits = c(0, max_pop$PopShare_max[max_pop$Country == country])) +
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
    ggsave(file.path(loc_graphic, "_agestructure", country, "share", paste0(year,".png")), 
           width = 1920/1080*5, height = 5)
}

## Animation

# List all graphs
input_gif = list.files(file.path(loc_graphic, "_agestructure", country, "count"),
                       pattern = "*.png", full.names = TRUE)

# Animation with all graphs // 4 images per second
list.files(file.path(loc_graphic, "_agestructure", country, "count"), 
           pattern = "*.png", full.names = TRUE) %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 4) %>% 
  image_write(file.path(loc_graphic, "_agestructure", paste0("agestruct_count_1y_", country, ".gif")))

# Animation with graphs each 5 years from 1950 to 2020
input_gif[input_gif %>% grep(paste(seq(1950, 2020, 5), collapse = "|"), .)] %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(file.path(loc_graphic, "_agestructure", paste0("agestruct_count_5y_", country, ".gif")))