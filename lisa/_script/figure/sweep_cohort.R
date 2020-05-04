sweep_cohort = function(){

# Ages for both cohorts
ncds_age = c(0, 7, 11, 16, 23, 33, 37, 42, 47, 50, 55)
ncds_sample = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
ncds_parinc = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)

bcs_age = c(0, 5, 10, 16, 21, 26, 30, 34, 38, 42, 46)
bcs_sample = c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
bcs_parinc = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0)

# Generate data frame
resp = data.frame(year = c(1958 + ncds_age, 1970 + bcs_age),
                  age = c(ncds_age, bcs_age),
                  cohort = c(rep("NCDS", length(ncds_age)), 
                             rep("BCS", length(bcs_age))),
                  y = c(rep(0.8, length(ncds_age)), rep(1, length(bcs_age))),
                  sample = as.factor(c(ncds_sample, bcs_sample)),
                  parinc = as.factor(c(ncds_parinc, bcs_parinc)))

# Predefine colors
colors = brewer.pal(8, "Set1")[c(1,2)]

#### ALL AGES ####
resp %>% 
  
  ggplot(aes(x = year, y = y, color = cohort, 
             label = age, shape = sample)) +
  geom_line() +
  geom_text_repel(direction = "y", nudge_y = -0.05, segment.size = 0.1, force = 0) +
  geom_point(size = 5, fill = "white") +
  annotate("text", x = 1960, y = 1.05, 
           label = "1970 Cohort (BCS)",
           color = colors[[1]], size = 6, hjust = 0) +
  annotate("text", x = 1960, y = 0.85, 
           label = "1958 Cohort (NCDS)",
           color = colors[[2]], size = 6, hjust = 0) +
  scale_x_continuous(breaks = seq(1960, 2015, 10)) +
  scale_y_continuous(limits = c(0.7, 1.05)) +
  scale_shape_manual(values = c(16, 21)) +
  scale_color_manual(values = colors) +
  theme_classic(base_size = 14) +
  theme(
    axis.line.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_line(arrow = arrow(angle = 30, length = unit(0.3, "cm"))),
    legend.position = "none"
  )
}
