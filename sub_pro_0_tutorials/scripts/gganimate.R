# R Programming 101
# ggamnimate

# 1) Load the libraries

library(tidyverse)
library(gganimate)
library(babynames)
library(gapminder)
library(viridis)
library(RColorBrewer)

# Keep only 3 names

baby_gif <- babynames %>%
  filter(name %in% c("James", "Paul", "Andrew", "John")) %>%
  filter(sex == "M") %>%
  ggplot(aes(x=year,
             y=n,
             group=name,
             color=name)) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Popularity of American names in the previous generations",
       x = "",
       y = "",
       color = "Names") + 
  theme(plot.title = element_text(size = 10,
                                  color = "steelblue")) +
  transition_reveal(year)

# Save the animation
anim_save("sub_pro_0_tutorials/images/gganimate_1.gif")
