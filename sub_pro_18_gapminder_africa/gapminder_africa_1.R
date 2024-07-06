# Gapminder Africa

# Type 1

library(tidyverse)

gapminder::gapminder %>%  
  filter(continent == "Africa") %>%  
  filter(year == 2007) %>%  
  select(country, pop) %>% 
  mutate(id = row_number()) ->  
  df_w_id

packcircles::circleProgressiveLayout(df_w_id$pop,  
                                     sizetype = 'area') ->  
  x0y0radius  

x0y0radius %>%  
  packcircles::circleLayoutVertices(npoints = 50) ->  
  circle_outlines  

circle_outlines %>% 
  left_join(df_w_id) %>% 
  ggplot() +  
  aes(x = x, y = y) +  
  geom_polygon(colour = "black", alpha = 0.6) +  
  aes(group = id) +  
  aes(fill = pop) +  
  geom_text(data = cbind(df_w_id, x0y0radius),  
            aes(x, y, size = pop, label = country,  
                group = NULL, fill = NULL)) +  
  theme(legend.position = "none") +  
  coord_equal()

# Type 2

library(ggcirclepack)
library(tidyverse)
library(magrittr)
library(dplyr)

gapminder::gapminder %>%
  filter(year == 2002) %>%
  ggplot() +
  labs(title = "142 gapminder countries in 2002") +
  aes(id = country) +  # req aes
  # default area is 1
  geom_polygon_circlepack(alpha = .5) +
  coord_equal() +
  aes(fill = continent) +
  aes(area = pop) +
  geom_text_circlepack() +
  scale_size_continuous(range = c(0, 4)) +
  facet_wrap(facets = vars(continent)) +
  theme(legend.position = "none") +
  aes(area = gdpPercap*pop) +
  aes(area = gdpPercap)