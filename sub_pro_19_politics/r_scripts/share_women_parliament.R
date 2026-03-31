# Share of Women in Parliament

# A) Load the required libraries and set up data

# Load libraries
library(tidyverse)
library(janitor)
library(viridis)
#library(hrbrthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# Also ensure that rnatural hi res is installed
library(ggrepel)
library(scales)
library(gghighlight)

# Load data
#share_women_parliament <- read_csv("https://ourworldindata.org/grapher/share-of-women-in-parliament.csv?v=1&csvType=full&useColumnShortNames=true")

# Save data
#write_csv(share_women_parliament, "sub_pro_19_politics/datasets/share_women_parliament.csv")

# Load data again
share_women_parliament <- read_csv("sub_pro_19_politics/datasets/share_women_parliament.csv")

# Clean the data

select_countries <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                      "Cuba", "Dominica", "Dominican Republic", "Grenada", "Guyana",
                      "Haiti", "Jamaica", "Saint Kitts and Nevis",
                      "Saint Lucia", "Saint Vincent and the Grenadines",
                      "Suriname", "Trinidad and Tobago")

share_women_parliament_select <- share_women_parliament %>%
  clean_names() %>%
  filter(entity %in% select_countries)
  

# B) EDA and Basic Plot

share_women_parliament_select |> 
  filter(year == 2020) |>
  arrange(desc(wom_parl_vdem_estimate_best)) |>
  ggplot(aes(x=reorder(entity, wom_parl_vdem_estimate_best), y = wom_parl_vdem_estimate_best, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_color_brewer(palette = "Set3") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 33.3, linetype = "dashed") +
  annotate("text", x=5, y=32, label="One-Third Share", size = 7.5, angle=90) +
  labs(x = "Country",
       y = "Share of women in parliament (%, 2020)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        panel.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_7_politics/images/share_women_parliament_2020.png", width = 12, height = 12, dpi = 300)

