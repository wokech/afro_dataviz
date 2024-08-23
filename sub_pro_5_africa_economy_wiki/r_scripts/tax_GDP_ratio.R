# An analysis of tax revenue to GDP ratio
# Tax as % of GDP (2020)

# (A) Load the required libraries

library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)

# # (B) Get the data from Wikipedia
# 
# link <- "https://en.wikipedia.org/wiki/List_of_sovereign_states_by_tax_revenue_to_GDP_ratio"
# tax_GDP_world <- link %>%
#   read_html("[class='wikitable sortable']") %>% 
#   html_table(fill = TRUE)
# 
# tax_GDP_world_1 <- tax_GDP_world[[6]]
# 
# tax_GDP_africa <- tax_GDP_world_1 |>
#   select(Continent, Country, `Taxrevenue(% of GDP)`) |>
#   filter(Continent == "Africa") |>
#   clean_names() |>
#   rename(tax_gdp = taxrevenue_percent_of_gdp) |>
#   filter(tax_gdp != "N/A") |>
#   mutate(tax_gdp = as.numeric(tax_gdp))
# 
# # Save datasets
# 
# write.csv(tax_GDP_africa, "sub_pro_5_africa_economy_wiki/processed_datasets/tax_gdp")

tax_GDP_africa <- read_csv("sub_pro_5_africa_economy_wiki/processed_datasets/tax_gdp")

# (E) Plots

## 1) A bar plot demonstrating the tax/GDP ratio 

ggplot(tax_GDP_africa, aes(reorder(country, +tax_gdp), tax_gdp, fill = country)) +
  geom_bar(stat = "identity") + coord_flip() +
  gghighlight(max(tax_gdp) > 25) + 
  geom_hline(yintercept = 25, linetype = "dashed") +
  annotate("text", x=15, y=24.5, label="Tax:GDP ratio = 25%", size = 5, angle=90) +
  scale_fill_brewer(palette="GnBu") +
  labs(x = "Country",
       y = "Tax:GDP Ratio (%)",
       title = "Seven African countries have a Tax:GDP ratio\ngreater than 25%",
       subtitle = "",
       caption = "Data Source: Wikipedia") +
  theme_classic() +
  scale_y_continuous(labels = comma,
                     breaks = seq(0, 100, by = 10),
                     minor_breaks = seq(0, 100, by = 5)) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 25, hjust = 0.5, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 20),
        plot.caption = element_text(family = "Helvetica",size = 15, face = "bold", hjust = -0.25, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.position = "none") 
