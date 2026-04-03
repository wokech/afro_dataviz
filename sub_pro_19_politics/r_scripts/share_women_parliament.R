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

# List the African countries first

african_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", 
                       "Burundi", "Cape Verde", "Cameroon", "Central African Republic", 
                       "Chad", "Comoros", "Congo", "Democratic Republic of Congo", 
                       "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", 
                       "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", 
                       "Guinea", "Guinea-Bissau", "Cote d'Ivoire", "Kenya", 
                       "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
                       "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", 
                       "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
                       "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
                       "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", 
                       "Uganda", "Zambia", "Zimbabwe")

share_women_parliament_select <- share_women_parliament %>%
  clean_names() %>%
  filter(entity %in% african_countries)

# Check for number of countries selected
unique(share_women_parliament_select$entity)

# B) EDA and Basic Plot

share_women_parliament_select |> 
  filter(year == 2025) |>
  arrange(desc(wom_parl_vdem_estimate_best)) |>
  ggplot(aes(x=reorder(entity, wom_parl_vdem_estimate_best), y = wom_parl_vdem_estimate_best, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 20,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 33.3, linetype = "dashed") +
  annotate("text", x=15, y=32, label="One-Third Share", size = 30, angle=90) +
  labs(x = "Country",
       y = "Share of women in parliament (%, 2025)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 80),
        axis.title.y = element_text(size = 80),
        axis.text.x = element_text(size = 80),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_19_politics/images/share_women_parliament_2025.png", width = 12, height = 12, dpi = 300)


# B) One-Third Plot

share_women_parliament_select |> 
  filter(year == 2025) |>
  arrange(desc(wom_parl_vdem_estimate_best)) |>
  ggplot(aes(x=reorder(entity, wom_parl_vdem_estimate_best), y = wom_parl_vdem_estimate_best, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  gghighlight(wom_parl_vdem_estimate_best > 33.3) +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 20,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 33.3, linetype = "dashed") +
  annotate("text", x=15, y=32, label="One-Third Share", size = 30, angle=90) +
  labs(x = "Country",
       y = "Share of women in parliament (%, 2025)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 80),
        axis.title.y = element_text(size = 80),
        axis.text.x = element_text(size = 80),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_19_politics/images/share_women_parliament_2025_one_third.png", width = 12, height = 12, dpi = 300)

# By region

# Northern Africa

select_northern_africa <- c("Morocco", "Algeria", "Egypt", 
                            "Tunisia", "Libya", "Mauritania")

share_women_parliament_select_northern_africa <- share_women_parliament_select |>
  filter(entity %in% select_northern_africa)

unique(share_women_parliament_select_northern_africa$entity)

share_women_parliament_select_northern_africa |> 
  filter(year == 2025) |>
  arrange(desc(wom_parl_vdem_estimate_best)) |>
  ggplot(aes(x=reorder(entity, wom_parl_vdem_estimate_best), y = wom_parl_vdem_estimate_best, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 30,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 33.3, linetype = "dashed") +
  annotate("text", x=2, y=32, label="One-Third Share", size = 30, angle=90) +
  labs(x = "Country",
       y = "Share of women in parliament (%, 2025)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 80),
        axis.title.y = element_text(size = 80),
        axis.text.x = element_text(size = 80),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_19_politics/images/share_women_parliament_2025_northern_africa.png", width = 12, height = 12, dpi = 300)


# Southern Africa

select_southern_africa <- c("South Africa", "Angola", "Zambia", "Mozambique", 
                            "Madagascar", "Comoros", "Namibia", "Malawi", 
                            "Zimbabwe", "Lesotho", "Botswana", 
                            "Eswatini", "Mauritius", "Seychelles")

share_women_parliament_select_southern_africa <- share_women_parliament_select |>
  filter(entity %in% select_southern_africa)

share_women_parliament_select_southern_africa |> 
  filter(year == 2025) |>
  arrange(desc(wom_parl_vdem_estimate_best)) |>
  ggplot(aes(x=reorder(entity, wom_parl_vdem_estimate_best), y = wom_parl_vdem_estimate_best, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 30,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 33.3, linetype = "dashed") +
  annotate("text", x=5, y=32, label="One-Third Share", size = 30, angle=90) +
  labs(x = "Country",
       y = "Share of women in parliament (%, 2025)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 80),
        axis.title.y = element_text(size = 80),
        axis.text.x = element_text(size = 80),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_19_politics/images/share_women_parliament_2025_southern_africa.png", width = 12, height = 12, dpi = 300)


# Western Africa

select_western_africa <- c("Senegal", "Mali", "Cape Verde", "Guinea", "Gambia", 
                           "Sierra Leone", "Guinea-Bissau", "Liberia", "Nigeria", 
                           "Cote d'Ivoire", "Burkina Faso", "Ghana", 
                           "Benin", "Niger", "Togo")


share_women_parliament_select_western_africa <- share_women_parliament_select |>
  filter(entity %in% select_western_africa)

share_women_parliament_select_western_africa |> 
  filter(year == 2025) |>
  arrange(desc(wom_parl_vdem_estimate_best)) |>
  ggplot(aes(x=reorder(entity, wom_parl_vdem_estimate_best), y = wom_parl_vdem_estimate_best, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 30,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 33.3, linetype = "dashed") +
  annotate("text", x=5, y=32, label="One-Third Share", size = 30, angle=90) +
  labs(x = "Country",
       y = "Share of women in parliament (%, 2025)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 80),
        axis.title.y = element_text(size = 80),
        axis.text.x = element_text(size = 80),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_19_politics/images/share_women_parliament_2025_western_africa.png", width = 12, height = 12, dpi = 300)


# Eastern Africa

select_eastern_africa <- c("Uganda", "Tanzania", "Kenya", "Sudan", "Rwanda", 
                           "Burundi", "Ethiopia", "South Sudan", 
                           "Djibouti", "Somalia")

share_women_parliament_select_eastern_africa <- share_women_parliament_select |>
  filter(entity %in% select_eastern_africa)

select_eastern_africa[!select_eastern_africa %in% share_women_parliament_select_eastern_africa$entity]

share_women_parliament_select_eastern_africa |> 
  filter(year == 2025) |>
  arrange(desc(wom_parl_vdem_estimate_best)) |>
  ggplot(aes(x=reorder(entity, wom_parl_vdem_estimate_best), y = wom_parl_vdem_estimate_best, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 30,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 33.3, linetype = "dashed") +
  annotate("text", x=2, y=32, label="One-Third Share", size = 30, angle=90) +
  labs(x = "Country",
       y = "Share of women in parliament (%, 2025)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 80),
        axis.title.y = element_text(size = 80),
        axis.text.x = element_text(size = 80),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_19_politics/images/share_women_parliament_2025_eastern_africa.png", width = 12, height = 12, dpi = 300)


# Central Africa

select_central_africa <- c("Cameroon", "Democratic Republic of Congo", "Gabon", 
                           "Equatorial Guinea", "Congo", 
                           "Central African Republic", "Chad", 
                           "Sao Tome and Principe")

share_women_parliament_select_central_africa <- share_women_parliament_select |>
  filter(entity %in% select_central_africa)

share_women_parliament_select_central_africa |> 
  filter(year == 2025) |>
  arrange(desc(wom_parl_vdem_estimate_best)) |>
  ggplot(aes(x=reorder(entity, wom_parl_vdem_estimate_best), y = wom_parl_vdem_estimate_best, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 30,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 33.3, linetype = "dashed") +
  annotate("text", x=2, y=32, label="One-Third Share", size = 30, angle=90) +
  labs(x = "Country",
       y = "Share of women in parliament (%, 2025)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 80),
        axis.title.y = element_text(size = 80),
        axis.text.x = element_text(size = 80),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_19_politics/images/share_women_parliament_2025_central_africa.png", width = 12, height = 12, dpi = 300)
