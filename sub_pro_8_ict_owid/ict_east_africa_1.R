# ICT in East Africa

# Load the required libraries and packages

# install.packages()
# library()

library(tidyverse)
library(janitor)
library(ggrepel)
library(ggthemes)

# Load the required datasets

# a) ICT adoption per 100 people

ict_per_100 <- read_csv("sub_pro_16_ict/datasets/ict-adoption-per-100-people.csv")

# b) Share of individuals using the internet

share_net <- read_csv("sub_pro_16_ict/datasets/share-of-individuals-using-the-internet.csv")

# Clean the datasets

ict_per_100_clean <- ict_per_100 %>%
  clean_names()

share_net_clean <- share_net %>%
  clean_names()

# Only include East African Community

eac <- c("Kenya", "Tanzania", 
         "Uganda", "Burundi", 
         "Rwanda", "South Sudan",
         "Democratic Republic of Congo", "Somalia")

ict_per_100_clean_eac <- ict_per_100_clean %>%
  rename("country" = "entity") %>%
  filter(country %in% eac)

share_net_clean_eac <- share_net_clean %>%
  rename("country" = "entity") %>%
  filter(country %in% eac)

# EDA plots

# 1) Number of landlines over time in EAC

ict_per_100_clean_eac_label_1 <- ict_per_100_clean_eac %>%
  filter(country %in% c("Kenya", "Tanzania")) %>%
  group_by(country) %>%
  filter(year == 1980) 

ict_per_100_clean_eac_label_2 <- ict_per_100_clean_eac %>%
  filter(country %in% c("Uganda", "South Sudan")) %>%
  group_by(country) %>%
  filter(year == 2017) 

ict_per_100_clean_eac_label_3 <- ict_per_100_clean_eac %>%
  filter(country %in% c("Somalia", "Burundi")) %>%
  group_by(country) %>%
  filter(year == 2013)

ict_per_100_clean_eac_label_4 <- ict_per_100_clean_eac %>%
  filter(country %in% c("Rwanda", "Democratic Republic of Congo")) %>%
  group_by(country) %>%
  filter(year == 2000)

ict_per_100_clean_eac %>%
  ggplot(aes(x = year, 
             y = fixed_telephone_subscriptions_per_100_people, 
             color = country)) + 
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Number of fixed telephone\nline subscriptions (per 100 people) ",
       title = "No place for landlines",
       subtitle = "At its peak, there was only about 1 landline for every 100 people in East Africa",
       caption = "Data Source: Our World in Data\nBy @afro_dataviz") +
  geom_label_repel(data = ict_per_100_clean_eac_label_1, 
                   aes(label = country), 
                   nudge_x = 0.1,
                   nudge_y = 0.1,
                   size = 7) +
  geom_label_repel(data = ict_per_100_clean_eac_label_2, 
                   aes(label = country), 
                   nudge_x = 0.1,
                   nudge_y = 0.1,
                   size = 7) +
  geom_label_repel(data = ict_per_100_clean_eac_label_3, 
                   aes(label = country), 
                   nudge_x = 0.1,
                   nudge_y = 0.1,
                   size = 7) +
  geom_label_repel(data = ict_per_100_clean_eac_label_4, 
                   aes(label = country), 
                   nudge_x = 0.1,
                   nudge_y = -0.1,
                   size = 7) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold", colour = "#000000"),
        axis.text.y = element_text(size = 24, face = "bold", colour = "#000000"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 20, hjust = 0.5),
        #plot.subtitle.position = "plot",
        plot.caption = element_text(family = "Helvetica",size = 20, face = "bold", hjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "none") 

# ggsave("sub_pro_16_ict/images/landlines_eac.png", width = 12, height = 12, dpi = 300)

# 2) Individuals using the internet as a percent of the population (2000 - 2020) in EAC

ict_per_100_clean_eac_label_5 <- ict_per_100_clean_eac %>%
  filter(country %in% c("Kenya", "Tanzania", "Uganda", "Burundi", "Democratic Republic of Congo")) %>%
  group_by(country) %>%
  filter(year == 2020) 

ict_per_100_clean_eac_label_6 <- ict_per_100_clean_eac %>%
  filter(country %in% c("Rwanda", "South Sudan", "Somalia")) %>%
  group_by(country) %>%
  filter(year == 2013) 

ict_per_100_clean_eac %>%
  filter(year>=2000 & year <= 2020) %>%
  ggplot(aes(x = year, 
             y = individuals_using_the_internet_percent_of_population, 
             color = country)) + 
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Share of the population\nusing the internet (%)",
       title = "How many East Africans are online?",
       subtitle = "The percentage of East Africans using the internet is generally increasing",
       caption = "Data Source: Our World in Data\nBy @afro_dataviz") +
  geom_label_repel(data = ict_per_100_clean_eac_label_5, 
                   aes(label = country), 
                   nudge_x = 0.5,
                   nudge_y = 0.5,
                   size = 7) +
  geom_label_repel(data = ict_per_100_clean_eac_label_6, 
                   aes(label = country), 
                   nudge_x = 0.5,
                   nudge_y = 0.5,
                   size = 7) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold", colour = "#000000"),
        axis.text.y = element_text(size = 24, face = "bold", colour = "#000000"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 20, hjust = 0.5),
        #plot.subtitle.position = "plot",
        plot.caption = element_text(family = "Helvetica",size = 20, face = "bold", hjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "none") 

# ggsave("sub_pro_16_ict/images/internet_percent_eac.png", width = 12, height = 12, dpi = 300)

# 3) Mobile phone subscriptions per 100 people (2000 - 2020) in EAC

ict_per_100_clean_eac_label_7 <- ict_per_100_clean_eac %>%
  filter(country %in% c("Kenya", "Tanzania", "Uganda", "Burundi", "Democratic Republic of Congo")) %>%
  group_by(country) %>%
  filter(year == 2020) 

ict_per_100_clean_eac_label_8 <- ict_per_100_clean_eac %>%
  filter(country %in% c("Rwanda", "South Sudan", "Somalia")) %>%
  group_by(country) %>%
  filter(year == 2012) 

ict_per_100_clean_eac %>%
  filter(year>=2000 & year <= 2020) %>%
  ggplot(aes(x = year, 
             y = mobile_cellular_subscriptions_per_100_people, 
             color = country)) + 
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 100, size = 1, linetype = "dashed") +
  annotate("text", x = 2005, y = 102, size = 8, face = "bold", label = "Cellular subscriptions:Population = 1:1") +
  labs(x = "Year",
       y = "Number of cellular and mobile\nsubscriptions (per 100 people)",
       title = "Mobile phones in East Africa",
       subtitle = "In Kenya, there are more cellular subscriptions than the total population",
       caption = "Data Source: Our World in Data\nBy @afro_dataviz") +
  geom_label_repel(data = ict_per_100_clean_eac_label_7, 
                   aes(label = country), 
                   nudge_x = 0.5,
                   nudge_y = 0.5,
                   size = 7) +
  geom_label_repel(data = ict_per_100_clean_eac_label_8, 
                   aes(label = country), 
                   nudge_x = 0.5,
                   nudge_y = 0.5,
                   size = 7) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold", colour = "#000000"),
        axis.text.y = element_text(size = 24, face = "bold", colour = "#000000"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 20, hjust = 0.5),
        plot.subtitle.position = "plot",
        plot.caption = element_text(family = "Helvetica",size = 20, face = "bold", hjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "none") 

# ggsave("sub_pro_16_ict/images/cellphone_subs_eac.png", width = 12, height = 12, dpi = 300)

# 4) Number of fixed vs mobile in SS Africa

ict_per_100_clean_ssa <- ict_per_100_clean %>%
  rename("country" = "entity") %>%
  filter(country == "Sub-Saharan Africa (WB)") %>%
  select(!c(code, individuals_using_the_internet_percent_of_population)) 

ict_per_100_clean_ssa_long <- ict_per_100_clean_ssa %>%
  pivot_longer(!c(country, year), names_to = "connection_type", values_to = "numbers_per_100")

# Label

ict_per_100_clean_ssa_long_label_5 <- ict_per_100_clean_ssa_long %>%
  group_by(connection_type) %>%
  filter(year == 2015) 

ict_per_100_clean_ssa_long_label_5[ict_per_100_clean_ssa_long_label_5 == "fixed_telephone_subscriptions_per_100_people"] <- "Fixed Telephone"
ict_per_100_clean_ssa_long_label_5[ict_per_100_clean_ssa_long_label_5 == "fixed_broadband_subscriptions_per_100_people"] <- "Fixed Broadband"
ict_per_100_clean_ssa_long_label_5[ict_per_100_clean_ssa_long_label_5 == "mobile_cellular_subscriptions_per_100_people"] <- "Mobile"

ict_per_100_clean_ssa_long %>%
  filter(year>=2000 & year <= 2021) %>%
  ggplot(aes(x = year, 
             y = numbers_per_100, 
             color = connection_type)) + 
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Number of users (per 100 people)",
       title = "Cellular dominates in Sub-Saharan Africa",
       subtitle = "Fixed line and Broadband lag significantly behind ",
       caption = "Data Source: Our World in Data\nBy @afro_dataviz") +
  #scale_fill_manual(values = c("darkred", "gold", "navy","darkred", "gold", "navy")) +
  scale_color_manual(values = c("darkred", "navy", "darkred", "navy", "gold3", "gold3")) +# figure out what the order does
  theme_classic() +
  geom_label_repel(data = ict_per_100_clean_ssa_long_label_5,
                   aes(label = connection_type), 
                   nudge_x = 0.5,
                   nudge_y = 0.5,
                   size = 7) +
  theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold", colour = "#000000"),
        axis.text.y = element_text(size = 24, face = "bold", colour = "#000000"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 20, hjust = 0.5),
        plot.subtitle.position = "plot",
        plot.caption = element_text(family = "Helvetica",size = 20, face = "bold", hjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "none") 


# ggsave("sub_pro_16_ict/images/communication_ssa.png", width = 12, height = 12, dpi = 300)



