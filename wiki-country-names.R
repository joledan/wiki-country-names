# title: wiki-country-names
# date: september 5, 2023
# by: jan oledan
# desc: scrape list of geographic country name changes from wiki

##### install and load packages #####
rm(list=ls())
# Package names
packages <- c("sf","readr","tidyr","dplyr","ggplot2",
              "stringr","magrittr", "ggrepel",
              "sysfonts","extrafont", "readxl", "cowplot",
              "patchwork", "Cairo", "lubridate", "ragg",
              "camcorder", "devtools", "janitor", "rvest")

# install packages not yet installed
install_package <- packages %in% rownames(installed.packages())
if (any(install_package == FALSE)) {
  install.packages(packages[!install_package])
}
invisible(lapply(packages, library, character.only = TRUE))

##### set up directories ######
main <- case_when(Sys.getenv("USERNAME") == "jgole" ~ "C:/Users/jgole/Dropbox/Portfolio",
                  Sys.getenv("USER") == "janoledan" ~ "/Users/janoledan/Dropbox/Portfolio")
plots <- paste(main, "wiki-name-change", sep = "/")

### link to geographic name changes
link <- "https://en.wikipedia.org/wiki/Geographical_renaming"

# define decade floor function
floor_decade = function(value){ return(value - value %% 10) }

# camcorder to set up and output plots
camcorder::gg_record(
  dir = plots,
  device = "png",
  scale = 1,
  width = 6,
  height = 3.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

### read list of geographic name changes
# clean data set
df <- read_html(link) %>% 
  html_elements(xpath = '//ul/li') %>% 
  html_text() %>%
  as.data.frame() %>%
  mutate(row_number = row_number()) %>%
  filter(between(row_number, 98, 165)) %>%
  rename(name = ".") %>%
  separate_longer_delim(name, delim= "→") %>%
  group_by(row_number) %>%
  mutate(order = row_number(),
         name = str_trim(name)) %>%
  ungroup() %>%
  separate_wider_delim(name, 
                       names = c("name", "year"), 
                       delim = "(",
                       too_few = "align_start") %>%
  mutate(year = as.numeric(str_sub(year, 1, 4)),
         decade = floor_decade(year),
         group = case_when(
           decade < 1900 ~ NA, # drop those before 1900s
           T ~ as.character(decade)
         )) %>%
  filter(!is.na(group)) 

### define arrow tibble, fontsize for annotated labels, and x-axis decade labels
arrows <- 
  tibble(
    x1 = c("1930", "1990"),
    x2 = c("1940", "1990"),
    y1 = c(14, 14.9), 
    y2 = c(12.2, 13.2)
  )
fontsize <- 6/.pt
decade_labels <- c('1900', '10', '20',
               '30', '40', '1950', '60',
               '70', "80", '90', '2000',
               '10', '20')


### bar plot
ggplot() +
  geom_bar(data=df,
           aes(x=group),
           fill="#0072B2") +
  scale_x_discrete(labels = decade_labels, expand = c(0, .75)) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme_minimal(base_family = "Noto Sans") +
  theme(plot.title = element_text(face="bold",
                                  size=12,
                                  margin=margin(t=0, r=0, b=3, l=0, "pt")),
        plot.subtitle = element_text(size=10,
                                     margin=margin(t=0,r=0,b=5,l=0, "pt")),
        plot.margin = unit(c(t=13.5,r=13.5,b=13.5,l=13.5), "pt"),
        plot.caption = element_text(hjust = 0,
                                    size = 6,
                                    color="grey",
                                    margin=margin(t=5,r=0,b=0,l=0, "pt")),
        plot.title.position = "plot",
        plot.caption.position = "plot") +
  theme(panel.border = element_blank(),  # Remove the panel border
        panel.grid.major.x = element_blank(), # remove major x line
        panel.grid.minor.x = element_blank(), # remove minor x, y lines
        panel.grid.minor.y = element_blank()) + 
  theme(axis.line.x = element_line(), 
        axis.title.x = element_blank(), # remove x, y axis title
        axis.title.y = element_blank(),
        axis.ticks.x = element_line(size = .5, colour = "black"),
        axis.text.y = element_text(hjust=0, size=8),
        axis.text.x = element_text(size=8)) + # adjust country names, left align
  guides(color=guide_legend(nrow = 1),
         fill=guide_legend(nrow=1,
                           label.position = "bottom",
                           label.hjust=1)) +
  annotate(geom = "text", x="1990", y = 15.7, label = "Soviet Union \n dissolves",
           size = fontsize,
           lineheight = 0.8,
           family = "Noto Sans",
           color = "black") +
  annotate(geom = "text", x="1920", y = 14, label = "Decolonization of \n Asia and Africa \n begins",
           size = fontsize,
           lineheight = 0.8,
           family = "Noto Sans",
           color = "black") +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(3, "pt")), size = 0.2,
    color = "black", curvature = -.2) +
  labs(title = "That's not my name",
       subtitle = "Number of country name changes by decade, 1900-2020",
       caption = bquote(paste(bold("Source:")~"Wikipedia • ",
                              ~bold("By:")~"Jan Oledan",
                              sep="\n")))


### end of code ###

x <- df %>%
  filter(decade %in% c("1940", "1950", "1960", "1970"))
