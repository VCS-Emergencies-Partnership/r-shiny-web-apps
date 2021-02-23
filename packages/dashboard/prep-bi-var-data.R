# - prep bivariate data fro map --
library('pals')

# https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/#create-a-bivariate-choropleth

# --- read in vulnerablity indices ---
# # --- local authority level ---
LA_vi <- read_csv('https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/output/vulnerability-LA.csv')
LA_vi <- LA_vi %>% rename('LAD19CD'=Code)
# Resilience
LA_res <- read_csv('https://github.com/britishredcrosssociety/resilience-index/raw/main/data/processed/resilience%20index.csv')



theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = default_font_family,
                          color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = default_font_color),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = default_font_color,
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}




# create 3 buckets for vulnerability
quantiles_vuln <- LA_vi %>%
  pull(`Vulnerability quintile`) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for resilience
quantiles_res <- LA_res %>%
  pull(`Capacity quintile`) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons
# bivariate_color_scale <- tibble(
#   "3 - 3" = "#3F2949", # high inequality, high income
#   "2 - 3" = "#435786",
#   "1 - 3" = "#4885C1", # low inequality, high income
#   "3 - 2" = "#77324C",
#   "2 - 2" = "#806A8A", # medium inequality, medium income
#   "1 - 2" = "#89A1C8",
#   "3 - 1" = "#AE3A4E", # high inequality, low income
#   "2 - 1" = "#BC7C8F",
#   "1 - 1" = "#CABED0" # low inequality, low income
# ) %>%
#   gather("group", "fill")

bivariate_color_scale <- tibble(
  "3 - 3" = "#f3f3f3", # high inequality, high income
  "2 - 3" = "#b4d3e1",
  "1 - 3" = "#509dc2", # low inequality, high income
  "3 - 2" = "#f3e6b3",
  "2 - 2" = "#b3b3b3", # medium inequality, medium income
  "1 - 2" = "#376387",
  "3 - 1" = "#f3b300", # high inequality, low income
  "2 - 1" = "#b36600",
  "1 - 1" = "#000000" # low inequality, low income
) %>%
  gather("group", "fill")


bivariate_color_scale <- tibble(
  "3 - 3" = "#000000", # high inequality, high income
  "2 - 3" = "#b36600",
  "1 - 3" = "#f3b300", # low inequality, high income
  "3 - 2" = "#376387",
  "2 - 2" = "#b3b3b3", # medium inequality, medium income
  "1 - 2" = "#f3e6b3",
  "3 - 1" = "#509dc2", # high inequality, low income
  "2 - 1" = "#b4d3e1",
  "1 - 1" = "#f3f3f3" # low inequality, low income
) %>%
  gather("group", "fill")


glimpse(bivariate_color_scale)
quantiles_res

# cut into groups defined above and join fill
LA_res %<>%
  mutate(
    vuln_quantiles = cut(
      `Vulnerability quintile`,
      breaks = quantiles_vuln,
      include.lowest = TRUE
    ),
    res_quantiles = cut(
      `Capacity quintile`,
      breaks = quantiles_res,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(vuln_quantiles), "-",
      as.numeric(res_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group")

#write_csv(LA_res, 'data/resilience_index_bivar.csv')
write_feather(LA_res, '/home/izzy-everall/r-shiny-web-apps/packages/dashboard/data/resilience_index_bivar.feather')

# -- create chart --

annotations <- tibble(
  label = c(
    "Grey areas mean\nlow income and\nlow inequality",
    "Blue areas mean\nhigh income and\nlow inequality",
    "Violet areas mean\nhigh income and\nhigh inequality",
    "Red areas mean\nlow income and\nhigh inequality"
  ),
  arrow_from = c(
    "548921,232972", # grey
    "771356,238335", # blue
    "781136,125067", # violet
    "616348,81869" # red
  ),
  arrow_to = c(
    "622435,206784", # grey
    "712671,261998", # blue
    "786229,149597", # violet
    "602334,122674" # red
  ),
  curvature = c(
    0.2, # grey
    0.1, # blue
    -0.1, # violet
    -0.2 # red
  ),
  nudge = c(
    "-3000,0", # grey
    "3000,5000", # blue
    "0,-5000", # violet
    "3000,0" # red
  ),
  just = c(
    "1,0", # grey
    "0,1", # blue
    "0.5,1", # violet
    "0,1" # red
  )
) %>%
  separate(arrow_from, into = c("x", "y")) %>%
  separate(arrow_to, into = c("xend", "yend")) %>%
  separate(nudge, into = c("nudge_x", "nudge_y"), sep = "\\,") %>%
  separate(just, into = c("hjust", "vjust"), sep = "\\,")


# separate the groups
test <- bivariate_color_scale %>%
  separate(group, into = c('vuln_quantiles', 'res_quantiles'), sep = " - ") %>%
  mutate(vuln = as.integer(vuln_quantiles),
         res = as.integer(res_quantiles))

legend <- ggplot() +
  geom_tile(
    data = test,
    mapping = aes(
      x = vuln,
      y = res,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Higher Vulnerability  ⟶️",
       y = "Higher Resilience ⟶️") +
  #theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 12)
  ) +
  # quadratic tiles
  coord_fixed()

legend


d<-expand.grid(x=1:3,y=1:3)
#dlabel<-data.frame(x=1:3,xlabel=c("X low", "X middle","X High"))
d<-merge(d,data.frame(x=1:3,xlabel=c("X low", "X middle","X high")),by="x")
d<-merge(d,data.frame(y=1:3,ylabel=c("Y low", "Y middle","Y high")),by="y")


geom.text.size = 7
theme.size = (14/5) * geom.text.size

g.legend<-
  ggplot(test, aes(vuln,res,fill=fill))+
  geom_tile()+
  scale_fill_identity() +
  scale_x_discrete(position = "top") +
  #geom_text(alpha=1)+
  theme_void()+
  theme(legend.position="none",
        panel.background=element_blank(),
        plot.margin=margin(t=10,b=10,l=10))+
  labs(x="Higher vulnerability", y=" Higher capacity")+
  theme(axis.title=element_text(color="black", size=12), axis.title.y= element_text(angle=90, vjust=-8, hjust=0.5)) +
  coord_fixed() +
  annotate("segment", x=0.5, y=3.75, xend=3.5, yend=3.75,
           col="black", arrow=arrow(length=unit(0.3, "cm"))) +
  annotate("segment", x=0.1, y=3.5, xend=0.1, yend=0.5,
           col="black", arrow=arrow(length=unit(0.3, "cm"))) #+
  #geom_text(data=test, aes(x=0, y=2, label="Higher capacity"), color="gray30", angle=90, vjust=0.1, size=4.233333333) # size here is 4.233 mm - have to find equivalent 
  #annotate()
  # Draw some arrows:
#  geom_segment(aes(x=3, xend = 3.5 , y=0, yend = 0), size=0.4,
#               arrow = arrow(length = unit(0.3,"cm"))) +
#  geom_segment(aes(x=0, xend = 0 , y=1, yend = 0.5), size=0.4,
#               arrow = arrow(length = unit(0.3,"cm"))) 
g.legend

brewer.seqseq2()
