Wildlife Overpass Review
================
Clayton Lamb, Liam Brennan, Emily Chow
24 June, 2022

## Load Data

``` r
library(here)
library(tidyverse)
library(sf)
library(readxl)
library(lubridate)
library(mapview)
library(ggmap)
library(raster)
library(stars)
library(velox)
library(hrbrthemes)
library(RColorBrewer)
library(knitr)
library(basemaps)
library(rworldmap)
library(RStoolbox)
library(ggpubr)
library(cowplot)
library(tidylog)
library(janitor)
library(tidymodels)
options(scipen=999)

##load data
df <- read_xlsx(here::here("data","2022-06-23-Clean-Crossing Structure Literature Review Data_Figures.xlsx"),sheet="1-Global (fill this first)")%>%clean_names()
eff.raw <- read_xlsx(here::here("data","2022-06-23-Clean-Crossing Structure Literature Review Data_Figures.xlsx"), sheet="CL_eff")%>%clean_names()
```

## Map

``` r
##make spatial, keep last check
op <- st_as_sf(df%>%mutate(lat=extract_numeric(lat),
                                                long=extract_numeric(long)),
               coords=c("long", "lat"),
               crs=4326)%>%
    mutate(continent=case_when(country%in%c("U.S.A", "Canada","U.S.A.")~"North America",
                             TRUE~"Europe, Asia, and Oceania"))%>%
  rename(width=estimated_inner_width_m_from_google_earth_in_cases_where_fencing_is_visible_inner_fence_rail_where_visible,
         length=estimated_length_m_headwall_end_to_end_of_physical_structure_often_indicated_by_start_and_stop_of_guard_rail_concreate_metail_edge_of_strcuture,
         lanes=number_of_lanes_spanned,
         year=year_of_build_clean)%>%
  mutate(length=as.numeric(length),
         width=as.numeric(width),
         lanes=as.numeric(lanes))

               
mapview(op)

world_sf <- st_as_sf(getMap(resolution = "low"))%>%
  st_transform_proj(world_sf, crs = "+proj=wintri")

grat_wintri <- 
  st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
  st_transform_proj(crs = "+proj=wintri")

# vectors of latitudes and longitudes that go once around the 
# globe in 1-degree steps
lats <- c(90:-90, -90:90, 90)
longs <- c(rep(c(180, -180), each = 181), 180)

# turn into correctly projected sf collection
wintri_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% 
  st_sf() %>%
  st_transform_proj(crs = "+proj=wintri") # transform to Winkel tripel



ggplot() + 
  geom_sf(data = wintri_outline, fill = "#56B4E950", color = NA) +
  geom_sf(data = grat_wintri, color = "gray10", size = 0.25/.pt, alpha=0.5) + 
  geom_sf(data = world_sf, color = "gray60", size = 0.5/.pt) +
  geom_sf(data = wintri_outline, fill = NA, color = "grey30", size = 0.5/.pt, alpha=0.5) +
  geom_sf(data=op%>%
  st_transform_proj(crs = "+proj=wintri"), size = 2/.pt, color="red")+
  coord_sf(datum = NULL)+
  theme_map()
```

![](README_files/figure-gfm/map-1.png)<!-- -->

``` r
ggsave(here::here("output","world_map.png"), height=4, width=7, unit="in", bg="white")


#prep basemap
register_google("AIzaSyCOwGx2D77XOqRgGhKmcb5F4Kt_S61tCLI")
#set_defaults(map_service = "osm", map_type = "terrain_bg")

##GLOBAL
bb = st_bbox(c(xmin = -185, xmax = 185, ymax = 85, ymin = -85), crs = st_crs(4326))

bb_ll = st_bbox(
  st_transform(
    st_as_sfc(bb), 
    3857
  )
)

a <- basemap_raster(bb_ll, map_type = "terrain_bg")

a <- a%>%projectRaster(crs="+proj=wintri")
writeRaster(a,here::here("data","basemap.tif"), overwrite=TRUE)
a <- brick(here::here("data","basemap.tif"))%>%
  mask(wintri_outline)

world <- getMap(resolution = "high")
world <- st_as_sf(world)

world <-ggRGB(a, r=1, g=2, b=3)+
  geom_sf(data=op%>%
  st_transform_proj(crs = "+proj=wintri"), size = 2/.pt, color="red")+
  coord_sf(datum = NULL)+
  theme_map()

##W-NA
bb = st_bbox(c(xmin = -137, xmax = -93, ymax = 55, ymin = 28), crs = st_crs(4326))

bb_ll = st_bbox(
  st_transform(
    st_as_sfc(bb), 
    3857
  )
)

bb_clip=st_bbox(c(xmin = -12.5E6, xmax = -10.3E6, ymin = 3.4E6, ymax = 6.4E6), crs = "+proj=wintri")

a <- basemap_raster(bb_ll, map_type = "terrain_bg")
a <- a%>%projectRaster(crs="+proj=wintri")%>%
  raster::crop(bb_clip%>%extent)
writeRaster(a,here::here("data","basemap.tif"), overwrite=TRUE)
a <- brick(here::here("data","basemap.tif"))%>%
  mask(wintri_outline)

WNA <- ggRGB(a, r=1, g=2, b=3)+
  geom_sf(data=op%>%filter(continent=="North America")%>%st_transform_proj(crs = "+proj=wintri")%>%st_intersection(bb_clip%>%st_as_sfc),
          size = 5/.pt, color="red")+
  coord_sf(datum = "+proj=wintri")+
  theme_map()



##Europe
bb = st_bbox(c(xmin = 2, xmax = 24, ymax = 58, ymin = 43), crs = st_crs(4326))

bb_ll = st_bbox(
  st_transform(
    st_as_sfc(bb),
    3857
  )
)

bb_clip=st_bbox(c(xmin = 0.2E6, xmax = 2.2E6, ymin = 4.4E6, ymax = 6.1E6), crs = "+proj=wintri")

a <- basemap_raster(bb_ll, map_type = "terrain_bg")
a <- a%>%projectRaster(crs="+proj=wintri")%>%
  raster::crop(bb_clip%>%extent)
writeRaster(a,here::here("data","basemap.tif"), overwrite=TRUE)
a <- brick(here::here("data","basemap.tif"))%>%
  mask(wintri_outline)

EU <- ggRGB(a, r=1, g=2, b=3)+
  geom_sf(data=op%>%filter(continent!="North America")%>%st_transform_proj(crs = "+proj=wintri")%>%st_intersection(bb_clip%>%st_as_sfc),
          size = 5/.pt, color="red")+
  coord_sf(datum = "+proj=wintri")+
  theme_map()


plot_grid(world,plot_grid(WNA, EU, labels = c("B","C"), rel_widths = c(1, 1.9)), labels = c("A",NA),nrow=2, rel_heights = c(1.9,1))
```

![](README_files/figure-gfm/map-2.png)<!-- -->

``` r
ggsave(here::here("output","map.png"), height=7, width=7, unit="in", bg="white")
```

## Plots

``` r
##clean up column names
plot.dat <- op%>%
  tibble%>%
  dplyr::select(continent,
                year,
                `Width (m)`=width,
                `Length (m)`=length,
                lanes)%>%
    mutate(`Width:length ratio`=`Width (m)`/`Length (m)`)%>%
  tidyr::pivot_longer(`Width (m)`:`Width:length ratio`)%>%
  mutate(name=fct_relevel(name,"Length (m)","Width (m)","Width:length ratio", "lanes"))

plot.dat%>%
  filter(!name%in%"lanes")%>%
  ggplot(aes(y=value))+
    geom_boxplot(fill="grey")+
    facet_wrap(vars(name), scales="free_y")+
  theme_ipsum()+
     theme(axis.title.x = element_text(size=17),
          axis.title.y = element_text(size=17),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size=13),
          plot.title = element_text(size=22),
          plot.subtitle = element_text(size=17),
          legend.position = "none")+
  labs(title="Overpass dimensions",y="")
```

![](README_files/figure-gfm/plots-1.png)<!-- -->

``` r
ggsave(here::here("output", "op_dims.png"), height=6, width=7, unit="in",bg="white")

plot.dat%>%
    filter(!name%in%"lanes")%>%
  ggplot(aes(y=value, fill=name))+
    geom_boxplot()+
    facet_grid(name~continent, scales="free_y")+
  theme_ipsum()+
     theme(axis.title.x = element_text(size=17),
          axis.title.y = element_text(size=17),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size=13),
          plot.title = element_text(size=22),
          plot.subtitle = element_text(size=17),
          legend.position = "none")+
  labs(title="Overpass dimensions",y="")
```

![](README_files/figure-gfm/plots-2.png)<!-- -->

``` r
ggsave(here::here("output", "op_dims_continent.png"), height=10, width=7, unit="in",bg="white")


by.year <- plot.dat%>%
      filter(name%in%"Width (m)")%>%
drop_na(year)%>%
  ggplot(aes(y=value, x=`year`))+
  geom_point()+
  theme_ipsum()+
      facet_wrap(vars(continent))+
     theme(axis.title.x = element_text(size=17),
          axis.title.y = element_text(size=17),
          axis.text = element_text(size=13),
          plot.title = element_text(size=22),
          plot.subtitle = element_text(size=17),
          legend.position = "none")+
  labs(title="Width through time",y="Width (m)", x="Year built")

plot.dat%>%
      filter(name%in%"Width (m)")%>%
      drop_na(year)%>%
  lm(value~year+ continent, data=.)%>%
  summary

by.size <- op%>%
drop_na(approx_size)%>%
  mutate(approx_size=fct_relevel(approx_size,"Small (<50 lbs)","Medium (50-350 lbs)","Large (>350 lbs)"))%>%
  ggplot(aes(y=width, x=approx_size))+
    geom_boxplot(fill="grey")+
  theme_ipsum()+
     theme(axis.title.x = element_text(size=17),
          axis.title.y = element_text(size=17),
          axis.text = element_text(size=13),
          plot.title = element_text(size=22),
          plot.subtitle = element_text(size=17),
          legend.position = "none")+
  labs(title="Width varies by target species",y="Width (m)", x="Target species body size")

ggarrange(by.size,by.year, labels="AUTO", ncol=1)
```

![](README_files/figure-gfm/plots-3.png)<!-- -->

``` r
ggsave(here::here("output", "op_dims_bodysize_year.png"), height=10, width=7, unit="in",bg="white")

op%>%
drop_na(approx_size)%>%
  mutate(approx_size=fct_relevel(approx_size,"Small (<50 lbs)","Medium (50-350 lbs)","Large (>350 lbs)"))%>%
  lm(width~approx_size, data=.)%>%
  summary
```

## Summary stats

``` r
plot.dat%>%
  group_by(name) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            n=n())%>%
  kable
```

| name               |       mean |        min |        max |   n |
|:-------------------|-----------:|-----------:|-----------:|----:|
| Length (m)         | 65.1796667 | 21.0700000 | 138.280000 | 120 |
| Width (m)          | 33.9691753 |  3.3200000 |  76.210000 | 120 |
| Width:length ratio |  0.5820743 |  0.0608661 |   2.761843 | 120 |
| lanes              |  3.9459459 |  0.0000000 |   8.000000 | 120 |

``` r
plot.dat%>%
  group_by(continent, name) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            n=n())%>%
  kable
```

| continent                 | name               |       mean |        min |        max |   n |
|:--------------------------|:-------------------|-----------:|-----------:|-----------:|----:|
| Europe, Asia, and Oceania | Length (m)         | 66.6034921 | 21.0700000 | 138.280000 |  91 |
| Europe, Asia, and Oceania | Width (m)          | 34.2773913 |  3.3200000 |  76.210000 |  91 |
| Europe, Asia, and Oceania | Width:length ratio |  0.6024552 |  0.0608661 |   2.761843 |  91 |
| Europe, Asia, and Oceania | lanes              |  4.0121951 |  0.0000000 |   8.000000 |  91 |
| North America             | Length (m)         | 61.8574074 | 29.1200000 | 109.290000 |  29 |
| North America             | Width (m)          | 33.2096429 |  5.6000000 |  64.970000 |  29 |
| North America             | Width:length ratio |  0.5345191 |  0.0886497 |   1.101943 |  29 |
| North America             | lanes              |  3.7586207 |  2.0000000 |   8.000000 |  29 |

``` r
op%>%
  drop_na(year)%>%
  mutate(period=case_when(year<2010~"before",
                          year>=2010~"after"))%>%
  group_by(period) %>%
  summarise(W = mean(width, na.rm = TRUE),
            L=mean(length, na.rm = TRUE),
            n=n())%>%
  kable
```

| period |        W |        L |   n |
|:-------|---------:|---------:|----:|
| after  | 32.73833 | 57.81552 |  35 |
| before | 31.09857 | 60.83682 |  44 |

## Effectiveness

``` r
eff <- eff.raw%>%
    rename(`Width (m)`=estimated_inner_width_m_from_google_earth_in_cases_where_fencing_is_visible_inner_fence_rail_where_visible,
         `Length (m)`=estimated_length_m_headwall_end_to_end_of_physical_structure_often_indicated_by_start_and_stop_of_guard_rail_concreate_metail_edge_of_strcuture,
         )%>%
  mutate(`Length (m)`=as.numeric(`Length (m)`),
         `Width (m)`=as.numeric(`Width (m)`),
         `W:L ratio`=`Width (m)`/`Length (m)`)%>%
  dplyr::select(crossing=name_if_applicable,`Width (m)`,`W:L ratio`,days=approximate_number_of_monitoring_days, total:coyote)%>%
  dplyr::select(-big_horned_sheep)%>% ##not enough structures near sheep range to test
  pivot_longer(total:coyote)%>%
  mutate(rate=(value/days)*30,
         name=str_replace_all(name,"_"," "))
  
###plot
eff%>%
  filter(name%in%"total")%>%
  dplyr::select(crossing, rate, `Width (m)`,`W:L ratio`)%>%
  pivot_longer(`Width (m)`:`W:L ratio`)%>%
  ggplot(aes(y=rate, x=value, fill=name))+
    facet_wrap(vars(name), scales="free_x")+
  theme_ipsum()+
  geom_smooth(method="lm",se=FALSE, color="black", linetype="dashed")+
        geom_point(alpha=0.5)+
     theme(axis.title.x = element_text(size=17),
          axis.title.y = element_text(size=17),
          axis.text.x = element_text(size=17),
          axis.text.y = element_text(size=13),
          plot.title = element_text(size=22),
          plot.subtitle = element_text(size=17),
          legend.position = "none")+
  labs(title="Overpass dimensions",y="Crossings per month",x="")
```

![](README_files/figure-gfm/Effectiveness-1.png)<!-- -->

``` r
ggsave(here::here("output", "op_dims_effectiveness.png"), height=5, width=7, unit="in",bg="white")


###Stats
##total
eff%>%
  filter(name%in%"total")%>%
  dplyr::select(crossing, rate, `Width (m)`,`W:L ratio`)%>%
  pivot_longer(`Width (m)`:`W:L ratio`)%>%
  nest(data = c(-name))%>% 
  mutate(
    fit = map(data, ~ lm(rate ~ value, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  dplyr::select(-data, -fit)%>%
  filter(term=="value")%>%
  mutate_if(is.numeric, round, 5)%>%
  kable
```

| name      | term  | estimate | std.error | statistic | p.value |
|:----------|:------|---------:|----------:|----------:|--------:|
| Width (m) | value |  0.40561 |   0.41584 |   0.97538 | 0.35237 |
| W:L ratio | value | 34.89408 |  29.11922 |   1.19832 | 0.25842 |

``` r
##summarise total across lengths
eff%>%
  dplyr::select(crossing, rate, `Width (m)`)%>%
  mutate(width.bin=case_when(`Width (m)`<=40~"<=40",
                             `Width (m)`>40~">40"))%>%
  drop_na(width.bin)%>%
   lm(rate ~ width.bin, data=.)%>%
  tidy%>%
  kable
```

| term          | estimate | std.error | statistic |  p.value |
|:--------------|---------:|----------:|----------:|---------:|
| (Intercept)   | 4.505227 |  3.520520 |  1.279705 | 0.203610 |
| width.bin\>40 | 6.777833 |  4.343797 |  1.560348 | 0.121837 |

``` r
eff%>%
  dplyr::select(crossing, rate, `Width (m)`)%>%
  mutate(width.bin=case_when(`Width (m)`<=40~"<=40",
                             `Width (m)`>40~">40"))%>%
  drop_na(width.bin)%>%
  group_by(width.bin)%>%
  summarise(mean=mean(rate, na.rm=TRUE),
            se=sd(rate, na.rm=TRUE)/sqrt(n()))

##by species

eff%>%
  filter(!name%in%"total")%>%
  dplyr::select(crossing, rate, `Width (m)`,name)%>%
  ggplot(aes(y=rate, x=`Width (m)`, fill=name))+
    facet_wrap(vars(name), scales="free_y")+
  theme_ipsum()+
  geom_smooth(method="lm",se=FALSE, color="black", linetype="dashed")+
      geom_point(alpha=0.5)+
     theme(axis.title.x = element_text(size=17),
          axis.title.y = element_text(size=17),
          axis.text.x = element_text(size=17),
          axis.text.y = element_text(size=13),
          plot.title = element_text(size=22),
          plot.subtitle = element_text(size=17),
          legend.position = "none")+
  labs(title="Overpass dimensions",y="Crossings per month",x="")
```

![](README_files/figure-gfm/Effectiveness-2.png)<!-- -->

``` r
ggsave(here::here("output", "op_dims_effectiveness_byspecies.png"), height=8, width=10, unit="in",bg="white")


##length
length.by.sp <- eff%>%
  filter(!name%in%"total")%>%
  dplyr::select(crossing, rate,name, `Width (m)`)%>%
  nest(data = c(-name))%>% 
  mutate(
    fit = map(data, ~ lm(rate ~ `Width (m)`, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  dplyr::select(-data, -fit)%>%
  filter(term!="(Intercept)")%>%
  mutate_if(is.numeric, round, 5)%>%
  kable



eff%>%
  filter(!name%in%"total")%>%
  dplyr::select(crossing, rate,name, `Width (m)`)%>%
  group_by(name)%>%
  mutate(rate_scale=scale(rate))%>%
  ungroup%>%
  nest(data = c(-name))%>% 
  mutate(
    fit = map(data, ~ lm(rate_scale ~ `Width (m)`, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  dplyr::select(-data, -fit)%>%
  filter(term!="(Intercept)")%>%
  mutate_if(is.numeric, round, 5)%>%
  ggplot(aes(x=estimate, xmin=estimate-std.error,xmax=estimate+std.error,y=name))+
  geom_point()+
  geom_linerange()+
  theme_ipsum()+
  geom_vline(xintercept=0, linetype="dashed")+
       theme(axis.title.x = element_text(size=17),
          axis.title.y = element_text(size=17),
          axis.text.x = element_text(size=17),
          axis.text.y = element_text(size=13),
          plot.title = element_text(size=22),
          plot.subtitle = element_text(size=17),
          legend.position = "none")+
  labs(title="",y="Species",x="Effect of width on crossings (standardized)")
```

![](README_files/figure-gfm/Effectiveness-3.png)<!-- -->

``` r
##summarise  across lengths
eff%>%
  filter(!name%in%"total")%>%
  dplyr::select(crossing, rate,name,name, `Width (m)`)%>%
  mutate(width.bin=case_when(`Width (m)`<=40~"<=40",
                             `Width (m)`>40~">40"))%>%
  drop_na(width.bin)%>%
  nest(data = c(-name))%>% 
  mutate(
    fit = map(data, ~ lm(rate ~ width.bin, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  dplyr::select(-data, -fit)%>%
  filter(term!="(Intercept)")%>%
  mutate_if(is.numeric, round, 5)%>%
  kable
```

| name         | term          | estimate | std.error | statistic | p.value |
|:-------------|:--------------|---------:|----------:|----------:|--------:|
| deer         | width.bin\>40 | 21.93395 |  19.13549 |   1.14624 | 0.27838 |
| elk          | width.bin\>40 |  5.42005 |   6.06313 |   0.89394 | 0.39236 |
| moose        | width.bin\>40 |  0.15712 |   0.28483 |   0.55163 | 0.59463 |
| black bear   | width.bin\>40 |  0.30255 |   0.13845 |   2.18527 | 0.05376 |
| grizzly bear | width.bin\>40 |  1.37041 |   0.45340 |   3.02249 | 0.01932 |
| cougar       | width.bin\>40 | -1.07488 |   0.80592 |  -1.33373 | 0.21187 |
| wolf         | width.bin\>40 |  0.92302 |   0.92354 |   0.99944 | 0.34685 |
| coyote       | width.bin\>40 |  0.79029 |   1.42315 |   0.55531 | 0.59089 |

``` r
##Width:length
eff%>%
  filter(!name%in%"total")%>%
  dplyr::select(crossing, rate,name, `W:L ratio`)%>%
  nest(data = c(-name))%>% 
  mutate(
    fit = map(data, ~ lm(rate ~ `W:L ratio`, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  dplyr::select(-data, -fit)%>%
  filter(term!="(Intercept)")%>%
  mutate_if(is.numeric, round, 5)%>%
  kable
```

| name         | term        | estimate | std.error | statistic | p.value |
|:-------------|:------------|---------:|----------:|----------:|--------:|
| deer         | `W:L ratio` | 38.80397 |  27.27650 |   1.42262 | 0.18528 |
| elk          | `W:L ratio` | -1.81429 |   9.24103 |  -0.19633 | 0.84828 |
| moose        | `W:L ratio` |  0.13985 |   0.41634 |   0.33589 | 0.74465 |
| black bear   | `W:L ratio` |  0.52422 |   0.18352 |   2.85653 | 0.01706 |
| grizzly bear | `W:L ratio` |  2.09944 |   0.65517 |   3.20442 | 0.01497 |
| cougar       | `W:L ratio` | -1.19641 |   1.22837 |  -0.97398 | 0.35304 |
| wolf         | `W:L ratio` |  1.46147 |   1.32013 |   1.10706 | 0.30044 |
| coyote       | `W:L ratio` | -1.02972 |   2.09817 |  -0.49077 | 0.63418 |
