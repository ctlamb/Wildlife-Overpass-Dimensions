Wildlife Overpass Review
================
Clayton Lamb, Liam Brennan, Emily Chow
30 September, 2021

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
options(scipen=999)

##load data
df <- read_xlsx(here::here("data","Crossing Structure Literature Review_Appendix (83_overpasses_only).xlsx"))
```

## Map

``` r
##make spatial, keep last check
op <- st_as_sf(df%>%mutate(Lat=extract_numeric(Lat),
                                                Long=extract_numeric(Long)),
               coords=c("Long", "Lat"),
               crs=4326)%>%
    mutate(Continent=case_when(Country%in%c("U.S.A", "Canada","U.S.A.")~"North America",
                             TRUE~"Europe, Asia, and Oceania"))

               
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
#prep basemap
register_google("xAIzaSyCOwGx2D77XOqRgGhKmcb5F4Kt_S61tCLIx")
set_defaults(map_service = "osm", map_type = "terrain_bg")

##GLOBAL
bb = st_bbox(c(xmin = -185, xmax = 185, ymax = 85, ymin = -85), crs = st_crs(4326))

bb_ll = st_bbox(
  st_transform(
    st_as_sfc(bb), 
    3857
  )
)
a <- basemap_raster(bb_ll)

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

a <- basemap_raster(bb_ll)
a <- a%>%projectRaster(crs="+proj=wintri")%>%
  raster::crop(bb_clip%>%extent)
writeRaster(a,here::here("data","basemap.tif"), overwrite=TRUE)
a <- brick(here::here("data","basemap.tif"))%>%
  mask(wintri_outline)

WNA <- ggRGB(a, r=1, g=2, b=3)+
  geom_sf(data=op%>%filter(Continent=="North America")%>%st_transform_proj(crs = "+proj=wintri")%>%st_intersection(bb_clip%>%st_as_sfc),
          size = 5/.pt, color="red")+
  coord_sf(datum = "+proj=wintri")+
  theme_map()



##Europe
bb = st_bbox(c(xmin = 2, xmax = 24, ymax = 54, ymin = 43), crs = st_crs(4326))

bb_ll = st_bbox(
  st_transform(
    st_as_sfc(bb), 
    3857
  )
)

bb_clip=st_bbox(c(xmin = 0.2E6, xmax = 2.2E6, ymin = 4.7E6, ymax = 6.1E6), crs = "+proj=wintri")

a <- basemap_raster(bb_ll)
a <- a%>%projectRaster(crs="+proj=wintri")%>%
  raster::crop(bb_clip%>%extent)
writeRaster(a,here::here("data","basemap.tif"), overwrite=TRUE)
a <- brick(here::here("data","basemap.tif"))%>%
  mask(wintri_outline)

EU <- ggRGB(a, r=1, g=2, b=3)+
  geom_sf(data=op%>%filter(Continent!="North America")%>%st_transform_proj(crs = "+proj=wintri")%>%st_intersection(bb_clip%>%st_as_sfc),
          size = 5/.pt, color="red")+
  coord_sf(datum = "+proj=wintri")+
  theme_map()

mapview(op%>%filter(Continent!="North America")%>%st_transform_proj(crs = "+proj=wintri")%>%st_intersection(bb_clip%>%st_as_sfc))+mapview(bb_clip)

plot_grid(world,plot_grid(WNA, EU, labels = c("B","C"), rel_widths = c(1, 1.9)), labels = c("A",NA),nrow=2, rel_heights = c(1.9,1))
```

![](README_files/figure-gfm/map-2.png)<!-- -->

``` r
ggsave(here::here("output","map.png"), height=7, width=7, unit="in", bg="white")
```

## Plots

``` r
plot.dat <- op%>%
  tibble%>%
  dplyr::select(Continent,`Width (m)`,`Length (m)`, lanes=`Number of lanes spanned`)%>%
  mutate(`Length (m)`=as.numeric(`Length (m)`),
         lanes=as.numeric(lanes))%>%
    mutate(`Width:length ratio`=`Width (m)`/as.numeric(`Length (m)`))%>%
  tidyr::pivot_longer(`Width (m)`:`Width:length ratio`)%>%
  mutate(name=fct_relevel(name,"Length (m)","Width (m)","Width:length ratio", "lanes"))

plot.dat%>%
  filter(!name%in%"lanes")%>%
  ggplot(aes(y=value))+
    geom_boxplot(color="grey")+
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
    facet_grid(name~Continent, scales="free_y")+
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


by.year <- op%>%
drop_na(`Year of build_clean`)%>%
  ggplot(aes(y=`Width (m)`, x=`Year of build_clean`))+
  geom_point()+
  theme_ipsum()+
      facet_wrap(vars(Continent))+
     theme(axis.title.x = element_text(size=17),
          axis.title.y = element_text(size=17),
          axis.text = element_text(size=13),
          plot.title = element_text(size=22),
          plot.subtitle = element_text(size=17),
          legend.position = "none")+
  labs(title="Width through time",y="Width (m)", x="Year built")
op%>%
drop_na(`Year of build_clean`)%>%
  lm(`Width (m)`~`Year of build_clean`+ Continent, data=.)%>%
  summary

by.size <- op%>%
drop_na(ApproxSize)%>%
  mutate(ApproxSize=fct_relevel(ApproxSize,"Small (<50 lbs)","Medium (50-350 lbs)","Large (>350 lbs)"))%>%
  ggplot(aes(y=`Width (m)`, x=ApproxSize))+
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
drop_na(ApproxSize)%>%
  mutate(ApproxSize=fct_relevel(ApproxSize,"Small (<50 lbs)","Medium (50-350 lbs)","Large (>350 lbs)"))%>%
  lm(`Width (m)`~`ApproxSize`, data=.)%>%
  summary
```

## PSummary stats

``` r
plot.dat%>%
  group_by(name) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            n=n())

plot.dat%>%
  group_by(Continent, name) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            n=n())


op%>%
  drop_na(`Year of build_clean`)%>%
  mutate(period=case_when(`Year of build_clean`<2010~"before",
                          `Year of build_clean`>=2010~"after"))%>%
  group_by(period) %>%
  summarise(W = mean(`Width (m)`, na.rm = TRUE),
            L=mean(`Length (m)`),
            n=n())
```