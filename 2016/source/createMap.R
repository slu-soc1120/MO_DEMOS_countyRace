# create map of race by county

## Dependencies
### tidyverse
library(dplyr)
library(ggplot2)

### mapping
library(classInt)
library(sf)
library(tidycensus)
library(tigris)

### other
library(prener)

## Create state outline
### download missouri
mo <- states(cb = FALSE, resolution = "20m")

### convert to sf object
mo <- st_as_sf(mo)

### subset observations
mo <- filter(mo, STATEFP == 29)

## Create combined census tract object
### download missouri counties
moCounties <- counties(state = "MO", cb = FALSE, resolution = "20m")

### convert to sf object
moCounties <- st_as_sf(moCounties)

### subset columns
moCounties <- select(moCounties, GEOID, COUNTYFP, NAMELSAD, ALAND)

### download county population data
moRace <- get_acs(geography = "county",  state = "MO", output = "wide", table = "B02001")

### subset columns and calculate estimate
moRace %>%
  mutate(pctBlack = (B02001_003E/B02001_001E)*100) %>%
  select(GEOID, B02001_001E, B02001_003E, pctBlack) -> moRace

### combine spatial and geometric data
raceMap <- left_join(moCounties, moRace, by = "GEOID")


jenks <- classIntervals(raceMap$pctBlack, n=5, style="jenks")
race <- cut(raceMap$pctBlack, breaks = c(jenks$brks))


## base map
base <- ggplot() + 
  geom_sf(data = mo, fill = "#ffffff", color = NA) + 
  geom_sf(data = raceMap, mapping = aes(fill = race), color = NA) +
  geom_sf(data = mo, fill = NA, color = "#000000", size = .25) +
  scale_fill_brewer(palette = "BuPu", name = "Percent",
    labels = c("0.00 - 2.96", "2.97 - 8.23", "8.24 - 15.90", "15.91 - 27.00", "27.01 - 47.90")) +
  labs(
    title = "African American Population by County, 2016",
    subtitle = "Percentage of African American Residents in Missouri",
    caption = "Data via U.S. Census Bureau \nMap by Christopher Prener, Ph.D."
  ) 

cp_plotSave(filename = "2016/results/raceMap16-base.png", plot = base, preset = "lg", dpi = 500)

## map with white background
map01 <- base +
  cp_sequoiaTheme(background = "white", map = TRUE)

cp_plotSave(filename = "2016/results/raceMap16-white.png", plot = map01, preset = "lg", dpi = 500)

## map with transparent background
map02 <- base +
  cp_sequoiaTheme(background = "transparent", map = TRUE)

cp_plotSave(filename = "2016/results/raceMap16-trans.png", plot = map02, preset = "lg", dpi = 500)

