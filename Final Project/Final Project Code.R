shhLoad=function(x){
  if( !x %in% installed.packages()[,1] ){
    install.packages(x, repos='https://cloud.r-project.org') }
  
  suppressMessages(
    suppressWarnings(
      library(x, character.only = TRUE))) }
char = function(x){as.character(x)}
num = function(x){as.numeric(char(x))}

pkgs=c(
  'dplyr', 'ggplot2', 'tidyr', 
  'tidyverse', 'maps', 'ggmap',
  'rgeos', 'maptools', 'mapproj',
  'broom',
  'gridExtra',
  'gifski', 'av', 'transformr', 'gganimate'
)

for(pkg in pkgs){  shhLoad(pkg) }


library(maps)

worldMap = map_data("world")

head(worldMap)

Top6_cases <- c(
  'USA','Spain', 'Germany',
  'Italy', 'France', 'China'
)

Top6Map = worldMap %>%
  filter(
    region %in% Top6_cases
  )

ggplot() +
  geom_map(
    data=Top6Map, 
    map=Top6Map,
    aes(map_id=region),
    fill='grey80',
    color='grey40'
  ) +
  expand_limits(x=Top6Map$long, y=Top6Map$lat)


confirmedCases = c(
  587.357, 172.655,130.400, 159.516,98.076,82.249
)



confirmedData = data.frame(
  cntry=Top6_cases, 
  cases=confirmedCases,
  stringsAsFactors = FALSE
)

options(scipen=5)

ggplot(
  confirmedData, aes(map_id=cntry)) +
  geom_map(
    map=Top6Map, 
    aes(fill=cases),
    color='grey40'
  ) +
  expand_limits(x=Top6Map$long, y=Top6Map$lat) +
  theme_void() +
  scale_fill_distiller(direction=1) +
  scale_x_discrete(limits=c("600", "200", "100")) + 
  theme(legend.background = element_rect(fill="lightblue",
                                         size=0.5, linetype="solid", 
                                         colour ="darkblue")) +
  labs(
    fill='Total Confirmed Covid-19 Cases (In Thousands)'
  ) +
  theme(
    legend.position='top',
    legend.spacing.x = unit(1.0, 'cm')
  )