#' @title top10
#' @param x user inputed year from 1999-2013
#' @return Choropleth of the top 10 types of death
#' @description This function creates a choropleth of the Top 10 deaths by year.
#' @import dplyr ggplot2
#' @export
#'

top10 = function(x)
{
  
  us <- map_data("state")
  
  AADR.DF <- read.csv("data/AADR-all.csv")
  
  YEAR=region=DEATHS=AADR=long=lat=NULL
  
  top10wYear <- subset(AADR.DF, YEAR == x, select=c (YEAR, region, DEATHS, AADR))
  
  top10wYear <- top10wYear %>% 
    mutate(region=tolower(region))
  
  gg <- ggplot()
  
  gg <- gg + geom_map(data=us, map=us,
                      aes(x=long, y=lat, map_id=region),
                      fill="#ffffff", color="#ffffff", size=0.15)
  
  gg <- gg + geom_map(data=top10wYear, map=us,
                      aes(fill=DEATHS, map_id=region),
                      color="#ffffff", size=0.15)
  
  gg <- gg + scale_fill_distiller(palette = "YlGn", direction = 1)
  
  gg <- gg + labs(x=NULL, y=NULL) + ggtitle(paste0(x, " Top 10 Deaths per 100,000"))
  
  gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45)
  
  gg <- gg + theme(panel.border = element_blank())
  
  gg <- gg + theme(panel.background = element_blank())
  
  gg <- gg + theme(axis.ticks = element_blank())
  
  gg <- gg + theme(axis.text = element_blank())
  
  gg
  
}