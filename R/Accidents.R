#' @title Accidents
#' @return Choropleth of Accident related deaths
#' @description This function creates a choropleth of Accident related deaths by year.
#' @param x user inputed year from 1999-2013
#' @import dplyr ggplot2
#' @export
#'

Accidents = function(x)
{
  
  us <- map_data("state")
  
  AADR.DF <- read.csv("data/AADR-accidents.csv")
  
  YEAR=region=DEATHS=AADR=long=lat=NULL
  
  AccidentwYear <- subset(AADR.DF, YEAR == x, select=c (YEAR, region, DEATHS, AADR))
  
  AccidentwYear <- AccidentwYear %>% 
    mutate(region=tolower(region))
  
  gg <- ggplot()
  
  gg <- gg + geom_map(data=us, map=us,
                      aes(x=long, y=lat, map_id=region),
                      fill="#ffffff", color="#ffffff", size=0.15)
  
  gg <- gg + geom_map(data=AccidentwYear, map=us,
                      aes(fill=DEATHS, map_id=region),
                      color="#ffffff", size=0.15)
  
  gg <- gg + scale_fill_distiller(palette = "YlOrRd", direction = 1)
  
  gg <- gg + labs(x=NULL, y=NULL) + ggtitle(paste0(x, " Accidental Deaths per 100,000"))
  
  gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45)
  
  gg <- gg + theme(panel.border = element_blank())
  
  gg <- gg + theme(panel.background = element_blank())
  
  gg <- gg + theme(axis.ticks = element_blank())
  
  gg <- gg + theme(axis.text = element_blank())
  
  gg
  
}