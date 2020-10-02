library(BAMMtools)
library(shiny)
library(leaflet)
library(raster)
library(sf)
library(shiny)
library(shinythemes)
library(stringr)
library(tidyverse)

setwd("~/SSPHC/R Shiny Practice")

# read in county boundaries

counties <- st_read("cb_2018_us_county_5m.shp", stringsAsFactors = FALSE)

# remove territories

counties <- subset(counties, STATEFP != 60 & STATEFP != 66 & STATEFP != 69 & STATEFP != 72 & STATEFP != 78)

# read in cases

cases <- read.csv("CDC-LD-Case-Counts-by-County-00-18.csv")

# remove state rows

cases <- subset(cases, CTYCODE != 999)

# create a GEOID field in cases data. GEOID == 2-digit state code + 3-digit county code

cases$STCODE_pad <- str_pad(cases$STCODE, width = 2, side = "left", pad = "0")
cases$CTYCODE_pad <- str_pad(cases$CTYCODE, width = 3, side = "left", pad = "0")
cases$GEOID <- paste(cases$STCODE_pad, cases$CTYCODE_pad, sep = "")

# change annual cases from wide to long format

cases <- cases %>% 
  gather(YEAR, CASES, Cases2000:Cases2018) %>% 
  separate(YEAR, c("tobedeleted", "YEAR"), sep = 5)
 
cases <- cases[ -c(3:6, 8) ]

# join cases to counties based on GEOID

counties <- counties %>% 
  left_join(cases, c("GEOID" = "GEOID"))

# change year field to numeric

counties$YEAR <- as.numeric(counties$YEAR)

# read in population data. This comes in two files, 2000-10 and 2010-19

pop_00_10 <- read.csv("co-est00int-tot.csv")

pop_10_19 <- read.csv("co-est2019-alldata.csv")

# remove states

pop_00_10 <- subset(pop_00_10, COUNTY != 0)

pop_10_19 <- subset(pop_10_19, COUNTY != 0)

# create GEOID field for both

pop_00_10$STATE_pad <- str_pad(pop_00_10$STATE, width = 2, side = "left", pad = "0")
pop_00_10$COUNTY_pad <- str_pad(pop_00_10$COUNTY, width =3 , side = "left", pad = "0")
pop_00_10$GEOID <- paste(pop_00_10$STATE_pad, pop_00_10$COUNTY_pad, sep = "")

pop_10_19$STATE_pad <- str_pad(pop_10_19$STATE, width = 2, side = "left", pad = "0")
pop_10_19$COUNTY_pad <- str_pad(pop_10_19$COUNTY, width = 3, side = "left", pad = "0")
pop_10_19$GEOID <- paste(pop_10_19$STATE_pad, pop_10_19$COUNTY_pad, sep = "")

# for 2000-10, change annual population from wide to long format, first removing 
# 2000 estimate base and 2010 estimates because 2010 is included in second file

pop_00_10 <- pop_00_10[ -c(8, 19:22) ]

pop_00_10 <- pop_00_10 %>% 
  gather(YEAR, POP, POPESTIMATE2000:POPESTIMATE2009) %>%
  separate(YEAR, c("tobedeleted", "YEAR"), sep = 11)

pop_00_10 <- pop_00_10[ -c(1, 4:5, 9) ]

# repeat for 2010-19, first removing 2010 census pop/estimate base as well as 
# all the extra birth/death/migration fields

pop_10_19 <- pop_10_19[ -c(8:9, 20:166) ]

pop_10_19 <- pop_10_19  %>% 
  gather(YEAR, POP, POPESTIMATE2010:POPESTIMATE2019) %>%
  separate(YEAR, c("tobedeleted", "YEAR"), sep = 11)

pop_10_19 <- pop_10_19[ -c(1, 4:5, 9) ]

# combine population data and join to counties

pop <- rbind(pop_00_10, pop_10_19)

pop$YEAR <- as.numeric(pop$YEAR)

counties <- counties %>% 
  left_join(pop, c("GEOID" = "GEOID", "YEAR" = "YEAR"))

# make sure all county names are capitalized

counties$Ctyname <- str_to_title(counties$Ctyname)

# create new field RATE as cases per 100,000 people

counties$RATE <- (counties$CASES / counties$POP) * 100000

# get jenk breaks for cases and rate

#getJenksBreaks(counties$CASES, 5, subset = NULL)
#getJenksBreaks(counties$RATE, 5, subset = NULL)

# define boundaries between intervals

bins <- c(0, 1, 36, 135, 322, 1581)

# define color palette for bins

pal <- colorBin("YlOrBr", domain = counties$RATE, bins = bins)

### SHINY APP ####
# bootstrapPage allows you to use HTML/CSS. There are many layout options

ui <- bootstrapPage(
  theme = shinythemes::shinytheme('simplex'),
  leaflet::leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, id = 'controls',
                sliderInput("year", "Select Year", value = 2018, min = 2000, max = 2018, sep = "")),
  tags$style(type = "text/css", "
  html, body {width:100%;height:100%}
  #controls{background-color:white;padding:20px;}
  ")
)
server <- function(input, output, session){
  rval_rate <- reactive({
    counties %>%
      filter(
        YEAR == input$year,
      )
  })
  output$map <- leaflet::renderLeaflet({
    rval_rate () %>%
      mutate(popup = str_c("<strong>", Ctyname, ", ", Stname, "</strong>",
                           "<br/>",
                           round(RATE, digits = 2), " cases per 100,000 people",
                           "<br/>", 
                           CASES, " cases total") %>%
               map(htmltools::HTML)) %>%
      leaflet() %>%
      addTiles() %>%
      setView(-98.58, 39.82, zoom = 5) %>%
      addPolygons(label = ~popup,
                  fillColor = ~pal(RATE),
                  color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal,
                values = ~RATE,
                opacity = 1,
                title = NULL,
                position = "bottomright")
  })
}
shinyApp(ui, server)

