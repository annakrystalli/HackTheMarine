library(shiny)
library(leaflet)
library(RColorBrewer)
require(png)
require(stringr)
library(raster)
library(Grid2Polygons)
require(leaflet)
require(dplyr)
require(ggmap)

input.folder <- "/Users/Anna/Google Drive/HackTheMarine/inputs/data/"
output.folder <- "/Users/Anna/Google Drive/HackTheMarine/outputs/"
data <- read.csv(paste(input.folder, "2001-5-.csv", sep = ""))
setwd(input.folder)

spp <- "cfin"
load(file="thresholds.Rdata")


img<-readPNG("~/Documents/Documents/Presentations/BES 2012/Images/M2000122-2000152.uk.sstp.AVH.L3_median.01may00-31may00.v1.20122500252.rsg_grey.png")

#Load geo.matrix
load(file="~/Documents/Documents/CPR/DATA/RAW DATA/SeaWiFS/r files/geo.matrix.RData")
lon<-c(geo.matrix[1,,1],11.01279)
lat<-c(rev(geo.matrix[,1,2]), 61.00876)

layers <- names(data)[grep("AC", names(data))]

data[data$OC1 < THRESH.spp[[spp]]["PredPrev=Obs"], grep("AC", names(data))] <- 0
data <- cbind(lat = lat[data$r], long = lon[data$c], data)

pred.map<-matrix(0, ncol=926, nrow=1112)
pred.map[img==1]<- NA  

i = 1


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  plotOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("cutoff", "setCutOffs", 0, 1,
                            value = c(0), step = 0.01
                ),
                selectInput("layer", "layer",layers)
                
  )
)


server <- function(input, output, session) {
  
  
  output$map <- renderPlot({

    pred.map[cbind(data$r,data$c)]<- data[,input$layer]
    
    
    r <- raster(pred.map, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat)) 
    s <- as(r, 'SpatialGridDataFrame')
    crs(s) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    map_uk <- get_map("UK", zoom = 5)
    
    
    ggmap(map_uk) + 
      geom_polygon(data=data.frame(s), aes(x=s1, y=s2), fill=layer, alpha=layer)
    
    
    
  })
  

}

shinyApp(ui, server)
