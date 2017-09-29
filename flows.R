library(dplyr)
library(networkD3)
library(parsetR) # devtools::install_github("timelyportfolio/parsetR")
library(rnaturalearth)
library(rgdal)
library(geosphere)
library(leaflet)
library(threejs) 


# Fake data ----

set.seed(9)

df <- data_frame(origins = sample(c('Portugal', 'Australia', 'Nigeria', 'China'), 
                                  size = 100, replace = TRUE), 
                 destinations = sample(c('Japan', 'Australia', 'Turkey', 'Ireland'), 
                                       size = 100, replace = TRUE))

df2 <- df %>%
  group_by(origins, destinations) %>%
  summarize(counts = n()) %>%
  ungroup() %>%
  arrange(desc(counts)) %>%
  filter(!(origins == 'Australia' & destinations == 'Australia')) %>%
  mutate(direction = as.numeric(origins == "Australia"))


## adjusted a few for more line variation
df2 <- df2 %>%
  mutate(counts = ifelse(origins=='China' & destinations =='Ireland', 3, counts)) %>%
  mutate(counts = ifelse(origins=='Portugal' & destinations =='Ireland', 3, counts)) %>%
  mutate(counts = ifelse(origins=='China' & destinations =='Japan', 8, counts)) %>%
  mutate(counts = ifelse(origins=='China' & destinations =='Turkey', 4, counts)) %>%
  mutate(counts = ifelse(origins=='Nigeria' & destinations =='Japan', 2, counts))

df2 


# network-ize ----

name_vec <- c(unique(df2$origins), unique(df2$destinations))

nodes <- data.frame(name = name_vec, id = 0:7)

links <- df2 %>%
  left_join(nodes, by = c('origins' = 'name')) %>%
  rename(origin_id = id) %>%
  left_join(nodes, by = c('destinations' = 'name')) %>%
  rename(dest_id = id)


# Sankey ----

## Australia is duplicating in the basic version
sankeyNetwork(Links = links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id', 
              Value = 'counts', NodeID = 'name', fontSize = 16)

## This is much prettier 
parset(df2, dimensions = c('origins', 'destinations'),
       value = htmlwidgets::JS("function(d){return d.counts}"),
       tension = 0.5)


# Get polygons and coordinates  ----

countries <- ne_countries()

countries$longitude <- coordinates(countries)[,1]

countries$latitude <- coordinates(countries)[,2]

countries_xy <- countries@data %>%
  select(admin, longitude, latitude)


# Join the coordinates to the data ----

df3 <- df2 %>%
  left_join(countries_xy, by = c('origins' = 'admin')) %>%
  left_join(countries_xy, by = c('destinations' = 'admin')) 

df3$longitude.x <- as.numeric(as.character(df3$longitude.x))
df3$latitude.x <- as.numeric(as.character(df3$latitude.x))
df3$longitude.y <- as.numeric(as.character(df3$longitude.y))
df3$latitude.y <- as.numeric(as.character(df3$latitude.y))


# Calcuate the great circles between the points ----
## Todo find something else that works better for a flat map (loops around to the americas) 
## http://dsgeek.com/2013/06/08/DrawingArcsonMaps.html
flows <- gcIntermediate(df3[,5:6], df3[,7:8], sp = TRUE, addStartEnd = TRUE)

flows$counts <- df3$counts

flows$origins <- df3$origins

flows$destinations <- df3$destinations

flows$direction <- df3$direction

# Map ----

## Split into two layers
flows.In <- flows[flows@data$direction == 0, ]

flows.Out <- flows[flows@data$direction == 1, ]


hover.In <- paste0(flows.In$origins, " to ", 
                flows.In$destinations, ': ', 
                as.character(flows.In$counts))

hover.Out<- paste0(flows.Out$origins, " to ", 
                   flows.Out$destinations, ': ', 
                   as.character(flows.Out$counts))

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data = flows.In, weight = ~counts, label = hover.In, 
               group = 'In', color = '#2b8cbe') %>%
  addPolylines(data = flows.Out, weight = ~counts, label = hover.Out,
               group = 'Out', color = '#e6550d') %>%
  addLayersControl(overlayGroups = c('In', 'Out'), 
                   options = layersControlOptions(collapsed = FALSE))

## TODO
### Try addFlows() https://www.rdocumentation.org/packages/leaflet.minicharts/versions/0.5.1/topics/
### This also looks fun https://francoisguillem.shinyapps.io/shiny-demo/ 


# 3D just because
library(threejs) 

df4 <- arrange(df3, origins)

df4 <- df4 %>%
  mutate(colors = ifelse(direction == 1, '#e6550d', '#2b8cbe'))

weights <- 1.5 * df4$counts

arcs <- data.frame(lat1 = df4$latitude.x, lon1 = df4$longitude.x, 
                   lat2 = df4$latitude.y, lon2 = df4$longitude.y)

earth <- tempfile(fileext=".jpg")


jpeg(earth, width=2048, height=1024, quality=100, bg="#C7C9CE", antialias="default")
par(mar = c(0,0,0,0), pin = c(4,2), pty = "m",  xaxs = "i",
    xaxt = "n",       xpd = FALSE,  yaxs = "i", bty = "n", yaxt = "n")
plot(countries, col="#F2F3F0", bg="#C7C9CE", border="black", ann=FALSE,
     setParUsrBB=TRUE)
dev.off()

globejs(earth, color = "#00aaff", bodycolor="#aaaaff", arcs = arcs, arcsColor = df4$colors,
        arcsHeight = 0.4, arcsLwd = weights, arcsOpacity = 0.8, atmosphere = FALSE,
        bg = "white")







