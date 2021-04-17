library("data.table")
library("spdep")
library("sp")
library("PerformanceAnalytics")
library("parallel")
library("rjson")
library("leaflet")
library("rgdal")
library("rgeos")
library("spatialEco")
library("Hmisc")
library(readr)
library(dplyr)
library("readxl") 
library(MCI)
library("geosphere")
library(stringr)

# Change the path 
path<-"/Users/hamzaouammou/Desktop/Geomarketing"

#Token
Token_map_box<-"https://api.mapbox.com/styles/v1/hamzaouammou/ckknqlb8t5z6f17pb4jhvji7k/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiaGFtemFvdWFtbW91IiwiYSI6ImNra25xZXc2NzMzOXAycHF0M3I3Zm02Nm4ifQ.UJ7gM9v9Y_qK2WM9t8LryA"

#Environments
load("/Users/hamzaouammou/Desktop/Geomarketing/Environment.RData")

#Projections definition
LIIE<-"+proj=lcc +lat_1=46.8 +lat_0=46.8 +lon_0=0 +k_0=0.99987742 +x_0=600000 +y_0=2200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs"
WGS84<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#Iconshop
icon_shop <- makeIcon("https://image.flaticon.com/icons/png/128/138/138310.png",iconHeight = 30,iconWidth = 30)

#Define your objectif function.
# You must use:
# 1 -	The market share as a principal criteria (not the sales).
order_<-match(market_zones@data$IRIS,market_potential$IRIS)
market_zones@data$mp<-market_potential[order_]$mp
market_zones@data$ms<-market_zones@data$sales/market_zones@data$mp
market_zones@data$ms[which(!is.finite(market_zones@data$ms))]<-0

# 2 -	The INSEE IRIS socioeconomic data. 
names(market_zones)

subset_INSEE <- c(
  # Caractéristiques des logements
  'P17_LOG', # nb de logements
  'P17_LOGVAC', # nb de logements vacants
  'P17_MAISON', # nb de maisons
  'P17_APPART', # nb d'appartements
  # Caractéristiques des résidences principales
  'P17_RP', # nb de résidence principales
  'P17_RP_3P', # nb de résidences principales de 3 pièces
  'P17_RP_4P', # nb de résidences principales de 4 pièces
  'P17_RP_5PP', # nb de résidences principales de 5 pièces ou plus
  # Caractéristiques des ménages
  'C17_MEN', # nb de ménages
  'C17_MENPSEUL', # nb de ménages d'une seule personne
  'C17_MENCOUPSENF', # nb de ménages dont la famille principale est formée d'un couple sans enfant
  'C17_MENCOUPAENF', # nb de ménages dont la famille principale est formée d'un couple avec enfant(s)
  # Caractéristiques des personnes
  'P17_POP', # Population
  'P17_PMEN', # nb de personnes des ménages
  'P17_POPF', # nb total de femmes
  'P17_POPH', # nb total d'hommes 
  'P17_POP65P', # nb de personnes de 65 ans ou plus
  'C17_POP15P_CS3', # nb de personnes de 15 ans ou plus Cadres et Professions intellectuelles supérieures
  'C17_POP15P_CS5', # nb de personnes de 15 ans ou plus Emplyés
  'C17_POP15P_CS8' # nb de personnes de 15 ans ou plus Autres sans activité
)
market_zones@data <- market_zones@data[c('IRIS', 'lon', 'lat', 'pos_id', 'distance', 'ms')]
market_zones@data <- merge(market_zones@data,
                           data.table(geo1@data[,c("IRIS", subset_INSEE)]),
                           by="IRIS")

# -	The SIRENE establishments database.
# There is two ways to construct and index:  
         # over of sirene with the IRIS
         # proximity index
# Let's use the one that we are familiar with :
sirene_sp <- SpatialPointsDataFrame(coords = siren_competitors[,c("longitude","latitude")], data = siren_competitors, proj4string = CRS(WGS84))
geo1_WGS84 <- spTransform(geo1, CRS(WGS84))
ov <- over(sirene_sp, geo1_WGS84)

# Now, we will aggregate by IRIS and will count the nb of competitors in each IRIS 
ov_IRIS <- aggregate(ov, by=list(ov$IRIS), FUN=length)
competitors <- ov_IRIS[,c('Group.1', 'IRIS')]
colnames(competitors) <- c('IRIS', 'nb_competitors')

market_zones@data <- merge(market_zones@data, competitors, by="IRIS") 

# -	The constraints.

# Ex: The pos has to be in a 'carreau' where at least 75 people reside.

subset_geo2_data <- subset(geo2_data, ind_c > quantile(geo2_data$ind_c, c(0.92)))
geo2_data_sp <- SpatialPointsDataFrame(coords = subset_geo2_data[,c("lon","lat")],data = subset_geo2_data, proj4string = CRS(WGS84))
geo2_data_sp_LIIE <- spTransform(geo2_data_sp,CRS(LIIE))
geo2_data_sp_LIIE_blocs <- gBuffer(spgeom = geo2_data_sp_LIIE,byid = TRUE,width = 100,capStyle = "SQUARE")
blocs_WGS84 <- spTransform(gBuffer(spgeom = geo2_data_sp_LIIE_blocs,byid = TRUE,width = 100,capStyle = "SQUARE"),CRS(WGS84))
blocs_WGS84 <- spChFIDs(blocs_WGS84,as.character(blocs_WGS84$idgeo2))
coord_points <- SpatialPointsDataFrame(data = market_zones@data[,c('lon','lat')],coords = market_zones@data[,c('lon','lat')],
                                       proj4string = CRS(WGS84))
res <- over(coord_points, blocs_WGS84)
w <- which(!is.na(res$idgeo2))
length(w)

# We add this constant as a column in the dataset
market_zones@data$constraint <- FALSE
market_zones@data$constraint[w] <- TRUE

## MODEL ###
# Model preparation: building some new variables of interest
get_variables_of_interest <- function(df){
  # Caractéristiques des logements
  df$prop_LOGVAC = df$P17_LOGVAC / df$P17_LOG 
  df$prop_MAISON = df$P17_MAISON / df$P17_LOG
  df$prop_APPART = df$P17_APPART / df$P17_LOG
  # Caractéristiques des résidences principales
  df$prop_RP_3PP = (df$P17_RP_3P + df$P17_RP_4P + df$P17_RP_5PP) / df$P17_RP
  df$prop_RP_5PP = df$P17_RP_5PP / df$P17_RP
  # Caractéristiques des ménages
  df$prop_MENPSEUL = df$C17_MENPSEUL / df$C17_MEN
  df$prop_MENCOUPSENF = df$C17_MENCOUPSENF / df$C17_MEN
  df$prop_MENCOUPAENF = df$C17_MENCOUPAENF / df$C17_MEN
  # Caractéristiques des personnes
  df$prop_PMEN = df$P17_PMEN / df$P17_POP
  df$prop_POPF = df$P17_POPF / df$P17_POP
  df$prop_POPH = df$P17_POPH / df$P17_POP
  df$prop_POP65P = df$P17_POP65P / df$P17_POP
  df$prop_POP15P_CS3 = df$C17_POP15P_CS3 / df$P17_POP
  df$prop_POP15P_CS5 = df$C17_POP15P_CS5 / df$P17_POP
  df$prop_POP15P_CS8 = df$C17_POP15P_CS8 / df$P17_POP
  
  return(df)
}

market_zones@data <- get_variables_of_interest(market_zones@data)


variables_to_model <- c('ms', 'distance', 'nb_competitors',
                        'prop_LOGVAC', 'prop_MAISON', 'prop_APPART',
                        'prop_RP_3PP', 'prop_RP_5PP', 'prop_MENPSEUL', 'prop_MENCOUPSENF',
                        'prop_PMEN', 'prop_POPF', 'prop_POP65P', 'prop_POP15P_CS3', 'prop_POP15P_CS5', 'prop_POP15P_CS8'
                        )
model_dataset <- market_zones@data[variables_to_model]
model_dataset <- na.omit(model_dataset)

# We build a spatial interaction model based on the in-sample matrix in order to predict the market share
LM1 <- lm(log(ms+0.001) ~log(nb_competitors)+ log(distance) + log(prop_LOGVAC+0.001) + log(prop_MAISON+0.001) + log(prop_APPART+0.001) + 
            log(prop_RP_3PP+0.001) + log(prop_RP_5PP+0.001) + log(prop_MENPSEUL+0.001) + 
            log(prop_MENCOUPSENF+0.001) + log(prop_PMEN + 0.001) + log(prop_POPF + 0.001) + log(prop_POP65P + 0.001) +
            log(prop_POP15P_CS3 + 0.001) + 
            log(prop_POP15P_CS5+ 0.001) + log(prop_POP15P_CS8+0.001)+., data = model_dataset)

summary(LM1)

# 5. At least 10 spatial positions for the candidate points.
# You need to apply MODEL (LM1) to the 10 new positions
# You need to construct the market_zones matrix of the new 10 points
nb_candidate_points = 10
set.seed(123) # to get always the same sample (one without failing points)
new_positions <- sirene_sp[sample(1:length(sirene_sp), nb_candidate_points),]

# -	At least 10 spatial positions for the candidate points.
# 1. contruct their market_zones matrix
# 2. apply the LM1 model
# 3. based on your constraints what is the best and worst area

get_market_zone_for_competitor <- function(i) {
  # i will go from 1:nb_candidate_points
  ith_competitor_position <- coordinates(spTransform(new_positions[i,], CRS(LIIE)))
  
  # We get all geo1 variables of the 1000 closest (euclid dist) IRIS to the ith_competitor_position.
  # This creates a market zone
  IRIS_positions <- data.frame(coordinates(spTransform(SpatialPoints(coords = geo1@data[,c("lon","lat")], proj4string = CRS(WGS84)), CRS(LIIE))))
  colnames(IRIS_positions) <- c("x","y")
  new_market_zone <- geo1@data[c(FNN::get.knnx(data = IRIS_positions,
                                          query = ith_competitor_position, k = 1000)$nn.index),]
  # l'object ci-dessus est bien de taille 1000
  
  # socio-demographic
  new_market_zone <- new_market_zone[,c('IRIS', 'lon', 'lat', subset_INSEE)]
  
  # competitors (c'est cette ligne qui fait qu'on a pas 1000 obs par pos_id)
  new_market_zone <- merge(new_market_zone, competitors, by="IRIS")

  # the origins are the customers
  origin <- new_market_zone[,c("IRIS","lon","lat")]
  setnames(origin, c("id","x","y"))
  # the destination is the i-th point of sales
  destination <- data.frame(id=paste("new_pos_",i,sep=""), coordinates(new_positions[i,]))
  setnames(destination,c("id","x","y"))
  # now we can get the Traveling distance
  new_market_zone$distance <- distHaversine(origin[,c("x","y")],destination[,c("x","y")])
  
  # variables of interest
  new_market_zone <- get_variables_of_interest(new_market_zone)
  
  # add index new_pos_i
  new_pos_i <- paste("new_pos_", i, sep="")
  new_market_zone$pos_id <- new_pos_i
  
  return(new_market_zone)
}

# for the 10 selected competitors
all_new_market_zones <- data.frame()
for (i in 1:length(new_positions)) {
  print(i)
  all_new_market_zones <- rbind(all_new_market_zones, get_market_zone_for_competitor(i))
}

# Applying LM1
all_new_market_zones$market_share_predicted <-rexp(predict( LM1, newdata = all_new_market_zones))

# -	At least one of the following two spatial data set : INSEE Bloc 200m, Land Cover.
##  constraint
res_new <- over(new_positions[,c('longitude','latitude')], blocs_WGS84)
w_new <- which(!is.na(res_new$idgeo2))
length(w_new)
new_positions$constraint <- FALSE
new_positions$constraint[w_new] <- TRUE


# add the predicted market shares
predicted_market_zones <- select(all_new_market_zones, pos_id, market_share_predicted)
predicted_market_zones$market_share_predicted[is.na(predicted_market_zones$market_share_predicted)]<-0

# add sum of market shares
sum_market_zones <- aggregate(predicted_market_zones$market_share_predicted, by=list(pos_id=predicted_market_zones$pos_id), FUN=sum)
sum_market_zones$pos_id <- str_sub(sum_market_zones$pos_id,start=-1)
for (i in 1:length(new_positions)){
  print(i)
  new_positions$sum_market[i]<-filter(sum_market_zones, pos_id == i-1)$x
}

# add count of competitors
count_market_zones<-aggregate(predicted_market_zones$market_share_predicted, by=list(pos_id=predicted_market_zones$pos_id), FUN=length)
count_market_zones$pos_id<-str_sub(count_market_zones$pos_id,start=-1)
for (i in 1:length(new_positions)){
  print(i)
  new_positions$count_market[i]<-filter(count_market_zones, pos_id == i-1)$x
}

best <- new_positions[which.max(new_positions$sum_market),]
worst <- new_positions[which.min(new_positions$sum_market),]

New_market_zones<-SpatialPolygonsDataFrame(Sr =geo1[all_new_market_zones$IRIS,], data=data.frame(all_new_market_zones),match.ID = FALSE)
New_market_zones<-spChFIDs(New_market_zones,paste(as.character(New_market_zones@data$IRIS),as.character(New_market_zones@data$pos_id),sep=""))
leaflet_project <- leaflet() %>%  addTiles(urlTemplate = Token_map_box) %>%
  addPolygons(data=New_market_zones,stroke = TRUE,color ="bleu",weight = 1.5, fillOpacity = 0.6, smoothFactor = 0.1,
              label = paste("IRIS : ",New_market_zones@data$IRIS," // ",New_market_zones@data$market_share_predicted," €",sep=""),
              highlightOptions = highlightOptions(color = "white", weight = 7,bringToFront = FALSE,fillOpacity = 0.5),
              labelOptions = labelOptions(noHide = FALSE,direction = 'top',offset=c(0,0),textOnly = TRUE,
                                          style=list('color'='rgba(0,0,0,1)','font-family'= 'Arial Black','font-style'= 'bold',
                                                     'box-shadow' = '0px 0px rgba(0,0,0,0.25)','font-size' = '14px',
                                                     'background-color'='rgba(255,255,255,0.7)','border-color' = 'rgba(0,0,0,0)')))%>%
  addMarkers(lng = new_positions@data$longitude, lat = new_positions@data$latitude,label = unique(New_market_zones@data$pos_id),
             labelOptions = labelOptions(noHide = FALSE,direction = 'top',offset=c(0,0),textOnly = TRUE,
                                         style=list('color'='rgba(0,0,0,1)','font-family'= 'Arial Black','font-style'= 'bold',
                                                    'box-shadow' = '0px 0px rgba(0,0,0,0.25)','font-size' = '12px',
                                                    'background-color'='rgba(255,255,255,0.7)','border-color' = 'rgba(0,0,0,0)')))%>%
  addCircleMarkers(data = best,color="green", group = "best_worst",fillOpacity = 0.5)%>%
  addCircleMarkers(data = worst,color="red", group = "best_worst",fillOpacity = 0.5)
leaflet_project

save(leaflet_project, file=paste(path, "/www/leaflet_project.RData", sep=''))


################################################# ShinyApp #######################################################


# deployment 
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("Map", width = "100%", height = "100%"),
  absolutePanel(id = 'panel',
                bottom = 20, left = 20, style="z-index:500; opacity:0.95; background-color:rgba(192,192,192,.5)", draggable = FALSE,
                h3("Choose the characteristics !"),
                sliderInput("competitors", label="Number of competitors ?:",
                            min=0, max=max(new_positions$count_market), value=c(0,835), step=100, width=280),
                checkboxInput("constraint", "Population constraint respected", TRUE),
                checkboxGroupInput("employees", label = "Number of employees:",
                                   choices = list("No employees" = 'NN', "0 employees" = "0", "1 or 2 employees" = 2, "3 to 5 employees" = 3),
                                   selected = c("NN","0","1","2","3"))
  ))


server<-function(input, output, session) {
  
  san<-reactive({
    subset(new_positions,new_positions$count_market>=input$competitors[1]&
             new_positions$count_market<=input$competitors[2]&
             new_positions$EFETCENT %in% input$employees&
             new_positions$constraint == input$constraint)
  })
  
  output$Map <- renderLeaflet({leaflet_project})
  
  observe({
    leafletProxy("Map",data=san()) %>%
      clearMarkers() %>%
      addPolygons(data=New_market_zones,stroke = TRUE,color ="bleu",weight = 1.5, fillOpacity = 0.6, smoothFactor = 0.1,
                  label = paste("IRIS : ",New_market_zones@data$IRIS," // ",New_market_zones@data$market_share_predicted," €",sep=""),
                  highlightOptions = highlightOptions(color = "white", weight = 7,bringToFront = FALSE,fillOpacity = 0.5),
                  labelOptions = labelOptions(noHide = FALSE,direction = 'top',offset=c(0,0),textOnly = TRUE,
                                              style=list('color'='rgba(0,0,0,1)','font-family'= 'Arial Black','font-style'= 'bold',
                                                         'box-shadow' = '0px 0px rgba(0,0,0,0.25)','font-size' = '14px',
                                                         'background-color'='rgba(255,255,255,0.7)','border-color' = 'rgba(0,0,0,0)')))%>%
      addMarkers(lng = new_positions@data$longitude, lat = new_positions@data$latitude,label = unique(New_market_zones@data$pos_id),
                 labelOptions = labelOptions(noHide = FALSE,direction = 'top',offset=c(0,0),textOnly = TRUE,
                                             style=list('color'='rgba(0,0,0,1)','font-family'= 'Arial Black','font-style'= 'bold',
                                                        'box-shadow' = '0px 0px rgba(0,0,0,0.25)','font-size' = '12px',
                                                        'background-color'='rgba(255,255,255,0.7)','border-color' = 'rgba(0,0,0,0)')))%>%
      addCircleMarkers(data = best,color="green", group = "best_worst",fillOpacity = 0.5)%>%
      addCircleMarkers(data = worst,color="red", group = "best_worst",fillOpacity = 0.5)
  })
}

shinyApp(ui = ui, server = server)





