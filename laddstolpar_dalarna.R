library(httr)
library(jsonlite)
library(mapview)
library(tidyr)
library(tidyverse)
library(sf)
library(tmap)
library(dplyr)

source("G:/skript/func/func_diagramfunktioner.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_text.R", encoding = "utf-8", echo = FALSE)

mapp_scbadmgranser <- "G:/Samhällsanalys/GIS/Grundkartor/Adm gränser med kustgränser/"
filnamn_kommuner <- "Kommungränser_SCB_07.shp"

sokvag_kommuner_sv <- paste0(mapp_scbadmgranser, filnamn_kommuner)
output_mapp = "G:/Skript/projekt/gis/laddstolpar/utdata/" 

# ==========================================================================================================

kommuner_sv <- st_read(sokvag_kommuner_sv)
lan_sv <- st_read(sokvag_lan_sv)

#####Här laddas punktlagret med laddstolpar

laddst_sv <- GET("https://nobil.no/api/server/datadump.php?apikey=2b9e9c013e44cd573e8f4b2db6813ad1&countrycode=SWE&fromdate=2012-06-02&format=json&file=false")
laddst_sv_resp <- fromJSON(content(laddst_sv, as = "text"), flatten = FALSE)
laddst_sv_df <- laddst_sv_resp$chargerstations$csmd

# ====================================aggregera anslutningspunkter till kommuner ==============================================

laddstolpar_choropleth <- laddst_sv_df %>% #byter namn på variabler 
  rename(
    namn = name,
    gata = Street,
    gatnr = House_number,
    postnr = Zipcode,
    ort = City,
    kom_kod = Municipality_ID,
    kommun = Municipality,
    lan_kod = County_ID,
    lan = County,
    lages_bskrvng = Description_of_location,
    agare = Owned_by,
    operator = Operator,
    n_ladd_punkt = Available_charging_points,
    kommentar = User_comment,
    kontakt = Contact_info,
    skapad = Created,
    uppdaterad = Updated,
    station_status = Station_status,
  )

#Sparar endast nödvändiga variabler
laddstolpar_choropleth <- laddstolpar_choropleth %>% 
  select(id, namn, gata, gatnr, postnr, ort, kom_kod, lan, lan_kod, 
         kommun, lages_bskrvng, agare, operator, n_ladd_punkt, kommentar, 
         kontakt, skapad, uppdaterad, station_status)

# Filtrerar på Dalarna länskod = 20
laddstolpar_choropleth <- laddstolpar_choropleth %>% filter(lan_kod == '20')

laddstolpar_choropleth <- laddstolpar_choropleth %>% 
  group_by(kom_kod, kommun) %>% 
  summarise(sum_laddstolpar_kom = sum(n_ladd_punkt))

# lägg ihop kommunpolygonerna med laddstolpar per kommun-statistiken
laddstolpar_kommun <- left_join(kommuner_sv, laddstolpar_choropleth, by = c("KNKOD" = "kom_kod"))

laddstolpar_kommun <- laddstolpar_kommun %>% filter(Lanskod_tx == '20')

mapview(laddstolpar_kommun, zcol = "sum_laddstolpar_kom")

#Karta för utskrift
utskrift <- tm_shape(laddstolpar_kommun, projection = 3006) +
  tm_polygons(col = "sum_laddstolpar_kom", 
              palette = diagramfarger("bla_sex")) +
  tm_layout(main.title = paste0("Laddstolpar per kommun "),
            main.title.size = 2,
            main.title.position = "center",
            legend.outside = FALSE,
            legend.position = c(0.02,0.02),              #c("left", "top"),
            #legend.title = 0.8,
            #legend.title.size = 0.8,
            legend.text.size = 0.8,
            frame = FALSE)

tmap_save(tm = utskrift, 
          filename = paste0(output_mapp, "laddstolpar_kommun.png"))



#ta bort parenteser

laddst_sv_df$Position <- gsub("[()]", "", as.character(laddst_sv_df$Position))

#splitta kolumnen position

laddstolpar <- laddst_sv_df %>% separate_wider_delim(Position, ",", names = c("lat", "lon")) #WGS84 Decimal (lat, lon)

laddstolpar_sf = st_as_sf(laddstolpar, coords = c("lon", "lat"), 
                 crs = 4326, agr = "constant")
                                              #gör om till SWEREF99TM?

# Byt namn på variabler
# head(laddstolpar_sf)
# glimpse(laddstolpar_sf)

laddstolpar_rename <- laddstolpar_sf %>% 
  rename(
    namn = name,
    gata = Street,
    gatnr = House_number,
    postnr = Zipcode,
    ort = City,
    kom_kod = Municipality_ID,
    kommun = Municipality,
    lan_kod = County_ID,
    lan = County,
    lages_bskrvng = Description_of_location,
    agare = Owned_by,
    operator = Operator,
    n_ladd_punkt = Available_charging_points,
    kommentar = User_comment,
    kontakt = Contact_info,
    skapad = Created,
    uppdaterad = Updated,
    station_status = Station_status,
  )

# summary(laddstolpar_rename$n_ladd_punkt)

#Sparar endast nödvändiga variabler
laddstolpar <- laddstolpar_rename %>% 
  select(id, namn, gata, gatnr, postnr, ort, kom_kod, lan, lan_kod, 
         kommun, lages_bskrvng, agare, operator, n_ladd_punkt, kommentar, 
         kontakt, skapad, uppdaterad, station_status)

# Filtrerar på Dalarna, länskod = 20
laddstolpar <- laddstolpar %>% filter(lan_kod == '20')

#plot(laddstolpar)

mapview(laddstolpar, zcol = "n_ladd_punkt", legend = TRUE)

# sum_laddstolpar <- sum(laddstolpar$n_ladd_punkt)
# sum_laddstolpar

sum_laddstolpe_kom <- laddstolpar %>% 
  group_by(kom_kod, kommun) %>% 
  summarise(summa_laddstolpar_kom = sum(n_ladd_punkt))

#rm(sum_laddstolpe_kom_komkod, sum_laddstolpe_kommun,meuse)

  #Skapa diagram
# laddstolpar_diagram <- sum_laddstolpe_kom %>% 
#   select(kommun, summa_laddstolpar_kom) %>% 
#   arrange(sum_laddstolpe_kom) %>% 
#   head(15)
# 
# laddstolpar_diagram %>% 
#   ggplot(aes(x = kommun, y = summa_laddstolpar_kom, color = summa_laddstolpar_kom))+
#   geom_col()

ggplot(sum_laddstolpe_kom,
       aes(x = reorder(kommun, -summa_laddstolpar_kom),
           y = summa_laddstolpar_kom),
       palette = diagramfarger())+
  geom_col()+
  labs(
    title = "Laddstolpar per kommun i Dalarna",
    caption = "Källa: NOBIL",
    y = "Antal anslutningar/laddstolpar",
    x = "Kommuner")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95)
  )

# För att göra flera lager ovanpå varandra i samma karta, se 
# https://bookdown.org/nicohahn/making_maps_with_r5/docs/mapview.html
# To visualize several maps at the same time, several mapview() 
# functions can be connected with +. This way the European elevation 
# raster map from chapter 2 can be reproduced. We use alpha.regions 
# so that only the outlines of the countries are visible, and make 
# the size of the circles dependent on the pop variable by using the cex argument.
# 
# mapview(europe_raster, legend = FALSE) +
#   mapview(europe_shape, legend = FALSE, alpha.regions = 0) +
#   mapview(cities, legend = FALSE, cex = "pop")

mapview(laddstolpar_kommun, zcol = "sum_laddstolpar_kom")+
  mapview(laddstolpar, zcol = "n_ladd_punkt", legend = TRUE)

#Ett försök med leaflet från file:///G:/skript/gis/dalarna.html

library(leaflet)

# # Följande felmeddelande betyder antagligen att: först convertera till SWEREF99TM
# Warning messages:
#   1: sf layer is not long-lat data 
# 2: sf layer has inconsistent datum (+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs).
# Need '+proj=longlat +datum=WGS84' 

pal <- colorNumeric(
  palette = "Blues",
  domain = laddstolpar_kommun$sum_laddstolpar_kom)



leaflet(laddstolpar_kommun) %>%
  addTiles(urlTemplate = 'http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png') %>%
  addPolygons(fillColor = ~pal(sum_laddstolpar_kom),
              fillOpacity = 0.9,
              popup = ~paste(sum_laddstolpar_kom, "hållplatser finns i", kommun, "kommun")) %>%   
  addLegend(pal = pal, 
            values = ~sum_laddstolpar_kom, 
            # labFormat = labelFormat(suffix = "%",
            #                         transform = function(x) 100 * x),
            title = "Laddstolpar", position = "bottomright")
