#Load packages
library(sf)
library(ggplot2)
library(dplyr)

#Load the shapefile and subset Nairobi county
kenya <- read_sf("gadm41_KEN_2.json")
nairobi <- subset(kenya, NAME_1 == "Nairobi")
plot(nairobi["NAME_2"], main = "Nairobi County Constituencies")

#Highlight the study sites
#Define the study sites
study_sites <- c("EmbakasiWest", "Kamukunji", "Kibra")

#Create a new column that labels constituencies
nairobi <- nairobi %>% 
  mutate(StudySite = case_when(NAME_2 == "EmbakasiWest" ~ "Embakasi West", 
                               NAME_2 == "Kamukunji" ~ "Kamukunji", 
                               NAME_2 == "Kibra" ~ "Kibra", 
                               TRUE ~ "Other"))
#Choose custom colours
site_colours <- c("Embakasi West" = "red", 
                  "Kamukunji" = "blue", 
                  "Kibra" = "green", 
                  "Other" = "lightgray")

#Plot the map
ggplot(nairobi) +
  geom_sf(aes(fill = StudySite), color = "black", size = 0.2) +
  scale_fill_manual(values = site_colours, name = "Constituency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size =14), 
        legend.title = element_text(face = "bold"), 
        legend.position = "right")

#Plot Kilifi County
#Load Kenya level 3 shapefile and subset Kilifi
kenya3 <- read_sf("gadm41_KEN_3.json")
kilifi <- subset(kenya3, NAME_1 == "Kilifi")

#Load the shapefile of Kenyan rivers
rivers <- st_read("ke_major-rivers/ke_major-rivers.shp")

#Merge the Kilifi and river network shapefiles
st_crs(kilifi)
st_crs(rivers)
rivers <- st_transform(rivers, crs = st_crs(kilifi))
kilifi_rivers <- st_intersection(rivers, kilifi)

#Create a data.frame of prevalence data
fgs_subcounty <- data.frame(SubCounty = c("Magarini", "Rabai"), 
                             SubCountyPrevalence = c(43.3, 26.1))
write.csv(fgs_subcounty, "fgs_prevalence_subcounty.csv")

fgs_ward <- data.frame(Ward = c("Magarini", "Sabaki", "Ruruma", "Garashi"), 
                             WardPrevalence = c(44.4, 56.7, 26.1, 19.0))
write.csv(fgs_ward, "fgs_prevalence_ward.csv")

fgs_location <- data.frame(Location = c("Marikebuni", "Sabaki", "Pumwani", "Jimba", 
                                          "Mleji", "Garashi"), 
                             LocationPrevalence = c(68.6, 56.7, 31.3, 26.8, 25.0, 19.0))
write.csv(fgs_location, "fgs_prevalence_location.csv")

#Merge the FGS prevalence data with the geospatial data
kilifi_subcounty <- left_join(kilifi, fgs_subcounty, 
                                     by = c("NAME_2" = "SubCounty"))
kilifi_ward <- left_join(kilifi, fgs_ward, 
                                by = c("NAME_3" = "Ward"))
#Plot map of FGS prevalence at the sub-county level
ggplot() +
  geom_sf(data = kilifi_subcounty, aes(fill = SubCountyPrevalence), color = "black") +
  geom_sf(data = kilifi_rivers, color = "steelblue", size = 0.5) +
  scale_fill_viridis_c(option = "plasma", name = "FGS Prevalence (%)", na.value = "lightgray") +
  theme_bw() +
  labs(caption = "Rivers from World Resources Institute | Gray areas = No data")

#Plot map of FGS prevalence at the ward level
ggplot() +
  geom_sf(data = kilifi_ward, aes(fill = WardPrevalence), color = "black") +
  geom_sf(data = kilifi_rivers, color = "steelblue", size = 0.5) +
  scale_fill_viridis_c(option = "plasma", name = "FGS Prevalence (%)", na.value = "lightgray") +
  theme_bw() +
  labs(caption = "Rivers from World Resources Institute | Gray areas = No data")

#Plot a map showing the wards in Kilifi county
#Define the study sites
kilifi <- kilifi %>% 
  mutate(StudySite = case_when(NAME_3 == "Magarini" ~ "Magarini", 
                               NAME_3 == "Sabaki" ~ "Sabaki", 
                               NAME_3 == "Ruruma" ~ "Ruruma", 
                               NAME_3 == "Garashi" ~ "Garashi", 
                               TRUE ~ "Other"))

#Define site colours
site_colours <- c("Garashi" = "pink", "Magarini" = "blue", 
                  "Ruruma" = "yellow", "Sabaki" = "orange")

ggplot(kilifi) +
  geom_sf(aes(fill = StudySite), color = "black", size = 0.2) +
  scale_fill_manual(values = site_colours, name = "Ward") +
  theme_bw() +
  theme(legend.title = element_text(face = "bold"))

#Plot sub-county map highlighting study sites
kilifi <- kilifi %>% 
  mutate(StudySiteSubcounty = case_when(NAME_2 == "Magarini" ~ "Magarini", 
                                        NAME_2 == "Rabai" ~ "Rabai", 
                                        TRUE ~ "Other"))
#Define site colours
site_colours_subcounty <- c("Magarini" = "red", "Rabai" = "cyan")

ggplot(kilifi) +
  geom_sf(aes(fill = StudySiteSubcounty), color = "black", size = 0.2) +
  scale_fill_manual(values = site_colours_subcounty, name = "Sub-county") +
  theme_bw() +
  theme(legend.title = element_text(face = "bold"))
