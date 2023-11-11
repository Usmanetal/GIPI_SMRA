library(readxl)
ROB <- read_excel("~/Sample Meta_Data on prevalence of GIPI in HIV Disease.xlsx", 
                  sheet = "ROB")
head(ROB)

library(readxl)
GIPI_sample <- read_excel("~/Sample Meta_Data on prevalence of GIPI in HIV Disease.xlsx", 
                          sheet = "Sheet2")
head(GIPI_sample)

library(raster)
library(tidyverse)
library(geoR)
library(rgdal)
library(sf)
library(viridis)

#Using "apply",lapply,sapply,mapply function to "function" in R
#mpg_category<-function(mpg){
 # if(mpg > 30){
  #  return("High")
#} else if(mpg > 20){
 # return("Medium")
#}

#return("Low")
#}

#lapply(data$mpg, mpg_category)

B<-vector()

prevalence<- for (i in 1:37) {
  B[c(12,14,30,34)]<-c(0.05,0,0.9,0.4)
  B[i]<-B[i]
  print(B)
}

GIPI_sample[-c(1,5:8),]%>% 
  select(`First Author...6`,`Publication Year`,
         Region,`positive parasitic infection (n)`,`Study Population`)%>%
  rename(Author=`First Author...6`,
         Year=`Publication Year`,
         Positive=`positive parasitic infection (n)`,
        Total=`Study Population`)%>%
  group_by(Region)%>%
  summarise(x=sum(Positive,na.rm= TRUE),y=sum(Total,na.rm= TRUE))%>%
  mutate(y=recode(y,"3211"=285))%>%mutate(z=x/y)
 
Nig<-getData('GADM', country='Ng', level=1)
Nig1<-Nig
Nig1@data$prevalence<-B
num<-c(1:37)
Nig1@data$num<-num

map <- st_as_sf(Nig1)
ggplot(map) + geom_sf(aes(fill = prevalence)) +
  theme_classic()+scale_fill_viridis(na.value="white")
#%>%  addScaleBar(position = c("bottomleft"))

# Zoom in and out of a map plot
ggplot(map) + geom_sf(aes(fill = prevalence)) +
  theme_classic()+scale_fill_viridis(na.value="white")+
  coord_sf(xlim = c(7,4.3),ylim=c(14.1,11.3),expand = FALSE)

library(rvest)
library(usethis)
library(tidygeocoder)

url<-"https://en.wikipedia.org/wiki/List_of_Nigerian_states_by_population"
population<-url%>%
  read_html%>%
  html_element(".wikitable")%>%
  html_table()
#population%>%select(`Population (2019)`)%>%
 # mutate(test=sub(",","",population$`Population (2019)`))


population%>%select(`Population (2019)`)%>%
  mutate(test=as.numeric(str_replace_all(population$`Population (2019)`,",","")))

Nig1$idarea <- 1:nrow(Nig1@data)
Nig1$idarea2 <- 1:nrow(Nig1@data)
formula <- prevalence ~ alt +f(idarea, model = "besag", graph = g, 
                               scale.model  = TRUE)+
                           f(idarea2, model = "iid")
res <- inla(formula,
            family = "poisson", data = Nig1@data,
            E = prevalence, control.predictor = list(compute = TRUE)
)

prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3))
)

library(spdep)
library(INLA)
nb <- poly2nb(Nig1)
head(nb)

nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

library(spdep)
# nb <- poly2nb(map1)
# head(nb)

## Modelling using INLA
# to get the Latitude and longitude coordinates for Nigeria
Nig1@data[,c("long", "lat")]<-coordinates(Nig)
Nig1@data$alt<-raster::extract(alt, Nig1@data[,c("long", "lat")])
#library(leaflet)
#library(viridis)

pal <- colorBin("viridis", bins = c(0, 0.25, 0.5, 0.75, 1))
leaflet(Nig1) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~long, lat = ~lat, color = ~ pal(prevalence)) %>%
  addLegend("bottomright",
            pal = pal, values = ~prevalence,
            title = "Prev."
  ) %>%
  addScaleBar(position = c("bottomleft"))



# Environmental covariates
#To model GIPI prevalence using a covariate that indicates the altitude in
#Nigeria

pal1 <- colorNumeric("viridis", values(alt),
                    na.color = "transparent"
)
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(alt, colors = pal1, opacity = 0.5) %>%
  addLegend("bottomright",
            pal = pal1, values = values(alt),title = "Altitude"
  ) %>%
  addScaleBar(position = c("bottomleft"))

library(INLA)
coo <- cbind(Nig1@data$long, Nig1@data$lat)
mesh <- inla.mesh.2d(
  loc = coo, max.edge = c(0.1, 5),
  cutoff = 0.01
)
mesh$n
plot(mesh)
points(coo, col = "red")
