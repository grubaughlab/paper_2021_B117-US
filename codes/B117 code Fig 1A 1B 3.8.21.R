library(maps)
library(readxl)
library(tigris)
library(tidyverse)
library(sf)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
library(ggrepel)
library(ggpubr)
setwd("/Users/Jessica/Documents/Yale/Grubaugh lab")

#### Read in UK data ####
UK<-read_excel("UK_SA_BR_to_US_Dec.xlsx", sheet = "UK to US cities")
per_airport<-read_excel("UK_SA_BR_to_US_Dec.xlsx", sheet = "Passengers_per_airport")
UK<-subset(UK,`Percent of Total Passengers`>1 )
UK.subset<-cbind.data.frame(UK$Airport_short,UK$Lon, UK$Lat)
UK.subset<-UK.subset %>% rename(name=`UK$Airport_short`,  lon=`UK$Lon`,lat=`UK$Lat`)

#### Import counties shapefile ####
test<-counties(state = NULL, cb = FALSE, resolution = "500k", year = NULL)
test<-st_as_sf(test)

#### Create a distance matrix with the distances between each airport and each county in the US in km####
UK.subset2<-st_as_sf(UK.subset[2:3],coords = c("lon", "lat"))
UK.subset2<-UK.subset2 %>% st_set_crs(st_crs(test[18]))
UK.dist.mat2<-st_distance(test[18],UK.subset2,by_element = FALSE)
UK.dist.mat2<-as.data.frame(UK.dist.mat2)*0.001
row.names(UK.dist.mat2)<-test$GEOID
colnames(UK.dist.mat2)<-UK.subset$name
UK.dist.mat2<-drop_units(UK.dist.mat2)

#### Subset for each airport the counties that are within 200km ####
UK.dist.JFK<-UK.dist.mat2%>%select(`New York JFK`)
UK.dist.JFK<-as.data.frame(ifelse(UK.dist.JFK<=200,UK.dist.JFK$`New York JFK`, NA ))
UK.dist.ohare<-UK.dist.mat2%>%select(`Chicago O'Hare`)
UK.dist.ohare<-as.data.frame(ifelse(UK.dist.ohare<=200,UK.dist.ohare$`Chicago O'Hare`, NA ))
UK.dist.newark<-UK.dist.mat2%>%select(`Newark Liberty`)
UK.dist.newark<-as.data.frame(ifelse(UK.dist.newark<=200,UK.dist.newark$`Newark Liberty`, NA ))
UK.dist.miami<-UK.dist.mat2%>%select(Miami)
UK.dist.miami<-as.data.frame(ifelse(UK.dist.miami<=200,UK.dist.miami$Miami, NA ))
UK.dist.LAX<-UK.dist.mat2%>%select(`Los Angeles`)
UK.dist.LAX<-as.data.frame(ifelse(UK.dist.LAX<=200,UK.dist.LAX$`Los Angeles`, NA ))
UK.dist.logan<-UK.dist.mat2%>%select(`Boston Logan`)
UK.dist.logan<-as.data.frame(ifelse(UK.dist.logan<=200,UK.dist.logan$`Boston Logan`, NA ))
UK.dist.SFO<-UK.dist.mat2%>%select(`San Francisco`)
UK.dist.SFO<-as.data.frame(ifelse(UK.dist.SFO<=200,UK.dist.SFO$`San Francisco`, NA ))
UK.dist.dulles<-UK.dist.mat2%>%select(`Washington Dulles`)
UK.dist.dulles<-as.data.frame(ifelse(UK.dist.dulles<=200,UK.dist.dulles$`Washington Dulles`, NA ))
UK.dist.atlanta<-UK.dist.mat2%>%select(`Atlanta Hartsfield-Jackson`)
UK.dist.atlanta<-as.data.frame(ifelse(UK.dist.atlanta<=200,UK.dist.atlanta$`Atlanta Hartsfield-Jackson`, NA ))
UK.dist.dallas<-UK.dist.mat2%>%select(`Dallas Fort Worth`)
UK.dist.dallas<-as.data.frame(ifelse(UK.dist.dallas<=200,UK.dist.dallas$`Dallas Fort Worth`, NA ))
UK.dist.houston<-UK.dist.mat2%>%select(`Houston Bush`)
UK.dist.houston<-as.data.frame(ifelse(UK.dist.houston<=200,UK.dist.houston$`Houston Bush`, NA ))
UK.dist.vegas<-UK.dist.mat2%>%select(`Las Vegas McCarran`)
UK.dist.vegas<-as.data.frame(ifelse(UK.dist.vegas<=200,UK.dist.vegas$`Las Vegas McCarran`, NA ))
UK.dist.seattle<-UK.dist.mat2%>%select(`Seattle Tacoma`)
UK.dist.seattle<-as.data.frame(ifelse(UK.dist.seattle<=200,UK.dist.seattle$`Seattle Tacoma`, NA ))
UK.dist.orlando<-UK.dist.mat2%>%select(Orlando)
UK.dist.orlando<-as.data.frame(ifelse(UK.dist.orlando<=200,UK.dist.orlando$Orlando, NA ))
UK.dist.denver<-UK.dist.mat2%>%select(Denver)
UK.dist.denver<-as.data.frame(ifelse(UK.dist.denver<=200,UK.dist.denver$Denver, NA ))

#### combine airport distance dataframes ####
UK.dist.airports<-cbind.data.frame(UK.dist.JFK, UK.dist.LAX,UK.dist.newark,UK.dist.logan,UK.dist.dulles,UK.dist.miami,
                                   UK.dist.ohare,UK.dist.SFO,UK.dist.dallas,
                                   UK.dist.atlanta, UK.dist.houston, UK.dist.orlando,
                                   UK.dist.vegas, UK.dist.seattle, UK.dist.denver)

#### Structure data for Huff model ####
test4<-as.data.frame(t(UK$`Number of Passengers Arriving`))
colnames(test4)<-UK$Airport_short

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
test3<-as.data.frame(rep.row(as.numeric(test4),3233))
row.names(test3)<-test$GEOID
colnames(test3)<-UK.subset$name
UK.dist.airports[UK.dist.airports<1] <- 1
UK.dist.airports$Miami<-as.data.frame(ifelse(UK.dist.airports$Miami<5, 10, UK.dist.airports$Miami))
UK.dist.airports$`Los Angeles`<-as.data.frame(ifelse(UK.dist.airports$`Los Angeles`<5, 10, UK.dist.airports$`Los Angeles`))
UK.dist.airports$`San Francisco`<-as.data.frame(ifelse(UK.dist.airports$`San Francisco`<5, 5, UK.dist.airports$`San Francisco`))
UK.dist.airports$`Houston Bush`<-as.data.frame(ifelse(UK.dist.airports$`Houston Bush`<5,5 , UK.dist.airports$`Houston Bush`))
UK.dist.airports$`Seattle Tacoma`<-as.data.frame(ifelse(UK.dist.airports$`Seattle Tacoma`<5,10 , UK.dist.airports$`Seattle Tacoma`))
UK.dist.airports$`Las Vegas McCarran`<-as.data.frame(ifelse(UK.dist.airports$`Las Vegas McCarran`<5,10 , UK.dist.airports$`Las Vegas McCarran`))
UK.dist.airports$Orlando<-as.data.frame(ifelse(UK.dist.airports$Orlando<5,5 , UK.dist.airports$Orlando))
UK.dist.airports$Denver<-as.data.frame(ifelse(UK.dist.airports$Denver<4,2 , UK.dist.airports$Denver))
UK.dist.airports$`Boston Logan`<-as.data.frame(ifelse(UK.dist.airports$`Boston Logan`<4,2 , UK.dist.airports$`Boston Logan`))
colnames(UK.dist.airports)<-UK$Airport_short
UK.numerator<-test3/(UK.dist.airports^2)
UK.numerator[is.na(UK.numerator)] <- 0
UK.denominator<-as.data.frame(t(colSums(UK.numerator)))
UK.denom2<-as.data.frame(rep.row(as.numeric(UK.denominator),3233))
row.names(UK.denom2)<-test$GEOID
colnames(UK.denom2)<-UK.subset$name

#### Huff model probabilities####
UK.pij<-UK.numerator/UK.denom2
UK.pij<-tibble::rownames_to_column(UK.pij, var = "county")


#### Create Figure 1A for ALL AIRPORTS ####
pass1<-cbind.data.frame(UK$Airport_short, UK$`Number of Passengers Arriving`)
pass<-as.data.frame(t(pass1[,-1]))
colnames(pass) <- pass1$`UK$Airport_short`
num.passengers<-as.data.frame(rep.row(as.numeric(pass),3233))
colnames(num.passengers) <- pass1$`UK$Airport_short`
sum(UK$`Number of Passengers Arriving`)
row.names(UK.pij)<-test$GEOID
UK.pij.all<-UK.pij[,-1]*num.passengers
county.totals<-as.data.frame(rowSums(UK.pij.all))
colnames(county.totals)<-"Number of Passengers"


UK.pij.all3<-tibble::rownames_to_column(county.totals, var = "county")
UK.pij.all3[UK.pij.all3==0] <- NA
UK.pij.all3<-na.omit(UK.pij.all3)
UK.pij.all3<-UK.pij.all3 %>% rename(value="Number of Passengers", county="county")
data(county.regions)
county.all <- left_join(UK.pij.all3, county.regions, by = c("county" = "county.fips.character"))
county.all$value<-cut(county.all$value,  c(0, 1, 1000,2000, 3000, 4000, 5000,6000))
county.map.all <- county_choropleth(county.all,
                                    legend = "Number of Passengers",
                                    state_zoom = c("alabama", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", 
                                                   "district of columbia", "florida", "georgia", "idaho", "illinois", "indiana", "iowa", "kansas", 
                                                   "kentucky", "louisiana", "maine", "maryland", "massachusetts", "michigan", "minnesota", 
                                                   "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire", "new jersey", 
                                                   "new mexico", "new york", "north carolina", "north dakota",  "ohio", "oklahoma", "oregon",
                                                   "pennsylvania", "rhode island", "south carolina", "south dakota", "tennessee", "texas", 
                                                   "utah", "vermont", "virginia", "washington", "west virginia", "wisconsin", "wyoming"))+
  geom_point(data = UK, mapping = aes(Lon, Lat),  color="black", size = 0.1, inherit.aes = FALSE)+
  geom_text_repel(data=UK,aes(x=Lon, y=Lat, label = `Airport_short`),fontface = 'bold',box.padding   = 2.25, point.padding = 0.25,segment.color = "grey50", inherit.aes = FALSE)+
  scale_fill_brewer(name = "Number of Passengers", palette = "YlOrRd", na.value="white",drop=FALSE)
county.map.all

#### Create Figure 1B ####
UK.subset.4<-UK[c(1,3),]
county.regions.northeast <- filter(county.regions, state.name == "new jersey"| state.name=="new york"|
                                     state.name=="connecticut")
county.northeast <- left_join(county.regions.northeast,UK.pij.all3,  by = c("county.fips.character" = "county"))
county.northeast$value<-cut(county.northeast$value,  c(0, 1, 1000, 2000, 3000, 4000, 5000,6000))
county.map.northeast<- county_choropleth(county.northeast, 
                                         legend = "Number of Passengers",
                                         state_zoom = c( "connecticut", "new jersey", "new york"))+
  geom_point(data = UK.subset.4, mapping = aes(Lon, Lat),  color="black", size = 0.1, inherit.aes = FALSE)+
  geom_text_repel(data=UK.subset.4,aes(x=Lon, y=Lat, label = `Airport_short`),fontface = 'bold',box.padding   = 4.5, point.padding = 0.25,segment.color = "grey50", inherit.aes = FALSE)+
  scale_fill_brewer(name = "Number of Passengers", palette = "YlOrRd", na.value="white",drop=FALSE)+theme(legend.position = "none")
county.map.northeast

#### Extract legend####
legend.UK <- get_legend(county.map.all)
# Convert to a ggplot and print
as_ggplot(legend.UK)


