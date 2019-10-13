ACLED.Africa <- read.csv('ACLEDAfrica.csv')
African.Pop <- read.csv('africanPop.csv')
African.GDP <- read.csv('africanGDP.csv')
#Data Exploration
summary(ACLED.Africa)

#What countries are missing?
Afr.ctrs <- read.csv('CountryList.csv')
setdiff(Afr.ctrs$Country, ACLED.Africa$COUNTRY)


#Check all factor levels

levels(ACLED.Africa$COUNTRY)

levels(ACLED.Africa$EVENT_TYPE)
#One value mistakenly uppercase (Violence Against Civilians), fix tis
library(forcats)
ACLED.Africa$EVENT_TYPE <- fct_collapse(ACLED.Africa$EVENT_TYPE, "Violence against civilians" = c("Violence against civilians",
                                                            "Violence Against Civilians"))
table(ACLED.Africa$EVENT_TYPE)

#Fix Population and GDP data
setdiff(ACLED.Africa$COUNTRY,intersect(ACLED.Africa$COUNTRY,African.Pop$Country.Name))
setdiff(ACLED.Africa$COUNTRY,intersect(ACLED.Africa$COUNTRY,African.GDP$Country.Name))

library(ggplot2)
ggplot(ACLED.Africa, aes(EVENT_TYPE))  + geom_bar()  +coord_flip()
ggplot(ACLED.Africa, aes(REGION))  + geom_bar()  +coord_flip()

#Find top actors
ACTORS <- table(c(as.vector(ACLED.Africa$ACTOR1), as.vector(ACLED.Africa$ACTOR2)))
tail(ACTORS[order(ACTORS)], 10)

#Show GDP trend
African.GDP <- African.GDP[African.GDP$Country.Name %in% levels(ACLED.Africa$COUNTRY),c("Country.Name",sapply(1997:2017,function(x) paste("X",as.character(x),sep="")))]
colnames(African.GDP)
library(reshape2)
library(dplyr)
African.GDP <- melt(African.GDP)
colnames(African.GDP) <- c("Country", "Year", "GDP")
African.GDP$Year <- sapply(African.GDP$Year, function(x) as.numeric(substring(x, 2)))
ggplot(African.GDP, aes(Year, GDP,color=Country))  + geom_line(aes(group=Country))
ggplot(African.GDP[African.GDP$Country=="Nigeria",], aes(Year, GDP,color=Country))  + geom_line(aes(group=Country))
#Show population trend
African.Pop <- African.Pop[African.Pop$Country.Name %in% levels(ACLED.Africa$COUNTRY),c("Country.Name",sapply(1997:2017,function(x) paste("X",as.character(x),sep="")))]
colnames(African.Pop)
library(reshape2)

library(dplyr)
African.Pop <- melt(African.Pop)
colnames(African.Pop) <- c("Country", "Year", "Population")
African.Pop$variable <- sapply(African.Pop$variable, function(x) as.numeric(substring(x, 2)))
ggplot(African.Pop, aes(Year, Population,color=Country))  + geom_line(aes(group=Country))
ggplot(African.Pop[African.Pop$Country=="Nigeria",], aes(Year, Population,color=Country))  + geom_line(aes(group=Country))    

#Looking at fatalities
ggplot(ACLED.Africa, aes(REGION, FATALITIES)) + geom_point(position="jitter", colour="red")
library(reshape)
#Table of conflict by country
Country.Conflict <- cast(ACLED.Africa, COUNTRY ~ EVENT_TYPE, fill=FALSE)
write.csv(Country.Conflict, file="AfricaConflicts.csv")

#Test Map Data

#Dependencies
library(maptools)
library(gpclib)
library(ggmap)
library(rgdal)
gpclibPermit()

#Build Map
#africa = readOGR("Africa.shp")
#africa$COUNTRY
#africa.map = fortify(africa, region="COUNTRY")
#https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/
world = readOGR("ne_50m_admin_0_countries.shp")


africa.border = fortify(world, region="CONTINENT")
africa.border = africa.border[africa.border$id == "Africa",]

africa = world[world$CONTINENT == 'Africa',]
africa.region.map = fortify(africa, region="SUBREGION")
africa.country.map = fortify(africa, region="NAME")
#africa.map1 =  africa.map[africa.map$id == 'Sudan', ]
#africa.map1$color = "orange"

map <- ggplot() + geom_polygon(data = africa.region.map, aes(x = long, y = lat, group = group, fill = id),
                               size = .2) + geom_polygon(data = africa.country.map, aes(x = long, y = lat, group = group), fill = NA,
                                                                          color="gray", size = .2) + ggtitle("Conflicts Over Time") +
                              geom_polygon(data = africa.border, aes(x = long, y = lat, group = group), fill = NA,
                                                                                                                  color="black", size = .3) + 
                              scale_fill_manual(values=c("#ffb3ba", "#ffdfba", "#ffffba", "#baffc9", "#bae1ff"),guide=FALSE)+theme_bw()+
                              theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                                    axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                                    panel.border = element_blank(), panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5))
  
print(map) 

ACLED.A2013 = ACLED.Africa[ACLED.Africa$YEAR>=2013 & ACLED.Africa$YEAR<=2018,]
ACLED.A2013$EVENT_DATE = as.Date(ACLED.A2013$EVENT_DATE, "%d-%B-%Y")

conflicts <- geom_point(data = ACLED.A2013, aes(x = LONGITUDE, y = LATITUDE, color=as.factor(YEAR)), size = 0.1, shape = 21)

print(map + conflicts)

map_projected <- map + conflicts + scale_color_manual(values=c("#E473FF", "#DD4CFF", "#C523EB", "#9F00C5", "#80009F","#610078"),guide=FALSE) + 
  coord_map()

#Dashboard map-------------------------------------------------------------------------------------
#Need to add year scale legend
print(map_projected)

#Event linegraph--------------------------------------------------------------------------------------------
ACLED.Months <- ACLED.A2013
ggplot(data=ACLED.Months, aes(x=EVENT_DATE, fill=EVENT_TYPE)) + geom_histogram(bins=400) + ggtitle("Frequency of Conflicts Over Time") +
  theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5),
                                                                                                              panel.border = element_blank(), legend.position="bottom",
                                                                                                              legend.text=element_text(size=15)) + 
  guides(fill=guide_legend(title="Event Type", nrow=3))
#Event region barchart---------------------------------------------------------------------
ggplot(data=ACLED.Months, aes(x=REGION, fill=REGION)) + geom_bar(stat="count") + theme_bw() + ggtitle("Frequency of Conflicts by Region") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.border = element_blank(),  panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=14)) +
  scale_fill_manual(values=c("#ff7380", "#ffc582", "#ffff7f", "#7bff98", "#7dc6ff"),guide=FALSE) +
  scale_x_discrete(labels=c("Eastern\nAfrica", "Middle\nAfrica", "Northern\nAfrica", "Southern\nAfrica", "Western\nAfrica"))

#East Africa Top actors------------------------------------------------------------------------------------------------------
Act.data <- ACLED.A2013
ACTORS <- table(c(as.vector(Act.data[Act.data$REGION == "Eastern Africa",]$ACTOR1), as.vector(Act.data[Act.data$REGION == "Eastern Africa",]$ACTOR2)))

#Remove missing value
ACTORS <- tail(ACTORS[order(ACTORS)])
ACTORS <- ACTORS[c(FALSE,FALSE,TRUE,TRUE,FALSE,TRUE)]

east.actors <- as.data.frame(ACTORS)
colnames(east.actors) <- c("Actor", "Conflicts")
levels(east.actors$Actor)[1] <- "Unidentified Armed\n Group (Somalia)"
ggplot(east.actors, aes(x=Actor, y=Conflicts)) + geom_bar(stat="identity", fill="#ff7380") + coord_flip() + labs(x = "Eastern Actors") +
  geom_text(aes(label = Actor),
            position = position_stack(vjust = 0.5)) + theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
                                                                         panel.grid.major.y = element_blank(), panel.border = element_blank()) + 
  guides(fill=FALSE)

#Middle Africa Top actors ----------------------------------------------------
Act.data <- ACLED.A2013
ACTORS <- table(c(as.vector(Act.data[Act.data$REGION == "Middle Africa",]$ACTOR1), as.vector(Act.data[Act.data$REGION == "Middle Africa",]$ACTOR2)))
#Remove missing value
ACTORS <- tail(ACTORS[order(ACTORS)])
ACTORS <- ACTORS[c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE)]

middle.actors <- as.data.frame(ACTORS)

colnames(middle.actors) <- c("Actor", "Conflicts")
levels(middle.actors$Actor)[2] <- "Military Forces of Democratic\n Republic of Congo (2001-)"
ggplot(middle.actors, aes(x=Actor, y=Conflicts)) + geom_bar(stat="identity",fill="#ffc582") + coord_flip() + labs(x = "Middle Actors") +
  geom_text(aes(label = Actor),
            position = position_stack(vjust = 0.5)) + theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
                                                                         panel.grid.major.y = element_blank(), panel.border = element_blank()) + guides(fill=FALSE)

#Northern Africa Top actors----------------------------------------------------------------
Act.data <- ACLED.A2013
ACTORS <- table(c(as.vector(Act.data[Act.data$REGION == "Northern Africa",]$ACTOR1), as.vector(Act.data[Act.data$REGION == "Northern Africa",]$ACTOR2)))
#Remove missing value
ACTORS <- tail(ACTORS[order(ACTORS)])
ACTORS <- ACTORS[c(FALSE,FALSE,TRUE,TRUE,TRUE,FALSE)]

northern.actors <- as.data.frame(ACTORS)
colnames(northern.actors) <- c("Actor", "Conflicts")
levels(northern.actors$Actor)[1] <- "Unidentified Armed\n Group (Libya)"
ggplot(northern.actors, aes(x=Actor, y=Conflicts)) + geom_bar(stat="identity",fill="#ffff7f") + coord_flip() + labs(x = "Northern Actors") +
  geom_text(aes(label = Actor),
            position = position_stack(vjust = 0.5)) + theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
                                                                         panel.grid.major.y = element_blank(), panel.border = element_blank()) + guides(fill=FALSE)

#Southern Africa Top actors---------------------------------------------------------------------------
Act.data <- ACLED.A2013
ACTORS <- table(c(as.vector(Act.data[Act.data$REGION == "Southern Africa",]$ACTOR1), as.vector(Act.data[Act.data$REGION == "Southern Africa",]$ACTOR2)))
#Remove missing value
ACTORS <- tail(ACTORS[order(ACTORS)])
ACTORS <- ACTORS[c(FALSE,FALSE,TRUE,TRUE,TRUE,FALSE)]

southern.actors <- as.data.frame(ACTORS)
colnames(southern.actors) <- c("Actor", "Conflicts")
levels(southern.actors$Actor)[1] <- "Police Forces of\n South Africa (1994-)"
ggplot(southern.actors, aes(x=Actor, y=Conflicts)) + geom_bar(stat="identity",fill="#7bff98") + coord_flip() + labs(x = "Southern Actors") +
  geom_text(aes(label = Actor),
            position = position_stack(vjust = 0.5)) + theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
                                                                         panel.grid.major.y = element_blank(), panel.border = element_blank()) + guides(fill=FALSE)

#Western Africa Top actors-----------------------------------------------------------------------------------------
Act.data <- ACLED.A2013
ACTORS <- table(c(as.vector(Act.data[Act.data$REGION == "Western Africa",]$ACTOR1), as.vector(Act.data[Act.data$REGION == "Western Africa",]$ACTOR2)))
#Remove missing value
ACTORS <- tail(ACTORS[order(ACTORS)])
ACTORS <- ACTORS[c(FALSE,FALSE,TRUE,TRUE,TRUE,FALSE)]

western.actors <- as.data.frame(ACTORS)
colnames(western.actors) <- c("Actor", "Conflicts")
levels(western.actors$Actor)[1] <- "Boko Haram -\n Wilayat Gharb Ifriqiyyah"
ggplot(western.actors, aes(x=Actor, y=Conflicts)) + geom_bar(stat="identity",fill="#7dc6ff") + coord_flip() + labs(x = "Western Actors") +
  geom_text(aes(label = Actor),
            position = position_stack(vjust = 0.5)) + theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
                                                                         panel.grid.major.y = element_blank(), panel.border = element_blank()) + guides(fill=FALSE)
#DATA FIX-----------------------------------------------------------------------------------------------------------------------------
ACLED.A2013[ACLED.A2013$COUNTRY=="Zambia",]$REGION <- "Eastern Africa"
ACLED.A2013[ACLED.A2013$COUNTRY=="Zimbabwe",]$REGION <- "Eastern Africa"

#Eastern Africa Info------------------------------------------------------------------------------------------------------
eastern.africa <- africa[africa$SUBREGION == "Eastern Africa",]
eastern.africa.countries <- fortify(eastern.africa, region="NAME")
eastern.africa <- fortify(eastern.africa, region="SUBREGION")
eastern.map <- ggplot() + geom_polygon(data = eastern.africa.countries, aes(x = long, y = lat, group = group), fill="#ffb3ba",
                               size = .2) + geom_polygon(data = eastern.africa.countries, aes(x = long, y = lat, group = group), fill = NA,
                                                         color="gray", size = .2) +
  geom_polygon(data = eastern.africa, aes(x = long, y = lat, group = group), fill = NA,
               color="black", size = .3) + theme_bw()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5))

conflicts.types <- geom_point(data = ACLED.A2013[ACLED.A2013$REGION=="Eastern Africa",], aes(x = LONGITUDE, y = LATITUDE, color=EVENT_TYPE), size = 0.5, shape = 21)

map_projected <- eastern.map + conflicts.types + guides(fill=FALSE)  + guides(color=guide_legend(title="Event Type")) +
  coord_map()

print(map_projected)

#Middle Africa Info 700x600------------------------------------------------------------------------------------------------------
middle.africa <- africa[africa$SUBREGION == "Middle Africa",]
middle.africa.countries <- fortify(middle.africa, region="NAME")
middle.africa <- fortify(middle.africa, region="SUBREGION")
middle.map <- ggplot() + geom_polygon(data = middle.africa.countries, aes(x = long, y = lat, group = group), fill="#ffdfba",
                                       size = .2) + geom_polygon(data = middle.africa.countries, aes(x = long, y = lat, group = group), fill = NA,
                                                                 color="gray", size = .2) +
  geom_polygon(data = middle.africa, aes(x = long, y = lat, group = group), fill = NA,
               color="black", size = .3) + theme_bw()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5))

conflicts.types <- geom_point(data = ACLED.A2013[ACLED.A2013$REGION=="Middle Africa",], aes(x = LONGITUDE, y = LATITUDE, color=EVENT_TYPE), size = 0.5, shape = 21)

map_projected <- middle.map + conflicts.types + guides(fill=FALSE)  + guides(color=guide_legend(title="Event Type")) +
  coord_map()

print(map_projected)

#Northern Africa Info------------------------------------------------------------------------------------------------------
northern.africa <- africa[africa$SUBREGION == "Northern Africa",]
northern.africa.countries <- fortify(northern.africa, region="NAME")
northern.africa <- fortify(northern.africa, region="SUBREGION")
northern.map <- ggplot() + geom_polygon(data = northern.africa.countries, aes(x = long, y = lat, group = group), fill="#ffdfba",
                                      size = .2) + geom_polygon(data = northern.africa.countries, aes(x = long, y = lat, group = group), fill = NA,
                                                                color="gray", size = .2) +
  geom_polygon(data = northern.africa, aes(x = long, y = lat, group = group), fill = NA,
               color="black", size = .3) + theme_bw()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5))

conflicts.types <- geom_point(data = ACLED.A2013[ACLED.A2013$REGION=="Northern Africa",], aes(x = LONGITUDE, y = LATITUDE, color=EVENT_TYPE), size = 0.5, shape = 21)

map_projected <- northern.map + conflicts.types + guides(fill=FALSE)  + guides(color=guide_legend(title="Event Type")) +
  coord_map()

print(map_projected)

#Southern Africa Info------------------------------------------------------------------------------------------------------
southern.africa <- africa[africa$SUBREGION == "Southern Africa",]
southern.africa.countries <- fortify(southern.africa, region="NAME")
southern.africa <- fortify(southern.africa, region="SUBREGION")
southern.map <- ggplot() + geom_polygon(data = southern.africa.countries, aes(x = long, y = lat, group = group), fill="#baffc9",
                                        size = .2) + geom_polygon(data = southern.africa.countries, aes(x = long, y = lat, group = group), fill = NA,
                                                                  color="gray", size = .2) +
  geom_polygon(data = southern.africa, aes(x = long, y = lat, group = group), fill = NA,
               color="black", size = .3) + theme_bw()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5))

fixACLED <- ACLED.A2013[!ACLED.A2013$COUNTRY=="Zambia" & !ACLED.A2013$COUNTRY=="Zimbabwe",]
conflicts.types <- geom_point(data = fixACLED[fixACLED$REGION=="Southern Africa",], aes(x = LONGITUDE, y = LATITUDE, color=EVENT_TYPE), size = 0.5, shape = 21)

map_projected <- southern.map + conflicts.types + guides(fill=FALSE)  + guides(color=guide_legend(title="Event Type")) +
  coord_map()

print(map_projected)

#Western Africa Info------------------------------------------------------------------------------------------------------
western.africa <- africa[africa$SUBREGION == "Western Africa",]
western.africa.countries <- fortify(western.africa, region="NAME")
western.africa <- fortify(western.africa, region="SUBREGION")
western.map <- ggplot() + geom_polygon(data = western.africa.countries, aes(x = long, y = lat, group = group), fill="#bae1ff",
                                        size = .2) + geom_polygon(data = western.africa.countries, aes(x = long, y = lat, group = group), fill = NA,
                                                                  color="gray", size = .2) +
  geom_polygon(data = western.africa, aes(x = long, y = lat, group = group), fill = NA,
               color="black", size = .3) + theme_bw()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5))

conflicts.types <- geom_point(data = ACLED.A2013[ACLED.A2013$REGION=="Western Africa",], aes(x = LONGITUDE, y = LATITUDE, color=EVENT_TYPE), size = 0.5, shape = 21)

map_projected <- western.map + conflicts.types + guides(fill=FALSE)  + guides(color=guide_legend(title="Event Type")) +
  coord_map()

print(map_projected)

#Scatter Plot GDPxCountry---------------------------------------------------------------------------------------------------
ACLED.A2017 <- ACLED.A2013[ACLED.A2013$YEAR=="2017",]
ACLED.A2017 <- as.data.frame(table(ACLED.A2017$COUNTRY))
colnames(ACLED.A2017) <- c("Country", "Conflicts")
Africa.2017.GDP <- African.GDP[African.GDP$Year=="2017",]
Africa.Scatter <- ACLED.A2017 %>% inner_join(Africa.2017.GDP, by="Country")
Africa.Scatter <- Africa.Scatter[!is.na(Africa.Scatter$GDP),]
Africa.Scatter$Color <- "black"
Africa.Scatter[Africa.Scatter$Country=="Angola" | Africa.Scatter$Country=="Somalia" | Africa.Scatter$Country=="Nigeria",]$Color <- "red"
library(scales)
ggplot(Africa.Scatter, aes(x=GDP,y=Conflicts)) + geom_point() + geom_smooth(method=lm) + theme_bw() + theme(panel.border = element_blank()) +
  scale_x_continuous(name="GDP", labels = comma) + scale_y_continuous(name="Conflict Amount", labels = comma) + 
  geom_point(data=Africa.Scatter[Africa.Scatter$Country=="Angola" | Africa.Scatter$Country=="Somalia" | Africa.Scatter$Country=="Nigeria",], aes(x=GDP,y=Conflicts),
             colour="red") + geom_text(data=Africa.Scatter[Africa.Scatter$Country=="Angola" | Africa.Scatter$Country=="Somalia",], 
aes(label=Country),hjust=-0.15, vjust=0, color="red") + geom_text(data=Africa.Scatter[Africa.Scatter$Country=="Nigeria",], 
                                                    aes(label=Country),hjust=1.15, vjust=0,color="red")

#Fatality Gardient Map----------------------------------------------------------------------------------------------------------
table(africa.country.map$id)
africa.country.map.fix <- africa.country.map
levels(factor(africa.country.map.fix$id))
africa.country.map.fix$id[africa.country.map.fix$id=="Central African Rep."] <- "Central African Republic"
africa.country.map.fix$id[africa.country.map.fix$id=="Dem. Rep. Congo"] <- "Democratic Republic of Congo"
africa.country.map.fix$id[africa.country.map.fix$id=="Congo"] <- "Republic of Congo"
africa.country.map.fix$id[africa.country.map.fix$id=="Côte d'Ivoire"] <- "Ivory Coast"
africa.country.map.fix$id[africa.country.map.fix$id=="Eq. Guinea"] <- "Equatorial Guinea"
africa.country.map.fix$id[africa.country.map.fix$id=="S. Sudan"] <- "South Sudan"
levels(factor(ACLED.A2013$COUNTRY))
ACLED.A2013.fat <- ACLED.A2013 %>% select(COUNTRY, FATALITIES)
colnames(ACLED.A2013.fat) <- c("id", "fat")
ACLED.A2013.fat <- ACLED.A2013.fat %>% group_by(id) %>% 
  summarise(fat = sum(fat))
ACLED.A2013.fat <- as.data.frame(ACLED.A2013.fat)
africa.country.map.fix <- africa.country.map.fix %>% left_join(ACLED.A2013.fat, by="id")
fatality.map <- ggplot() + geom_polygon(data = africa.country.map.fix, aes(x = long, y = lat, group = group, fill = fat),
                                        color="gray", size = .2) +
  geom_polygon(data = africa.border, aes(x = long, y = lat, group = group), fill = NA,
               color="black", size = .3) + theme_bw()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5)) + scale_fill_gradient(low = "#431313", high = "#f75656", 
                                                                                                         name="Fatalities")
print(fatality.map + coord_map())

#Fatality by region-------------------------------------------------------------------------------------------------
fat.region <- ACLED.A2013 %>% group_by(REGION) %>% summarize(FATALITIES=sum(FATALITIES))
fat.region <- as.data.frame(fat.region)
ggplot(data=fat.region, aes(x=REGION, y=FATALITIES, fill=REGION)) + geom_bar(stat="identity") + theme_bw() + ggtitle("By Region") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.border = element_blank(),  panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=14)) +
  scale_fill_manual(values=c("#ff7380", "#ffc582", "#ffff7f", "#7bff98", "#7dc6ff"),guide=FALES) +
  scale_x_discrete(labels=c("Eastern\nAfrica", "Middle\nAfrica", "Northern\nAfrica", "Southern\nAfrica", "Western\nAfrica"))

#Fatality by region2-------------------------------------------------------------------------------------------------
fat.region.country <- ACLED.A2013 %>% group_by(REGION,COUNTRY) %>% summarize(FATALITIES=sum(FATALITIES))
fat.region.country <- as.data.frame(fat.region.country)
ggplot(data=fat.region.country, aes(group=REGION, y=FATALITIES, fill=REGION)) + geom_boxplot() + theme_bw() + ggtitle("By Region") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.border = element_blank(),  panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=14)) +
  scale_fill_manual(values=c("#ff7380", "#ffc582", "#ffff7f", "#7bff98", "#7dc6ff"),guide=FALSE) +
  scale_x_discrete(labels=c("Eastern\nAfrica", "Middle\nAfrica", "Northern\nAfrica", "Southern\nAfrica", "Western\nAfrica"))

#Fatality by event-------------------------------------------------------------------------------------------------
fat.event <- ACLED.A2013 %>% group_by(EVENT_TYPE) %>% summarize(FATALITIES=sum(FATALITIES))
fat.event <- as.data.frame(fat.event)
levels(fat.event$EVENT_TYPE)[5] <- "Non-violent transfer\nof territory"
levels(fat.event$EVENT_TYPE)[1] <- "Battle-Government\nregains territory"
levels(fat.event$EVENT_TYPE)[2] <- "Battle-No change\n of territory"
levels(fat.event$EVENT_TYPE)[3] <- "Battle-Non-state actor\novertakes territory"
levels(fat.event$EVENT_TYPE)[4] <- "Headquarters or\nbase established"
ggplot(data=fat.event, aes(x=EVENT_TYPE, y=FATALITIES, fill=EVENT_TYPE)) + geom_bar(stat="identity") + theme_bw() + ggtitle("By Event") + coord_flip() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.border = element_blank(),  panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5),axis.text.y = element_text(size=14)) + guides(fill=FALSE)

#Fatality scatter-------------------------------------------------------------------------------------------------
fat.country.year <- ACLED.A2013 %>% group_by(COUNTRY, YEAR) %>% summarize(FATALITIES=sum(FATALITIES))
fat.country.year <- as.data.frame(fat.country.year)
fat.country.year2 <-ACLED.A2013[ACLED.A2013$FATALITIES >550,]
ggplot(ACLED.A2013, aes(x=EVENT_DATE,y=FATALITIES)) + geom_point() + theme_bw() + theme(panel.border = element_blank()) +
  labs(x = "Event Date", y="Fatalities") + geom_point(data=fat.country.year2,aes(x=EVENT_DATE,y=FATALITIES),color="red") +
  geom_text(data=fat.country.year2, 
            aes(label=ACTOR1),hjust=-0.15, vjust=0,color="red")

#Fatality/Population----------------------------------------------------------------------------------------------
fat.region.country <- ACLED.A2013 %>% group_by(REGION,COUNTRY) %>% summarize(FATALITIES=sum(FATALITIES))
fat.region.country <- as.data.frame(fat.region.country)
colnames(fat.region.country)[2] <- "Country"
fat.region.country <- fat.region.country %>% inner_join(African.Pop[African.Pop$Year=="X2017",], by="Country")
fat.region.country<- fat.region.country[!is.na(fat.region.country$Population),]
fat.region <- fat.region.country %>% group_by(REGION) %>% summarize(FATPOP=sum(FATALITIES)/sum(Population)*10000)
fat.region <- as.data.frame(fat.region)
ggplot(data=fat.region, aes(x=REGION, y=FATPOP, fill=REGION)) + geom_bar(stat="identity") + theme_bw() + ggtitle("By Region (Population Controlled)") +
  theme(axis.title.x=element_blank(), panel.border = element_blank(),  panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=14)) +
  scale_fill_manual(values=c("#ff7380", "#ffc582", "#ffff7f", "#7bff98", "#7dc6ff"),guide=FALSE) +
  scale_x_discrete(labels=c("Eastern\nAfrica", "Middle\nAfrica", "Northern\nAfrica", "Southern\nAfrica", "Western\nAfrica")) + 
  scale_y_continuous(name="Fatalities Per 10,000 People")

#Per country conflict v gdp ------------------------------------------------------------------------------------------
detach("package:plyr", unload=TRUE) 
remove.packages("plyr")
detach("package:dplyr", unload=TRUE) 
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
ACLED.Africa.date <- arrange(ACLED.Africa, EVENT_DATE)
ACLED.Africa.date <- ACLED.Africa.date %>% group_by(COUNTRY, YEAR) %>% summarize(n=n()) %>% mutate(log_change = exp((log10(n)-log10(lag(n))))) %>%
  mutate(pct_change = (n-lag(n))/lag(n))
ACLED.Africa.date <- as.data.frame(ACLED.Africa.date)
ACLED.Africa.date <- ACLED.Africa.date[ACLED.Africa.date$YEAR >= 2013 & ACLED.Africa.date$YEAR < 2018,]
ACLED.Africa.datem <- ACLED.Africa.date %>% group_by(COUNTRY) %>% summarise(log_changem = mean(log_change, na.rm=TRUE))
ACLED.Africa.datem <- as.data.frame(ACLED.Africa.datem)

African.GDP.date <- arrange(African.GDP, Year)
African.GDP.date <- African.GDP.date  %>% group_by(Country, Year) %>% summarize(GDP=sum(GDP)) %>% mutate(log_change = exp((log10(GDP)-log10(lag(GDP))))) %>%
  mutate(pct_change = (GDP-lag(GDP))/lag(GDP))
African.GDP.date <- as.data.frame(African.GDP.date)
African.GDP.date <- African.GDP.date[African.GDP.date$Year >= 2013 & African.GDP.date$Year < 2018,]
African.GDP.datem <- African.GDP.date %>% group_by(Country) %>% summarise(log_changem = mean(log_change, na.rm=TRUE))
African.GDP.datem <- as.data.frame(African.GDP.datem)

countrygdpvcon <- function(x){
  eastern.countries <- as.data.frame(table(ACLED.Africa[ACLED.Africa$REGION==x,]$COUNTRY))
  eastern.countries <- eastern.countries[eastern.countries$Freq>0,1]
  print(length(eastern.countries))
  print(eastern.countries)
  for(country in eastern.countries){
    print(country)
    print(ACLED.Africa.date[ACLED.Africa.date$COUNTRY==country,])
    print(African.GDP.date[African.GDP.date$Country==country,])
    print(ggplot(ACLED.Africa.date[ACLED.Africa.date$COUNTRY==country,],
           aes(x=YEAR, y=log_change-ACLED.Africa.datem[ACLED.Africa.datem$COUNTRY==country,2])) + geom_line(color='red') + 
        geom_line(data=African.GDP.date[African.GDP.date$Country==country,],
                  aes(x=Year, y=log_change-African.GDP.datem[African.GDP.datem$Country==country,2]),color='blue')+theme_bw()+# ggtitle(country)+
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5)))
  }
}

#Region stacked bar chart-------------------------------------------------------------------
#Eastern Africa
#Event linegraph--------------------------------------------------------------------------------------------
region.event.dist <- function(x){

  ACLED.Months <- ACLED.A2013[ACLED.A2013$REGION==x,]
  ggplot(data=ACLED.Months, aes(x=EVENT_DATE, fill=EVENT_TYPE)) + geom_histogram(bins=400,position = "fill",show.legend = FALSE) + ggtitle("Event Distribution") +
    theme_bw() + theme(axis.title.x=element_blank(), 
                       axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                       panel.border = element_blank(), legend.position="bottom",
                       legend.text=element_text(size=15),plot.title = element_text(hjust = 0.5)) +
    guides(fill=guide_legend(title="Event Type", nrow=3))
}

#Largest fat battle eartern -------------------------------------------------------------------
largestBat <- tail(ACLED.A2013[ACLED.A2013$REGION=="Eastern Africa" & ACLED.A2013$EVENT_TYPE=="Battle-No change of territory",] %>% arrange(FATALITIES),1)
largestBat

largestBat <- ACLED.A2013[ACLED.A2013$REGION=="Middle Africa",] %>% group_by(REGION,EVENT_TYPE) %>% summarise(n=n(), fatsum=sum(FATALITIES), high=Mode(ACTOR1))
largestBat
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Top actors Tunisia
largestBat <- ACLED.A2013[ACLED.A2013$COUNTRY=="Tunisia",] %>% group_by(EVENT_TYPE) %>% summarise(n=n(), fatsum=sum(FATALITIES))
largestBat

ACTORS <- table(c(as.vector(ACLED.A2013[ACLED.A2013$COUNTRY=="Tunisia",]$ACTOR1), as.vector(ACLED.A2013[ACLED.A2013$COUNTRY=="Tunisia",]$ACTOR2)))
tail(ACTORS[order(ACTORS)], 10)

largestBat <- ACLED.A2013[ACLED.A2013$COUNTRY=="Sudan",] %>% group_by(EVENT_TYPE) %>% summarise(n=n(), fatsum=sum(FATALITIES))
largestBat

largestBat <- ACLED.A2013[ACLED.A2013$COUNTRY=="Sudan",] %>% group_by(COUNTRY) %>% summarise(n=n(), fatsum=sum(FATALITIES))
largestBat
largestBat <- ACLED.A2013[ACLED.A2013$COUNTRY=="Sudan",] %>% arrange(FATALITIES)
tail(largestBat,1)

largestBat <- ACLED.A2013[ACLED.A2013$REGION=="Southern Africa",] %>% group_by(REGION,EVENT_TYPE) %>% summarise(n=n(), fatsum=sum(FATALITIES), high=Mode(ACTOR1))
largestBat

largestBat <- tail(ACLED.A2013[ACLED.A2013$REGION=="Southern Africa" & ACLED.A2013$EVENT_TYPE=="Riots/Protests",] %>% arrange(FATALITIES),1)
largestBat

largestBat <- ACLED.A2013[ACLED.A2013$REGION=="Western Africa" & ACLED.A2013$EVENT_TYPE=="Riots/Protests",] %>% group_by(COUNTRY) %>% summarise(n=n(), fatsum=sum(FATALITIES), high=Mode(ACTOR1))
largestBat

largestBat <- ACLED.A2013[ACLED.A2013$REGION=="Western Africa" & ACLED.A2013$EVENT_TYPE=="Violence against civilians",] %>% group_by(COUNTRY) %>% summarise(n=n(), fatsum=sum(FATALITIES), high=Mode(ACTOR1))
largestBat
largestBat <- tail(ACLED.A2013[ACLED.A2013$COUNTRY=="Nigeria" & ACLED.A2013$EVENT_TYPE=="Violence against civilians",] %>% arrange(FATALITIES),1)
largestBat

largestBat <- tail(ACLED.A2013[ACLED.A2013$COUNTRY=="Somalia" & ACLED.A2013$YEAR==2017,] %>% arrange(FATALITIES),1)
largestBat

largestBat <- ACLED.A2013[ACLED.A2013$COUNTRY=="Nigeria" & ACLED.A2013$YEAR==2017,] %>% 
                            group_by(EVENT_TYPE) %>% summarise(n=n(), fatsum=sum(FATALITIES), high=Mode(EVENT_TYPE))
largestBat                          

largestBat <- tail(ACLED.A2013[ACLED.A2013$FATALITIES>550,] %>% arrange(FATALITIES),2)
largestBat

#LEGEND-------------------------------------------------------------------------------------------

#Label countries
africa.country.lbl <- africa.country.map.fix[africa.country.map.fix$id %in% levels(factor(ACLED.Africa$COUNTRY)),]

africa2 = world[world$CONTINENT == 'Africa',]
levels(africa2$NAME)
levels(africa2$NAME)[43] <- "Central African Republic"
levels(africa2$NAME)[48] <- "Democratic Republic of Congo"
levels(africa2$NAME)[49] <- "Republic of Congo"
levels(africa2$NAME)[52] <- "Ivory Coast"
levels(africa2$NAME)[66] <- "Equatorial Guinea"
levels(africa2$NAME)[180] <- "South Sudan"
africa2 <- africa2[africa2$NAME %in% levels(factor(ACLED.Africa$COUNTRY)),]
idList <- africa2@data$NAME
centroids.df <- as.data.frame(coordinates(africa2))
centroids.df <- data.frame(centroids.df,idList, c=1:49)
legend.map <- ggplot() + geom_polygon(data = africa.region.map, aes(x = long, y = lat, group = group, fill = id),
                               size = .2) + geom_polygon(data = africa.country.map, aes(x = long, y = lat, group = group), fill = NA,
                                                         color="gray", size = .2) +
  geom_polygon(data = africa.border, aes(x = long, y = lat, group = group), fill = NA,
               color="gray", size = .3) + 
  scale_fill_manual(values=c("#ffb3ba", "#ffdfba", "#ffffba", "#baffc9", "#bae1ff"))+
  geom_text(data=centroids.df,aes(x=V1,y=V2,label=c),  size=3,show.legend = FALSE) +
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),legend.position="top",legend.title=element_blank())

map_projected <- legend.map + 
  coord_map() 
print(map_projected)

#EASTERN--------------------------------------------------
Act.data <- ACLED.A2013
ACTORS <- table(c(as.vector(Act.data[Act.data$REGION == "Al Shabaab",]$ACTOR1), as.vector(Act.data[Act.data$REGION == "",]$ACTOR2)))
#Remove missing value
ACTORS <- tail(ACTORS[order(ACTORS)])
ACTORS <- ACTORS[c(FALSE,FALSE,TRUE,TRUE,TRUE,FALSE)]

northern.actors <- as.data.frame(ACTORS)
colnames(northern.actors) <- c("Actor", "Conflicts")
levels(northern.actors$Actor)[1] <- "Unidentified Armed\n Group (Libya)"
ggplot(northern.actors, aes(x=Actor, y=Conflicts)) + geom_bar(stat="identity",fill="#ffff7f") + coord_flip() + labs(x = "Northern Actors") +
  geom_text(aes(label = Actor),
            position = position_stack(vjust = 0.5)) + theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
                                                                         panel.grid.major.y = element_blank(), panel.border = element_blank()) + guides(fill=FALSE)

fat.event <- ACLED.A2013[ACLED.A2013$ACTOR1=="Al Shabaab" | ACLED.A2013$ACTOR2=="Al Shabaab",]
sum(fat.event$FATALITIES)
nrow(fat.event)
fat.event <- fat.event %>% group_by(EVENT_TYPE) %>% summarize(FATALITIES=n())
fat.event <- as.data.frame(fat.event)
levels(fat.event$EVENT_TYPE)[5] <- "Non-violent transfer\nof territory"
levels(fat.event$EVENT_TYPE)[1] <- "Battle-Government\nregains territory"
levels(fat.event$EVENT_TYPE)[2] <- "Battle-No change\n of territory"
levels(fat.event$EVENT_TYPE)[3] <- "Battle-Non-state actor\novertakes territory"
levels(fat.event$EVENT_TYPE)[4] <- "Headquarters or\nbase established"
ggplot(data=fat.event, aes(x=EVENT_TYPE, y=FATALITIES, fill=EVENT_TYPE)) + geom_bar(stat="identity") + theme_bw() + ggtitle("Al Shabaab Involvement By Event") + coord_flip() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.border = element_blank(),  panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5),axis.text.y = element_text(size=14)) + guides(fill=FALSE)

#MIDDLE--------------------------------------------------
Act.data <- ACLED.A2013
ACTORS <- table(c(as.vector(Act.data[Act.data$REGION == "Western Africa",]$ACTOR1), as.vector(Act.data[Act.data$REGION == "Western Africa",]$ACTOR2)))
#Remove missing value
ACTORS <- tail(ACTORS[order(ACTORS)],20)
ACTORS <- ACTORS[c(FALSE,FALSE,TRUE,TRUE,TRUE,FALSE)]

northern.actors <- as.data.frame(ACTORS)
colnames(northern.actors) <- c("Actor", "Conflicts")
levels(northern.actors$Actor)[1] <- "Unidentified Armed\n Group (Libya)"
ggplot(northern.actors, aes(x=Actor, y=Conflicts)) + geom_bar(stat="identity",fill="#ffff7f") + coord_flip() + labs(x = "Northern Actors") +
  geom_text(aes(label = Actor),
            position = position_stack(vjust = 0.5)) + theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
                                                                         panel.grid.major.y = element_blank(), panel.border = element_blank()) + guides(fill=FALSE)

fat.event <- ACLED.A2013[ACLED.A2013$ACTOR1=="Boko Haram - Wilayat Gharb Ifriqiyyah" | ACLED.A2013$ACTOR2=="Boko Haram - Wilayat Gharb Ifriqiyyah",]
sum(fat.event$FATALITIES)
nrow(fat.event)
fat.event <- fat.event %>% group_by(EVENT_TYPE) %>% summarize(FATALITIES=n())
fat.event <- as.data.frame(fat.event)
levels(fat.event$EVENT_TYPE)[5] <- "Non-violent transfer\nof territory"
levels(fat.event$EVENT_TYPE)[1] <- "Battle-Government\nregains territory"
levels(fat.event$EVENT_TYPE)[2] <- "Battle-No change\n of territory"
levels(fat.event$EVENT_TYPE)[3] <- "Battle-Non-state actor\novertakes territory"
levels(fat.event$EVENT_TYPE)[4] <- "Headquarters or\nbase established"
ggplot(data=fat.event, aes(x=EVENT_TYPE, y=FATALITIES, fill=EVENT_TYPE)) + geom_bar(stat="identity") + theme_bw() + ggtitle("Boko Haram - Wilayat Gharb Ifriqiyyah Involvement By Event") + coord_flip() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.border = element_blank(),  panel.grid.major.x = element_blank(), 
        plot.title = element_text(hjust = 0.5),axis.text.y = element_text(size=14)) + guides(fill=FALSE)
