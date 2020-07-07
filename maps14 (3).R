library("ggplot2")
theme_set(theme_bw())
library("sf")
library("plyr")
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")
library("dplyr")
library("rworldmap")
library("ggmap")
library("ggspatial")
library("rgeos")
library("xml2")
library("rvest")
library("RgoogleMaps")
library("lattice")
library("car")
install.packages("car")
install.packages(pbkrtest)
library("xml2")
library("XML")
require(rvest)
require(magrittr)
require(stringr)

# read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-30-2020.csv")
covid19 <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/06-15-2020.csv")
myData <- covid19

names(myData)[6] <- "Latitude" #changing name from Lat to Latitude
names(myData)[7] <- "Longitude"


newmap <- getMap(resolution = "low")
plot(newmap)
points(myData$Longitude, myData$Latitude, col="red", cex = 0.6)

register_google(key = "AIzaSyCHZajLSLxCHGSLjGc_MwqRqcsOuh3ILAw")
# map <- get_googlemap(location="World", zoom=4)


us <- c(left = -125, bottom =24, right = -67, top =50)
get_stamenmap(us, zoom=5, maptype = "toner-lite") %>% ggmap()

# qmplot(Longitude, Latitude, data = myData, color=I("red"))

texas <- c(left = -107, bottom = 26, right = -93, top = 35)
get_stamenmap(texas, zoom=6, maptype = "toner-lite") %>% ggmap()




txInfo <- subset(myData, Province_State == "Texas")


txInfo$lethality <- txInfo$Deaths/txInfo$Confirmed

qmplot(Longitude, Latitude, data=txInfo, geom = "blank") +
  geom_point(aes(size=Confirmed, color="red"))

qmplot(Longitude, Latitude, data=txInfo, geom = "blank") +
  geom_point(aes(size=Deaths, color="red"))

qmplot(Longitude, Latitude, data=txInfo, geom = "blank") +
  geom_point(aes(size=lethality, color="red"))

mean(txInfo$lethality, trim = 0, na.rm = TRUE)
sum(txInfo$Confirmed, na.rm = TRUE)
sum(txInfo$Deaths, na.rm = TRUE)

sum(txInfo$Deaths, na.rm = TRUE) / sum(txInfo$Confirmed, na.rm = TRUE) * 100


p <- ggplot(txInfo, aes(lethality)) + geom_histogram()
p

#Codes below needs more work . Will send out the rest later.





url <- "https://www.texas-demographics.com/counties_by_population"
pagina <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
pagina %>%  
  html_nodes("table") %>% 
  #Here, we indicate that this is the table we want to extract.
  .[[1]] %>% 
  html_table(fill=T) -> x
x
x <- x[-c(68),]

x$County <- str_remove(x$County, " County")
names(x)[2] <- "Admin2"
x <- x[,-1]
txInfo <- txInfo[,-14]
t <- merge(x=txInfo, y=x, by="Admin2",all.x=FALSE)
txInfo <- t

colnames <- as.matrix(names(txInfo))

txInfo$Population = as.numeric(gsub("[\\$,]", "", txInfo$Population))

txInfo$ConperCapita  <- txInfo$Confirmed / txInfo$Population
qmplot(Longitude, Latitude, data=txInfo, geom = "blank") +
  geom_point(aes(size=ConperCapita, color="red"))

txInfo$DeathsPerCap <- txInfo$Deaths / txInfo$Population
qmplot(Longitude, Latitude, data = txInfo, geom = "blank") +
  geom_point(aes(size=DeathsPerCap, color="red"))

scatterplotMatrix(~DeathsPerCap+ConperCapita, data=txInfo,
                 main="scatterplot")

scatterplotMatrix(~Confirmed+Population, data=txInfo,
                  main="scatterplot")

plot(txInfo$Population, txInfo$Confirmed)
plot(txInfo$DeathsPerCap, txInfo$ConperCapita)

url <- "http://www.usa.com/rank/texas-state--land-area--county-rank.htm"
pagina <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
pagina %>%  
  html_nodes("table") %>% 
  #Here, we indicate that this is the table we want to extract.
  .[[2]] %>% 
  html_table(header = TRUE, fill=T) -> x
x <- within(x, rm(Rank))
names(x)[1] <- "SqMiles"
names(x)[2] <- "Admin2"
x$SqMiles = as.numeric(gsub("[\\$, sq mi]", "", x$SqMiles))
x$Admin2 = as.character(gsub(", FL /", "", x$Admin2))
x$Admin2 = as.character(gsub(",", "", x$Admin2))
x$Admin2 = as.character(gsub(" [0-9]", "", x$Admin2))
x$Admin2 = as.character(gsub("[0-9]", "", x$Admin2))
x$Admin2
x$Admin2[x$Admin2 == "Saint Johns"] <-"St. Johns"
x$Admin2[x$Admin2 == "Saint Lucie"] <-"St. Lucie"
x$Admin2[x$Admin2 == "De Soto"] <-"DeSoto"

txInfo <- merge(x=txInfo, y=x, by="Admin2",all.x=FALSE)

txInfo$PopDensity <- txInfo$Population / txInfo$SqMiles

plot(txInfo$PopDensity, txInfo$Confirmed)
plot(txInfo$PopDensity, txInfo$DeathsPerCap)

getwd()
write.csv(flInfo,'flInfo.csv')


####################################
# This section is for collecting information across states... not needed for Florida
# this is a good plot

usainfo <- subset(myData,
                         -125 <= Longitude & Longitude <= -67 &
                           24 <= Latitude & Latitude <=  49
)

# qmplot(Longitude, Latitude, data = usainfo, color=I("red"), size=Confirmed)


# qmplot(Longitude, Latitude, data = usainfo, color=I("red"), size=Deaths)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

library("ggspatial")

#extract age by state from wikipedia

webpage_url <- "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_median_age"
webpage <- xml2::read_html(webpage_url)
ExOffndrsRaw <- rvest::html_table(webpage)[[1]] %>% 
  tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
ExOffndrsRaw %>% dplyr::glimpse(45)
ExOffndrsRaw %>% dplyr::select(dplyr::contains('Link')) %>% head(5)
Avgage <- ExOffndrsRaw
colnames(Avgage)
colnames(myData)
names(Avgage)[2] <- "Province_State"
names(Avgage)[3] <- "Med.Age"
usainfo2 <- merge(x=myData, y=Avgage, by="Province_State",all.x=FALSE)

Population <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv")

names(Population)[5] <- "Province_State"
usainfo3 <- merge(x=usainfo2, y=Population, by="Province_State", all.x=FALSE)
str(usainfo3)
usainfo4 <- usainfo3
usainfo4$percapita <- usainfo4$Confirmed / usainfo4$POPESTIMATE2019 *100
usainfo4$lethality <- usainfo4$Deaths/usainfo4$Confirmed


usainfo4 <- subset(usainfo4,
                  -125 <= Longitude & Longitude <= -67 &
                    23 <= Latitude & Latitude <=  50
)


# qmplot(Longitude, Latitude, data = usainfo4, color=I("red"), size=percapita)
# qmplot(Longitude, Latitude, data = usainfo4, color=I("red"), size=lethality)


# scatterplotMatrix(~POPESTIMATE2019+lethality+Med.Age, data=usainfo4,
#                   main="scatterplot")

plot(usainfo4$Confirmed,usainfo4$Deaths)
boxplot(usainfo4$lethality~usainfo4$Med.Age)
plot(usainfo4$POPESTIMATE2019,usainfo4$lethality)
usainfo5 <- subset(usainfo4, Deaths!=0)
plot(usainfo5$Confirmed,usainfo5$Deaths)
boxplot(usainfo5$lethality~usainfo5$Med.Age)
boxplot(usainfo5$lethality~usainfo5$POPESTIMATE2019)

hist(usainfo5$lethality)
str(usainfo5)
plot(usainfo5$POPESTIMATE2019, usainfo5$Confirmed)


url <- "https://dqydj.com/average-income-by-state-median-top-percentiles/"
pagina <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
pagina %>%  
  html_nodes("table") %>% 
  #Here, we indicate that this is the table we want to extract.
  .[[2]] %>% 
  html_table(fill=T) -> x
x <- x[-1,]
names(x)[1] <- "Province_State"
names(x)[2] <- "MedianInc"

x$MedianInc = as.numeric(gsub("[\\$,]", "", x$MedianInc))
usainfo4 <- merge(x=usainfo4, y=x, by="Province_State", all.x=FALSE)
usainfo5 <- subset(usainfo4, Deaths!=0)
# usainfo5 <- within(usainfo5, rm(lethality))
plot(usainfo5$MedianInc, usainfo5$Confirmed)
plot(usainfo5$Confirmed, usainfo5$lethality)








