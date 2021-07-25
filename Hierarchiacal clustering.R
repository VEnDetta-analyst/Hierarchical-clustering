install.packages('ggdendro')
library(ggdendro)
library(ggplot2)
download.file("https://ibm.box.com/shared/static/mv6g5p1wpmpvzoz6e5zgo47t44q8dvm0.csv", destfile = "WeatherStations.csv", quiet = FALSE)
WeatherStations <- read.csv("WeatherStations.csv", sep =',')
head(WeatherStations)
WeatherStations=WeatherStations[complete.cases(WeatherStations$Lat,WeatherStations$Long,WeatherStations$P),]
#filter for any entries that lie outside canada
#canada coordinates
llon <- -140
ulon <- -50
llat <- 40
ulat <- 65
filtered_stations=subset(WeatherStations,Lat>llat&Lat<ulat&Long>llon&Long<ulon)
number_stations=30
set.seed(1234)
random_stations = filtered_stations[sample.int(nrow(filtered_stations))[1:number_stations],]
rownames(random_stations)=1:nrow(random_stations)
head(random_stations)
#Dendogram
temp_loc_dataframe=scale(data.frame("TotalPrecipitation"=random_stations$P,"Lat"=random_stations$Lat,"Long"=random_stations$Long),center = TRUE, scale=TRUE)
D=dist(as.matrix(temp_loc_dataframe))
hc=hclust(D)
ggdendrogram(hc,rotate = TRUE,theme_dendro = TRUE,color="tomato")
#adding cluster labels to dataframe
threshold=1.5
groups <- cutree(hc, h=threshold) 
random_stations=cbind(random_stations, clusters =as.factor(as.vector(groups)))
head(random_stations)
