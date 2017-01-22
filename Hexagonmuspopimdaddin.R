library(geojsonio)
library(sp)
library(rgeos)
library(rgdal)
###############################################################################################################
#Create a shape file of the UK that is overlayed with hexagons

states<-readOGR("E:\\data","NUTS_RG_03M_2010")

#Selects only the NUTS2 geographies
states<-subset(states, states@data$STAT_LEVL_=='0')

#Selects the UK by matching NUTS codes that start with UK
UK<-subset(states, grepl("^UK", states@data$NUTS_ID)==TRUE)

#Adds a buffer to the UK to make the hexagons fit the approximate contour of the UK, not the fine detail of coastline
UK<-spTransform(UK, CRS("+init=epsg:27700")) 

#Generates a buffer for the UK width is how much inside the boundary the buffer extends 
UK<-gBuffer(UK, width=3000,quadsegs=5,capStyle="ROUND", byid=TRUE)
 
#Transforms back the two areas to the standard projection we are using
UK<-spTransform(UK, CRS("+proj=longlat +datum=WGS84"))

#Creates a Hexagonal grid that fills the UK
#Randomly samples inside the shape of the UK, but in a hexagonal pattern was 4000
Hexrnd<-spsample(UK, 6000, type="hexagonal", nsig=TRUE)

#Creates a hexagonal grid
HexPols <- HexPoints2SpatialPolygons(Hexrnd)

HexPols<-spTransform(HexPols,CRS("+proj=longlat +datum=WGS84"))
HexPols<-spTransform(HexPols, CRS("+init=epsg:27700"))

############################################################################################################
#Reads in the lower superoutput data
 #Reads in the lsao shape fileslsoa<-readOGR("filepath\\Boundaries", "Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales")

#Transforms the projection
lsoa<-spTransform(lsoa, CRS("+proj=longlat +datum=WGS84"))

#Reads in the population data. This is midyear 2015 data
pop2015<-read.csv("filepath\ONS data\\2015 lsoa population estimates.csv", header=T)

#Attach the population
lsoa@data<-cbind(lsoa@data, pop2015[match(lsoa@data[,c("lsoa11cd")],pop2015[ ,c("Area.Codes")]),c("All.Ages")])
#Recodes the last column name2015 lsoa population estimates.csv
names(lsoa@data)[ncol(lsoa@data)]<-"Population"

#Reads in the index of multiple deprivation data
multdep<-read.csv("filepath\ONS data\\indexofmultiple deprivation2015.csv", header=T, stringsAsFactors = FALSE)


#Extracts the part of the lsoacoding we want. It works, but inelegant better to do with a regex
multdep$lsoacode<-paste("E0",lapply(strsplit(multdep$lsoa, " E0"), `[`, 2), sep="")

lsoa@data$lsoa11cd<-as.character(lsoa@data$lsoa11cd)

#Binds the data to the dataframe
lsoa@data<-cbind(lsoa@data, multdep[match(lsoa@data[,c("lsoa11cd")],multdep[ ,c("lsoacode")]),])
###############################################################################################################
 #Extracts the coordinates of the lower super output areas
lpoints = SpatialPoints(coordinates(lsoa))

#Binds the data to the points to create a spatial points data frame
lpoints = SpatialPointsDataFrame(lpoints, lsoa@data)

##sets up the projection for the photos as the same
proj4string(lpoints) <- CRS("+proj=longlat +datum=WGS84")

###############################################################################################################
 #Estimates the population falling within the polygons #This bit is still not working

#finds the population that falls within the hexagons using the centroids of the lsoas
popc<-over(HexPols,lpoints, returnList = TRUE)

pops=matrix(nrow=length(popc),ncol=2) 

#Loops through the list of museums and sums the population within lsoas with centroids falling in the hexagon 
for (i in 1:length(popc)){
   pops[i,]<-sum(as.numeric(popc[[i]]$Population))
#Selects area that are in the bottom two deciles for indices of multiple deprivation and then sums the population in those areas
idep<-subset(popc[[i]], popc[[i]]$IndexofMultipleDeprivation==1 | popc[[i]]$IndexofMultipleDeprivation==2 )
 pops[i,2]<-sum(as.numeric(idep$Population))
 }

colnames(pops)<-c("Population", "PopulationIMD")


#############################################################################################################
#This part calculates the number of museums falling within given distances of the centroids of the hexagon
#Reads in the geojson that has the museums data

mus = readOGR("filepath\\museumsmap\\public\\data.geojson" , "OGRGeoJSON")

#Removes the museums that are outside the UK. A bbox would be better
mus<-subset(mus, coordinates(mus)[,1] > -8.650007 & coordinates(mus)[,1] <1.762916)
mus<-subset(mus,  coordinates(mus)[,2] > 49.864747& coordinates(mus)[,2] <60.860761)

#Transforms the projection of the museums to do the distance analysis
mus<-spTransform(mus, CRS("+init=epsg:27700")) 

#Extracts the central point of the hexagons
Hexpoint<-SpatialPoints(coordinates(HexPols), proj4string =CRS("+init=epsg:27700"))

###########################################################################################################


#Creats an empty data frame to store the results
df<-data.frame(matrix(NA, nrow = length(HexPols), ncol = 1))
rownames(df)<-names(HexPols)


#################################################################################################
#Function that counts the number of museums within the radius of x from the centre of the hexagon
muscount<-function(x){

wd=x*1000 #converts to km

#Generates a buffer for the UK width is how much inside the boundary the buffer extends 
#The byid keeps the points separate rather than merging the buffer

hexbuf<-gBuffer(Hexpoint, width=wd,quadsegs=5,capStyle="ROUND", byid=TRUE)
plot(hexbuf)

#finds the lpoints that fall within the buffer
hexc<-over(hexbuf,mus, returnList = TRUE)
                                         
results=matrix(nrow=length(hexc),ncol=1) 
#Loops through the list of museums and sums the population within 100km
for (i in 1:length(hexc)){

results[i,1]<-ifelse(dim(as.data.frame(hexc[i]))[1]>0, dim(as.data.frame(hexc[i]))[1],0)

}


na1<-paste("kmpop",x,sep="")

#Creates the column names)
colnames(results)<-c(na1)


df<<-cbind(df,results)


}
#End of function

###########################################################################################################
# Applies the function to the distance intervals
#The distance intervals

dists<-c(60,50,40,30,20,10)

#Applies the population count function

lapply(dists, muscount)

#Removes the first column

df<-df[,-1]

#Creates a SpatialPolygons data frame by merging with HexPolys
SPDF = SpatialPolygonsDataFrame(HexPols, cbind(df, pops))
#Transforms the projection
SPDF<-spTransform(SPDF, CRS("+proj=longlat +datum=WGS84"))
################################################################################################################
#Writes the file out as a geojson

library(geojsonio)

musg<-geojson_json(SPDF)

#writes to disk
geojson_write(musg, file = "filepath\\Museummap\\musmap11rat.geojson")
