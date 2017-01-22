library(geojsonio)
library(sp)
library(rgeos)
library(rgdal)


###################################################################
#Create a shape file of the lsoas with the population and the indices of multiple deprivation added in.

#Reads in the lsao shape files
lsoa<-readOGR("filepath\\Boundaries", "Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales")

#Transforms the projection
lsoa<-spTransform(lsoa, CRS("+proj=longlat +datum=WGS84"))

#Reads in the population data. This is midyear 2015 data
pop2015<-read.csv("filepath\\ONS data\\2015 lsoa population estimates.csv", header=T)

#Attach the population
lsoa@data<-cbind(lsoa@data, pop2015[match(lsoa@data[,c("lsoa11cd")],pop2015[ ,c("Area.Codes")]),c("All.Ages")])

#Recodes the last column name
names(lsoa@data)[ncol(lsoa@data)]<-"Population"

#Reads in the indices of multiple deprivation data
multdep<-read.csv("filepath\\indexofmultiple deprivation2015.csv", header=T, stringsAsFactors = FALSE)

#Extracts the part of the lsoacoding we want. It works, but inelegant better to do with a regex
multdep$lsoacode<-paste("E0",lapply(strsplit(multdep$lsoa, " E0"), `[`, 2), sep="")

lsoa@data$lsoa11cd<-as.character(lsoa@data$lsoa11cd)

#Binds the data to the dataframe
lsoa@data<-cbind(lsoa@data, multdep[match(lsoa@data[,c("lsoa11cd")],multdep[ ,c("lsoacode")]),])
###############################################################################################################

#Extracts the coordinates of the lower super output areas
lpoints = SpatialPoints(coordinates(lsoa))


##sets up the projection for the photos as the same
proj4string(lpoints) <- CRS("+proj=longlat +datum=WGS84")

#Binds the data to the points to create a spatial points data frame
lpoints = SpatialPointsDataFrame(lpoints, lsoa@data)


#Reads in the museums data
mus = readOGR("filpath\public\\data.geojson" , "OGRGeoJSON")

#Removes the museums that are outside the UK. A bbox would be better
mus<-subset(mus, coordinates(mus)[,1] > -8.650007 & coordinates(mus)[,1] <1.762916)
mus<-subset(mus,  coordinates(mus)[,2] > 49.864747& coordinates(mus)[,2] <60.860761)

mus<-spTransform(mus, CRS("+init=epsg:27700")) 


lpoints<-spTransform(lpoints, CRS("+init=epsg:27700")) 

#Creats an empty data frame
df<-data.frame(matrix(NA, nrow = length(lpoints), ncol = 1))
rownames(df)<-names(lsoa@data$objectid)

#################################################################################################
#Function that counts the number of museums within the radius of x from the centre of the hexagon
muscount<-function(x){
  
  
  wd=x*1000
  
  #Generates a buffer around the centroid of the lsoa
  #The byid keeps the points separate rather than merging the buffer
  
  musbuf<-gBuffer(lpoints, width=wd,quadsegs=5,capStyle="ROUND", byid=TRUE)
  plot(musbuf)
  
  #finds the lpoints that fall within the buffer
  s<-over(musbuf,mus, returnList = TRUE)
  
  print(length(s))
  
  results=matrix(nrow=length(s),ncol=1) 
  
  
  #Loops through the list of museums and sums the population within 100km
  for (i in 1:length(s)){
    
    results[i,1]<-ifelse(dim(as.data.frame(s[i]))[1]>0, dim(as.data.frame(s[i]))[1],0)
    
  }
  
  
  na1<-paste("kmpop",x,sep="")
  
  #Creates the column names
  colnames(results)<-c(na1)
  
  dim(df)
  dim(results)
  
  df<<-cbind(df,results)
  
  
}
###############################################################################################################
# Applies the function to the distance intervals
#The distance intervals

f<-c(60,50,40,30,20,10)

#Applies the population count function

lapply(f, muscount)

#Removes the first column

df<-df[,-1]

####################################
#Merges back the data
lsoa@data<-cbind(lsoa@data, df[match(lsoa@data[,c("objectid")],rownames(df)),])

#Projects back to latitude and longitude
lsoa<-spTransform(lsoa, CRS("+proj=longlat +datum=WGS84"))

################################################################################################################
#Writes the file out as a geojson

musg<-geojson_json(lsoa)

#writes to disk
geojson_write(musg, file = "filepath\\Museummap\\lsoawpopandmus.geojson")

mus2 = readOGR("filepath\\Museummap\\lsoawpopandmus.geojson" , "OGRGeoJSON")


