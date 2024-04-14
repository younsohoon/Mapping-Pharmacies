# Mapping the position of pharmacies in Waterloo
WaterlooRegion = readOGR(dsn=".",layer="WaterlooRegion")
ringroad = readOGR("ringroad.kml")
pharmacy = readOGR("pharmacies.kml")
M3 = readOGR("m3.kml")
WaterlooRegion.transformed = spTransform(WaterlooRegion, CRS(proj4string(ringroad)))

plot(WaterlooRegion.transformed[7,])
lines(ringroad, col="red")
points(M3, pch='.' ,col="blue")
points(pharmacy,pch='.',col="green")

# this code is creating a point pattern object M3.ppp based on a set of points M3@coords that fall 
# within a specific polygonal window PolyWat[310:1, ], and then plotting this point pattern object 
# within that window. This can be useful for visualizing patterns of points and their distribution 
# within a specific area.

PolyWat <- WaterlooRegion.transformed@polygons[[7]]@Polygons[[1]]@coords
M3.ppp <- as.ppp(M3@coords[1:2],  W=owin(poly=PolyWat[310:1, ]))
plot(M3.ppp)


phr.location = data.frame(pharmacy@coords[,1:2])
# remove the repeated location
phr.location = phr.location[duplicated(phr.location)==FALSE,]

PolyWat <- WaterlooRegion.transformed@polygons[[7]]@Polygons[[1]]@coords
phr.location.ppp <- as.ppp(phr.location[1:2],  W=owin(poly=PolyWat[310:1, ]))
plot(phr.location.ppp)


phr.ppp = phr.location.ppp
# calculate and plot the k-function
k = Kest(phr.ppp)
plot(k,main="K function")

# Calculate and plot the G-function 
g = Gest(phr.ppp)
plot(g,main="G function")