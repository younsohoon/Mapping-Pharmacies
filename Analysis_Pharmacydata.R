# Mapping the position of pharmacies in Waterloo
WaterlooRegion = readOGR(dsn=".",layer="WaterlooRegion")
ringroad = readOGR("ringroad.kml")
pharmacy = readOGR("pharmacies.kml")
M3 = readOGR("m3.kml")
WaterlooRegion.transformed = spTransform(WaterlooRegion, CRS(proj4string(ringroad)))

# Visualization
plot(WaterlooRegion.transformed[7,])
lines(ringroad, col="red")
points(M3, pch='.' ,col="blue")
points(pharmacy,pch='.',col="green")


# Extract the population of WaterlooRegion 
pop = c(129920,10215,11260,20545,233222,25006,104986)
pop_df = data.frame(ID=row.names(WaterlooRegion@data),pop=pop)
WaterlooRegion = cbind(WaterlooRegion, pop_df)
WaterlooRegion@data
cat("pop of WaterlooRegion is", WaterlooRegion$pop)
# Calculate the density of WaterlooRegion
dens = pop/area[,2]
dens_df = data.frame(ID=row.names(WaterlooRegion@data),dens=dens)
WaterlooRegion = cbind(WaterlooRegion, dens_df)
cat("dens of WaterlooRegion is", WaterlooRegion$dens)
# Visualization
plotmap(values=log(WaterlooRegion@data$dens),WaterlooRegion)




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


# Two line graphs, estimated K function and estimated G function, are drawn respectively. 
# Basically, they are for analyzing the spatial pattern of point processes. 
# X-axis represents the measuring distance between points, and K(r) and G(r) are estimated K function and G function under CSR(complete spatial randomness). 
# The K function and G function are methods for analyzing the spatial pattern of point processes. 
# But K function is based on measuring the expected number of points within a given distance of an arbitrary point, while G function is measured based on the distribution function of the nearest neighbor distance of an arbitrary point. 
# Both function can be used to test whether the point process is clustered, dispersed, or random at different scales.
# The estimated K function and G function line graph have generally greater values than the empirical K function regardless of scale, which means that the point process is more clustered than the empirical distribution at all distances. 
# Hence, according to both graphs, pharmacies in Waterloo seems to be clustered.