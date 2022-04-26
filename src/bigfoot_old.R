library(sp)
data(bigfoot)
aea.prj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 
+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
library(sp)
coordinates(bigfoot) <- ~Lon+Lat
proj4string(bigfoot) <- CRS("+proj=lonlat +datum=WGS84")
library(rgdal)
bigfoot.aea <- spTransform(bigfoot, CRS(aea.prj))
# Load the covariates:
data(USAWgrids)
gridded(USAWgrids) <- ~s1+s2
proj4string(USAWgrids) <- CRS(aea.prj)
# Visualize data:
data(SAGA_pal)
pnts <- list("sp.points", bigfoot.aea, pch="+", col="yellow")
spplot(USAWgrids[2], col.regions=rev(SAGA_pal[[3]]), sp.layout=pnts)

bigfoot_df <- as.data.frame(bigfoot)

base = get_map(location=c(-140,20,-60,65), maptype="terrain-background")

map1 = ggmap(base)

map1 +
  geom_point(data=bigfoot_df, aes(x=Lon, y=Lat), cex=2.5, color = "yellow") +
  labs(x="Longitude", y="Latitude") + # label the axes
  theme_bw() + theme(legend.position="bottom", axis.text = element_text(size = rel(0.75)), legend.key = element_rect(colour = "white"), axis.text.x = element_text(angle=45, vjust=0.5)) # tweak the plot's appearance and legend position


library(dismo)
