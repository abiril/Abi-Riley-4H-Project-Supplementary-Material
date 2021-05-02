library(readxl)
library(gstat)
library(sp)
library(dplyr) # for "glimpse"
library(ggplot2)
library(scales) # for "comma"
library(magrittr)
library(mapplots)
library(sf)
library(spData)
library(rgeos)
library(rgdal)
library(raster)
library(tmap)

temp <- read_excel("~/4th Year/Project/Data Sets/temperatures.xls")

plot(temp$long, temp$lat)

ggplot(temp, aes(long,lat, col=`temp C`))+
  geom_point() +
  scale_color_gradient(low = "blue", high="red")

tempfit <- lm(temp$`temp C` ~ lat + poly(long,3), data = temp)
summary(tempfit)

coordinates(temp) <- ~long + lat
plot(temp)
exp.var <- variogram(temp$`temp C` ~ 1, data = temp)
plot(exp.var)

exp.var.cloud <- variogram(temp2$`temp C` ~ 1, data = temp2, cloud = TRUE)
plot(exp.var.cloud)

fit.var <- fit.variogram(exp.var, vgm("Gau"))
summary(fit.var)
plot(exp.var, model = fit.var)


# to compare, recall the bubble plot above; those points were what there were values for. this is much more sparse
plot1 <- temp %>% as.data.frame %>%
  ggplot(aes(long, lat)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points with measurements")

plot1

y <- seq(20,55, by = 0.5)
x <- seq (-130,-60, by = 0.5)
temp.grid <- merge(x,y)

#temp.grid2 <- makegrid(temp, cellsize = 1)
#temp.grid2 <- SpatialPoints(temp.grid2, proj4string = CRS(proj4string(temp)))

#us_states

#temp.grid2 <- temp %>%
 # st_make_grid(cellsize = 1, what = "centers") %>%
  #st_intersection(us_states)
#plot(temp.grid2) 

us_states <- readOGR("~/4th Year/Project/R Code/us_state/cb_2018_us_nation_5m")
#plot(us_states)
#grid <- raster(extent(us_states))
#res(grid) <- 2
#gridpolygon <- rasterToPolygons(grid)
#land.grid <- intersect(us_states, gridpolygon)
#plot(land.grid)


ggplot()+
  geom_polygon(data = us_states, aes(x=long, y = lat, group = group), fill = NA, color = "black") + 
  geom_point(data = temp, aes(long,lat, col=`temp C`)) +
  scale_color_gradient(low = "blue", high="red") +
  xlim(-130,-60) + 
  ylim(22,55) +
  coord_equal()

plot(temp.grid)
coordinates(temp.grid) <-  ~ x + y
proj4string(temp.grid) <- CRS("+proj=longlat +datum=WGS84")

land.grid <- intersect(temp.grid, us_states)
plot(land.grid)


# this is clearly gridded over the region of interest
plot2 <- land.grid %>% as.data.frame %>%
  ggplot(aes(x,y)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points at which to estimate")

plot2
library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)

proj4string(temp) <- CRS("+proj=longlat +datum=WGS84 +no_defs ")
coordinates(temp.grid) <- ~long + lat
temp.kriged <- krige(temp$`temp C` ~ 1, temp, temp.grid, model=fit.var)

temp.kriged %>% as.data.frame %>%
  ggplot(aes(x=x, y=y)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "blue", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw()
