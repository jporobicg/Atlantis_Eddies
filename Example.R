rm(list = ls())
## libraries
library(RColorBrewer)
library(fields)
library(lattice)
library(RNetCDF)
library(cluster)
## Tools
source('/home/demiurgo/Documents/PhD/Atlantis_Model/tools/oceano_tool.R')

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Identification Eddies
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
s.folder <- '/home/demiurgo/Documents/PhD/Oceanography/_oceanography_construction_poly/Count_eddies/eddies_output'
folder <- '/media/demiurgo/TOSHIBA EXT/Data_fisica_AJF/ROMS/avg/'
for(id.year in 2000 : 2008){
    folder2  <- paste(folder, id.year, sep = '')
    files    <- dir(folder2, pattern = 'nest_avg_parent')
    ## movement of the eddy 1.5 km and the area is 300km
    out.year <- eddies.year(files, folder2)
    ##save fle
    save(out.year, file = paste(s.folder,'/' , id.year, '.RData', sep = ''))
    rm(out.year)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tracking eddies trajectory and duration
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
o.folder <- '/home/demiurgo/Documents/2015/Oceanography/Count_eddies/eddies_output/identification'
s.folder <- '/home/demiurgo/Documents/2015/Oceanography/Count_eddies/eddies_output/tracking'
for(id.year in 2000 : 2008){
    file             <- paste(o.folder,'/' , id.year, '.RData', sep = '')
    ## the duratin of the eddies if for around one month
    cic.track.eddies <- eddy.track(file, cyclonic = TRUE,  min.temp = 6)
    ant.track.eddies <- eddy.track(file, cyclonic = FALSE, min.temp = 6)
    ##save fle
    save(cic.track.eddies, file = paste(s.folder,'/cic_' , id.year, '.RData', sep = ''))
    save(ant.track.eddies, file = paste(s.folder,'/ant_' , id.year, '.RData', sep = ''))
    rm('cic.track.eddies', 'ant.track.eddies')
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Eddies by polygons and by Season
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coor.dat      <- 'coordinates.RData'
polygons.dat  <- '/home/demiurgo/Documents/PhD/Polygonos/Map/Polygons_27042016.csv'
o.folder      <- '/home/demiurgo/Documents/PhD/Oceanography/Count_eddies/eddies_output/tracking'
centroids     <- read.csv('centroids.csv')
years         <- 2000 : 2008
nyears        <- length(years)
cyc.eddy.year <- ant.eddy.year <- Sum <- list()

for(i in 1 : nyears){
    ant.file   <- paste(o.folder,'/ant_' , years[i], '.RData', sep = '')
    cyc.file   <- paste(o.folder,'/cic_' , years[i], '.RData', sep = '')
    cyc.eddy.year[[i]] <- bypol.eddy(cyc.file, coor.dat, polygons.dat)
    ant.eddy.year[[i]] <- bypol.eddy(ant.file, coor.dat, polygons.dat, cyclonic = FALSE)
    ## cummulative number
    if(i==1){
        Sum[[1]] <- unlist(cyc.eddy.year[[1]][[1]][, 1 : 6])
        Sum[[2]] <- unlist(ant.eddy.year[[1]][[1]][, 1 : 6])
    } else {
        Sum[[1]] <- Sum[[1]] + unlist(cyc.eddy.year[[i]][[1]][, 1 : 6])
        Sum[[2]] <- Sum[[2]] + unlist(ant.eddy.year[[i]][[1]][, 1 : 6])
    }
}

file.name <- ('/home/demiurgo/Documents/PhD/Oceanography/Count_eddies/eddies_output/Summary/Summary.RData')
#save(Sum, file = file.name)

## Final Step.  Summary
## Average number of eddies
##~~~~~~~~~~~~~~~~~~~~
source('/home/demiurgo/Documents/PhD/Atlantis_Model/tools/oceano_tool.R')
load(file.name)
average <- lapply(Sum, function(x)  x / 9)
sum.Cyc <- data.frame(matrix(average[[1]], ncol = 6))
sum.Ant <- data.frame(matrix(average[[2]], ncol = 6))
tot.edd <- cbind(sum.Cyc[, 1 : 4] + sum.Ant[, 1 : 4], sum.Cyc[, 5 : 6])

## Weighted area
w.cyc <- w.area(sum.Cyc, area.rm = c(48, 45))
w.ant <- w.area(sum.Ant, area.rm = c(48, 45))
w.edd <- w.area(tot.edd, area.rm = c(48, 45))




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plotting
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# just some twiks for ploting
col2 <- brewer.pal(9, "RdYlGn")
library(ggplot2)
source('/home/demiurgo/Documents/PhD/Atlantis_Model/tools/Atlantis_tools.R')
load('coordinates.RData')
polygons <- read.poly(polygons.dat)
lon      <- as.matrix(polygons$coor[, c(seq(from = 1, to = dim(polygons$coor)[2], by = 2))])
lat      <- as.matrix(polygons$coor[, c(seq(from = 2, to = dim(polygons$coor)[2], by = 2))])
png('eddies_total2.png', width = 1200, height = 700)
par(mfrow = c(2, 2),  mar=c(0,0,1,0), oma=c(0.5,0,2,.5)) # just some twiks for ploting
#col2<-brewer.pal(9, "YlGnBu")

for( j in 1 : 4){
plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c( -82,  - 72), ylim = c( - 35,  - 32), main = (colnames(w.edd[[2]])[j]))

 for( i in 1 : dim(lat)[1]){
            polygon(lon[i, ], lat[i, ], col  = col2[(w.edd[[2]][i, j] / max(w.edd[[2]][, 1 : 4], na.rm=TRUE) * 9)],  border = 'gray9')
        }
}

dev.off()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Talbe output
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
eddies <- w.edd[[2]][order(w.edd[[2]]$BoxId), ]

write.table(eddies, 'Eddies.csv')
