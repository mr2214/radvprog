library(rgdal); library(raster); library(ncdf4); library(data.table)
setwd("C:/Users/Toshiba/Downloads/r_spatial_data")
pha <- readOGR(dsn = "PRAHA_P_shp",
               layer = "PRAHA_P")

#plot(x = pha,
#     col = "red4")

#pha@data
#pha@proj4string 

pha_wgs <- spTransform(x = pha,
                       CRSobj = CRS(projargs = "+init=epsg:4326"))

#plot(x = pha_wgs,
#     col = "royalblue4")

extent(x = pha)
extent(x = pha_wgs)

fls <- list.files(path = "raw (3)/raw",
                  pattern = ".nc",
                  full.names = TRUE)



#plot(x = b[[1]])

######################################################

fls <- fls[c(1:36,38:566)] 
number <- 565
pr <- data.table()
lat <- data.table()
lon <- data.table()
b <- raster()
list_of_files <- vector(mode = "list", length = number)
system.time(
for (i in 1:number){
  nc <- nc_open(filename = fls[i])
  pr <- ncvar_get(nc = nc,varid = "pr")
  lon <- ncvar_get(nc = nc,varid = "lon")
  lat <- ncvar_get(nc = nc,varid = "lat")
  nc_close(nc)
  b <- brick(x = pr)
  extent(x = b) <- c(range(x = lon),range(x = lat))
  ext <- extent(x = pha_wgs) * 1.75
  aux <- as.data.table(x = t(x = extract(x = b,y = ext)))
  names(x = aux) <- as.character(x = cellsFromExtent(object = b[[1]], extent = ext))
  n <- nchar(x = fls[i])
  d <- strsplit(x = substr(x = fls[i],start = n - 27,stop = n - 3),split = "-")
  dt <- data.table(date = seq(from = as.POSIXct(x = d[[1]][1],format = "%Y%m%d%H%M"),
                            to = as.POSIXct(x = d[[1]][2],format = "%Y%m%d%H%M"),by = "hour"),aux)
  dt_m <- melt(data = dt,id.vars = "date", variable.name = "id",variable.factor = FALSE)
  dt_m[, c("lon", "lat") := as.data.frame(x = xyFromCell(object = b[[1]],cell = as.numeric(x = id)))]
  dt_m[, forcing := paste(c(strsplit(fls[i], '_')[[1]][4:9]), collapse = "_")]
  list_of_files[[i]] <- dt_m
  print(i)})
 ######################################################
all_data <- rbindlist(list_of_files)

e <- try(
  expr = {
    
    pr <- ncvar_get(nc = nc,
                    varid = "pr")
    
  }
  , silent = TRUE
)

if (inherits(x = e,
             what = "try-error")) {
  
  e <- try(
    expr = {
      
      pr <- ncvar_get(nc = nc,
                      varid = "precipitation_flux")
      
    }
    , silent = TRUE
  )
}

e <- try(
  expr = {
    
    lon <- ncvar_get(nc = nc,
                     varid = "lon")
    lat <- ncvar_get(nc = nc,
                     varid = "lat")
    
  }
  , silent = TRUE
)

if (inherits(x = e,
             what = "try-error")) {
  
  e <- try(
    expr = {
      
      lon <- ncvar_get(nc = nc,
                       varid = "longtitude")
      lat <- ncvar_get(nc = nc,
                       varid = "latitude")
      
    }
    , silent = TRUE
  )
}

