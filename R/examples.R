#---------------------------------------------------------------------------#
#   The default example
#---------------------------------------------------------------------------#

foo.ncv <- function()
{
     ##  Create a new NetCDF dataset and define two dimensions
     nc <- create.nc(paste(tempdir(),"/foo.nc",sep=""))

     dim.def.nc(nc, "station", 5)
     dim.def.nc(nc, "time", unlim=TRUE)
     dim.def.nc(nc, "max_string_length", 32)

     ##  Create three variables, one as coordinate variable
     var.def.nc(nc, "time", "NC_INT", "time")
     var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))
     var.def.nc(nc, "name", "NC_CHAR", c("max_string_length", "station"))

     ##  Put some _FillValue attribute for temperature
     att.put.nc(nc, "temperature", "_FillValue", "NC_DOUBLE", -99999.9)

     ##  Define variable values
     mytime        <- c(1:2)
     mytemperature <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, NA, NA, 9.9)
     myname        <- c("alfa", "bravo", "charlie", "delta", "echo")

     ##  Put the data
     var.put.nc(nc, "time", mytime)
     var.put.nc(nc, "temperature", array(mytemperature, dim=c(5,2)) )
     var.put.nc(nc, "name", myname)

     close.nc(nc)
}

#---------------------------------------------------------------------------#
#   Pavel's var.put.nc example
#---------------------------------------------------------------------------#

ex.pavel.ncv <- function()
{
    ##  Define dimensions and coordinates

    dim1 <- dim.def.ncv("station", 5)
    dim2 <- coord.def.ncv("time", data=c(1,2), unlim=TRUE)
    dim3 <- dim.def.ncv("max_string_len", 30)

    ##  Define the variables
    name <- var.def.ncv("name", c("alfa", "bravo", "charlie", "delta", "echo"),
	dim=list(dim3, dim1) )

    temp <- var.def.ncv("temperature", 
	c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, NA, NA, 9.9), 
	xtype="NC_FLOAT", count=c(5,2),
	dim=list(dim1, dim2), 
	att=list("_FillValue", -99999.9), mvar=list(name) )

    var.put.ncv(paste(tempdir(),"/foo2.nc",sep=""), temp, verb=T) 
}


#---------------------------------------------------------------------------#
#   CF Conventions: Example 5.1
#---------------------------------------------------------------------------#

ex.cf5.1.ncv <- function()
{
    ##  Define dimensions and coordinates

    lon <- coord.def.ncv("lon", seq(36)*1.0,
	att=list("long_name", "longitude", "units", "degrees_east") )
    lat <- coord.def.ncv("lat", seq(18)*1.0, 
	att=list("long_name", "latitude", "units", "degrees_north") )
    pres <- coord.def.ncv("pres", seq(15)*1.0,
	att=list("long_name", "pressure", "units", "hPa") )
    time <- coord.def.ncv("time", seq(4), xtype="NC_DOUBLE",
	att=list("long_name", "time", "units", "days since 1990-1-1") )

    ##  Define the variables
    xwind <- var.def.ncv("xwind", xtype="NC_FLOAT", 
	dim=list(lon, lat, pres, time),
	att=list("long_name", "zonal wind", "units", "m/s") )

    var.put.ncv(paste(tempdir(),"/foo.nc",sep=""), xwind, data=FALSE)
    var <- var.get.ncv(paste(tempdir(),"/foo.nc",sep=""), "xwind", mode="cf")
    print.ncv(var)

}


#---------------------------------------------------------------------------#
#   CF Conventions: Example 5.2
#---------------------------------------------------------------------------#

ex.cf5.2.ncv <- function()
{
    ## Define dimensions and coordinates

    xc <- coord.def.ncv("xc", seq(128)*1.0,
        att=list("long_name", "x-coordinate in Cartesian system",
		 "units", "m") )
    yc <- coord.def.ncv("yc", seq(64)*1.0,
        att=list("long_name", "y-coordinate in Cartesian system", 
		"units", "m") )
    lev <- coord.def.ncv("lev", seq(18)*1.0,
        att=list("long_name", "pressure level", "units", "hPa") )

    ## Define auxillary coordinate variables

    lon <- var.def.ncv("lon", xtype="NC_FLOAT",
	dim=list(xc, yc), 
	att=list("long_name", "longitude", "units", "degrees_east") )

    lat <- var.def.ncv("lat", xtype="NC_FLOAT",
        dim=list(xc, yc),
        att=list("long_name", "latitude", "units", "degrees_north") )

    ##  Define the variables

    var <- var.def.ncv("T", data=array(0, dim=c(128,64,18)),
        dim=list(xc, yc, lev),
        att=list("long_name", "temperature", "units", "K",
		    "coordinates", "lon lat"),
	mvar=list(lon, lat) )

    var.put.ncv(paste(tempdir(),"/foo.nc",sep=""), var)

    var <- var.get.ncv(paste(tempdir(),"/foo.nc",sep=""), "T", mode="cf")

    print.ncv(var)

}

#---------------------------------------------------------------------------#
#   CF Conventions: Example 5.6
#---------------------------------------------------------------------------#

ex.cf5.6.ncv <- function()
{
    ## Define dimensions and coordinates

    rlon <- coord.def.ncv("rlon", seq(128)*1.0,
        att=list("long_name", "longitude in rotated pole grid",
                 "units", "degrees",
		"standard_name", "grid_longitude") )
    rlat <- coord.def.ncv("rlat", seq(64)*1.0,
        att=list("long_name", "latitude in rotated pole grid",
                "units", "degrees",
		"standard_name", "grid_longitude") )
    lev <- coord.def.ncv("lev", seq(18)*1.0,
        att=list("long_name", "pressure level", "units", "hPa") )

    ## Define auxillary coordinate variables

    lon <- var.def.ncv("lon", xtype="NC_FLOAT",
        dim=list(rlon, rlat),
        att=list("long_name", "longitude", "units", "degrees_east") )

    lat <- var.def.ncv("lat", xtype="NC_FLOAT",
        dim=list(rlon, rlat),
        att=list("long_name", "latitude", "units", "degrees_north") )

    ## Define grid mapping variable

    gmap <- var.def.ncv("rotated_pole", 0.,
	att=list("grid_mapping_name", "rotated_latitude_longitude",
		"grid_north_pole_latitude", 32.5,
		"grid_north_pole_longitude", 170.) )

    ## Define the data variable

    var <- var.def.ncv("T", data=array(0, dim=c(128,64,18)),
        dim=list(rlon, rlat, lev),
        att=list("long_name", "temperature", "units", "K",
                    "coordinates", "lon lat",
		    "grid_mapping", "rotated_pole"),
        mvar=list(lon, lat, gmap),
	gatt=list("Conventions","CF-1.0") )

    var.put.ncv(paste(tempdir(),"/foo.nc",sep=""), var)

    var <- var.get.ncv(paste(tempdir(),"/foo.nc",sep=""), "T", mode="cf")

    print.ncv(var)

    var.put.ncv(paste(tempdir(),"/foo2.nc",sep=""), var)

}


#---------------------------------------------------------------------------#
#   CF Conventions: Example 7.2
#---------------------------------------------------------------------------#

ex.cf7.2.ncv <- function()
{
    ## Define dimensions and coordinates

    cell <- dim.def.ncv("cell", 2562)
    time <- coord.def.ncv("time", seq(12)*1.0,
        att=list("long_name", "time",
                 "units", "days since 1979-01-01 0:0:0") )
    nv <- dim.def.ncv("nv", 6)

    ## Define auxillary coordinate variables

    lon.vert <- var.def.ncv("lon_vertices", xtype="NC_FLOAT", dim=list(nv, cell) )
    lat.vert <- var.def.ncv("lat_vertices", seq(2562*12)*1.0, dim=list(nv, cell) )

    lon <- var.def.ncv("lon", xtype="NC_FLOAT",
        dim=list(cell),
        att=list("long_name", "longitude", "units", "degrees_east",
		"bounds", "lon_vertices"),
	mvar=list(lon.vert) )

    lat <- var.def.ncv("lat", xtype="NC_FLOAT",
        dim=list(cell),
        att=list("long_name", "latitude", "units", "degrees_north",
		"bounds", "lat_vertices"),
	mvar=list(lat.vert) )

    ## Cell measures

    cell.area <- var.def.ncv("cell_area", xtype="NC_FLOAT",
	dim=list(cell),
	att=list("long_name", "area of grid cell", "standard_name", "area",
		"units", "m2") )

    ## Define the data variable

    var <- var.def.ncv("PS", xtype="NC_FLOAT",
        dim=list(cell, time),
        att=list("units", "hPa", "coordinates", "lon lat",
                    "cell_measures", "area: cell_area"),
        mvar=list(lon, lat, cell.area),
        gatt=list("Conventions","CF-1.0") )

    var.put.ncv(paste(tempdir(),"/foo.nc",sep=""), var)
    var <- var.get.ncv(paste(tempdir(),"/foo.nc",sep=""), "PS", mode="cfall")
    print.ncv(var)
    var.put.ncv(paste(tempdir(),"/foo2.nc",sep=""), var)
}
