#===========================================================================#
#									    #
#  Name:       ncvar.R							    #
#									    #
#  Version:    0.9-0							    #
#									    #
#  Purpose:    High-level NetCDF interface for R.			    #
#									    #
#  Author:     Juerg Schmidli						    #
#									    #
#  Copyright:  (C) 2004 Juerg Schmidli					    #
#									    #
#===========================================================================#
#									    #
#  This program is free software; you can redistribute it and/or modify	    #
#  it under the terms of the GNU General Public License as published by	    #
#  the Free Software Foundation; either version 2 of the License, or	    #
#  (at your option) any later version.					    #
#									    #
#  This program is distributed in the hope that it will be useful,	    #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of	    #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	    #
#  GNU General Public License for more details.				    #
#									    #
#  You should have received a copy of the GNU General Public License	    #
#  along with this program; if not, write to the Free Software		    #
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA  #
#									    #
#===========================================================================#
#  Implementation and Revisions						    #
#---------------------------------------------------------------------------#
#  Author   Date       Description					    #
#  ------   ----       -----------					    #
#  js	    2004-07-27 major revisions					    #
#  js       2004-06-25 First implementation				    #
#									    #
#===========================================================================# 

#---------------------------------------------------------------------------#
#   att.def.ncv								    #
#
#   create an attribute object (class: att.ncv):
#	att$name
#	att$value
#	att$xtype
#
#---------------------------------------------------------------------------#

att.def.ncv <- function(name=NULL, value=NULL, xtype=NULL, attlist=NULL)
{
    if (is.null(name) && is.null(attlist)) {
	return(NULL)
    }

    if (!is.null(name)) {
	if (is.null(xtype)) {
	    xtype <- xtype.ncv(value)
	    if (is.null(xtype)) { stop("Variable has wrong type") }
	}

	att <- list()
	attr(att, "class") <- "att.ncv"
	att$name <- name
	att$value <- value
	att$xtype <- xtype
    } 

    if (!is.null(attlist)) {
	if (class(attlist) == "att.list.ncv") {
	    return(attlist)
	}
	if (!is.list(attlist)) { stop("Attlist must be of type list") }
	n.att <- length(attlist)
	if ((n.att %%2) != 0) { stop("Attlist must have an even number of elements") }
	n.att <- n.att / 2

	att <- list()
	attr(att, "class") <- "att.list.ncv"

	for (i in 1:n.att) {
	    if (!is.character(attlist[[2*i-1]])) { stop("Attribute name must be of character type") }
	    att[[i]] <- list()
	    attr(att[[i]], "class") <- "att.ncv"
	    att[[i]]$name <- attlist[[2*i-1]]
	    att[[i]]$value <- attlist[[2*i]]
	    xtype <- xtype.ncv(attlist[[2*i]])
	    if (is.null(xtype)) { stop("Attribute has wrong type") }
	    att[[i]]$xtype <- xtype
	}
    }

    return(att)
}


#---------------------------------------------------------------------------#
#   att.get.ncv (internal)                                                  #
#---------------------------------------------------------------------------#

att.get.ncv <- function(ncfile, variable, natts)
{
    att <- list()
    attr(att, "class") <- "att.list.ncv"

    if (natts > 0) {
	for (i in 1:natts) {
	    att[[i]] <- list()
	    attr(att[[i]], "class") <- "att.ncv"

	    tmp <- att.inq.ncv(ncfile, variable, i-1)
	    att[[i]]$name <- tmp$name
	    att[[i]]$value <- att.get.nc(ncfile, variable, i-1)
	    att[[i]]$xtype <- tmp$type
	}
    }

    return(att)
}


#---------------------------------------------------------------------------#
#   att.inq.ncv (internal)                                                  #
#---------------------------------------------------------------------------#

att.inq.ncv <- function(nc, variable, attno)
{
    attinfo <- try(att.inq.nc(nc, variable, attno), silent=TRUE)
    if(class(attinfo) == "try-error") {
        return(NULL)
    } else
        return(attinfo)
}


#---------------------------------------------------------------------------#
#   att.val.ncv (internal)                                                  #
#---------------------------------------------------------------------------#

att.val.ncv <- function(att, name)
{
    n.att <- length(att)
    if (n.att > 0) {
	for (i in 1:n.att) {
	   if (att[[i]]$name == name)
		return(att[[i]]$value)
	}
    }

    return(NULL)
}


#---------------------------------------------------------------------------#
#   att.put.ncv (internal)                                                  #
#---------------------------------------------------------------------------#

att.put.ncv <- function(ncfile, variable, att)
{
    n.att <- length(att)
    if (n.att > 0) {
	for (i in 1:length(att)) {
	    if (class(att[[i]]) != "att.ncv") 
		stop("att[[i]] is not of type att.ncv")
	    att.put.nc(ncfile, variable, att[[i]]$name, att[[i]]$xtype, att[[i]]$value)
	}
    }
}


#---------------------------------------------------------------------------#
#   coord.def.ncv                                                           # 
#---------------------------------------------------------------------------#

coord.def.ncv <- function(name, data=NULL, xtype=NULL, att=NULL, mvar=NULL, 
			    unlim=FALSE)
{
    return(var.def.ncv(name, data=data, xtype=xtype, att=att, mvar=mvar, 
	    unlim=unlim, coord=TRUE) )
}

#---------------------------------------------------------------------------#
#   dim.def.ncv                                                             #
#
#   create a dimension object (class: dim.ncv):
#       dim$name
#       dim$value
#       dim$unlim
#
#---------------------------------------------------------------------------#

dim.def.ncv <- function(name=NULL, value=NULL, unlim=FALSE, dimlist=NULL)
{
    if (is.null(name) && is.null(dimlist)) {
        return(NULL)
    }

    if (!is.null(name)) {
        dim <- list()
        attr(dim, "class") <- "dim.ncv"
        dim$name <- name
	if (unlim) { 
	    dim$value <- -1
        } else {
	    dim$value <- value 
	}
        dim$unlim <- unlim
    } 

    if (!is.null(dimlist)) {
        if (!is.list(dimlist)) { stop("Dimlist must be of type list") }
        n.dim <- length(dimlist)
        if ((n.dim %%2) != 0) { stop("Dimlist must have an even number of elements") }
        n.dim <- n.dim / 2

        dim <- list()
        attr(dim, "class") <- "dim.list.ncv"

        for (i in 1:n.dim) {
            if (!is.character(dimlist[[2*i-1]])) { stop("Dimension name must be of character type") }
            dim[[i]] <- list()
            attr(dim[[i]], "class") <- "dim.ncv"
            dim[[i]]$name <- dimlist[[2*i-1]]
            dim[[i]]$value <- dimlist[[2*i]]
            dim[[i]]$unlim <- FALSE
        }
    }

    return(dim)
}


#---------------------------------------------------------------------------#
#   dim.get.ncv (internal)                                                  #
#---------------------------------------------------------------------------#

dim.get.ncv <- function(ncfile, dimension)
{
    dim <- list()
    attr(dim, "class") <- "dim.ncv"
    tmp <- dim.inq.nc(ncfile, dimension)
    dim$name <- tmp$name
    dim$value <- tmp$length
    dim$unlim <- tmp$unlim

    return(dim)
}


#---------------------------------------------------------------------------#
#   dim.put.ncv (internal)                                                  #
#---------------------------------------------------------------------------#

dim.put.ncv <- function(ncfile, dim)
{
    if (class(dim) != "dim.ncv") stop("dim is not of class dim.ncv")

    if (exists("rv", inherits=FALSE)) rm(rv)
    try(rv <- dim.inq.nc(ncfile, dim$name), silent=TRUE)
    if (!exists("rv", inherits=FALSE)) {
        dim.def.nc(ncfile, dim$name, dimlength=dim$value, unlim=dim$unlim)
    } else {
	## dimension already defined
    }
    return(NULL)
}


#---------------------------------------------------------------------------#
#   dim.val.ncv (internal)                                                  #
#---------------------------------------------------------------------------#

dim.val.ncv <- function(dim)
{
    n.dim <- length(dim)
    if (n.dim > 0) {
	rv <- vector(n.dim, mode="numeric")
	for (i in 1:n.dim) {
	    if (class(dim[[i]]) == "dim.ncv")
		rv[i] <- dim[[i]]$value
	    if (class(dim[[i]]) == "coord.ncv")
		rv[i] <- length(dim[[i]]$data)
	}
    } else {
	rv <- NULL
    }

    return(rv)
}


#---------------------------------------------------------------------------#
#   print.ncv                                                               #
#---------------------------------------------------------------------------#

print.ncv <- function(x, ...)
{
    n.dim <- length(x$dim)
    n.mvar <- length(x$mvar)

    cat("dimensions: \n")
    if (n.dim > 0) {
	for (i in 1:n.dim) {
	    if (class(x$dim[[i]]) == "dim.ncv")
		len <- x$dim[[i]]$value
	    else
		len <- length(x$dim[[i]]$data)
    
	    cat("	", x$dim[[i]]$name, " = ", len, " ; \n", sep="")
	}
    }

    # coordinate variables

    cat("\n")
    cat("variables: \n")
    if (n.dim > 0) {
	for (i in 1:n.dim) {
	    if (class(x$dim[[i]]) == "coord.ncv")
		print.var.ncv(x$dim[[i]])
	}
    }

    # other variables

    if (n.mvar > 0) {
	for (i in 1:n.mvar) {
	    if (class(x$mvar[[i]]) == "var.ncv")
		print.var.ncv(x$mvar[[i]])
	}
    }

    # data variable

    if (class(var) == "var.ncv")
	print.var.ncv(x)
 
    # list of attribute objects

    if (class(x[[1]]) == "att.ncv" && length(x) > 0) {
        n.att <- length(x)
	for (i in 1:n.att) {
            if (is.numeric(x[[i]]$value[1])) {
                cat("                ", "variable:",
                    x[[i]]$name, " = ", x[[i]]$value, " ; \n", sep="")
            } else {
                cat("                ", "variable:",
                    x[[i]]$name, " = \"", x[[i]]$value, "\" ; \n", sep="")
            }
        }
    }

    # global attributes

    cat("\n")
    cat("// global attributes: \n")

    n.gatt <- length(x$gatt)
    if (n.gatt > 0) {
	for (i in 1:n.gatt) {
           if (is.numeric(x$gatt[[i]]$value)) {
                cat("                ", ":",
                    x$gatt[[i]]$name, " = ", x$gatt[[i]]$value, " ; \n", sep="")
            } else {
                cat("                ", ":",
                    x$gatt[[i]]$name, " = \"", x$gatt[[i]]$value, "\" ; \n", sep="")
            }
        }
    }


}

#---------------------------------------------------------------------------#
#   print.var.ncv							    #
#---------------------------------------------------------------------------#

print.var.ncv <- function(x, ...)
{
    n.dim <- length(x$dim)
    if (n.dim > 0) {
	dimnames <- vector(n.dim, mode="character")
	for (i in 1:n.dim) {
	    dimnames[i] <- x$dim[[i]]$name
        }
    }

    if (n.dim > 0) {
	cat("        ", xtype2str.ncv(x$xtype), " ", x$name, "(", sep="")
	cat(dimnames, sep=",")
	cat(") ; \n", sep="")
    } else {
	cat("        ", xtype2str.ncv(x$xtype), " ", x$name, " ; \n", sep="")
    }

    n.att <- length(x$att)
    if (n.att > 0) {
        for (i in 1:n.att) {
	    if (is.numeric(x$att[[i]]$value[1])) {
		cat("                ", x$name, ":",
		    x$att[[i]]$name, " = ", x$att[[i]]$value, " ; \n", sep="")
	    } else {
                cat("                ", x$name, ":",
                    x$att[[i]]$name, " = \"", x$att[[i]]$value, "\" ; \n", sep="")
	    }
        }
    }

    # other variables

    #n.mvar <- length(x$mvar)
    #if (n.mvar > 0) {
    #    for (i in 1:n.mvar) {
    #        if (class(x$mvar[[i]]) == "var.ncv")
    #            print.var.ncv(x$mvar[[i]])
    #    }
    #}

}
    

#---------------------------------------------------------------------------#
#   var.def.ncv								    #
#
#   create a variable object (class: var.ncv and coord.ncv):
#       var$name
#       var$data
#       var$xtype
#	var$start
#	var$count
#	var$att <- list(att1, att2, ...)
#	var$dim <- list(dim1, coord2, ...)
#	var$mvar <- list(var1, var2, ...)
#	var$gatt <- list(att1, att2, ...)
#	var$unlim
#
#---------------------------------------------------------------------------#

var.def.ncv <- function(name, data=NULL, xtype=NULL, start=NA, count=NA, 
		    dim=NULL, att=NULL, mvar=NULL, gatt=NULL, 
		    coord=FALSE, unlim=FALSE)
{
    if (is.null(data) && is.null(xtype)) stop("Too few arguments")
    if (is.null(xtype)) {
	xtype <- xtype.ncv(data)
	if (is.null(xtype)) stop("Data has wrong type")
    }

    var <- list()
    if (coord) {
	if (xtype == "NC_CHAR") stop("A coordinate variable has to be numeric")
	attr(var, "class") <- "coord.ncv"
    } else {
	attr(var, "class") <- "var.ncv"
    }

    var$name <- name
    if (class(var) == "var.ncv" && is.numeric(var$data)) {
	if (!is.na(count[1])) {
	    var$data <- array(data, dim=count)
	} else {
	    count <- dim.val.ncv(dim)
	    if (!is.null(count))
		var$data <- array(data, dim=count)
	    else
		var$data <- data
	}
    } else {
	var$data <- data
    }
    var$xtype <- xtype
    var$start <- start
    var$count <- count
    var$att <- att.def.ncv(attlist=att)
    var$dim <- dim
    var$mvar <- mvar
    var$gatt <- att.def.ncv(attlist=gatt)
    var$unlim <- unlim

    return(var)
}   


#---------------------------------------------------------------------------#
#   var.get.ncv                                                             #
#
#   attributes which are analyzed for further metadata (see CF conventions):
#	- bounds		boundaries
#	- climatology		climatology
#	- coordinates		auxillary and scalar coordinates, labels
#	- grid_mapping		grid mapping variable
#       - ancillary_variables   ancillary variable
#	- cell_measures		cell measures
#	- cell_methods		cell methods
#
#   the coordinate variables are determined by the variable dimensions
#
#---------------------------------------------------------------------------#

var.get.ncv <- function(path, name, start=NA, count=NA, mode="attonly",
		    data=TRUE, gatts=FALSE, coord=FALSE, recursion=0, 
		    verbose=FALSE)
{
    if (verbose) cat("recursion = ", recursion, "; name = ", name, "\n")

    # open file, if necessary 

    if (class(path) == "NetCDF")
	ncfile <- path
    else {
	ncfile <- open.nc(path)
	if (is.null(ncfile)) { return(NULL) }
    }

    # define variable object

    var <- list()
    if (coord)
	attr(var, "class") <- "coord.ncv"
    else
	attr(var, "class") <- "var.ncv"

    tmp <- var.inq.ncv(ncfile, name)
    if (is.null(tmp)) { return(NULL) }

    var$name <- name
    if (data)
	var$data <- var.get.nc(ncfile, name, start=start, count=count)
    else
	var$data <- NULL

    var$xtype <- tmp$type
    var$start <- start
    var$count <- count
    var$unlim <- FALSE

    # get dimensions

    if (verbose) cat("reading dimensions...\n")
    var$dim <- list()
    attr(var$dim, "class") <- "dim.list.ncv"
    if (tmp$ndims > 0) {
	for (i in 1:tmp$ndims) {
	#    var$dim[[i]] <- dim.get.ncv(ncfile, tmp$dimids[tmp$ndims-i+1])
            var$dim[[i]] <- dim.get.ncv(ncfile, tmp$dimids[i])
	}
    }

    if (mode == "nometa") return(var)

    # get variable attributes

    var$att <- att.get.ncv(ncfile, tmp$id, tmp$natts)

    # get global attributes

    if (recursion == 0 && (gatts || mode != "nometa")) {
	finq <- file.inq.nc(ncfile)
	var$gatt <- att.get.ncv(ncfile, "NC_GLOBAL", finq$ngatts)
    } else
	var$gatt <- NULL

    # get coordinate variables

    if (mode == "netcdf" || mode == "cf" || mode == "cfall") {
	if (tmp$ndims > 0) {
	    for (i in 1:tmp$ndims) {
		if (var$dim[[i]]$name != var$name) {
		    coord <- var.inq.ncv(ncfile, var$dim[[i]]$name)
		    if (!is.null(coord)) {
			if (verbose) cat("reading coordinates...\n")
			var$dim[[i]] <- var.get.ncv(ncfile, coord$name, coord=TRUE,
				    mode=mode, rec=recursion+1, verbose=verbose)
		    }
		}
	    }
	}
    }

    # get bounds and climatology variables
    # get auxillary coordinate, label, and scalar coordinate variables
    # get grid mapping variables
    # get cell measures and cell methods variables

    varname <- list()
    attname <- NULL

    if (mode == "cf")
	attname <- c("bounds", "climatology", "coordinates", "grid_mapping")

    if (mode == "cfall")
	attname <- c("bounds", "climatology", "coordinates", "grid_mapping",
	    "ancillary_variables")

    if (!is.null(attname)) {
	n.mvar <- length(attname)
	if (n.mvar > 0) {
	    for (i in 1:n.mvar) {
		attval <- att.val.ncv(var$att, attname[i]) 
		if (!is.null(attval))
		    varname <- append(varname, strsplit(attval, " "))
	    }
	}
    }

    # special treatment for cell_measures and cell_methods
    if (mode == "cfall") {
	attval <- att.val.ncv(var$att, "cell_measures")
	if (!is.null(attval)) {
	    tmp <- strsplit(attval, " ")
	    n <- length(tmp[[1]])
	    if (n > 1) {
		for (i in 1:(n/2)) {
		    varname <- append(varname, tmp[[1]][2*i])
		}
	    }
	}

        attval <- att.val.ncv(var$att, "cell_methods")
        if (!is.null(attval)) {
            tmp <- strsplit(attval, " ")
            n <- length(tmp[[1]])
            if (n > 1) {
                for (i in 1:(n/2)) {
                    varname <- append(varname, tmp[[1]][2*i-1])
                }
            }
        }
    }    

    var$mvar <- list()
    varname <- unlist(varname)
    n.mvar <- length(varname)
    if (verbose) cat("n.mvar", n.mvar, "\n")
    if (verbose) cat(varname, "\n")
    if (n.mvar > 0) {
	for (i in 1:n.mvar) {
	    var$mvar[[i]] <- var.get.ncv(ncfile, varname[i], mode=mode, rec=recursion+1, verbose=verbose)
	    if (verbose) cat(varname[i],"\n")
	}
    }

    if (recursion == 0)
	close.nc(ncfile)

    return(var)
}


#---------------------------------------------------------------------------#
#   var.inq.ncv (internal)                                                  #
#---------------------------------------------------------------------------#

var.inq.ncv <- function(nc, name)
{
    varinfo <- try(var.inq.nc(nc, name), silent=TRUE)
    if(class(varinfo) == "try-error") {
	return(NULL)
    } else
        return(varinfo)
}


#---------------------------------------------------------------------------#
#   var.put.ncv                                                             #
#---------------------------------------------------------------------------#

var.put.ncv <- function(path, var, new=TRUE, define=TRUE, data=TRUE,
		    recursion=0, verbose=FALSE)
{
    # open file if necessary
    # ----------------------

    if (class(path) == "NetCDF")
	ncfile <- path
    else {
	if (new)
	    ncfile <- create.nc(path)
	else
	    ncfile <- open.nc(path, write=TRUE)
    }
    if (is.null(ncfile)) stop("Could not create netcdf file")

    if (is.null(var$data)) data=FALSE

    # define mode
    # -----------

    if (define) {
	if (verbose) cat("recursion = ", recursion, "; define = ", var$name, "\n")

	# define pure dimensions (recursion >= 1)

	if (class(var) == "dim.ncv") {
	    dim.put.ncv(ncfile, var)
	}

	# define coordinate variable

	if (class(var) == "coord.ncv" && recursion == 1) {
	    dim <- dim.def.ncv(var$name, length(var$data), unlim=var$unlim)
	    dim.put.ncv(ncfile, dim)
	    var.def.nc(ncfile, var$name, var$xtype, var$name)
	    att.put.ncv(ncfile, var$name, var$att)
	}

	# define data variable

	if (class(var) == "var.ncv") {

	    # iterate over dimensions/coordinates

	    n.dim <- length(var$dim)
	    if (n.dim > 0) {
		for (i in 1:length(var$dim)) {
		    var.put.ncv(ncfile, var$dim[[i]], data=FALSE, rec=recursion+1, verbose=verbose)
		}
	    }

	    # determine dimension names

	    if (n.dim > 0) {
		dimnames <- vector(n.dim, mode="character")
		for (i in 1:n.dim) {
		    dimnames[i] <- var$dim[[i]]$name
		}
	    }

	    if (n.dim > 0)
		rv <- var.def.nc(ncfile, var$name, var$xtype, dimnames[1:n.dim])
	    else
		rv <- var.def.nc(ncfile, var$name, var$xtype, NA)

	    ## print(rv)
	    att.put.ncv(ncfile, var$name, var$att)

	    # define global attributes
	    att.put.ncv(ncfile, "NC_GLOBAL", var$gatt)

	}

	# iterate over metadata variables

	n.mvar <- length(var$mvar)
	if (n.mvar > 0) {
	    for (i in 1:length(var$mvar)) {
		var.put.ncv(ncfile, var$mvar[[i]], data=FALSE, rec=recursion+1, verbose=verbose)
	    }
	}
    } 

    # data mode
    # ---------

    if (data && (class(var) == "var.ncv" || class(var) == "coord.ncv")) {
	if (verbose) cat("recursion = ", recursion, "; data = ", var$name, "\n")

	# iterate over coordinates
	# ------------------------

	if (recursion == 0) {
	    n.dim <- length(var$dim)
	    if (n.dim > 0) {
		for (i in 1:n.dim) {
		    var.put.ncv(ncfile, var$dim[[i]], define=FALSE, rec=recursion+1, verbose=verbose)
		}
	    }	
	}

	# write variable
	# --------------	

	if (class(var) == "var.ncv") {
	    # reform data
	
	    if (!is.na(var$count[1])) {
		var$data <- array(var$data, dim=var$count)
	    }
	
	    # put variable

	    var.put.nc(ncfile, var$name, var$data, start=var$start, count=var$count)

	}

	if (class(var) == "coord.ncv") {
	    var.put.nc(ncfile, var$name, var$data)
	}

	# iterate over metadata variables
	# -------------------------------

	n.mvar <- length(var$mvar)
	if (n.mvar > 0) {
	    for (i in 1:n.mvar) {
		var.put.ncv(ncfile, var$mvar[[i]], define=FALSE, rec=recursion+1, verbose=verbose)
	    }
	}

    }

    if (recursion == 0)
	close.nc(ncfile)

    if (verbose) cat("  ...returned\n")
}


#---------------------------------------------------------------------------#
#   xtype.ncv (internal)						    #
#---------------------------------------------------------------------------#

xtype.ncv <- function(value)
{
    if (is.character(value)) {
	xtype <- "NC_CHAR"
    } else {
	if (is.numeric(value)) {
	    if (is.integer(value)) {
		xtype <- "NC_INT"
	    } else {
		xtype <- "NC_FLOAT"
	    }
	} else {	    # neither character nor numeric
	    xtype <- NULL
	}
    }

    return(xtype)
}


#---------------------------------------------------------------------------#
#   xtype2str.ncv (internal)                                                    #
#---------------------------------------------------------------------------#

xtype2str.ncv <- function(xtype)
{
    rv <- NULL

    if (xtype == "NC_CHAR")
	rv <- "char"
    if (xtype == "NC_BYTE")
        rv <- "byte"
    if (xtype == "NC_SHORT")
        rv <- "short"
    if (xtype == "NC_INT")
        rv <- "int"
    if (xtype == "NC_FLOAT")
        rv <- "float"
    if (xtype == "NC_DOUBLE")
        rv <- "double"

    return(rv)
}
