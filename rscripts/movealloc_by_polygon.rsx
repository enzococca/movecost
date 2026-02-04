##movecost script=group
##Movealloc by Polygon=name
##CRS=crs
##Area_of_interest=vector polygon
##Points=vector point
##Move=enum literal 16;8;4
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h
##Breaks=number 1
##Time=selection h;m
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9
##Output_DTM=output raster
##Output_Alloc_Raster=output raster
##Output_Isoline=output vector
##Output_Polygon=output vector
##showplots

# Function to install missing packages
install_if_missing <- function(packages) {
    for (pkg in packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            message(paste("Installing package:", pkg))
            install.packages(pkg, repos = "https://cloud.r-project.org/", dependencies = TRUE)
        }
    }
}

# All dependencies required by movecost (including indirect dependencies)
all_dependencies <- c("chron", "terra", "gdistance", "Matrix", "igraph", "sp", "sf", "raster", "progress")
install_if_missing(all_dependencies)

# Function to check and update movecost package
check_movecost_version <- function(min_version = "2.1") {
    if (!requireNamespace("movecost", quietly = TRUE)) {
        message("Installing movecost package...")
        install.packages("movecost", repos = "https://cloud.r-project.org/", dependencies = TRUE)
    } else {
        installed_version <- as.character(packageVersion("movecost"))
        if (compareVersion(installed_version, min_version) < 0) {
            message(paste("Updating movecost from", installed_version, "to latest version..."))
            install.packages("movecost", repos = "https://cloud.r-project.org/", dependencies = TRUE)
        }
    }
}

# Check and update movecost if needed (minimum version 2.1)
check_movecost_version("2.1")

# Load libraries (chron, gdistance, igraph are movecost dependencies)
library(chron)
library(Matrix)
library(igraph)
library(gdistance)
library(sp)
library(sf)
library(terra)
library(raster)
library(movecost)
library(progress)


# Define utility function for mapping numbers to strings
get_string_value <- function(val, string_map) {
    string_map[val + 1] # +1 because R indexing starts from 1
}
# Get CRS from input vector (Origin)
# Assuming 'Origin' is an sf object and you need to get its CRS
origin_crs <- CRS
p <- as_Spatial(Points)
studyplot_sp <-as_Spatial(Area_of_interest)


# Map numbers to strings using utility function
function_map <- c("t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Function <- get_string_value(Function, function_map)

time_map <- c("h", "m")
Time <- get_string_value(Time, time_map)
# Map numbers to strings using utility function
# Move is passed as literal value (16, 8, or 4), just convert to numeric
Move <- as.numeric(Move)


r<-movealloc(dtm=NULL, origin=p, studyplot=studyplot_sp,  move=Move, time=Time,funct=Function, cogn.slp=Cognitive_Slope,  sl.crit=Critical_Slope,W=Walker_Body_Weight, L=Carried_Load_Weight,N=N, V=Speed, z=Zoom_Level, cont.lab=TRUE, isolines=TRUE, breaks=Breaks, export=FALSE)

dem = r$dtm
sf_dem = dem
# Imposta il CRS se non è definito
if (is.na(crs(sf_dem))) {
  crs(sf_dem) <- CRS # esempio con WGS84
}
crs(studyplot_sp)<-crs(sf_dem)
sf_dem_cropped = mask(sf_dem, studyplot_sp)
Output_DTM=sf_dem_cropped


raster <- r$cost.allocation.raster
if (is.na(crs(raster))) {
  crs(raster) <- CRS # esempio con WGS84
}


Output_Alloc_Raster=raster

a1=r$isolines
sf_object = st_as_sf(a1)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object))) {
  sf_object <- st_set_crs(sf_object, CRS) # esempio con WGS84
}

# Add length fields for isolines
sf_object$length_m <- as.numeric(st_length(sf_object))
sf_object$length_km <- sf_object$length_m / 1000

# Ora esporta il file
Output_Isoline=sf_object

a2<-r$alloc.boundaries
sf_object2 = st_as_sf(a2)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object2))) {
  sf_object2 <- st_set_crs(sf_object2, CRS) # esempio con WGS84
}

# Add area fields for allocation polygons
sf_object2$area_m2 <- as.numeric(st_area(sf_object2))
sf_object2$area_km2 <- sf_object2$area_m2 / 1000000
sf_object2$area_ha <- sf_object2$area_m2 / 10000

Output_Polygon=sf_object2
