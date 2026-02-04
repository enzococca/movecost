##movecost script=group
##Movealloc=name
##CRS=crs
##DTM=raster
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
##Speed=number 1.2
##Zoom_Level=number 9
##Output_Isoline=output vector
##Output_Polygon=output vector
##Output_Alloc_Raster=output raster
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

# Save filename before conversion (brick loses file reference)
dtm_filename <- tryCatch(sources(DTM)[1], error = function(e) filename(DTM))
studyplot_sp <- raster(DTM)

# Force read values into memory if not already loaded
if (!hasValues(studyplot_sp)) {
    if (!is.null(dtm_filename) && dtm_filename != "" && file.exists(dtm_filename)) {
        message(paste("Reading raster from file:", dtm_filename))
        studyplot_sp <- raster(dtm_filename)
        studyplot_sp <- readAll(studyplot_sp)
    } else {
        studyplot_sp <- readAll(studyplot_sp)
    }
}

crs(studyplot_sp) <- origin_crs

# Final check if raster has values
if (!hasValues(studyplot_sp)) {
    stop("ERROR: The input raster (DTM) has no values. Please provide a valid Digital Terrain Model with elevation data.")
}

# Check DTM validity
dtm_min <- cellStats(studyplot_sp, stat='min', na.rm=TRUE)
dtm_max <- cellStats(studyplot_sp, stat='max', na.rm=TRUE)
dtm_range <- dtm_max - dtm_min

message(paste("DTM min:", dtm_min, "max:", dtm_max, "range:", dtm_range))

# Validate DTM has actual elevation data (not a mask or constant values)
if (is.na(dtm_min) || is.na(dtm_max)) {
    stop("ERROR: The input DTM raster contains only NA values. Please provide a valid Digital Terrain Model.")
}
if (dtm_range < 1) {
    stop(paste("ERROR: The input DTM has no elevation variation (all values =", dtm_min, "). Please provide a valid Digital Terrain Model with actual elevation data, not a mask layer."))
}

# Replace only Inf values with max elevation (leave NAs as they are - movecost handles them)
inf_count <- cellStats(studyplot_sp == Inf, stat='sum', na.rm=TRUE)
if (inf_count > 0) {
    message(paste("Replacing", inf_count, "Inf values with max elevation"))
    studyplot_sp[studyplot_sp == Inf] <- dtm_max
}

print(studyplot_sp)

# Create a polygon from DTM valid land data extent for clipping
# Only include cells with elevation > 0 (exclude sea/water)
dtm_land <- studyplot_sp
dtm_land[dtm_land <= 0] <- NA
dtm_poly <- rasterToPolygons(dtm_land, dissolve=TRUE)
dtm_sf_boundary <- st_as_sf(dtm_poly)
if (is.na(st_crs(dtm_sf_boundary))) {
  dtm_sf_boundary <- st_set_crs(dtm_sf_boundary, CRS)
}

# Map numbers to strings using utility function
function_map <- c("t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Function <- get_string_value(Function, function_map)

time_map <- c("h", "m")
Time <- get_string_value(Time, time_map)
# Map numbers to strings using utility function
# Move is passed as literal value (16, 8, or 4), just convert to numeric
Move <- as.numeric(Move)
r<-movealloc(dtm=studyplot_sp, origin=p, studyplot=NULL,funct=Function, cogn.slp=Cognitive_Slope,  sl.crit=Critical_Slope,W=Walker_Body_Weight, L=Carried_Load_Weight,N=N, V=Speed, z=Zoom_Level, cont.lab=TRUE, isolines=TRUE, breaks=Breaks, export=FALSE)


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

# Clip isolines to DTM boundary
sf_object <- st_intersection(sf_object, st_union(dtm_sf_boundary))

# Ora esporta il file
Output_Isoline=sf_object

a2<-r$alloc.boundaries
sf_object2 = st_as_sf(a2)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object2))) {
  sf_object2 <- st_set_crs(sf_object2, CRS) # esempio con WGS84
}

# Clip boundaries to DTM boundary
sf_object2 <- st_intersection(sf_object2, st_union(dtm_sf_boundary))

# Add area fields for allocation polygons
sf_object2$area_m2 <- as.numeric(st_area(sf_object2))
sf_object2$area_km2 <- sf_object2$area_m2 / 1000000
sf_object2$area_ha <- sf_object2$area_m2 / 10000

Output_Polygon=sf_object2