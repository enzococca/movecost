##movecost script=group
##Movealloc=name
##CRS=crs
##DTM=raster
##Points=vector point
##Move=selection 16;8;4 ;
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Breaks=number 1
##Time=selection h;m ;
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

# Function to check and update movecost package
check_movecost_version <- function(min_version = "2.1") {
    if (!requireNamespace("movecost", quietly = TRUE)) {
        message("Installing movecost package...")
        install.packages("movecost", repos = "https://cloud.r-project.org/")
    } else {
        installed_version <- as.character(packageVersion("movecost"))
        if (compareVersion(installed_version, min_version) < 0) {
            message(paste("Updating movecost from", installed_version, "to latest version..."))
            install.packages("movecost", repos = "https://cloud.r-project.org/")
        }
    }
}

# Check and update movecost if needed (minimum version 2.1)
check_movecost_version("2.1")

# Load required libraries
required_packages <- c("movecost", "sp", "sf", "progress", "raster")
lapply(required_packages, require, character.only = TRUE)

# Load libraries
library(sp)
library(sf)
library(movecost)
library(progress)
library(raster)


# Define utility function for mapping numbers to strings
get_string_value <- function(val, string_map) {
    string_map[val + 1] # +1 because R indexing starts from 1
}
# Get CRS from input vector (Origin)
# Assuming 'Origin' is an sf object and you need to get its CRS
origin_crs <- CRS
p <- as_Spatial(Points)
studyplot_sp <- raster(DTM)
crs(studyplot_sp) <- origin_crs
summary(studyplot_sp)  # Questo ti darà una panoramica dei dati, inclusi i valori min e max
any(is.na(studyplot_sp))  # Controlla se ci sono valori NA
any(studyplot_sp == Inf)  # Controlla se ci sono valori infiniti
studyplot_sp[is.na(studyplot_sp)] <- 0  # Sostituisce i valori NA con 0
studyplot_sp[studyplot_sp == Inf] <- max(studyplot_sp, na.rm = TRUE)  # Sostituisce i valori infiniti con il massimo valore non infinito
print(studyplot_sp)


# Map numbers to strings using utility function
function_map <- c("t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Function <- get_string_value(Function, function_map)

time_map <- c("h", "m")
Time <- get_string_value(Time, time_map)
# Map numbers to strings using utility function
move_map <- c(16,8,4)
Move <- get_string_value(Move, move_map)
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

# Ora esporta il file
Output_Isoline=sf_object

a2<-r$alloc.boundaries
sf_object2 = st_as_sf(a2)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object2))) {
  sf_object2 <- st_set_crs(sf_object2, CRS) # esempio con WGS84
}

Output_Polygon=sf_object2