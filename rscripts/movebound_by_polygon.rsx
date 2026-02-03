
##movecost script=group
##CRS=crs
##Area_of_interest=vector polygon
##Points=vector point
##Movebound by Polygon=name
##Barrier=optional vector
##PlotBarrier=string FALSE
##Area=string TRUE
##Move=selection 16;8;4 ;
##Cost_Value=number 1
##Field=number 0
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Time=selection h;m ;
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9
##Output_DTM=output raster
##Output_Isoline=output vector
##Output_Area=output vector
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
studyplot_sp <- as_Spatial(Area_of_interest)

# Map numbers to strings using utility function
function_map <- c("t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Function <- get_string_value(Function, function_map)

time_map <- c("h", "m")
Time <- get_string_value(Time, time_map)
# Map numbers to strings using utility function
move_map <- c(16,8,4)
Move <- get_string_value(Move, move_map)
# Convert string to logical
PlotBarrier <- as.logical(PlotBarrier)
# If PlotBarrier is FALSE, set Barrier and Field to NULL
if(!PlotBarrier) {
  Barrier <- NULL
  Field <- NULL
} else {
  Move <- 8
}

Cognitive_Slope <- as.logical(Cognitive_Slope)
r<-movebound(dtm=NULL,
  origin=p,
  studyplot=studyplot_sp,
  barrier = Barrier,
  plot.barrier = PlotBarrier,
  field = Field,
  funct=Function,
  time=Time, move=Move,
  cont.value=Cost_Value,
  cogn.slp=Cognitive_Slope,
  sl.crit=Critical_Slope,
  W=Walker_Body_Weight,
  L=Carried_Load_Weight,
  N=N,
  V=Speed,
  z=Zoom_Level,
  cont.lab=TRUE,
  add.geom=Area,
  export=FALSE)

dem = r$dtm
sf_dem = dem
# Imposta il CRS se non è definito
if (is.na(crs(sf_dem))) {
  crs(sf_dem) <- CRS # esempio con WGS84
}
crs(studyplot_sp)<-crs(sf_dem)
sf_dem_cropped = mask(sf_dem, studyplot_sp)
Output_DTM=sf_dem_cropped

a1=r$isolines
sf_object = st_as_sf(a1)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object))) {
  sf_object <- st_set_crs(sf_object, CRS) # esempio con WGS84
}

# Ora esporta il file
Output_Isoline=sf_object

if(Area==TRUE){

a2<-r$origin_w_isolines_geom
sf_object2 = st_as_sf(a2)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object2))) {
  sf_object2 <- st_set_crs(sf_object2, CRS) # esempio con WGS84
}

# Ora esporta il file
Output_Area=sf_object2
}
traceback()