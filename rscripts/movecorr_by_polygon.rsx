##movecost script=group
##Movecorr by Polygon=name
##CRS=crs
##Area_of_interest=vector polygon
##Points=vector point
##Selection_ID_Point_A=number 1
##Selection_ID_Point_B=number 2
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Move=enum literal 16;8;4
##Field=number 0
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h
##Time=selection h;m
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9

##Output_DTM=output raster
##Output_LC_corridor=output raster
##Output_Accum_Cost_Surface_A=output raster
##Output_Accum_Cost_Surface_B=output raster
##Output_LCP_A_to_B=output vector
##Output_LCP_B_to_A=output vector



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
all_dependencies <- c("chron", "terra", "gdistance", "Matrix", "igraph", "sp", "sf", "raster", "progress", "dplyr")
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
library(dplyr)

# Define utility function for mapping numbers to strings
get_string_value <- function(val, string_map) {
    string_map[val + 1] # +1 because R indexing starts from 1
}

# Load input raster
#DTM <- raster(DTM)

# Get CRS from input vector (Origin)
origin_crs <- CRS

# Set CRS for DTM to match Origin's CRS
#raster::crs(DTM) <- origin_crs
studyplot_sp <- as(Area_of_interest, "Spatial")
# Map numbers to strings using utility function
function_map <- c("t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Function <- get_string_value(Function, function_map)

time_map <- c("h", "m")
Time <- get_string_value(Time, time_map)
# Map numbers to strings using utility function
# Move is passed as literal value (16, 8, or 4), just convert to numeric
Move <- as.numeric(Move)
# Convert string to logical
PlotBarrier <- as.logical(PlotBarrier)
# If PlotBarrier is FALSE, set Barrier and Field to NULL
if(!PlotBarrier) {
  Barrier <- NULL
  Field <- NULL
} else {
  # Convert Barrier from sf to Spatial object (movecost requires Spatial* object)
  if (!is.null(Barrier) && inherits(Barrier, "sf")) {
    Barrier <- as_Spatial(Barrier)
  }
}
IrregularDTM <- as.logical(IrregularDTM)

Cognitive_Slope <- as.logical(Cognitive_Slope)
p<-as_Spatial(Points)
r<-movecorr(dtm=NULL,a=p[Selection_ID_Point_A,],
b=p[Selection_ID_Point_B,],plot.barrier = PlotBarrier,
  barrier = Barrier,
  irregular.dtm = IrregularDTM,
  field = Field,funct=Function,time=Time,move=Move,
studyplot=as_Spatial(Area_of_interest),
cogn.slp=Cognitive_Slope,sl.crit=Critical_Slope,
W=Walker_Body_Weight,L=Carried_Load_Weight,N=N,V=Speed,z=Zoom_Level)
print(r)

dem = r$dtm
sf_dem = dem
# Imposta il CRS se non è definito
if (is.na(crs(sf_dem))) {
  crs(sf_dem) <- CRS # esempio con WGS84
}
crs(studyplot_sp)<-crs(sf_dem)
sf_dem_cropped = mask(sf_dem, studyplot_sp)
Output_DTM=sf_dem_cropped


LC<- r$lc.corridor
if (is.na(crs(LC))) {
        crs(LC) <- CRS # esempio con WGS84sf_object_lcp <- st_set_crs(sf_object_lcp, CRS) # esempio con WGS84
}
crs(studyplot_sp)<-crs(LC)
LC_cropped = mask(LC, studyplot_sp)

Output_LC_corridor=LC_cropped



raster <- r$accum_cost_surf_a
if (is.na(crs(raster))) {
        crs(raster) <- CRS # esempio con WGS84sf_object_lcp <- st_set_crs(sf_object_lcp, CRS) # esempio con WGS84
}
crs(studyplot_sp)<-crs(raster)
raster_cropped = mask(raster, studyplot_sp)

Output_Accum_Cost_Surface_A=raster_cropped



raster2 <- r$accum_cost_surf_b
if (is.na(crs(raster2))) {
        crs(raster2) <- CRS # esempio con WGS84sf_object_lcp <- st_set_crs(sf_object_lcp, CRS) # esempio con WGS84
}
crs(studyplot_sp)<-crs(raster2)
raster2_cropped = mask(raster2, studyplot_sp)

Output_Accum_Cost_Surface_B=raster2_cropped




# Convert time based on the Time parameter (h=hours, m=minutes)
converti_tempo <- function(tempo_valore) {
    if (is.na(tempo_valore)) return(NA)
    if (Time == "h") {
        ore_totali <- tempo_valore
        giorni <- floor(ore_totali / 24)
        ore <- floor(ore_totali %% 24)
        minuti_decimali <- (ore_totali - floor(ore_totali)) * 60
        minuti <- floor(minuti_decimali)
        secondi <- round((minuti_decimali - minuti) * 60)
    } else {
        minuti_totali <- tempo_valore
        giorni <- floor(minuti_totali / (24 * 60))
        ore <- floor((minuti_totali %% (24 * 60)) / 60)
        minuti <- floor(minuti_totali %% 60)
        secondi <- round((minuti_totali - floor(minuti_totali)) * 60)
    }
    if (secondi >= 60) { secondi <- 0; minuti <- minuti + 1 }
    if (minuti >= 60) { minuti <- 0; ore <- ore + 1 }
    if (ore >= 24) { ore <- ore %% 24; giorni <- giorni + 1 }
    parts <- c()
    if (giorni > 0) parts <- c(parts, paste(giorni, ifelse(giorni == 1, "day", "days")))
    if (ore > 0) parts <- c(parts, paste(ore, ifelse(ore == 1, "hour", "hours")))
    if (minuti > 0) parts <- c(parts, paste(minuti, "minutes"))
    if (secondi > 0) parts <- c(parts, paste(sprintf("%02d", secondi), "seconds"))
    if (length(parts) == 0) return("0 seconds")
    return(paste(parts, collapse = " "))
}

# Extract cost values from accumulated cost surfaces at endpoints
point_b <- p[Selection_ID_Point_B,]
point_a <- p[Selection_ID_Point_A,]
cost_a_to_b <- NULL
cost_b_to_a <- NULL

tryCatch({
    cost_a_to_b <- extract(raster, st_coordinates(st_as_sf(point_b)))
    cost_b_to_a <- extract(raster2, st_coordinates(st_as_sf(point_a)))
}, error = function(e) {
    message(paste("Could not extract endpoint costs:", e$message))
})

a1=r$lcp_a_to_b
sf_object_a1 = st_as_sf(a1)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object_a1))) {
  sf_object_a1 <- st_set_crs(sf_object_a1, CRS) # esempio con WGS84
}

# Add cost field if not present
if (!"cost" %in% names(sf_object_a1) && !is.null(cost_a_to_b) && !is.na(cost_a_to_b)) {
    sf_object_a1$cost <- as.numeric(cost_a_to_b)
}

# Add length fields
sf_object_a1$length_m <- as.numeric(st_length(sf_object_a1))
sf_object_a1$length_km <- sf_object_a1$length_m / 1000

# Add time_converted field based on cost
if ("cost" %in% names(sf_object_a1)) {
    sf_object_a1$time_converted <- sapply(sf_object_a1$cost, converti_tempo)
}

Output_LCP_A_to_B =sf_object_a1

b1=r$lcp_b_to_a
sf_object_b1 = st_as_sf(b1)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object_b1))) {
  sf_object_b1 <- st_set_crs(sf_object_b1, CRS) # esempio con WGS84
}

# Add cost field if not present
if (!"cost" %in% names(sf_object_b1) && !is.null(cost_b_to_a) && !is.na(cost_b_to_a)) {
    sf_object_b1$cost <- as.numeric(cost_b_to_a)
}

# Add length fields
sf_object_b1$length_m <- as.numeric(st_length(sf_object_b1))
sf_object_b1$length_km <- sf_object_b1$length_m / 1000

# Add time_converted field based on cost
if ("cost" %in% names(sf_object_b1)) {
    sf_object_b1$time_converted <- sapply(sf_object_b1$cost, converti_tempo)
}

Output_LCP_B_to_A =sf_object_b1
