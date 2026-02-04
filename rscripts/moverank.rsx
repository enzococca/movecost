##movecost script=group
##CRS=crs
##DTM=raster
##Origin=vector point
##Destination=vector point
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Moverank=name
##Move=enum literal 16;8;4
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h
##Time=selection h;m
##LCPN=number 3
##Use_Corridor=string TRUE
##Legend_Position=selection topright;bottomright;bottom;left;topleft;right;center
##Cognitive_Slope=string TRUE
##Add_Chart=string FALSE
##Bubble_cex=number 0.5
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9

##Output_LCP=output vector
##Output_Least_Cost_Corridor=output raster
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

# Load input raster - save filename before conversion (brick loses file reference)
dtm_filename <- tryCatch(sources(DTM)[1], error = function(e) filename(DTM))
DTM <- raster(DTM)

# Get CRS from input vector (Origin)
origin_crs <- CRS

raster::crs(DTM) <- origin_crs
studyplot_sp <- DTM

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

# Mappa i numeri in stringhe utilizzando la funzione di utilità
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
} else {
  Move <- 8
  # Convert Barrier from sf to Spatial object (movecost requires Spatial* object)
  if (!is.null(Barrier) && inherits(Barrier, "sf")) {
    Barrier <- as_Spatial(Barrier)
  }
}
IrregularDTM <- as.logical(IrregularDTM)
Cognitive_Slope <- as.logical(Cognitive_Slope)
Add_Chart <- as.logical(Add_Chart)
Use_Corridor <- as.logical(Use_Corridor)
# Esegui la funzione movecost
r <- moverank(
  dtm = studyplot_sp,
  origin = as_Spatial(Origin),
  destin = as_Spatial(Destination),
  studyplot = NULL,
  barrier = Barrier,
  plot.barrier = PlotBarrier,
  irregular.dtm = IrregularDTM,
  funct = Function,
  time = Time,
  lcp.n = LCPN,
  use.corr=Use_Corridor,
  move = Move,
  cogn.slp = Cognitive_Slope,
  sl.crit = Critical_Slope,
  W = Walker_Body_Weight,
  L = Carried_Load_Weight,
  N = N,
  V = Speed,
  z = Zoom_Level,
  add.chart=Add_Chart,
  export = FALSE
)

b1=r$LCPs
sf_object_b1 = st_as_sf(b1)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object_b1))) {
  sf_object_b1 <- st_set_crs(sf_object_b1, CRS) # esempio con WGS84
}

# Ensure cost field exists - try to extract from ranking table if missing
if (!"cost" %in% names(sf_object_b1)) {
    # Try to get cost from dest.loc.w.cost if available
    if (!is.null(r$dest.loc.w.cost)) {
        dest_costs_sf <- st_as_sf(r$dest.loc.w.cost)
        if ("cost" %in% names(dest_costs_sf)) {
            dest_costs <- dest_costs_sf$cost
            if (length(dest_costs) == nrow(sf_object_b1)) {
                sf_object_b1$cost <- dest_costs
            } else if (length(dest_costs) >= 1) {
                sf_object_b1$cost <- dest_costs[1:min(length(dest_costs), nrow(sf_object_b1))]
            }
        }
    }
}

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


    # Visualizza i valori non numerici o mancanti
    valori_non_numerici_o_mancanti <- is.na(sf_object_b1$cost) | sapply(sf_object_b1$cost, function(x) !is.numeric(x))
    print(sf_object_b1[valori_non_numerici_o_mancanti, ])


    sf_object_b1$time_converted <- sapply(sf_object_b1$cost, converti_tempo)

    # Add length fields
    sf_object_b1$length_m <- as.numeric(st_length(sf_object_b1))
    sf_object_b1$length_km <- sf_object_b1$length_m / 1000

    print(sf_object_b1)

    # Clip LCPs to DTM boundary
    sf_object_b1 <- st_intersection(sf_object_b1, st_union(dtm_sf_boundary))

    Output_LCP =sf_object_b1

# Convert raster output to SpatialPixelsDataFrame and write to GeoTIFF
if(Use_Corridor==TRUE) {
    print(r$'least-cost corridor')
    sf_object_lcp <- r$'least-cost corridor'
    if (is.na(crs(sf_object_lcp))) {
        crs(sf_object_lcp) <- CRS # esempio con WGS84sf_object_lcp <- st_set_crs(sf_object_lcp, CRS) # esempio con WGS84
}
    crs(studyplot_sp)<-crs(sf_object_lcp)
    sf_object_cropped = mask(sf_object_lcp, studyplot_sp)
    Output_Least_Cost_Corridor=sf_object_cropped
}