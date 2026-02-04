##movecost script=group
##Movecomp by Polygon=name
##CRS=crs
##Area_of_interest=vector polygon
##Origin=vector point
##Destination=vector point
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Movecomp by polygon=name
##Move=enum literal 16;8;4
##Field=number 0
##Functions_to_compare=string t,mp,ug
##Time=selection h;m
##Return_Base=string TRUE
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9
##Output_DTM=output raster
##Output_LCP=output vector
##Output_LCP_Back=output vector
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
# Parse comma-separated function names from string input
# Valid functions: t,tofp,mp,icmonp,icmoffp,icfonp,icfoffp,ug,ma,alb,gkrs,r,ks,trp,wcs,ree,b,e,p,pcf,m,hrz,vl,ls,a,h
valid_functions <- c("t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")

# Split the input string by comma and trim whitespace
choices <- trimws(unlist(strsplit(Functions_to_compare, ",")))
# Filter to only valid function names
choices <- choices[choices %in% valid_functions]

# Ensure at least 2 functions are selected for comparison
if (length(choices) < 2) {
    stop("ERROR: Please specify at least 2 cost functions to compare, separated by commas. Example: t,mp,ug")
}
message(paste("Comparing cost functions:", paste(choices, collapse = ", ")))

# Convert string to logical
logical_vars <- c("PlotBarrier", "IrregularDTM", "Return_Base", "Cognitive_Slope")
for (var in logical_vars) assign(var, as.logical(get(var)))

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
  Move <- 8
  # Convert Barrier from sf to Spatial object (movecost requires Spatial* object)
  if (!is.null(Barrier) && inherits(Barrier, "sf")) {
    Barrier <- as_Spatial(Barrier)
  }
}
IrregularDTM <- as.logical(IrregularDTM)
Return_Base <- as.logical(Return_Base)
Cognitive_Slope <- as.logical(Cognitive_Slope)	
# Execute movecomp function with error handling
tryCatch({
  r <- movecomp(
    dtm = NULL, 
    origin = as_Spatial(Origin),
    destin = as_Spatial(Destination),
    studyplot=studyplot_sp,
    barrier = Barrier,
    plot.barrier = PlotBarrier,
    irregular.dtm = IrregularDTM,
    field = Field,
    choice = choices,
    move = Move,
    return.base = Return_Base,
    cogn.slp = Cognitive_Slope,
    sl.crit = Critical_Slope,
    W = Walker_Body_Weight,
    L = Carried_Load_Weight,
    N = N,
    V = Speed,
    z = Zoom_Level,
    oneplot = FALSE,
    export = FALSE
  )
},
error = function(e) {
  message(str(e))
})
print(r$LCPs.Back)
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


# Check if r$LCPs is not NULL before converting to sf object
if (!is.null(r$LCPs)) {
  sf_dl1 = st_as_sf(r$LCPs)
  # Set the CRS if not defined
  if (is.na(st_crs(sf_dl1))) {
    sf_dl1 <- st_set_crs(sf_dl1, CRS) # Example with WGS84
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
    valori_non_numerici_o_mancanti <- is.na(sf_dl1$cost) | sapply(sf_dl1$cost, function(x) !is.numeric(x))
    print(sf_dl1[valori_non_numerici_o_mancanti, ])


    sf_dl1$time_converted <- sapply(sf_dl1$cost, converti_tempo)

    # Add length fields
    sf_dl1$length_m <- as.numeric(st_length(sf_dl1))
    sf_dl1$length_km <- sf_dl1$length_m / 1000

    print(sf_dl1)
  Output_LCP = sf_dl1
} else {
  message("r$LCPs is NULL and cannot be converted to an sf object.")
}

if(Return_Base == TRUE) {
  # Check if r$LCPs.back is not NULL before converting to sf object
  if (!is.null(r$LPCs.back)) {
    sf_dl = st_as_sf(r$LPCs.back)
    if (is.na(st_crs(sf_dl))) {
      sf_dl <- st_set_crs(sf_dl, CRS) # Example with WGS84
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
    valori_non_numerici_o_mancanti <- is.na(sf_dl$cost) | sapply(sf_dl$cost, function(x) !is.numeric(x))
    print(sf_dl[valori_non_numerici_o_mancanti, ])


    sf_dl$time_converted <- sapply(sf_dl$cost, converti_tempo)

    # Add length fields
    sf_dl$length_m <- as.numeric(st_length(sf_dl))
    sf_dl$length_km <- sf_dl$length_m / 1000

    print(sf_dl)
    Output_LCP_Back = sf_dl
  } else {
    message("r$LCPs.back is NULL and cannot be converted to an sf object.")
  }
}
