##movecost script=group
##CRS=crs
##DTM=raster
##Origin=vector point
##Destination=vector point
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Moverank=name
##Move=selection 16;8;4 ;
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Time=selection h;m ;
##LCPN=number 3
##Use_Corridor=string TRUE
##Legend_Position=selection topright;bottomright;bottom;left;topleft;right;center ;
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

# Load input raster
DTM <- raster(DTM)

# Get CRS from input vector (Origin)
origin_crs <- CRS

raster::crs(DTM) <- origin_crs
studyplot_sp <- raster(DTM)

summary(studyplot_sp)  # Questo ti darà una panoramica dei dati, inclusi i valori min e max
any(is.na(studyplot_sp))  # Controlla se ci sono valori NA
any(studyplot_sp == Inf)  # Controlla se ci sono valori infiniti
studyplot_sp[is.na(studyplot_sp)] <- 0  # Sostituisce i valori NA con 0
studyplot_sp[studyplot_sp == Inf] <- max(studyplot_sp, na.rm = TRUE)  # Sostituisce i valori infiniti con il massimo valore non infinito
print(studyplot_sp)
# Mappa i numeri in stringhe utilizzando la funzione di utilità
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

} else {
  Move <- 8
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

converti_tempo <- function(tempo_giorni) {
    if (is.na(tempo_giorni)) {
        return(NA)  # Return NA if the time in days is NA
    }

    giorni <- floor(tempo_giorni)
    ore_decimali <- (tempo_giorni - giorni) * 24

    ore <- floor(ore_decimali)
    minuti_decimali <- (ore_decimali - ore) * 60
    minuti <- floor(minuti_decimali)
    secondi <- round((minuti_decimali - minuti) * 60)

    # Ensure seconds do not exceed 59
    if (secondi >= 60) {
        secondi <- 0
        minuti <- minuti + 1
    }

    # Ensure minutes do not exceed 59
    if (minuti >= 60) {
        minuti <- 0
        ore <- ore + 1
    }

    # Plural handling for days and hours
    giorni_label <- ifelse(giorni == 1, "day", "days")
    ore_label <- ifelse(ore == 1, "hour", "hours")

    tempo_convertito <- paste(
        if (!is.na(giorni) && giorni > 0) paste(giorni, giorni_label) else NULL,
        if (!is.na(ore) && ore > 0) paste(ore, ore_label) else NULL,
        if (!is.na(minuti) && minuti > 0) paste(minuti, "minutes") else NULL,
        if (!is.na(secondi) && secondi > 0) paste(sprintf("%02d", secondi), "seconds") else NULL,
        sep = " "
    )

    return(tempo_convertito)
}


    # Visualizza i valori non numerici o mancanti
    valori_non_numerici_o_mancanti <- is.na(sf_object_b1$cost) | sapply(sf_object_b1$cost, function(x) !is.numeric(x))
    print(sf_object_b1[valori_non_numerici_o_mancanti, ])


    sf_object_b1$time_converted <- sapply(sf_object_b1$cost, converti_tempo)
    print(sf_object_b1)
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