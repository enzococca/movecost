
##movecost script=group
##CRS=crs
##DTM=raster
##Origin=vector point
##Destination=vector point
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Movecomp=name
##Move=selection 16;8;4 ;
##Field=number 0
##Choice1=selection ; t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Choice2=selection ; t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Choice3=selection ; t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Time=selection h;m ;
##Return_Base=string TRUE
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9
##Output_LCP=output vector
##Output_LCP_Back=output vector

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
origin_crs <- st_crs(Origin)$proj4string
origin_sp <- as(Origin, "Spatial")
destin_sp <- as(Destination, "Spatial")
studyplot_sp <- raster(DTM)

summary(studyplot_sp)  # Questo ti darà una panoramica dei dati, inclusi i valori min e max
any(is.na(studyplot_sp))  # Controlla se ci sono valori NA
any(studyplot_sp == Inf)  # Controlla se ci sono valori infiniti
studyplot_sp[is.na(studyplot_sp)] <- 0  # Sostituisce i valori NA con 0
studyplot_sp[studyplot_sp == Inf] <- max(studyplot_sp, na.rm = TRUE)  # Sostituisce i valori infiniti con il massimo valore non infinito
print(studyplot_sp)
# Map numbers to strings using utility function
function_map <- c("", "t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Choice1 <- get_string_value(Choice1, function_map)
Choice2 <- get_string_value(Choice2, function_map)
Choice3 <- get_string_value(Choice3, function_map)

# Define choices based on input
choices <- c(Choice1, Choice2)
if (Choice3 != '') choices <- c(choices, Choice3)

# Convert string to logical
logical_vars <- c("PlotBarrier", "IrregularDTM", "Return_Base", "Cognitive_Slope")
for (var in logical_vars) assign(var, as.logical(get(var)))

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
IrregularDTM <- as.logical(IrregularDTM)
Return_Base <- as.logical(Return_Base)
Cognitive_Slope <- as.logical(Cognitive_Slope)
# Execute movecomp function with error handling
tryCatch({
  r <- movecomp(
    dtm = studyplot_sp,
    origin = as_Spatial(Origin),
    destin = as_Spatial(Destination), ,
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

# Check if r$LCPs is not NULL before converting to sf object
if (!is.null(r$LCPs)) {
  sf_dl1 = st_as_sf(r$LCPs)
  # Set the CRS if not defined
  if (is.na(st_crs(sf_dl1))) {
    sf_dl1 <- st_set_crs(sf_dl1, CRS) # Example with WGS84
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
    valori_non_numerici_o_mancanti <- is.na(sf_dl1$cost) | sapply(sf_dl1$cost, function(x) !is.numeric(x))
    print(sf_dl1[valori_non_numerici_o_mancanti, ])


    sf_dl1$time_converted <- sapply(sf_dl1$cost, converti_tempo)
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
    valori_non_numerici_o_mancanti <- is.na(sf_dl$cost) | sapply(sf_dl$cost, function(x) !is.numeric(x))
    print(sf_dl[valori_non_numerici_o_mancanti, ])


    sf_dl$time_converted <- sapply(sf_dl$cost, converti_tempo)
    print(sf_dl)
    Output_LCP_Back = sf_dl
  } else {
    message("r$LCPs.back is NULL and cannot be converted to an sf object.")
  }
}
