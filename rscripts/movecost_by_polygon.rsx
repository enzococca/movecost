
##movecost script=group
##Movecost by Polygon=name
##CRS=crs
##Area_of_interest=vector polygon
##Origin=vector point
##Destination=vector point
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Move=enum literal 16;8;4; ;
##Field=number 0
##Breaks=number 0.5
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Time=selection h;m ;
##Outp=selection r;c ;
##Return_Base=string TRUE
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9
##RL=number 2
##CL=string TRUE
##DL=string TRUE
##CB=number 0.6
##CLL=number 0.6
##Output_DTM=output raster
##Output_Accumulation_Coast=output raster
##Output_Isoline=output vector
##Output_LCP=output vector
##Output_LCP_Back=output vector
##Output_W_Cost=output vector


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
studyplot_sp <- as(Area_of_interest, "Spatial")
# Map numbers to strings using utility function
function_map <- c("t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Function <- get_string_value(Function, function_map)
time_map <- c("h", "m")
Time <- get_string_value(Time, time_map)
move_map <- c(16,8,4)
Move <- get_string_value(Move, move_map)
Outp_map <- c("r", "c")
Outp <- get_string_value(Outp, Outp_map)
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
DL <- as.logical(DL)
CL <- as.logical(CL)
# Execute movecost function
r <- movecost(
  dtm = NULL,
  origin = as_Spatial(Origin),
  destin = as_Spatial(Destination),
  studyplot=as_Spatial(Area_of_interest),
  barrier = Barrier,
  plot.barrier = PlotBarrier,
  irregular.dtm = IrregularDTM,
  field = Field,
  funct = Function,
  time = Time,
  outp = Outp,
  move = Move,
  breaks = Breaks,
  return.base = Return_Base,
  cogn.slp = Cognitive_Slope,
  sl.crit = Critical_Slope,
  W = Walker_Body_Weight,
  L = Carried_Load_Weight,
  N = N,
  V = Speed,
  z = Zoom_Level,
  rb.lty = RL,
  cont.lab = CL,
  destin.lab = DL,
  cex.breaks = CB,
  cex.lcp.lab = CLL,
  oneplot = TRUE,
  #export = TRUE
)
warnings()
# Converti l'oggetto sf in Spatial
#sp_object <- as(Area_of_interest, "Spatial")


dem = r$dtm
sf_dem = dem
# Imposta il CRS se non è definito
if (is.na(crs(sf_dem))) {
  crs(sf_dem) <- CRS # esempio con WGS84
}
crs(studyplot_sp)<-crs(sf_dem)
sf_dem_cropped = mask(sf_dem, studyplot_sp)
Output_DTM=sf_dem_cropped

acc = r$accumulated.cost.raster
sf_acc = acc
# Imposta il CRS se non è definito
if (is.na(crs(sf_acc))) {
  crs(sf_acc) <- CRS # esempio con WGS84
}
crs(studyplot_sp)<-crs(sf_dem)
sf_acc_cropped <- mask(sf_acc,studyplot_sp)
Output_Accumulation_Coast=sf_acc_cropped

a1=r$isolines
sf_object = st_as_sf(a1)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object))) {
  sf_object <- st_set_crs(sf_object, CRS) # esempio con WGS84
}

# Ora esporta il file
Output_Isoline=sf_object


b1=r$LCPs
sf_object_b1 = st_as_sf(b1)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object_b1))) {
  sf_object_b1 <- st_set_crs(sf_object_b1, CRS) # esempio con WGS84
}
Output_LCP =sf_object_b1

if(Return_Base == TRUE) {
    lcp=r$LCPs.back
    sf_object_lcp = st_as_sf(lcp)
    if (is.na(st_crs(sf_object_lcp))) {
        sf_object_lcp <- st_set_crs(sf_object_lcp, CRS) # esempio con WGS84
}
    Output_LCP_Back = sf_object_lcp
}

if(DL == TRUE) {
    dl=r$dest.loc.w.cost
print(dl)
    sf_dl=st_as_sf(dl)
    if (is.na(st_crs(sf_dl))) {
            sf_dl <- st_set_crs(sf_dl, CRS) # esempio con WGS84
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



    # Assegna il SpatialPointsDataFrame con la colonna aggiunta al tuo Output_W_Cost
    #Output_W_Cost<-sf_dl
    Output_W_Cost=sf_dl
}








