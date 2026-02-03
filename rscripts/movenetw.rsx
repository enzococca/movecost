##movecost script=group
##Movenetw=name
##CRS=crs
##DTM=raster
##Origin=vector point
##Network_type=selection allpairs;neigh ;
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Field=number 0
##Move=selection 16;8;4 ;
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;wcs;ree;b;p;pcf;m;hrz;vl;ls;a ;
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9
##Output_LCPs_netw=output vector
##Output_LCPs_netw_merged=output vector
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

# Set CRS for DTM to match Origin's CRS
raster::crs(DTM) <- origin_crs
studyplot_sp <- raster(DTM)

summary(studyplot_sp)  # Questo ti darà una panoramica dei dati, inclusi i valori min e max
any(is.na(studyplot_sp))  # Controlla se ci sono valori NA
any(studyplot_sp == Inf)  # Controlla se ci sono valori infiniti
studyplot_sp[is.na(studyplot_sp)] <- 0  # Sostituisce i valori NA con 0
studyplot_sp[studyplot_sp == Inf] <- max(studyplot_sp, na.rm = TRUE)  # Sostituisce i valori infiniti con il massimo valore non infinito
print(studyplot_sp)
# Map numbers to strings using utility function
function_map <- c("t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Function <- get_string_value(Function, function_map)

# Map numbers to strings using utility function
move_map <- c(16,8,4)
Move <- get_string_value(Move, move_map)
Netype_map <- c("allpairs", "neigh")
Network_type <- get_string_value(Network_type, Netype_map)
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

Cognitive_Slope <- as.logical(Cognitive_Slope)

r<-movenetw(
  dtm = studyplot_sp,
  origin = as_Spatial(Origin),
  netw.type=Network_type,
  barrier = Barrier,
  plot.barrier = PlotBarrier,
  irregular.dtm = IrregularDTM,
  field = Field,
  funct=Function,
  move=Move,
  cogn.slp=Cognitive_Slope,
  sl.crit=Critical_Slope,
  W=Walker_Body_Weight,
  L=Carried_Load_Weight,
  N=N,
  V=Speed,
  z=Zoom_Level,
  lcp.dens=TRUE,
  export=FALSE)

if (Network_type == 'allpairs') {
  if (!is.null(r$LCPs.netw.merged)) {
    # Create data.frame with row names matching the line IDs
    df <- data.frame(id = 1:length(r$LCPs.netw.merged))
    row.names(df) <- sapply(slot(r$LCPs.netw.merged, "lines"), function(x) slot(x, "ID"))

    # Convert SpatialLines to SpatialLinesDataFrame
    Output_LCPs_netw_merged <- st_as_sf(r$LCPs.netw.merged, data = df)
    st_crs(Output_LCPs_netw_merged) <- origin_crs
  } else {
    # Handle the case where r$LCPs.netw.merged is NULL
    Output_LCPs_netw_merged <- NULL
    print("r$LCPs.netw.merged is NULL, so no SpatialLinesDataFrame was created.")
  }

    # Combine list of SpatialLines objects into one SpatialLines object
    a1 <- do.call(raster::bind, r$LCPs.netw)

    Output_LCPs_netw=a1# Create empty data.frame with row number equal to the number of SpatialLines
    df <- data.frame(id = seq_along(a1))


    # Convert SpatialLines to SpatialLinesDataFrame
    a1_df <- st_as_sf(a1, data = df)
    # Imposta il CRS se non è definito
    if (is.na(st_crs(a1_df))) {
    a1 <- st_set_crs(a1_df, CRS) # esempio con WGS84
    }
    # Now a1_df is a SpatialLinesDataFrame
    Output_LCPs_netw <- a1
} else {
  if (!is.null(r$LCPs.netw.neigh.merged)) {
    # Create data.frame with row names matching the line IDs
    df <- data.frame(id = 1:length(r$LCPs.netw.neigh.merged))
    row.names(df) <- sapply(slot(r$LCPs.netw.neigh.merged, "lines"), function(x) slot(x, "ID"))

    # Convert SpatialLines to SpatialLinesDataFrame
    Output_LCPs_netw_merged <- st_as_sf(r$LCPs.netw.neigh.merged, data = df)
    st_crs(Output_LCPs_netw_merged) <- origin_crs
  } else {
    # Handle the case where r$LCPs.netw.neigh.merged is NULL
    Output_LCPs_netw_merged <- NULL
    print("r$LCPs.netw.neigh.merged is NULL, so no SpatialLinesDataFrame was created.")
  }

  # Combine list of SpatialLines objects into one SpatialLines object
    a1 <- do.call(raster::bind, r$LCPs.netw.neigh)

    Output_LCPs_netw=a1# Create empty data.frame with row number equal to the number of SpatialLines
    df <- data.frame(id = seq_along(a1))

    # Convert SpatialLines to SpatialLinesDataFrame
    a1_df <- st_as_sf(a1, data = df)
    # Imposta il CRS se non è definito
    if (is.na(st_crs(a1_df))) {
  df <- st_set_crs(a1_df, CRS) # esempio con WGS84
}
    # Now a1_df is a SpatialLinesDataFrame
    Output_LCPs_netw <- a1_df
}


