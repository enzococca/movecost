##movecost script=group
##Movenetw=name
##CRS=crs
##DTM=raster
##Origin=vector point
##Network_type=selection allpairs;neigh
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Field=number 0
##Move=enum literal 16;8;4
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;wcs;ree;b;p;pcf;m;hrz;vl;ls;a
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

# Set CRS for DTM to match Origin's CRS
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

# Map numbers to strings using utility function
function_map <- c("t", "tofp", "mp", "icmonp", "icmoffp", "icfonp", "icfoffp", "ug", "ma", "alb", "gkrs", "r", "ks", "trp", "wcs", "ree", "b", "e", "p", "pcf", "m", "hrz", "vl", "ls", "a", "h")
Function <- get_string_value(Function, function_map)

# Map numbers to strings using utility function
# Move is passed as literal value (16, 8, or 4), just convert to numeric
Move <- as.numeric(Move)
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
  # Convert Barrier from sf to Spatial object (movecost requires Spatial* object)
  if (!is.null(Barrier) && inherits(Barrier, "sf")) {
    Barrier <- as_Spatial(Barrier)
  }
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

    # Add length fields
    Output_LCPs_netw_merged$length_m <- as.numeric(st_length(Output_LCPs_netw_merged))
    Output_LCPs_netw_merged$length_km <- Output_LCPs_netw_merged$length_m / 1000
  } else {
    # Handle the case where r$LCPs.netw.merged is NULL
    Output_LCPs_netw_merged <- NULL
    print("r$LCPs.netw.merged is NULL, so no SpatialLinesDataFrame was created.")
  }

    # Combine list of SpatialLines objects into one SpatialLines object
    a1 <- do.call(raster::bind, r$LCPs.netw)

    # Create empty data.frame with row number equal to the number of SpatialLines
    df <- data.frame(id = seq_along(a1))

    # Convert SpatialLines to SpatialLinesDataFrame
    a1_df <- st_as_sf(a1, data = df)
    # Imposta il CRS se non è definito
    if (is.na(st_crs(a1_df))) {
      a1_df <- st_set_crs(a1_df, CRS) # esempio con WGS84
    }

    # Add length fields
    a1_df$length_m <- as.numeric(st_length(a1_df))
    a1_df$length_km <- a1_df$length_m / 1000

    # Clip to DTM boundary
    a1_df <- st_intersection(a1_df, st_union(dtm_sf_boundary))
    # Now a1_df is a SpatialLinesDataFrame
    Output_LCPs_netw <- a1_df
} else {
  if (!is.null(r$LCPs.netw.neigh.merged)) {
    # Create data.frame with row names matching the line IDs
    df <- data.frame(id = 1:length(r$LCPs.netw.neigh.merged))
    row.names(df) <- sapply(slot(r$LCPs.netw.neigh.merged, "lines"), function(x) slot(x, "ID"))

    # Convert SpatialLines to SpatialLinesDataFrame
    Output_LCPs_netw_merged <- st_as_sf(r$LCPs.netw.neigh.merged, data = df)
    st_crs(Output_LCPs_netw_merged) <- origin_crs

    # Add length fields
    Output_LCPs_netw_merged$length_m <- as.numeric(st_length(Output_LCPs_netw_merged))
    Output_LCPs_netw_merged$length_km <- Output_LCPs_netw_merged$length_m / 1000
  } else {
    # Handle the case where r$LCPs.netw.neigh.merged is NULL
    Output_LCPs_netw_merged <- NULL
    print("r$LCPs.netw.neigh.merged is NULL, so no SpatialLinesDataFrame was created.")
  }

  # Combine list of SpatialLines objects into one SpatialLines object
    a1 <- do.call(raster::bind, r$LCPs.netw.neigh)

    # Create empty data.frame with row number equal to the number of SpatialLines
    df <- data.frame(id = seq_along(a1))

    # Convert SpatialLines to SpatialLinesDataFrame
    a1_df <- st_as_sf(a1, data = df)
    # Imposta il CRS se non è definito
    if (is.na(st_crs(a1_df))) {
      a1_df <- st_set_crs(a1_df, CRS) # esempio con WGS84
    }

    # Add length fields
    a1_df$length_m <- as.numeric(st_length(a1_df))
    a1_df$length_km <- a1_df$length_m / 1000

    # Clip to DTM boundary
    a1_df <- st_intersection(a1_df, st_union(dtm_sf_boundary))
    # Now a1_df is a SpatialLinesDataFrame
    Output_LCPs_netw <- a1_df
}


