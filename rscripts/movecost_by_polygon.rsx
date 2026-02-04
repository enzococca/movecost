##movecost script=group
##Movecost by Polygon=name
##CRS=crs
##Area_of_interest=vector polygon
##Origin=vector point
##Destination=vector point
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Move=enum literal 16;8;4
##Field=number 0
##Breaks=number 0.5
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h
##Time=selection h;m
##Outp=selection r;c
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
##Output_Plot=output file

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
# Move is passed as literal value (16, 8, or 4), just convert to numeric
Move <- as.numeric(Move)
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
  # Convert Barrier from sf to Spatial object (movecost requires Spatial* object)
  if (!is.null(Barrier) && inherits(Barrier, "sf")) {
    Barrier <- as_Spatial(Barrier)
  }
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

# Define time conversion function
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

# Add length fields for isolines
sf_object$length_m <- as.numeric(st_length(sf_object))
sf_object$length_km <- sf_object$length_m / 1000

# Ora esporta il file
Output_Isoline=sf_object


# Get cost values from destination locations
dest_costs <- NULL
if (!is.null(r$dest.loc.w.cost)) {
    dest_costs_sf <- st_as_sf(r$dest.loc.w.cost)
    if ("cost" %in% names(dest_costs_sf)) {
        dest_costs <- dest_costs_sf$cost
    }
}

b1=r$LCPs
sf_object_b1 = st_as_sf(b1)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object_b1))) {
  sf_object_b1 <- st_set_crs(sf_object_b1, CRS) # esempio con WGS84
}

# Add cost field from destination costs if not present
if (!"cost" %in% names(sf_object_b1) && !is.null(dest_costs)) {
    if (length(dest_costs) == nrow(sf_object_b1)) {
        sf_object_b1$cost <- dest_costs
    } else if (length(dest_costs) >= 1) {
        sf_object_b1$cost <- dest_costs[1:min(length(dest_costs), nrow(sf_object_b1))]
    }
}

# Add length fields
sf_object_b1$length_m <- as.numeric(st_length(sf_object_b1))
sf_object_b1$length_km <- sf_object_b1$length_m / 1000

# Add time conversion if cost field exists
if ("cost" %in% names(sf_object_b1)) {
    sf_object_b1$time_converted <- sapply(sf_object_b1$cost, converti_tempo)
}

Output_LCP =sf_object_b1

if(Return_Base == TRUE) {
    lcp=r$LCPs.back
    sf_object_lcp = st_as_sf(lcp)
    if (is.na(st_crs(sf_object_lcp))) {
        sf_object_lcp <- st_set_crs(sf_object_lcp, CRS) # esempio con WGS84
    }

    # Add cost field from destination costs if not present
    if (!"cost" %in% names(sf_object_lcp) && !is.null(dest_costs)) {
        if (length(dest_costs) == nrow(sf_object_lcp)) {
            sf_object_lcp$cost <- dest_costs
        } else if (length(dest_costs) >= 1) {
            sf_object_lcp$cost <- dest_costs[1:min(length(dest_costs), nrow(sf_object_lcp))]
        }
    }

    # Add length fields
    sf_object_lcp$length_m <- as.numeric(st_length(sf_object_lcp))
    sf_object_lcp$length_km <- sf_object_lcp$length_m / 1000

    # Add time conversion if cost field exists
    if ("cost" %in% names(sf_object_lcp)) {
        sf_object_lcp$time_converted <- sapply(sf_object_lcp$cost, converti_tempo)
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
# Convert time based on the Time parameter (h=hours, m=minutes)
converti_tempo <- function(tempo_valore) {
    if (is.na(tempo_valore)) {
        return(NA)
    }

    # The cost value is in hours (when Time='h') or minutes (when Time='m')
    if (Time == "h") {
        # Input is in hours (e.g., 2.587 = 2 hours 35 minutes)
        ore_totali <- tempo_valore
        giorni <- floor(ore_totali / 24)
        ore <- floor(ore_totali %% 24)
        minuti_decimali <- (ore_totali - floor(ore_totali)) * 60
        minuti <- floor(minuti_decimali)
        secondi <- round((minuti_decimali - minuti) * 60)
    } else {
        # Input is in minutes (e.g., 155.2 = 2 hours 35 minutes)
        minuti_totali <- tempo_valore
        giorni <- floor(minuti_totali / (24 * 60))
        ore <- floor((minuti_totali %% (24 * 60)) / 60)
        minuti <- floor(minuti_totali %% 60)
        secondi <- round((minuti_totali - floor(minuti_totali)) * 60)
    }

    # Handle overflow
    if (secondi >= 60) {
        secondi <- 0
        minuti <- minuti + 1
    }
    if (minuti >= 60) {
        minuti <- 0
        ore <- ore + 1
    }
    if (ore >= 24) {
        ore <- ore %% 24
        giorni <- giorni + 1
    }

    # Build output string
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
    print(sf_dl)



    # Assegna il SpatialPointsDataFrame con la colonna aggiunta al tuo Output_W_Cost
    #Output_W_Cost<-sf_dl
    Output_W_Cost=sf_dl
}

# Save the plot to a file for display in the plugin panel
plot_temp_dir <- file.path(tempdir(), "movecost_plots")
dir.create(plot_temp_dir, showWarnings = FALSE, recursive = TRUE)
plot_file <- file.path(plot_temp_dir, "movecost_latest_plot.png")

tryCatch({
    png(plot_file, width = 800, height = 600, res = 100)
    plot(r$accumulated.cost.raster, main = "Accumulated Cost Surface")
    if (exists("sf_object_b1")) {
        plot(st_geometry(sf_object_b1), add = TRUE, col = "red", lwd = 2)
    }
    if (exists("sf_object")) {
        plot(st_geometry(sf_object), add = TRUE, col = "blue", lty = 2)
    }
    points(st_coordinates(Origin), pch = 16, col = "green", cex = 1.5)
    points(st_coordinates(Destination), pch = 17, col = "red", cex = 1.5)
    legend("topright", legend = c("Origin", "Destination", "LCP", "Isolines"),
           pch = c(16, 17, NA, NA), lty = c(NA, NA, 1, 2),
           col = c("green", "red", "red", "blue"), lwd = c(NA, NA, 2, 1))
    dev.off()
    message(paste("Plot saved to:", plot_file))
    if (!is.null(Output_Plot) && Output_Plot != "" && Output_Plot != "NA") {
        file.copy(plot_file, Output_Plot, overwrite = TRUE)
    }
}, error = function(e) {
    message(paste("Warning: Could not save plot:", e$message))
    try(dev.off(), silent = TRUE)
})




