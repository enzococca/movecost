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
##Topo_Dist=string FALSE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Terrain=optional raster
##Terrain_Table=optional file
##Speed=number 1
##Zoom_Level=number 9
##Output_DTM=output raster
##Output_LCP=output vector
##Output_LCP_Back=output vector
##showplots

# ===========================================================================
# Ported to the movecost R package 3.0.0 (mc_* API).
#
# movecost 3.0.0 removed the legacy movecost()/movebound()/movecorr()/
# movealloc()/movecomp()/movenetw()/moverank() functions (.Defunct): the
# cost surface is now built once with mc_surface() and reused by every
# analysis function. The 2.x dependencies chron/gdistance/sp/Matrix are
# gone; the computational core is terra + sf + igraph, plotting is ggplot2.
# ===========================================================================

install_if_missing <- function(packages) {
    for (pkg in packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            message(paste("Installing package:", pkg))
            install.packages(pkg, repos = "https://cloud.r-project.org/", dependencies = TRUE)
        }
    }
}

# movecost 3.0 dependencies (raster is kept for the QGIS R provider, which
# hands rasters over as RasterBrick objects)
install_if_missing(c("terra", "sf", "igraph", "ggplot2", "raster", "elevatr"))

check_movecost_version <- function(min_version = "3.1.1") {
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

# movecost >= 3.1.1 is required: mc_surface() gains terrain / terrain.table
# (3.1.0) and 3.1.1 corrects the sign of the slope in several cost
# functions (downhill no longer costs like uphill), so results differ —
# correctly — from earlier versions; the legacy 2.x functions stay removed
check_movecost_version("3.1.1")

library(sf)
library(terra)
library(ggplot2)
library(movecost)

get_string_value <- function(val, string_map) {
    string_map[val + 1] # +1 because R indexing starts from 1
}

# --- shared helpers --------------------------------------------------------

# movecost 3.1: optional terrain-factor raster — either numeric (cell value =
# terrain factor N) or categorical, translated through a CSV with columns
# class,N. The scalar N of the dialog becomes the fallback for cells without
# a value; Inf makes a cell impassable. Cost functions that take no terrain
# factor warn when one is supplied (the analysis still runs).
mc_read_terrain <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.character(x) && !nzchar(x)) return(NULL)
    if (inherits(x, "SpatRaster")) return(x[[1]])
    if (is.character(x)) return(terra::rast(x)[[1]])
    src <- tryCatch(raster::filename(x), error = function(e) "")
    if (!is.null(src) && nzchar(src) && file.exists(src)) {
        terra::rast(src)[[1]]
    } else {
        terra::rast(x)[[1]]
    }
}

mc_read_terrain_table <- function(path) {
    if (is.null(path) || !is.character(path) || !nzchar(path)) return(NULL)
    if (!file.exists(path)) {
        stop(paste("ERROR: terrain table not found:", path))
    }
    tb <- utils::read.csv(path, stringsAsFactors = FALSE)
    names(tb) <- tolower(names(tb))
    if (!all(c("class", "n") %in% names(tb))) {
        stop("ERROR: the terrain table must be a CSV with columns 'class' and 'N'.")
    }
    data.frame(class = tb$class, N = as.numeric(tb$n))
}

# Accept the DTM however the QGIS R provider hands it over (RasterBrick /
# RasterLayer / SpatRaster / file path) and return an in-memory SpatRaster.
mc_read_dtm <- function(x, crs_fallback = NULL) {
    if (inherits(x, "SpatRaster")) {
        dtm <- x[[1]]
    } else if (is.character(x)) {
        dtm <- terra::rast(x)[[1]]
    } else {
        src <- tryCatch(raster::filename(x), error = function(e) "")
        dtm <- if (!is.null(src) && nzchar(src) && file.exists(src)) {
            terra::rast(src)[[1]]
        } else {
            terra::rast(x)[[1]]
        }
    }
    if (!terra::hasValues(dtm)) {
        stop("ERROR: The input raster has no values. Please provide a valid Digital Terrain Model (DTM) with elevation data.")
    }
    rng <- as.numeric(terra::global(dtm, c("min", "max"), na.rm = TRUE))
    message(paste("DTM min:", rng[1], "max:", rng[2], "range:", rng[2] - rng[1]))
    if (any(is.na(rng))) {
        stop("ERROR: The input DTM raster contains only NA values. Please provide a valid Digital Terrain Model.")
    }
    if ((rng[2] - rng[1]) < 1) {
        stop(paste("ERROR: The input DTM has no elevation variation (all values =", rng[1], "). Please provide a valid Digital Terrain Model with actual elevation data, not a mask layer."))
    }
    inf_count <- as.numeric(terra::global(is.infinite(dtm), "sum", na.rm = TRUE))
    if (isTRUE(inf_count > 0)) {
        message(paste("Replacing", inf_count, "Inf values with max elevation"))
        dtm[is.infinite(dtm)] <- rng[2]
    }
    if (is.na(terra::crs(dtm)) || terra::crs(dtm) == "") {
        if (!is.null(crs_fallback)) terra::crs(dtm) <- crs_fallback
    }
    dtm
}

# Polygon of the land area (elevation > 0), used to clip the vector outputs
# so isolines/paths do not run into sea cells — same behaviour as the 2.x
# scripts.
mc_land_boundary <- function(dtm, crs_fallback = NULL) {
    land <- dtm
    land[land <= 0] <- NA
    land <- !is.na(land)
    land[land == 0] <- NA
    poly <- sf::st_as_sf(terra::as.polygons(land, dissolve = TRUE))
    if (is.na(sf::st_crs(poly)) && !is.null(crs_fallback)) {
        poly <- sf::st_set_crs(poly, crs_fallback)
    }
    sf::st_union(poly)
}

mc_ensure_crs <- function(obj, crs_fallback) {
    if (inherits(obj, "sf") && is.na(sf::st_crs(obj))) {
        obj <- sf::st_set_crs(obj, crs_fallback)
    }
    obj
}

mc_add_lengths <- function(obj) {
    if (inherits(obj, "sf") && nrow(obj) > 0) {
        obj$length_m <- as.numeric(sf::st_length(obj))
        obj$length_km <- obj$length_m / 1000
    }
    obj
}

mc_add_areas <- function(obj) {
    if (inherits(obj, "sf") && nrow(obj) > 0) {
        obj$area_m2 <- as.numeric(sf::st_area(obj))
        obj$area_km2 <- obj$area_m2 / 1e6
        obj$area_ha <- obj$area_m2 / 1e4
    }
    obj
}

mc_clip <- function(obj, boundary) {
    if (!inherits(obj, "sf") || nrow(obj) == 0) return(obj)
    out <- tryCatch(suppressWarnings(sf::st_intersection(obj, boundary)),
                    error = function(e) obj)
    if (nrow(out) == 0) obj else out
}

mc_empty_sf <- function(crs_fallback) {
    sf::st_sf(geometry = sf::st_sfc(), crs = crs_fallback)
}

# Human-readable travel time from a cost value in hours (Time = "h") or
# minutes (Time = "m") — same output format as the 2.x scripts, consumed by
# the pyArchInit "Movecost" tab (field `time_converted`).
converti_tempo <- function(tempo_valore) {
    if (is.na(tempo_valore) || !is.numeric(tempo_valore)) return(NA)
    minuti_totali <- if (Time == "h") tempo_valore * 60 else tempo_valore
    giorni <- floor(minuti_totali / (24 * 60))
    ore <- floor((minuti_totali %% (24 * 60)) / 60)
    minuti <- floor(minuti_totali %% 60)
    secondi <- round((minuti_totali - floor(minuti_totali)) * 60)
    if (secondi >= 60) { secondi <- 0; minuti <- minuti + 1 }
    if (minuti >= 60) { minuti <- 0; ore <- ore + 1 }
    if (ore >= 24) { ore <- ore %% 24; giorni <- giorni + 1 }
    parts <- c()
    if (giorni > 0) parts <- c(parts, paste(giorni, ifelse(giorni == 1, "day", "days")))
    if (ore > 0) parts <- c(parts, paste(ore, ifelse(ore == 1, "hour", "hours")))
    if (minuti > 0) parts <- c(parts, paste(minuti, "minutes"))
    if (secondi > 0) parts <- c(parts, paste(sprintf("%02d", secondi), "seconds"))
    if (length(parts) == 0) return("0 seconds")
    paste(parts, collapse = " ")
}

mc_add_time <- function(obj, cost_col = "cost") {
    if (inherits(obj, "sf") && nrow(obj) > 0 && cost_col %in% names(obj)) {
        obj$time_converted <- sapply(obj[[cost_col]], converti_tempo)
    }
    obj
}

# Save the ggplot for the pyArchInit results panel and the QGIS result
# viewer (##showplots).
mc_finish_plot <- function(p, output_plot = NULL) {
    plot_temp_dir <- file.path(tempdir(), "movecost_plots")
    dir.create(plot_temp_dir, showWarnings = FALSE, recursive = TRUE)
    plot_file <- file.path(plot_temp_dir, "movecost_latest_plot.png")
    tryCatch({
        ggplot2::ggsave(plot_file, plot = p, width = 8, height = 6, dpi = 100)
        message(paste("Plot saved to:", plot_file))
        if (!is.null(output_plot) && output_plot != "" && output_plot != "NA") {
            file.copy(plot_file, output_plot, overwrite = TRUE)
        }
    }, error = function(e) {
        message(paste("Warning: Could not save plot:", e$message))
    })
    print(p)
}


# --- decode the Processing enum/string parameters --------------------------
time_map <- c("h", "m")
Time <- get_string_value(Time, time_map)
Move <- as.integer(Move)
Cognitive_Slope <- as.logical(Cognitive_Slope)
Topo_Dist <- as.logical(Topo_Dist)


PlotBarrier <- as.logical(PlotBarrier)
if (!PlotBarrier) {
    Barrier <- NULL
    Field <- 0
} else {
    # Barriers require the 8-direction move set (as in the 2.x scripts)
    Move <- 8L
    Barrier <- mc_ensure_crs(Barrier, CRS)
}


IrregularDTM <- as.logical(IrregularDTM)
if (isTRUE(IrregularDTM)) {
    message("Note: irregular DTMs are handled automatically by movecost >= 3.0 (NoData cells are excluded from the cost graph); the IrregularDTM option is kept for compatibility and has no effect.")
}


# Polygon variant: download the elevation data for the study area
# (mc_dtm() uses the optional elevatr package).
Area_of_interest <- mc_ensure_crs(Area_of_interest, CRS)
message(paste("Downloading elevation data (zoom level", Zoom_Level, ") ..."))
dtm <- mc_dtm(studyplot = Area_of_interest, z = Zoom_Level)
dtm <- mc_read_dtm(dtm, CRS)
land_boundary <- mc_land_boundary(dtm, CRS)


Origin <- mc_ensure_crs(Origin, CRS)
Destination <- mc_ensure_crs(Destination, CRS)
Return_Base <- as.logical(Return_Base)
functs <- trimws(strsplit(Functions_to_compare, ",")[[1]])
functs <- functs[nzchar(functs)]
if (length(functs) < 2) {
    stop("ERROR: Functions_to_compare must list at least two cost-function codes, e.g. 't,mp,ug'.")
}

Terrain_r <- mc_read_terrain(Terrain)
Terrain_tb <- mc_read_terrain_table(Terrain_Table)
cmp <- mc_comp(
    dtm = dtm,
    origin = Origin,
    destin = Destination,
    functs = functs,
    move = Move,
    return.base = Return_Base,
    time = Time,
    barrier = Barrier,
    field = if (is.null(Barrier)) 0 else Field,
    cogn.slp = Cognitive_Slope,
    topo.dist = Topo_Dist,
    N = N,
    terrain = Terrain_r,
    terrain.table = Terrain_tb,
    sl.crit = Critical_Slope,
    W = Walker_Body_Weight,
    L = Carried_Load_Weight,
    V = Speed
)

lcp <- mc_add_lengths(mc_ensure_crs(cmp$paths, CRS))
lcp <- mc_clip(lcp, land_boundary)
Output_LCP <- lcp

if (isTRUE(Return_Base) && inherits(cmp$paths.back, "sf")) {
    lcp_back <- mc_add_lengths(mc_ensure_crs(cmp$paths.back, CRS))
    lcp_back <- mc_clip(lcp_back, land_boundary)
    Output_LCP_Back <- lcp_back
} else {
    Output_LCP_Back <- mc_empty_sf(CRS)
}

p <- plot(cmp) + ggplot2::ggtitle("Cost-function comparison")
mc_finish_plot(p, NULL)


# Polygon variant: also return the downloaded DTM
Output_DTM <- dtm

