##movecost script=group
##Movecorr=name
##CRS=crs
##DTM=raster
##Points=vector point
##Selection_ID_Point_A=number 1
##Selection_ID_Point_B=number 2
##Barrier=optional vector
##PlotBarrier=string FALSE
##IrregularDTM=string FALSE
##Move=selection 16;8;4 ;
##Field=number 0
##Function=selection t;tofp;mp;icmonp;icmoffp;icfonp;icfoffp;ug;ma;alb;gkrs;r;ks;trp;wcs;ree;b;e;p;pcf;m;hrz;vl;ls;a;h ;
##Time=selection h;m ;
##Cognitive_Slope=string TRUE
##Critical_Slope=number 10
##Walker_Body_Weight=number 70
##Carried_Load_Weight=number 0
##N=number 1
##Speed=number 1
##Zoom_Level=number 9
##Output_LC_corridor=output raster
##Output_Accum_Cost_Surface_A=output raster
##Output_Accum_Cost_Surface_B=output raster
##Output_LCP_A_to_B=output vector
##Output_LCP_B_to_A=output vector



##showplots
required_packages <- c("movecost", "sp", "sf","progress", "raster")
lapply(required_packages, require, character.only = TRUE)

# Define utility function for mapping numbers to strings
get_string_value <- function(val, string_map) {
    string_map[val + 1] # +1 because R indexing starts from 1
}
# Load libraries
library(sp)
library(sf)
library(movecost)
library(progress)
library(raster)


# Load input raster
DTM <- raster(DTM)
print(DTM)
# Get CRS from input vector (Origin)
origin_crs <- CRS

# Set CRS for DTM to match Origin's CRS
crs(DTM) <- origin_crs
studyplot_sp <- DTM

summary(studyplot_sp)  # Questo ti darà una panoramica dei dati, inclusi i valori min e max
any(is.na(studyplot_sp))  # Controlla se ci sono valori NA
any(studyplot_sp == Inf)  # Controlla se ci sono valori infiniti
studyplot_sp[is.na(studyplot_sp)] <- 0  # Sostituisce i valori NA con 0
studyplot_sp[studyplot_sp == Inf] <- max(studyplot_sp, na.rm = TRUE)  # Sostituisce i valori infiniti con il massimo valore non infinito
print(studyplot_sp)

# Map numbers to strings using utility function
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
  Field <- NULL
}
IrregularDTM <- as.logical(IrregularDTM)

Cognitive_Slope <- as.logical(Cognitive_Slope)
p<-as_Spatial(Points)
r<-movecorr(dtm=studyplot_sp,a=p[Selection_ID_Point_A,],b=p[Selection_ID_Point_B,],plot.barrier = PlotBarrier,
  barrier = Barrier,
  irregular.dtm = IrregularDTM,
  field = Field,funct=Function,time=Time,move=Move,cogn.slp=Cognitive_Slope,sl.crit=Critical_Slope,W=Walker_Body_Weight,L=Carried_Load_Weight,N=N,V=Speed,z=Zoom_Level)

print(r)

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




a1=r$lcp_a_to_b
sf_object_b1 = st_as_sf(a1)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object_b1))) {
  sf_object_b1 <- st_set_crs(sf_object_b1, CRS) # esempio con WGS84
}


Output_LCP_A_to_B =sf_object_b1

b1=r$lcp_b_to_a
sf_object_b1 = st_as_sf(b1)
# Imposta il CRS se non è definito
if (is.na(st_crs(sf_object_b1))) {
  sf_object_b1 <- st_set_crs(sf_object_b1, CRS) # esempio con WGS84
}

Output_LCP_B_to_A =sf_object_b1
traceback()