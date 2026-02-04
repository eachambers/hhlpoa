#' Get shapefiles relevant for mapping
#'
#' @param bounds if NULL (default), will return all shapefiles; if bounds are provided, will crop to this extent
#' @param save_cropped whether to save files cropped to bounds (defaults to FALSE)
#'
#' @returns
#' @export list with shapefiles as sf objects
get_ontario_shapefiles <- function(bounds = NULL, save_cropped = FALSE) {
  path_to_shps <- "/Volumes/EAC/Anne_laptop_files/HHLPOA_Lake_Plan/ONTARIO_MAPS/"
  
  ontario <- sf::st_read(paste0(path_to_shps, "Ontario_Simple_Boundary/Ontario Simple Boundary.shp"))
  ecoregion <- sf::st_read(paste0(path_to_shps, "EcoRegion/EcoRegion.shp"))
  watershed_tert <- sf::st_read(paste0(path_to_shps, "LIO-2023-01-26/ONT_WSHED_BDRY_TERT_DERIVED.shp"))
  watershed_quat <- sf::st_read(paste0(path_to_shps, "LIO-2023-01-26_pruned/ONT_WSHED_BDRY_QUAT_DERIVED.shp"))
  reserves <- sf::st_read(paste0(path_to_shps, "Conservation_reserve_regulated_CROPPED/Conservation_reserve_regulated.shp"))
  
  bathymetry <- sf::st_read(paste0(path_to_shps, "Bathymetry%2C_Line/Bathymetry%2C_Line.shp"))
  waterbody <- sf::st_read(paste0(path_to_shps, "Ontario_Hydro_Network_(OHN)_-_Waterbody/Ontario_Hydro_Network_(OHN)_-_Waterbody.shp"))
  watercourse <- sf::st_read(paste0(path_to_shps, "Ontario_Hydro_Network_(OHN)_-_Watercourse/Ontario_Hydro_Network_(OHN)_-_Watercourse.shp"))
  contour <- sf::st_read(paste0(path_to_shps, "Contour/Contour.shp"))
  geology <- sf::st_read(paste0(path_to_shps, "MRD126-REVISION1/MRD126-REV1/ShapeFiles/Geology/Geopoly.shp"))
  quatgeo <- sf::st_read(paste0(path_to_shps, "EDS014-REV/GIS_DATA/Quaternary/geology_ll.shp"))
  # crownland <- sf::st_read(paste0(path_to_shps, "CrownLand_LIO-2024-12-11/XXX"))
  # landcover <- sf::st_read(paste0(path_to_shps))
  
  # Crop layers if boundaries are provided
  if (!is.null(bounds)) {
    bbox <- sf::st_bbox(bounds, crs = sf::st_crs(4269))
    
    bathymetry <- sf::st_crop(bathymetry, bbox)
    waterbody <- sf::st_make_valid(waterbody)
    waterbody <- sf::st_crop(waterbody, bbox)
    watercourse <- sf::st_crop(watercourse, bbox)
    contour <- sf::st_crop(contour, bbox)
    geology <- sf::st_crop(geology, bbox)
    quatgeo <- sf::st_make_valid(quatgeo)
    quatgeo <- sf::st_crop(quatgeo, bbox)
    # crownland <- sf::st_crop(crownland, bbox)
    # landcover <- sf::st_crop(landcover, bbox)
    
    if (save_cropped) {
      sf::write_sf(bathymetry, paste0(path_to_shps, "Cropped_maps/Cropped_bathymetry.shp"))
      sf::write_sf(waterbody, paste0(path_to_shps, "Cropped_maps/Cropped_waterbodies.shp"))
      sf::write_sf(watercourse, paste0(path_to_shps, "Cropped_maps/Cropped_watercourses.shp"))
      sf::write_sf(contour, paste0(path_to_shps, "Cropped_maps/Cropped_contours.shp"))
      sf::write_sf(geology, paste0(path_to_shps, "Cropped_maps/Cropped_geology.shp"))
      sf::write_sf(quatgeo, paste0(path_to_shps, "Cropped_maps/Cropped_quatgeology.shp"))
      # sf::write_sf(crownland, paste0(path_to_shps, "Cropped_maps/Cropped_crownland.shp"))
      # sf::write_sf(landcover, paste0(path_to_shps, "Cropped_maps/Cropped_landcover.shp"))
    }
  }
  
  return(list(ontario = ontario, 
              ecoregion = ecoregion,
              watershed_tert = watershed_tert,
              watershed_quat = watershed_quat,
              reserves = reserves, 
              bathymetry = bathymetry,
              waterbody = waterbody, 
              watercourse = watercourse, 
              contour = contour,
              geology = geology,
              quatgeo = quatgeo))
}

get_development_shapefiles <- function(bounds = NULL) {
  # TODO import all human-related layers
  aggregate <- "Aggregate_site_authorized_-_active/Aggregate_site_authorized_-_active.shp"
  landfills <- read_csv(paste0(path_to_shps, "HHLPOA_landfills.csv"), col_names = TRUE)
  roads <- "MNR_Road_Segments/MNR_Road_Segments.shp"
  dams <- "Ontario_Dam_Inventory/Ontario_Dam_Inventory.shp"
  hydro <- "Ontario_Hydro_Network_(OHN)_-_Hydrographic_Line/Ontario_Hydro_Network_(OHN)_-_Hydrographic_Line.shp"
  trails <- "Ontario_Trail_Network_(OTN)_Segment/Ontario_Trail_Network_(OTN)_Segment.shp"
  utility_lines <- "Utility_Line/Utility_Line.shp"
  waste_mgmt_site <- "Waste_Management_Site/Waste_Management_Site.shp"
  wells <- "Wells_WWIS_2025b/wwis_out.shp"
}