
#' @title Color Coding Salinity in San Francisco Bay
#'
#' @param Shapefile A shapefile of the desired area. This can be of type .shp or .RDS. Default projection is google mercator (3587).
#' @param StartPoint Start point where color coding will start. Make sure to convert coordinates to UTM from lat lon.
#' @param n get_strips(n) is the number of strips desired (defaults to 10). Usually the number of desired strips are the number of rows in 'data'
#' @param strip_width Width of strips defaults to 1000 meters (1 km). This can be changed in get_strips(strip_width = xxxx)
#' @param strip_length Length of strips defaults to 22000 meters (22 km). This can be changed in get_strips(strip_length = xxxx)
#' @param rotate_feature Rotate the strips to the desired angle (defaults to 0).
#' @param mapview/leaflet Use any interactive mapping software or ggplot2 to visualize shapefile and associated dataset.
#' @param data A dataset with 2 rows to overlay on shapefile
#' @param var The salinity value per kilometer
#' @return A shapefile associated with a dataframe
#' @import tidyverse tidyr magrittr ggplot2 dplyr sf mapview leaflet leafem
#'
#' @name get_strips
#' @examples
#' library(BD)
#' library(tidyverse)
#' library(mapview)
#' library(leaflet)
#' library(leafem)
#' library(sf)
#'
#' head(salinity)
#' start_UTM <- c(565727.42, 4213365.17) #UTM coordinates for color coding starting point.
#'
#' ##Can also use readRDS('myshape.RDS') or read_sf('myshape.shp'), or st_read('myshape.shp')
#'
#' #Some data manipulation first....
#' sf_bay <- sf_bay |> st_transform(26910)
#' a <- salinity[,2] ;a
#' b <- as.numeric(a$freq);b
#' length(b)
#'
#' #Apply the function to shapefile and dataset
#' the_strips <- get_strips(n = length(b), crs = st_crs(sf_bay)) |> rotate_feature(angle = 105, start_point = start_UTM)
#' the_strips2 <- cbind(the_strips,salinity)
#' the_strips2$freq <- as.factor(the_strips2$freq)
#' the_strips2 <- the_strips2 |> st_intersection(sf_bay)
#'
#' #Visualize the results
#' mapview(the_strips2, zcol = 'freq', layer.name = 'X2 frequency', col.regions= pals::brewer.blues(7))

#' @export

library(sf)
library(mapview)
library(leaflet)
library(tidyverse)
library(leafem)

mapviewOptions(fgb = FALSE) #Set this to false if using the Rconsole with tinn-r and not using Rstudio

##function to create each color coded strip
get_strips <- function(n = 10, ## default to 10 adjacent strips
                       strip_width = 1000,    #1e3, #unit: m default to 1 km
                       strip_length = 22000,   #strip_length = 1e5,   #9000 Works good
                       crs = 3857) ## default: 'google' mercator
                       {
  ## make horizontal line of length 'strip_length':
  baseline <- matrix(c(-strip_length/2, strip_length/2, 0, 0), ncol = 2) |> st_linestring()
  ## make line a polygon of width 'strip_width'
  basestrip  <- baseline |> st_buffer(strip_width/2, endCapStyle = 'FLAT')
  ## return a spatial dataframe of n adjacent strips from north to south or left to right:
  st_sf(strip_id = sprintf('strip_%02.f', 1:n),
  geometry = Map(1:n,f = \(index) basestrip - c(0, index * strip_width - strip_width/2))|>
          st_sfc() |> st_cast('MULTIPOLYGON') |> st_set_crs(crs))
         }

#function to rotate the geometry of a spatial dataframe
    rotate_feature <- function(feature,
                               angle = 0, ## rotation in degree
                               start_point = c(0, 0)) ## top center point of grid
                               {
      crs <- st_crs(feature)
      a <- -pi * angle/180 ## convert to radiant ccw
      ## create rotation matrix:
      rotmat <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
      ## rotate, recenter, restore CRS:
      st_geometry(feature) <- st_geometry(feature) * rotmat + start_point
      feature |> st_set_crs(crs)
    }
