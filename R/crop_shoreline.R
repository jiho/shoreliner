#' Crop shorelines
#'
#' Crop shorelines, dealing with crossing of the date line, and return a data.frame
#'
#' @param x shoreline data, output by \code{\link{read_shoreline}}.
#' @param w,e,s,n coordinates of the region to crop.
#'
#' @return The shorelines as a data.frame, with columns lon and lat
#'
#' @export
#'
#' @examples
#' # NB: read a very reduced version of the data, for tests
#' shorepath <- system.file("extdata", ".", package="shoreliner")
#' w <- read_shoreline("c", 1, path=shorepath)
#' plot(w, max.plot=1)
#'
#' head(crop_shoreline(w, -50, 50, -50, 50))
#'
#' plot(crop_shoreline(w), type="l")
#' plot(crop_shoreline(w, -50, 50, -50, 50), type="l")
#' plot(crop_shoreline(w, -170, 170, -50, 50), type="l")
#' plot(crop_shoreline(w, 140, 290, -50, 0), type="l")
#' plot(crop_shoreline(w, -200, -140, 40, 90), type="l")
#' \dontrun{
#' ggplot(crop_shoreline(w, 140, 290, -50, 0)) +
#'   geom_polygon(aes(lon, lat)) + coord_map()
#' }
crop_shoreline <- function(x, w=-180, e=180, s=-90, n=90) {
  wrap <- FALSE
  if (w < -180 | e > 180) {
    wrap <- TRUE
  }

  # check range of inputs
  w <- ifelse(w < -180, w %% 180, w)
  e <- ifelse(e > 180, -180 + (e %% 180), e)

  # convert to geometry (to be able to shift coordinates pas the date line)
  xg <- sf::st_geometry(x)
  if (wrap) {
    # convert to [0,360]
    xg <- (xg + c(360,90)) %% c(360) - c(0,90)
    # add a non-existent buffer just to avoid problems when croping geometries that are supperposed at the date line
    xg <- sf::st_buffer(xg, 0)
    w <- w %% 360
    e <- e %% 360
  }
  # then crop
  xc <- suppressMessages(sf::st_crop(xg, xmin=w, xmax=e, ymin=s, ymax=n))

  # extract coordinates and prepare them for plotting
  coords <- as.data.frame(do.call(rbind, lapply(xc, function(x) {
    rbind(sf::st_coordinates(x)[,1:2], NA)
  })))
  names(coords) <- c("lon", "lat")

  return(coords)
}

