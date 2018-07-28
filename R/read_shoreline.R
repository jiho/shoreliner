#' Read shoreline from GSHHG
#'
#' @param path path to the GSHHG data. By defautl this is set by an option. If the data is not found, it will be downloaded.
#' @param resolution resolution at which the data should be read; either the full name, an abbreviation, or a number.
#' \itemize{
#' \item 1 : cruise (c)
#' \item 2 : low (l)
#' \item 3 : intermediate (i)
#' \item 4 : high (h)
#' \item 5 : full (f)
#' }
#' @param level level \strong{down to} which the data should be read
#' \itemize{
#' \item 1 : boundary between land and ocean
#' \item 2 : boundary between lake and land
#' \item 3 : boundary between island-in-lake and lake
#' \item 4 : boundary between pond-in-island and island
#' }
#'
#' @return A "simple feature" (\code{\link[sf]{sf}}) object.
#'
#' @export
#'
#' @examples
#' shorepath <- system.file("extdata", ".", package="shoreliner")
#' # NB: this is a very reduced version of the data, only for tests
#'
#' # various abbreviations can be used
#' w <- read_shoreline("cruise", 1, path=shorepath)
#' w <- read_shoreline("c", 1, path=shorepath)
#' w <- read_shoreline(1, 1, path=shorepath)
#' plot(w, max.plot=1)
#'
#' # the resulting objet can be manipulated with all the functions in sf
#' sf::st_bbox(w)
#' plot(sf::st_crop(w, xmin=-10, xmax=50, ymin=30, ymax=70), max.plot=1)
#' # etc.
read_shoreline <- function(resolution, level, path=getOption("shoreline.path")) {
  # check arguments
  # resolution can be specified either as an abbreviation or as a number
  resolutions <- c("cruise", "low", "intermediate", "high", "full")
  if (is.numeric(resolution)) {
    if (resolution < 1 | resolution > 5) {
      stop("'resolution' should be between 1 and 5")
    } else {
      resolution <- resolutions[resolution]
    }
  } else {
    resolution <- match.arg(resolution, resolutions, several.ok=FALSE)
  }
  # abbreviate resolution
  resolution <- substr(resolution, 1, 1)

  if (! level %in% 1:4 ) {
    stop("`level` should be either 1, 2, 3, or 4.")
  }
  level <- 1:level
  # NB: when level 3 is requested, it actually means levels 1 to 3

  # list shapefiles to be read
  shapes <- paste0(path, "/GSHHS_shp/", resolution, "/GSHHS_",resolution, "_L", level, ".shp")

  # test if the data is there and if it is not, download and unzip it
  missing <- shapes[!file.exists(shapes)]
  if ( length(missing) > 0 ) {
    message("Cannot find files\n  ", paste(missing, collapse="\n  "))
    message("GSHHG data will be downloaded again in ", path)
    download_gshhg(path=path)
  }

  # read all shapefiles in a list
  x <- lapply(shapes, sf::st_read, quiet=TRUE)
  # and combine them
  x <- do.call(rbind, x)

  return(x)
}
