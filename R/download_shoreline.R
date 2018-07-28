#' Download GSHHG data
#'
#' Download GSHHG data as shapefiles and uncrompress them.
#'
#' @param path path to a directory where the data will be downloaded. By default the current directory.
#' @param set.options logical, when TRUE the path to the data is set as an option in ~/.Rprofile, so that new R sessions can find it. By default, the user is prompted in interactive sessions or it is set to `FALSE` in non-interactive ones.
#' @param server string identifying the server to download the data from; either `noaa` or `hawaii`.
#'
#' @return The path to the uncompressed data, invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' download_shoreline()}
download_shoreline <- function(path=".", set.options=NULL, server="noaa") {
  # choose server
  server <- match.arg(tolower(server), c("noaa", "hawaii"))
  if (server == "noaa") {
    server <- "https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/"
  } else {
    server <- "http://www.soest.hawaii.edu/pwessel/gshhg/"
  }

  # download and unzip data
  message("Downloading data (this may take a while)...")
  path <- normalizePath(path)
  destfile <- paste0(path, "/gshhg-shp.zip")
  download.file(url=paste0(server, "gshhg-shp-2.3.7.zip"), destfile=destfile)
  message("Uncompressing data...")
  path <- paste0(path, "/gshhg-shp/")
  unzip(destfile, exdir=path)

  # set options
  opt <- paste0("options(shoreline.path=\"", path, "\")")
  # if nothing is set, ask the user
  if (is.null(set.options)) {
    if (interactive()) {
      cat("You should set\n", opt, "\nin your ~/.Rprofile to avoid downloading the data again.")
      set.options <- "foo"
      while (! set.options %in% c("y", "n")) {
        set.options <- readline("Should I do it for you? [Y,n] ")
        set.options <- tolower(set.options)
      }
      if (set.options == "y") {
        set.options <- TRUE
      } else {
        set.options <- FALSE
      }
    } else {
      set.options <- FALSE
    }
  }
  # set the option in Rprofile if necessary
  if (set.options) {
    cat(opt, file=file.path(Sys.getenv("HOME"), ".Rprofile"), append=TRUE)
  }
  # make it active for the current session at least
  eval(parse(text=opt))

  return(invisible(path))
}
