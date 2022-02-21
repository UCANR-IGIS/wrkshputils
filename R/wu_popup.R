#' Plot a png or jpg image file
#'
#' @param x A filename or URL of a PNG or JPG image
#' @param height The (minimum) height of the viewer pane
#' @param bgcol Background color
#'
#' @details This will display a PNG or JPG image in the RStudio viewer pane.
#' \code{x} can be either a local file or a URL. This gives you the ability to
#' create 'popup' slides in a R script that you work through during a lesson.
#'
#' \code{height} determines the minimum height in screen pixels that the viewer pane will
#' resize itself to (if needed). Set to \code{NULL} to disable.
#'
#' \code{bgcol} can be a named HTML color (e.g., \code{gray}) or a hexadecimal HTML color value (e.g., \code{#FF0034}).
#'
#' @examples
#' \dontrun{
#' wu_popup("http://placekitten.com/400/300")
#' }
#'
#' @importFrom utils browseURL
#' @importFrom tools file_ext
#' @importFrom magrittr "%>%"
#' @export

wu_popup <- function(x, height = 360, bgcol = "#222") {

  if (grepl("^http", x, ignore.case = TRUE)) {
    ## This is a URL
    img_url <- x

  } else {
    ## This is a local image file
    if (!file.exists(x)) stop(paste0("Can't find ", x))

    ## Check the file name extension
    img_ext <- file_ext(x)
    if (!tolower(img_ext) %in% c("png", "jpg", "jpeg", "svg")) stop("Supported image formats include png, jpg, and svg files.")

    ## Create a file name in the temp dir
    img_tmp_fn <- tempfile(fileext = paste0(".", img_ext))

    ## Copy the file over to the temp dir (will be deleted when the R session closes)
    file.copy(x, img_tmp_fn)

    ## Save the basename as the 'URL' that will get inserted into the HTML template
    img_url <- basename(img_tmp_fn)

  }

  ## Get the viewer function. If not found (i.e., b/c we're not in RStudio) set it to NA
  viewer <- getOption("viewer", default = NA)

  ## Get the popup.html template
  popup_templt_fn <-system.file("popup.html", package="wrkshputils")
  popup_orig_chr <- readLines(popup_templt_fn)

  ## Swap in the URL and background color
  popup_swpd_chr <- popup_orig_chr %>%
    gsub("<img-url-here>", img_url, ., ignore.case = TRUE) %>%
    gsub("<bgcol>", bgcol, ., ignore.case = TRUE)

  ## Save the swapped HTML file to the temp dir
  popup_tmp_fn <- tempfile(fileext = ".html")
  cat(popup_swpd_chr, file = popup_tmp_fn)

  ## Open in the viewer
  if (identical(viewer, NA)) {
    utils::browseURL(popup_tmp_fn)
    ## Alternately we could use plot(magick::image_read(img_path_fn))
  } else {
    viewer(popup_tmp_fn, height = height)
  }

}
