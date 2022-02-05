#' Plot a png or jpg image file
#'
#' @param x A filename or URL to a PNG or JPG image
#'
#' @details This will plot a PNG or JPG image. \code{x} can be either a local file or a
#' URL. This gives you the ability to create 'popup' slides in a R script that you
#' work through during a lesson.
#'
#' @importFrom imager load.image
#' @importFrom utils download.file
#' @importFrom tools file_ext
#' @importFrom graphics par
#' @export


wu_popup <- function(x) {

  ## Identify the extension of the URL
  img_ext <- file_ext(x)
  if (!tolower(img_ext) %in% c("png", "jpg", "jpeg")) stop("Supported image formats include png and jpg files.")

  if (grepl("^http", x, ignore.case = TRUE)) {

    ## Create a temporary file name
    img_fn <- tempfile(fileext = paste0(".", img_ext))

    cat("Going to create", img_fn, "\n")
    download.file(x, img_fn, quiet = TRUE, mode = "wb")
    if (file.exists(img_fn)) {
      on.exit(unlink(img_fn))
    } else {
      stop("Could not download image file")
    }

  } else {
    img_fn <- x
    if (!file.exists(img_fn)) stop(paste0("Can't find ", x))
  }

  ## At this point, we have a valid local image file
  img_cimg <- load.image(img_fn)

  ## Change some plot settings but save the current one and queue them up to be restored
  opar <- par(mar = c(0,0,0,0), bg="gray40")
  on.exit(par(opar), add = TRUE)

  ## Draw the plot (finally!)
  plot(img_cimg, axes = FALSE)

  ## OK to exit. Par should get reset and the temp file deleted

}
