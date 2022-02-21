#' Generate a list of HTML tags to show stacked PNGs
#'
#' @param img_fns A vector of image files names
#' @param display_first Display the first image initially
#' @param img_class The class that image tags will be assigned
#' @param center Center the DIV on the document
#' @param border A valid CSS style for the DIV border
#'
#'
#' @details This will return a list of HTML tags that when inserted into a R markdown HTML document
#' will display the PNG files as a build. You can use this function within code chunks
#' in R markdown to create a build effect from a set of PNG files. The PNG
#' files will be added to the DIV in the order received. All images should have the same
#' dimensions, and images after the first one should either have transparent backgrounds
#' or contain the earlier elements in them.
#'
#' To use this function, add the following style to your Rmarkdown document:
#'
#' \preformatted{
#' ```{css echo = FALSE}
#' img.stackme {
#'   position:absolute;
#'   top:0;
#'   left:0;
#' }
#' ```
#' }
#'
#' Create the build with:
#'
#' \preformatted{
#' ```{r echo = FALSE, results = "asis"}
#' img_fns <- paste0("./images/rnotebooks (", 1:9, ").png")
#' htmltools::tagList(wrkshputils::wu_img_build(img_fns, display_first = TRUE, center = FALSE))
#' ```
#' }
#'
#' @importFrom htmltools img HTML
#' @importFrom magick image_read image_info
#' @export

wu_img_build <- function(img_fns, display_first = TRUE, img_class = "stackme", center = FALSE,
                         border=c("none", "1px solid gray")[1]) {

  if (FALSE %in% file.exists(img_fns)) stop("Image file(s) not found")

  ## Get the dimensions of all the images as a data frame
  prp_df <- do.call(rbind, lapply(img_fns, function(x) image_info( image_read(x))[ , c("width","height")]))
  h_max <- max(prp_df$height)

  if (center) {
    center_style <- paste0("width:", max(prp_df$width), "px; margin:0 auto; ")
  } else {
    center_style <- ""
  }

  res <- list(HTML(paste0("<div style='position:relative; border:", border, "; height:", h_max, "px;",
                          center_style, "'>")))

  if (display_first) {
    res[[length(res) + 1]] <- img(src = img_fns[1])
    start_idx <- 2
  } else {
    start_idx <- 1
  }

  for (i in start_idx:length(img_fns)) {
    res[[length(res) + 1]] <- img(src = img_fns[i], class = paste("incremental", img_class))
  }

  res[[length(res) + 1]] <- HTML("</div>")

  invisible(res)

}
